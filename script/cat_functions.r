panel_approx <- function(y, timevar, indexvar){
    # na.approx for panel data: 
    # does not interpolate between observations in two different countries etc.

    out <- y
    for (i in unique(indexvar)){
        if (sum(!is.na(y[indexvar==i])) > 1){
            out[indexvar==i] <- zoo::na.approx(object=y[indexvar==i], x=timevar[indexvar==i], na.rm=F)
        }
      }
  return(out)
}
find_duplicated_ids = function(ids, pattern='_'){
    ids_duplicated = ids[grepl(pattern, ids)]
    ids_duplicated = c(ids_duplicated, gsub(paste0(pattern, '.*'), '', ids_duplicated))
    return(sort(ids_duplicated))
}

to_city_obs = function(statobs, fullobs, res=100){
    data.table::setkey(statobs, osmid)
    data.table::setkey(fullobs, osmid, year)
    citobs = statobs[fullobs]

    citobs[, century:=ceiling(year/res)*res]
    citobs[, year:=round(year/res)*res]
    
    out = citobs[, list(im2_cnt=sum(im2_ann, na.rm=T), 
        im3_cnt=sum(im3_ann, na.rm=T)), by=list(city, year)]
    
    return(out)
}

to_annual_obs = function(dyn, churchlist){
    full = as.data.table(expand.grid(year=700:1800, osmid=names(churchlist), stringsAsFactors=FALSE))
    data.table::setkey(full, osmid, year)
    data.table::setkey(dyn, osmid, year)
    full = dyn[full]

    full = full[order(osmid, -year), ]
    full[, m2obs:=sum(!is.na(m2)), by=osmid]
    full[, hgtobs:=sum(!is.na(hgt)), by=osmid]
    full[, m3obs:=sum(!is.na(m3)), by=osmid]
    full = full[!(m2obs <=1 | hgtobs <=1 | m3obs <= 1), ]

    full[m2obs > 1, im2:=zoo::na.approx(m2, method='constant', na.rm=F), by=osmid]
    full[hgtobs > 1, ihgt:=zoo::na.approx(hgt, method='constant', na.rm=F), by=osmid]
    full[m3obs > 1, im3:=zoo::na.approx(m3, method='constant', na.rm=F), by=osmid]

    full[m2obs > 1, iphaselength:=zoo::na.approx(phaselength, method='constant', na.rm=F, rule=1:2), by=osmid]
    full[, irestphase:=zoo::na.approx(restphase, method='constant', na.rm=F, rule=1, f=0), by=osmid]
    full[, im2_ann:=im2/iphaselength]
    full[irestphase==1, im2_ann:=0]
    full[firstobs==TRUE & !is.na(firstobs), im2_ann:=0]

    # check m3 treatment
    full[, im3_ann:=im3/iphaselength]
    full[irestphase==1, im3_ann:=0]
    full[firstobs==TRUE & !is.na(firstobs), im3_ann:=0]
    
    full = full[order(osmid, year), ]
    full[, ibldindex:=zoo::na.approx(bldindex, method='constant', rule=2:1, na.rm=F), by=osmid]
    full[, osmid_buildindex:=paste(osmid, ibldindex, sep='_')]
    full[!is.na(im2_ann), im2_cml:=cumsum(im2_ann), by=osmid_buildindex]

    return(full)
}

to_dynobs = function(churchlist){
    dyn = do.call(rbind, lapply(churchlist, function(x) x$dynamic))
    dyn = data.table::as.data.table(dyn)
    # dyn = dyn[!is.na(m2), ][order(osmid, year)]
    dyn = dyn[!is.na(m2) | !is.na(year), ][order(osmid, year)]

    dyn[, m3:=m2*hgt]
    dyn[which(diff(year)==0) + 1, year:=year + 1, by=osmid]
    dyn[, nobs:= sum(!is.na(m2)), by=osmid]
    
    dyn[nobs > 2, phase:=1:length(m2), by=osmid]
    dyn[nobs <= 2, phase:=as.integer(1), by=osmid]
    dyn[, phaselength:=c(NA, diff(year)), by=osmid]
    dyn[, firstobs:=year==min(year), by=osmid]
    dyn[, restphase:=is.na(m2)]

    # building index for interpolation m2
    dyn[m2==0, bldindex:=1:length(m2), by=osmid]
    dyn[, nbblds:=sum(!is.na(bldindex)), by=osmid]
    dyn[nbblds > 0, bldindex:=as.integer(zoo::na.approx(bldindex, method="constant", na.rm=F, rule=1:2)), by=osmid]
    dyn[nbblds==0, bldindex:=1]
    dyn[is.na(bldindex), bldindex:=0]

    return(dyn)
}

check_order = function(dyn){
    # data.table works by reference, so copy to prevent global scope in functions!
    dyn = copy(dyn)
    dyn[order(osmid, year), dyear:=c(0, diff(year)), by=osmid]
    cat('reversed:')
    print(dyn[sort(c(which(dyear < 0), which(dyear <0) - 1)), ])
}

check_osm_total = function(full, churchlist){
    osm_surfaces = sapply(churchlist, function(x) x$static$surface)
    eb_surfaces = as.data.frame(full[!is.na(im2_cml), im2_cml[length(im2_cml)], by=osmid])
    eb_surfaces$osm_srfc = as.numeric(osm_surfaces[eb_surfaces$osmid])
    plot(V1 ~ osm_srfc, data=eb_surfaces)
    fit = lm(V1 ~ osm_srfc, data=eb_surfaces)
    abline(fit)
    outl = names(sort(-(fit$resid^2)))[1:15]
    outl = unique(c(outl, which((eb_surfaces$osm_srfc / eb_surfaces$V1) > 2 | (eb_surfaces$osm_srfc / eb_surfaces$V1) < 0.5)))
    return(eb_surfaces[outl, ])
}

check_annual_m2 = function(full, n=10){
    plot(unique(full[im2_ann > 0, list(im2_ann, iphaselength)]), log='xy')
    highann = unique(full[!is.na(im2_ann), list(osmid, im2_ann, iphaselength)][order(-im2_ann), ])[1:n, osmid]
    highann = c(highann, unique(full[!is.na(im2_ann) & iphaselength < 10, list(osmid, im2_ann, iphaselength)][order(-im2_ann), osmid][1:n]))
    outltest = car::outlierTest(lm(im2_ann ~ 1 , data=full))
    highann = c(highann, full[as.numeric(attr(outltest$rstudent, "names")), osmid])

    return(unique(highann))
}

checks = function(dyn, full, churchlist){
    # prb_osmids = list()

    m2miss = dyn[, all(is.na(m2)), by=osmid][V1==TRUE, ]
    cat('\n\nall m2 missing:\n')
    print(m2miss)
    # prb_osmids$m2miss = m2miss$osmid

    cat('\n\nall hgt missing:\n')
    hgtmiss = dyn[, all(is.na(hgt)), by=osmid][V1==TRUE, ]
    print(hgtmiss)
    # prb_osmids$hgtmiss = hgtmiss$osmid

    cat('\n\norder incorrect:\n')
    orderwrong = check_order(dyn)
    print(orderwrong)
    # prb_osmids$orderwrong = orderwrong$osmid

    cat('\n\nosm total and final series total:\n')
    totaloutl = check_osm_total(full, churchlist)
    print(totaloutl)
    # prb_osmids$totaloutl = totaloutl$osmid

    cat('\n\nhighest m2/annum:\n')
    ann2high = check_annual_m2(full)
    print(ann2high)
    # prb_osmids$ann2high = ann2high

    prb_osmids = list(m2miss=m2miss$osmid, hgtmiss=hgtmiss$osmid, 
        orderwrong=orderwrong$os, totaloutl=totaloutl$osmid, ann2high=ann2high)
    return(prb_osmids)
}

recombine_churches = function(churches, guesses=NULL){
    fill = list()
    for (id in unique(churches$osmid[churches$osmid!=''])){
        church = as.data.frame(churches[osmid==id, ])
        # dynstrt = which(church[2, 5:ncol(church)]!='' & !is.na(church[2, 5:ncol(church)]))
        # if (length(dynstrt)==0) next
        # dynvrbs = dynstrt[1]:ncol(church)
        dynvrbs = 5:ncol(church)
        temp = data.frame(osmid=rep(id, length(dynvrbs)), 
                          year=integer(length(dynvrbs)),
                          m2=integer(length(dynvrbs)),
                          hgt=integer(length(dynvrbs)),
                          gss_hgt=logical(length(dynvrbs)),
                          gss_m2=logical(length(dynvrbs)))
        temp$year = unlist(as.numeric(church[2, dynvrbs]))
        temp$m2 = unlist(as.numeric(church[3, dynvrbs]))
        temp$hgt = unlist(as.numeric(church[4, dynvrbs]))
        if (is.null(guesses)){
            temp$gss_m2 = unlist(as.logical(as.numeric(church[6, dynvrbs])))
            temp$gss_hgt = unlist(as.logical(as.numeric(church[6, dynvrbs])))
        } else{
            temp$gss_hgt = unlist(guesses[osmid==id, ][4, dynvrbs, with=F]) == "guestimate"
            temp$gss_m2 = unlist(guesses[osmid==id, ][3, dynvrbs, with=F]) == "guestimate"            
        }
        temp = temp[!(is.na(temp$year) & is.na(temp$m2) & is.na(temp$hgt)), ]

        fill[[id]][["static"]] = church[1, -grep('V\\d+', names(churches))]
        fill[[id]][["dynamic"]] = temp
    }
    return(fill)
}

write_filltable = function(dat, outfile, 
    baseinfo=c("osmid", "city", "osmname", "surface", "osmwikipedia", "osmlink", "lat", "lon"),
    fillvrbs=c("year", "surface", "height", "m3", "guestimate")){
    dat@data[, paste0('yr', sprintf("%o2d", 1:20))] = "x"
    write.table(dat@data[0, baseinfo], file=outfile, sep=',')
    for (rw in 1:nrow(dat@data)){
        write.table(dat@data[rw, c(baseinfo, grepr("yr", names(dat)))], 
            file=outfile, append=T, col.names=F, row.names=F, sep=',')
        for (fillvrb in fillvrbs){
            write.table(cbind(dat@data[rw, c("osmid", "city", "osmname")], fillvrb), 
                file=outfile, append=T, col.names=F, row.names=F, sep=',')
            
        }
        # write.table(cbind(dat@data[rw, c("city", "osmname")], fillvrb), 
        #     file=outfile, append=T, col.names=F, sep=',')
        # write.table(cbind(dat@data[rw, c("city", "osmname")], fillvrb), 
        #     file=outfile, append=T, col.names=F, sep=',')
        # write.table(cbind(dat@data[rw, c("city", "osmname")], fillvrb), 
        #     file=outfile, append=T, col.names=F, sep=',')
        # write.table(cbind(dat@data[rw, c("city", "osmname")], fillvrb), 
            # file=outfile, append=T, col.names=F, sep=',')
        write.table('', file=outfile, append=T, row.names=F, col.names=F, sep=',')
    }
}

get_osm_data_city = function(cty, what='way', radius=5){
    cat(cty$city)
    topo = get_osm_all_churches_rect(
        lat1=cty$lat - km2lat(radius),
        lat2=cty$lat + km2lat(radius),
        lon1=cty$lon - km2lon(radius, lat=cty$lat),
        lon2=cty$lon + km2lon(radius, lat=cty$lat),
        what=what)

    if (dim(topo)["ways"] == 0){
        cat("no results\n")
        return(NULL)
    }

    polys = osmar::as_sp(topo, what="polygons")
    
    if (is.null(polys)){
        cat("no polys in ways\n")
        return(NULL)
    }

    tags = get_osm_tags(topo, what=what)
    rownames(tags) = tags$osmid

    if (what=="relation"){
        rel_refs = topo$relations$refs[topo$relation$refs$ref %in% polys@data$id, ]

        polys = polys[match(rel_refs$ref, polys@data$id),] 
        # rel_refs = topo$relations$refs[match(polys@data$id, topo$relations$refs$ref), ]
        # polys = aggregate(polys, by=list(rel_refs$id), FUN=function(x) x[1], dissolve=FALSE)
        # polys = aggregate(polys, by=list(rel_refs$id), FUN=mean, dissolve=TRUE)
        # polys@data = data.frame(polys@data, tags[as.character(polys$Group.1), ])
        polys@data = data.frame(polys@data, tags[as.character(rel_refs$id), ])
        polys@data$id = rel_refs$ref
        polys@data$role = rel_refs$role
    } else {
        polys@data = data.frame(polys@data, tags[as.character(polys$id), ])
    }
    polys@data$city = cty$city
    return(polys)
}
get_osm_data_church = function(osmid, what=c("way", "relation")){
    # function for aditional single additions to the data
    # would also work with direct osmar item
    # also takes bounding box, but no obvious way 

    if (what=="way") topo = osmar::get_osm(osmar::way(osmid), full=TRUE)
    if (what=="relation") topo = osmar::get_osm(osmar::relation(osmid), full=TRUE)
    polys = osmar::as_sp(topo, what="polygons")
    tags = get_osm_tags(topo, what=what)
    
    rownames(tags) = tags$osmid
    if (what=="relation"){
        rel_refs = topo$relations$refs[topo$relation$refs$ref %in% polys@data$id, ]

        polys = polys[match(rel_refs$ref, polys@data$id),] 
        # shorten, see previous?
        polys = aggregate(polys, by=list(rel_refs$id), dissolve=FALSE)
        polys@data = data.frame(polys@data, tags[as.character(polys$Group.1), ])
        polys@data$id = rel_refs$ref
        polys@data$role = rel_refs$role
    }
    if (what=="way"){
        polys@data = tags
    }
    return(polys)
}
get_osm_tags <- function(topo, what="way"){
    # make sure it can handle relations as well
    # rbind topo$ways$tags with topo$ways$relations
    osmtags <- rbind(topo$ways$tags, topo$relations$tags)
    # and check uniqueness
    osmtags <- reshape(osmtags, direction='wide', idvar='id', timevar='k', v.names='v')
    osmtags <- factor2char(osmtags)
    
    names(osmtags) <- gsub("v[.]", "", names(osmtags))
    names(osmtags) <- paste0('osm', names(osmtags))
    # osmtags <- osmtags[osmtags$id %in% polyids, ]
    
    vrbs <- c("osmid", "osmamenity", "osmbuilding", "osmdenomination", 
              "osmheritage",  "osmname", "osmreligion", "osmwikipedia")
    vrbs <- vrbs[vrbs %in% names(osmtags)]
    osmtags <- osmtags[, vrbs, drop=F]

    if (! 'osmwikipedia' %in% names(osmtags)) osmtags[, 'osmwikipedia'] <- ''
    osmtags$osmwikipedia <- gsub(':', '.wikipedia.org/wiki/', osmtags$osmwikipedia)
    osmtags$osmwikipedia <- gsub(' ', '_', osmtags$osmwikipedia)

    osmtags$osmlink <- paste0('http://www.openstreetmap.org/', what, '/', osmtags$osmid)
    
    return(osmtags)
}


polylist2df = function(polylist, what="way"){
    polys = polylist[[1]]
    
    # if (what=="relation"){}
    rownames(polys@data)  = sapply(slot(polys, "polygons"), function(x) slot(x, "ID"))
    if (length(polylist) == 1){
        return(polylist[[1]])
    }
    for (i in 2:length(polylist)){
        polys4merge = polylist[[i]]
        duplpolys = polys4merge$id %in% polys$id
        polys4merge$id[duplpolys] = paste0(polys4merge$id[duplpolys], '_', i)
        polys4merge = sp::spChFIDs(polys4merge, as.character(polys4merge$id))
        row.names(polys4merge@data) = as.character(polys4merge$id)

        # fix missing variables between spdfs
        polys4merge@data[, setdiff(names(polys), names(polys4merge))] = NA
        polys@data[, setdiff(names(polys4merge), names(polys))] = NA
        # sapply(slot(polys, "polygons"), function(x) slot(x, "ID"))
        # rownames(polys@data)
        # sapply(slot(polys4merge, "polygons"), function(x) slot(x, "ID"))
        polys@data$timestamp = polys4merge@data$timestamp = character(1)
        polys = maptools::spRbind(polys, polys4merge)
    }
    return(polys)
}

plot_churches_by_city = function(polylist, siem){
    for (cityname in names(polylist)){
        plot(polylist[[cityname]], lwd=0.2, border=2)
        title(main=cityname)
        abline(v=siem[city==cityname, lon], h=siem[city==cityname, lat], lwd=0.5, col="gray")
        text(x=coordinates(polylist[[cityname]])[, 1],
             y=coordinates(polylist[[cityname]])[, 2] + 0.001,
             labels=polylist[[cityname]]@data$osmname, cex=0.2)
    }
}
aggregate_multipolys = function(polys){
    polys@data$lon = tapply(polys@data$lon, polys@data$Group.1, mean)[as.character(polys@data$Group.1)]
    polys@data$lat = tapply(polys@data$lat, polys@data$Group.1, mean)[as.character(polys@data$Group.1)]
    polys@data$surface = tapply(polys@data$surface, polys@data$Group.1, sum)[as.character(polys@data$Group.1)]
    out = aggregate(polys, by=list(polys$Group.1), FUN=function(x) x[1])
    # caution: used to be aggregate spdf and returned spdf, now requires FUN= to do that
    # out = aggregate.data.frame(polys, by=list(polys$Group.1), `[`, 1)
    return(out)
}

filter_prox = function(ctr_gcd, siem_ctr){
    failed = sapply(ctr_gcd, nrow) > 1
    ctr_2nd = ctr_gcd
    for (i in 1:sum(failed)){
        dsts = sp::spDistsN1(
            as.matrix(ctr_gcd[failed][[i]][, c("lon", "lat")]),
            as.matrix(siem_ctr[failed, ][i, c("east", "north")]), longlat=T)
        if (min(dsts) < 2){
            ctr_gcd[failed][[i]] = ctr_gcd[failed][[i]][which.min(dsts), ]
        } else {
            cat(siem_ctr$city[failed][[i]], 'failed\n\n')
        }
    }
    return(ctr_gcd)
}
check_geocodes = function(siem_ctr, ctr_gcd){
    cmpr = data.frame(siem_ctr, do.call(rbind, ctr_gcd))
    dstmat = sp::spDists(
        as.matrix(cmpr[, c("lon", "lat")]),
        as.matrix(cmpr[, c("east", "north")]), longlat=T)
    cmpr$distance = diag(dstmat)
    return(cmpr)
}
grepr <- function(pattern, x, ...){
    idx <- grep(pattern, x, ...)
    return(x[idx])
}
gregexprr <- function(pattern, string){
    # return all string matches of a regular expression
    # todo: check whether/how it work on multiple strings at once

    rgx <- gregexpr(pattern, string)
    out <- substring(string, rgx[[1]], rgx[[1]] + attr(rgx[[1]], 'match.length') - 1)
    return(out)
}

overpass_baseurl <- "http://overpass-api.de/api/interpreter?data="

factor2char <- function(dat){
    factors <- sapply(dat, class) == 'factor'
    dat[factors] <- sapply(dat[factors], as.character)
    return(dat)
}
km2lat <- function(km){
    km / 110.574
}
km2lon <- function(km, lat){
    km / (111.320 * cos(lat * (pi/180)))
}





get_osm_churches_rect <- function(lat1, lon1, lat2, lon2){
    basequery <-   '[out:xml][timeout:900];
        (
            node["building"="church"] %1$s;
            way["building"="church"] %1$s;
            relation["building"="church"] %1$s;
            node["building"="cathedral"] %1$s;
            way["building"="cathedral"] %1$s;
            relation["building"="cathedral"] %1$s;
        );
        out body;
        >;
        out skel qt;'
    
    bounding <- paste0('(',paste(c(lat1, lon1, lat2, lon2), collapse=','),')')
    qry <- sprintf(basequery, bounding)
    
    request <- URLencode(paste0(overpass_baseurl, qry))
    reqtime <- system.time(result <- readLines(request))
    if (sum(reqtime) > 900) warning('timeout exceeded')
    
    topo <- osmar::as_osmar(XML::xmlParse(result))
    cat('request returned', nrow(topo$ways[[1]]), 'ways', '\n')

    return(topo)
}

get_osm_all_churches_rect <- function(lat1, lon1, lat2, lon2, what="way"){
    # make this handle relations as well
    basequery <-   '[out:xml][timeout:900];
        (
            %2$s["building"="church"] %1$s;
            %2$s["building"="chapel"] %1$s;
            %2$s["amenity"="place_of_worship"] %1$s;
            %2$s["building"="cathedral"] %1$s;
        );
        out body;
        >;
        out skel qt;'
    
    bounding <- paste0('(',paste(c(lat1, lon1, lat2, lon2), collapse=','),')')
    qry <- sprintf(basequery, bounding, what)
    
    request <- URLencode(paste0(overpass_baseurl, qry))
    reqtime <- system.time(result <- readLines(request))
    if (sum(reqtime) > 900) warning('timeout exceeded')
    
    topo <- osmar::as_osmar(XML::xmlParse(result))
    cat('request returned', nrow(topo$ways[[1]]), 'ways', '\n')

    return(topo)
}

geocode <- function(loc, reg='', bounds=''){
    # barebones version of  geocode function on:
    # https://github.com/dkahle/ggmap

    require(jsonlite)
    if (length(loc) > 1) loc <- loc[1] # geocode api takes only one location
    loc <- loc
    base <- 'http://maps.googleapis.com/maps/api/geocode/json?address='
    request <- paste0(base, loc, '&region=', reg)
    request <- paste0(request, '&bounds=', bounds)
    request <- URLencode(request)
    result <- readLines(url(request))
    closeAllConnections()
    Sys.sleep(0.2) # max 5 calls per sec
    result <- paste(result, collapse='')
    result <- fromJSON(result)
    if (result$status != 'OK'){
        out <- data.frame(loc=loc, lat=NA, lon=NA, loc_frmtd=NA)
        return(out)
    } else {
        out <- data.frame(loc=loc,
                   lat=result$results$geometry$location$lat, 
                   lon=result$results$geometry$location$lng,
                   loc_frmtd=result$results$formatted_address,
                   stringsAsFactors=FALSE)
        return(out)
    }
}

distmatch <- function(ll1, ll2, maxdist=0.1){
    distm <- sp::spDists(ll1, ll2, longlat=T)
    ids <- which(distm < maxdist)
    rws <- row(distm)[distm < maxdist]
    cls <- col(distm)[distm < maxdist] # get rid of duplicated in polys, not in james

    dd <- data.frame(ll1=rws, ll2=cls, dist=distm[ids])
    if (anyDuplicated(dd$ll1)) warning('multiple churches in radius')
    dd <- do.call(rbind, lapply(split(dd, dd$ll1), function(x) x[which.min(x$dist), ]))

    return(dd)
}

###### old ######
#---------------#
# get_osm_data = function(siem, what='way', sleep=20, radius=5){
#     polylist = list()
#     for (i in 1:nrow(siem)){
#         cty = siem[i, city]
#         cat(cty, ' - ')

#         topo = get_osm_all_churches_rect(
#             lat1=siem[i, lat] - km2lat(radius),
#             lat2=siem[i, lat] + km2lat(radius),
#             lon1=siem[i, lon] - km2lon(radius, lat=siem[i, lat]),
#             lon2=siem[i, lon] + km2lon(radius, lat=siem[i, lat]),
#             what=what)

#         if (dim(topo)["ways"] == 0){
#             cat("no results, skipped\n")
#             next
#         }

#         polys = osmar::as_sp(topo, what="polygons")
#         tags = get_osm_tags(topo)
#         rownames(tags) = tags$osmid

#         # check duplicates
#         # issue: sometimes a way is caught separately from the relation
#         # in that case prefer relation?

#         polys@data = data.frame(polys@data, tags[as.character(polys$id), ])
#         polys@data$city = siem[i, city]

#         polylist[[cty]] = polys

#         Sys.sleep(sleep)
#     }
#     return(polylist)
# }

get_osm_all_relation_churches_rect <- function(lat1, lon1, lat2, lon2){
    # make this handle relations as well
    basequery <-   '[out:xml][timeout:900];
        (
            relation["building"="church"] %1$s;
            relation["building"="chapel"] %1$s;
            relation["amenity"="place_of_worship"] %1$s;
            relation["building"="cathedral"] %1$s;
        );
        out body;
        >;
        out skel qt;'
    
    bounding <- paste0('(',paste(c(lat1, lon1, lat2, lon2), collapse=','),')')
    qry <- sprintf(basequery, bounding)
    
    request <- URLencode(paste0(overpass_baseurl, qry))
    reqtime <- system.time(result <- readLines(request))
    if (sum(reqtime) > 900) warning('timeout exceeded')
    
    topo <- osmar::as_osmar(XML::xmlParse(result))
    cat('request returned', nrow(topo$ways[[1]]), 'ways', '\n')

    return(topo)
}


get_osm_relation_churches_rect <- function(lat1, lon1, lat2, lon2){
    basequery <-   '[out:xml][timeout:900];
        (
            relation["building"="church"] %1$s;
            relation["building"="chapel"] %1$s;
            relation["amenity"="place_of_worship"] %1$s;
            relation["building"="cathedral"] %1$s;
        );
        out body;
        >;
        out skel qt;'
    
    bounding <- paste0('(',paste(c(lat1, lon1, lat2, lon2), collapse=','),')')
    qry <- sprintf(basequery, bounding)
    
    request <- URLencode(paste0(overpass_baseurl, qry))
    reqtime <- system.time(result <- readLines(request))
    if (sum(reqtime) > 900) warning('timeout exceeded')
    
    topo <- osmar::as_osmar(XML::xmlParse(result))
    cat('request returned', nrow(topo$ways[[1]]), 'ways', '\n')

    return(topo)
}

get_osm_other_churches_rect <- function(lat1, lon1, lat2, lon2){
    basequery <-   '[out:xml][timeout:900];
        (
            way["building"="chapel"] %1$s;
            way["amenity"="place_of_worship"] %1$s;
        );
        out body;
        >;
        out skel qt;'
    
    bounding <- paste0('(',paste(c(lat1, lon1, lat2, lon2), collapse=','),')')
    qry <- sprintf(basequery, bounding)
    
    request <- URLencode(paste0(overpass_baseurl, qry))
    reqtime <- system.time(result <- readLines(request))
    if (sum(reqtime) > 900) warning('timeout exceeded')
    
    topo <- osmar::as_osmar(XML::xmlParse(result))
    cat('request returned', nrow(topo$ways[[1]]), 'ways', '\n')

    return(topo)
}
