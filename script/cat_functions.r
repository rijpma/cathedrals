thinmargins <- c(4, 4, 3, 0.5)


rtnorm = function (n, mean = 0, sd = 1, min = -Inf, max = Inf) {
    # replace Trunc distributions from envstats
    # in due time remove
    # laeken, plyr, MASS, Rcpp (≥ 0.11.0), e1071, parallel, nnet, doParallel, foreach, colorspace, VIM, methods, party, EnvStats, fitdistrplus, ranger

    ln <- length(n)
    if (ln < 1)
        stop("'n' must be non-empty.")
    if (ln > 1)
        n <- ln
    else {
        if (is.na(n) || n <= 0 || n != trunc(n))
            stop("'n' must be a positive integer or vector.")
    }
    arg.mat <- cbind(n.vec = rep(1, n), mean = as.vector(mean),
        sd = as.vector(sd), min = as.vector(min), max = as.vector(max))
    if (n < nrow(arg.mat))
        arg.mat <- arg.mat[1:n, , drop = FALSE]
    na.index <- apply(arg.mat, 1, function(x) any(is.na((x))))
    if (all(na.index))
        return(rep(NA, n))
    else {
        r <- numeric(n)
        r[na.index] <- NA
        r.no.na <- r[!na.index]
        n.vec <- arg.mat[!na.index, "n.vec"]
        mean <- arg.mat[!na.index, "mean"]
        sd <- arg.mat[!na.index, "sd"]
        min <- arg.mat[!na.index, "min"]
        max <- arg.mat[!na.index, "max"]
        if (any(sd < .Machine$double.eps))
            stop("All non-missing values of 'sd' must be positive.")
        if (any(min > max))
            stop(paste("All non-missing values of 'min' must be",
                "less than the corresponding elements of 'max'."))
        r[!na.index] <- qtnorm(p = runif(n.vec), mean = mean,
            sd = sd, min = min, max = max)
        return(r)
    }
}

qtnorm = function (p, mean = 0, sd = 1, min = -Inf, max = Inf)
{
    names.p <- names(p)
    arg.mat <- cbind(p = as.vector(p), mean = as.vector(mean),
        sd = as.vector(sd), min = as.vector(min), max = as.vector(max))
    na.index <- apply(arg.mat, 1, function(x) any(is.na((x))))
    if (all(na.index))
        q <- rep(NA, nrow(arg.mat))
    else {
        q <- numeric(nrow(arg.mat))
        q[na.index] <- NA
        q.no.na <- q[!na.index]
        for (i in c("p", "mean", "sd", "min", "max")) assign(i,
            arg.mat[!na.index, i])
        if (any(p < 0) || any(p > 1))
            stop("All non-missing values of 'p' must be between 0 and 1")
        if (any(sd < .Machine$double.eps))
            stop("All non-missing values of 'sd' must be positive.")
        if (any(min >= max))
            stop(paste("All non-missing values of 'min' must be",
                "less than the corresponding elements of 'max'."))
        p.low <- p == 0
        q.no.na[p.low] <- min[p.low]
        p.high <- p == 1
        q.no.na[p.high] <- max[p.high]
        if (any(index <- !(p.low | p.high))) {
            mean <- mean[index]
            sd <- sd[index]
            F.min <- pnorm(min[index], mean = mean, sd = sd)
            q.no.na[index] <- qnorm(p[index] * (pnorm(max[index],
                mean = mean, sd = sd) - F.min) + F.min, mean = mean,
                sd = sd)
        }
        q[!na.index] <- q.no.na
    }
    if (!is.null(names.p))
        names(q) <- rep(names.p, length = length(q))
    return(q)
}

calcMode <- function(x, ...) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}
add_borders = function(border=1, add=T){
    data(wrld_simpl)
    eur = wrld_simpl[wrld_simpl$REGION==150, ]
    eur = wrld_simpl[wrld_simpl$SUBREGION %in% c(39, 154, 155), ]
    eur = wrld_simpl[wrld_simpl$ISO3 %in% c("NLD", "BEL", "CHE", "FRA", "DEU", "GBR"), ]
    plot(eur, add=add, lwd=0.5, border=border)
}

sp_rbind = function(polys, polys4merge){
    
    polys4merge@data[, setdiff(names(polys), names(polys4merge))] = NA
    polys@data[, setdiff(names(polys4merge), names(polys))] = NA

    rownames(polys@data) = sapply(slot(polys, "polygons"), function(x) slot(x, "ID"))
    rownames(polys4merge@data) = sapply(slot(polys4merge, "polygons"), function(x) slot(x, "ID"))
    # row.names(polys4merge@data) = as.character(polys4merge$id)
    # row.names(polys@data) = as.character(polys$id)

    polys = rbind(polys, polys4merge, makeUniqueIDs=TRUE)
    # polys = maptools::spRbind(polys, polys4merge, makeUniqueIDs=TRUE)

    return(polys)
}
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

    full[, inobs:=zoo::na.approx(nobs, method='constant', rule=1, na.rm=F), by=osmid]
    full[m2obs > 1 & !is.na(inobs), iphaselength:=zoo::na.approx(phaselength, method='constant', na.rm=F, rule=2:1), by=osmid]
    # create a valid data range and then use rule=2 ?
    # no
    # zoo::na.approx does not work on nobs=2
    full[, irestphase:=zoo::na.approx(restphase, method='constant', na.rm=F, rule=1, f=0), by=osmid]
    full[, im2_ann:=im2 / iphaselength]
    full[irestphase==1, im2_ann:=0]
    full[firstobs==TRUE & !is.na(firstobs), im2_ann:=0]

    # check m3 treatment
    full[, im3_ann:=im3/iphaselength]
    full[irestphase==1, im3_ann:=0]
    full[firstobs==TRUE & !is.na(firstobs), im3_ann:=0]
    
    full = full[order(osmid, year), ]
    full[, ibldindex:=zoo::na.approx(bldindex, method='constant', rule=1, na.rm=F), by=osmid]
    full[is.na(newbld), newbld:=FALSE]
    full[, osmid_buildindex:=paste(osmid, ibldindex, sep='_')]

    full[!is.na(im2_ann), im2_cml:=cumsum(im2_ann) + im2[newbld==TRUE], by=osmid_buildindex]
    # full[!is.na(im2_ann), im2_cml:=im2_cml + im2[newbld==TRUE], by=osmid_buildindex]
    full[, im2_cml:=zoo::na.approx(im2_cml, method='constant', rule=1:2, yleft=0, na.rm=F), by=osmid]
    full[!is.na(im3_ann), im3_cml:=cumsum(im3_ann) + im3[newbld==TRUE], by=osmid_buildindex]
    full[, im3_cml:=zoo::na.approx(im3_cml, method='constant', rule=1:2, yleft=0, na.rm=F), by=osmid]

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
    dyn[, phaselength:=c(diff(year)[1], diff(year)), by=osmid]
    dyn[, firstobs:=year==min(year), by=osmid]
    dyn[, restphase:=is.na(m2)]

    # indexing entirely new buildings
    dyn[, newbld := ifelse((m2 == 0 & !is.na(m2)) | firstobs == TRUE, TRUE, FALSE), by=osmid]
    dyn[newbld == TRUE, bldindex := 1:length(nobs), by=osmid]
    dyn[, bldindex := zoo::na.approx(bldindex, method='constant', rule=1:2)]
    
    # dyn[m2==0, bldindex:=1:length(m2), by=osmid]
    # dyn[, nbblds:=sum(!is.na(bldindex)), by=osmid]
    # dyn[nbblds > 0, bldindex:=as.integer(zoo::na.approx(bldindex, method="constant", na.rm=F, rule=1:2)), by=osmid]
    # dyn[nbblds==0, bldindex:=1]
    # dyn[is.na(bldindex), bldindex:=0]

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

get_osm_data = function(cty, what='way', radius=5, block=FALSE, ruins=FALSE){
    if (block){
        lat1 = cty$lat1
        lat2 = cty$lat2
        lon1 = cty$lon1
        lon2 = cty$lon2
    } else {
        lat1 = cty$lat - km2lat(radius)
        lat2 = cty$lat + km2lat(radius)
        lon1 = cty$lon - km2lon(radius, lat=cty$lat)
        lon2 = cty$lon + km2lon(radius, lat=cty$lat)
    }

    cat(cty$city)
    topo = get_osm_all_churches_rect(lat1=lat1, lat2=lat2, 
        lon1=lon1, lon2=lon2, what=what, ruins=ruins)

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
aggregate_multipolys = function(polys, by="Group.1"){
    polys@data$lon = tapply(polys@data$lon, polys@data[, by], mean)[as.character(polys@data[, by])]
    polys@data$lat = tapply(polys@data$lat, polys@data[, by], mean)[as.character(polys@data[, by])]
    polys@data$surface = tapply(polys@data$surface, polys@data[, by], sum)[as.character(polys@data[, by])]
    out = aggregate(polys, by=list(polys@data[, by]), FUN=function(x) x[1])
    # caution: used to be aggregate spdf and returned spdf, now requires FUN= to do that
    # out = aggregate.data.frame(polys, by=list(polys[, by]), `[`, 1)
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

get_osm_all_churches_rect <- function(lat1, lon1, lat2, lon2, 
    what="way", ruins=F){
    # make this handle relations as well
    basequery <-   '[out:xml][timeout:900];
        (
            %2$s["building"="church"] %1$s;
            %2$s["building"="chapel"] %1$s;
            %2$s["building"="cathedral"] %1$s;
            %2$s["amenity"="place_of_worship"] %1$s;
        );
        out body;
        >;
        out skel qt;'
    if (ruins){
        basequery <-   '[out:xml][timeout:900];
        (
            %2$s["building"="church"] %1$s;
            %2$s["building"="chapel"] %1$s;
            %2$s["building"="cathedral"] %1$s;
            %2$s["building"="ruins"] %1$s;
            %2$s["historic"="ruins"] %1$s;
            %2$s["amenity"="place_of_worship"] %1$s;
        );
        out body;
        >;
        out skel qt;'
    }
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
# virids_map = read.csv("https://raw.githubusercontent.com/sjmgarnier/viridis/master/data-raw/viridis_map.csv")
# clsA = read.csv("https://raw.githubusercontent.com/sjmgarnier/viridis/master/data-raw/optionA.csv")
# clsB = read.csv("https://raw.githubusercontent.com/sjmgarnier/viridis/master/data-raw/optionB.csv")
# clsC = read.csv("https://raw.githubusercontent.com/sjmgarnier/viridis/master/data-raw/optionC.csv")
# clsD = read.csv("https://raw.githubusercontent.com/sjmgarnier/viridis/master/data-raw/optionD.csv")

clsA = structure(list(R = c(0.00146159096, 0.00225764007, 0.00327943222, 
0.00451230222, 0.00594976987, 0.0075879855, 0.0094260439, 0.0114654337, 
0.0137075706, 0.0161557566, 0.018815367, 0.021691934, 0.0247917814, 
0.0281228154, 0.0316955304, 0.0355204468, 0.0396084872, 0.043829535, 
0.0480616391, 0.0523204388, 0.0566148978, 0.060949393, 0.0653301801, 
0.0697637296, 0.0742565152, 0.0788150034, 0.0834456313, 0.088154773, 
0.0929486914, 0.097833477, 0.102814972, 0.107898679, 0.113094451, 
0.118405035, 0.123832651, 0.129380192, 0.135053322, 0.140857952, 
0.146785234, 0.152839217, 0.159017511, 0.165308131, 0.171713033, 
0.17821173, 0.184800877, 0.191459745, 0.198176877, 0.204934882, 
0.211718061, 0.21851159, 0.225302032, 0.232076515, 0.238825991, 
0.245543175, 0.252220252, 0.258857304, 0.265446744, 0.271994089, 
0.2784933, 0.284951097, 0.291365817, 0.297740413, 0.304080941, 
0.310382027, 0.316654235, 0.322899126, 0.329114038, 0.335307503, 
0.341481725, 0.347635742, 0.353773161, 0.359897941, 0.366011928, 
0.372116205, 0.378210547, 0.384299445, 0.390384361, 0.39646667, 
0.402547663, 0.408628505, 0.414708664, 0.420791157, 0.426876965, 
0.432967001, 0.439062114, 0.445163096, 0.451270678, 0.457385535, 
0.463508291, 0.469639514, 0.475779723, 0.481928997, 0.488088169, 
0.494257673, 0.500437834, 0.506628929, 0.512831195, 0.519044825, 
0.525269968, 0.531506735, 0.537755194, 0.544015371, 0.550287252, 
0.556570783, 0.562865867, 0.569172368, 0.575490107, 0.581818864, 
0.588158375, 0.594508337, 0.600868399, 0.607238169, 0.613617209, 
0.620005032, 0.626401108, 0.632804854, 0.639215638, 0.645632778, 
0.652055535, 0.658483116, 0.664914668, 0.671349279, 0.677785975, 
0.684223712, 0.69066138, 0.697097796, 0.7035317, 0.709961888, 
0.716387038, 0.722805451, 0.729215521, 0.735615545, 0.742003713, 
0.748378107, 0.754736692, 0.761077312, 0.767397681, 0.77369538, 
0.779967847, 0.786212372, 0.792426972, 0.79860776, 0.804751511, 
0.810854841, 0.816914186, 0.822925797, 0.82888574, 0.834790818, 
0.84063568, 0.846415804, 0.85212649, 0.85776287, 0.863320397, 
0.868793368, 0.874176342, 0.879463944, 0.884650824, 0.889731418, 
0.894700194, 0.899551884, 0.904281297, 0.908883524, 0.913354091, 
0.917688852, 0.921884187, 0.925937102, 0.92984509, 0.933606454, 
0.937220874, 0.940687443, 0.944006448, 0.947179528, 0.95021015, 
0.953099077, 0.955849237, 0.958464079, 0.960949221, 0.963310281, 
0.965549351, 0.967671128, 0.969680441, 0.971582181, 0.973381238, 
0.975082439, 0.976690494, 0.978209957, 0.979645181, 0.981000291, 
0.982279159, 0.983485387, 0.984622298, 0.985692925, 0.986700017, 
0.987646038, 0.988533173, 0.989363341, 0.990138201, 0.990871208, 
0.991558165, 0.992195728, 0.992784669, 0.993325561, 0.993834412, 
0.994308514, 0.994737698, 0.995121854, 0.995480469, 0.995809924, 
0.996095703, 0.996341406, 0.996579803, 0.996774784, 0.996925427, 
0.997077185, 0.997186253, 0.997253982, 0.99732518, 0.997350983, 
0.997350583, 0.997341259, 0.997284689, 0.997228367, 0.99713848, 
0.997019342, 0.996898254, 0.996726862, 0.996570645, 0.996369065, 
0.996162309, 0.995932448, 0.995680107, 0.995423973, 0.995131288, 
0.994851089, 0.994523666, 0.9942219, 0.993865767, 0.993545285, 
0.993169558, 0.992830963, 0.992439881, 0.992089454, 0.991687744, 
0.991331929, 0.990929685, 0.990569914, 0.990174637, 0.989814839, 
0.989433736, 0.989077438, 0.988717064, 0.988367028, 0.988032885, 
0.987690702, 0.987386827, 0.987052509), G = c(0.000466127766, 
0.00129495431, 0.00230452991, 0.00349037666, 0.00484285, 0.00635613622, 
0.00802185006, 0.00982831486, 0.0117705913, 0.0138404966, 0.0160262753, 
0.0183201254, 0.0207147875, 0.0232009284, 0.0257651161, 0.028397457, 
0.0310895652, 0.0338299885, 0.0366066101, 0.039406602, 0.0421598925, 
0.0447944924, 0.0473177796, 0.0497264666, 0.0520167766, 0.0541844801, 
0.0562249365, 0.0581331465, 0.0599038167, 0.0615314414, 0.0630104053, 
0.0643351102, 0.0654920358, 0.0664791593, 0.0672946449, 0.0679349264, 
0.0683912798, 0.068654071, 0.0687382323, 0.0686368599, 0.0683540225, 
0.0679108689, 0.067305326, 0.0665758073, 0.0657324381, 0.0648183312, 
0.0638624166, 0.0629066192, 0.0619917876, 0.0611584918, 0.0604451843, 
0.0598886855, 0.0595170384, 0.0593524384, 0.0594147119, 0.0597055998, 
0.0602368754, 0.0609935552, 0.0619778136, 0.0631676261, 0.0645534486, 
0.0661170432, 0.0678353452, 0.0697024767, 0.0716895272, 0.0737819504, 
0.0759715081, 0.0782361045, 0.0805635079, 0.0829463512, 0.0853726329, 
0.0878311772, 0.0903143031, 0.0928159917, 0.0953322947, 0.0978549106, 
0.100379466, 0.102902194, 0.105419865, 0.107929771, 0.110431177, 
0.11292021, 0.115395258, 0.117854987, 0.120298314, 0.122724371, 
0.125132484, 0.127522145, 0.129892998, 0.132244819, 0.1345775, 
0.13689139, 0.139186217, 0.141462106, 0.143719323, 0.145958202, 
0.148179144, 0.150382611, 0.152569121, 0.154739247, 0.156893613, 
0.159032895, 0.161157816, 0.163269149, 0.165367714, 0.167454379, 
0.169530062, 0.171595728, 0.173652392, 0.175701122, 0.177743036, 
0.179779309, 0.18181117, 0.183839907, 0.185866869, 0.187893468, 
0.189921182, 0.191951556, 0.19398621, 0.196026835, 0.198075202, 
0.200133166, 0.202202663, 0.204285721, 0.206384461, 0.2085011, 
0.210637956, 0.212797337, 0.214981693, 0.217193831, 0.219436516, 
0.221712634, 0.224025196, 0.226377345, 0.228772352, 0.231213625, 
0.233704708, 0.236249283, 0.23885117, 0.241514325, 0.24424225, 
0.247039798, 0.24991135, 0.252861399, 0.25589455, 0.259015505, 
0.262229049, 0.265539703, 0.268952874, 0.272473491, 0.276106469, 
0.279856666, 0.283729003, 0.287728205, 0.291858679, 0.296124596, 
0.30053009, 0.305078817, 0.309773445, 0.314616425, 0.319609981, 
0.324755126, 0.330051947, 0.335500068, 0.341098112, 0.346843685, 
0.352733817, 0.358764377, 0.364929312, 0.371224168, 0.377642889, 
0.384177874, 0.390819546, 0.397562894, 0.404400213, 0.411323666, 
0.418323245, 0.425389724, 0.432518707, 0.439702976, 0.446935635, 
0.45421017, 0.461520484, 0.468860936, 0.47622635, 0.483612031, 
0.491013764, 0.4984278, 0.505850848, 0.513280054, 0.520712972, 
0.528147545, 0.53558207, 0.543015173, 0.550445778, 0.557873075, 
0.565296495, 0.572706259, 0.580106828, 0.587501706, 0.594891088, 
0.602275297, 0.60964354, 0.616998953, 0.624349657, 0.631696376, 
0.639026596, 0.646343897, 0.653658756, 0.660969379, 0.668255621, 
0.675541484, 0.682827953, 0.690087897, 0.697348991, 0.704610791, 
0.711847714, 0.719089119, 0.726324415, 0.733544671, 0.740771893, 
0.747980563, 0.755189852, 0.762397883, 0.769590975, 0.77679486, 
0.783976508, 0.791167346, 0.798347709, 0.805527126, 0.812705773, 
0.819875302, 0.827051773, 0.834212826, 0.841386618, 0.848540474, 
0.855711038, 0.862858846, 0.870024467, 0.877168404, 0.884329694, 
0.891469549, 0.89862705, 0.905762748, 0.91291501, 0.920048699, 
0.927195612, 0.93432854, 0.941470354, 0.948604077, 0.95574152, 
0.962878026, 0.970012413, 0.977154231, 0.984287561, 0.991437853
), B = c(0.01386552, 0.0183311461, 0.0237083291, 0.0299647059, 
0.0371296695, 0.0449730774, 0.0528443561, 0.060749638, 0.0686665843, 
0.076602666, 0.0845844897, 0.092610105, 0.100675555, 0.108786954, 
0.116964722, 0.125209396, 0.133515085, 0.141886249, 0.150326989, 
0.158841025, 0.167445592, 0.176128834, 0.184891506, 0.193735088, 
0.202660374, 0.211667355, 0.220755099, 0.229921611, 0.239163669, 
0.248476662, 0.2578544, 0.267288933, 0.276783978, 0.286320656, 
0.295879431, 0.305442931, 0.31499989, 0.32453764, 0.334011109, 
0.34340445, 0.352688028, 0.361816426, 0.370770827, 0.379497161, 
0.387972507, 0.396151969, 0.404008953, 0.411514273, 0.418646741, 
0.425391816, 0.431741767, 0.437694665, 0.443255999, 0.448435938, 
0.453247729, 0.457709924, 0.461840297, 0.465660375, 0.469190328, 
0.472450879, 0.475462193, 0.478243482, 0.480811572, 0.48318634, 
0.485380429, 0.487408399, 0.489286796, 0.491024144, 0.492631321, 
0.494120923, 0.495501096, 0.496778331, 0.497959963, 0.499053326, 
0.500066568, 0.501001964, 0.501864236, 0.50265759, 0.503385761, 
0.504052118, 0.504661843, 0.505214935, 0.505713602, 0.506159754, 
0.506555026, 0.506900806, 0.507198258, 0.507448336, 0.507651812, 
0.507809282, 0.507921193, 0.507988509, 0.508010737, 0.507987836, 
0.507919772, 0.50780642, 0.50764757, 0.507442938, 0.507192172, 
0.50689486, 0.506550538, 0.506158696, 0.505718782, 0.50523021, 
0.504692365, 0.504104606, 0.503466273, 0.50277669, 0.502035167, 
0.501241011, 0.500393522, 0.499491999, 0.498535746, 0.497524075, 
0.496456304, 0.495331769, 0.494149821, 0.492909832, 0.491611196, 
0.490253338, 0.488835712, 0.487357807, 0.485819154, 0.484219325, 
0.482557941, 0.480834678, 0.47904927, 0.477201121, 0.47528978, 
0.473315708, 0.471278924, 0.469179541, 0.467017774, 0.464793954, 
0.462508534, 0.460162106, 0.457755411, 0.455289354, 0.452765022, 
0.450183695, 0.447543155, 0.444848441, 0.442101615, 0.439304963, 
0.436461074, 0.433572874, 0.430643647, 0.427671352, 0.42466562, 
0.421631064, 0.418572767, 0.415496319, 0.412402889, 0.409303002, 
0.406205397, 0.403118034, 0.40004706, 0.397001559, 0.393994634, 
0.391036674, 0.388136889, 0.385308008, 0.382563414, 0.379915138, 
0.377375977, 0.374959077, 0.372676513, 0.370540883, 0.368566525, 
0.366761699, 0.365136328, 0.36370113, 0.362467694, 0.361438431, 
0.360619076, 0.360014232, 0.359629789, 0.35946902, 0.359529151, 
0.359810172, 0.36031112, 0.361030156, 0.361964652, 0.363111292, 
0.364466162, 0.366024854, 0.367782559, 0.369734157, 0.371874301, 
0.374197501, 0.376698186, 0.379370774, 0.382209724, 0.385209578, 
0.388365009, 0.391670846, 0.395122099, 0.398713971, 0.402441058, 
0.406298792, 0.410282976, 0.414389658, 0.418613221, 0.422949672, 
0.427396771, 0.431951492, 0.436607159, 0.441360951, 0.446213021, 
0.451160201, 0.456191814, 0.461314158, 0.466525689, 0.471811461, 
0.477181727, 0.482634651, 0.488154375, 0.493754665, 0.499427972, 
0.505166839, 0.510983331, 0.516859378, 0.522805996, 0.528820775, 
0.534892341, 0.541038571, 0.547232992, 0.553498939, 0.559819643, 
0.566201824, 0.572644795, 0.57914013, 0.585701463, 0.592307093, 
0.598982818, 0.605695903, 0.612481798, 0.6192993, 0.626189463, 
0.633109148, 0.640099465, 0.647116021, 0.654201544, 0.661308839, 
0.668481201, 0.675674592, 0.682925602, 0.690198194, 0.697518628, 
0.704862519, 0.712242232, 0.719648627, 0.727076773, 0.734536205, 
0.742001547, 0.749504188)), .Names = c("R", "G", "B"), class = "data.frame", row.names = c(NA, 
-256L))
clsB = structure(list(R = c(0.00146159096, 0.00226726368, 0.00329899092, 
0.00454690615, 0.00600552565, 0.00767578856, 0.00956051094, 0.0116634769, 
0.0139950388, 0.0165605595, 0.0193732295, 0.0224468865, 0.0257927373, 
0.0294324251, 0.0333852235, 0.0376684211, 0.0422525554, 0.0469146287, 
0.0516437624, 0.0564491009, 0.06133972, 0.066331262, 0.0714289181, 
0.076636756, 0.0819620773, 0.0874113897, 0.0929901526, 0.0987024972, 
0.104550936, 0.110536084, 0.116656423, 0.122908126, 0.129284984, 
0.13577845, 0.142377819, 0.149072957, 0.155849711, 0.162688939, 
0.169575148, 0.176493202, 0.183428775, 0.190367453, 0.197297425, 
0.204209298, 0.211095463, 0.217948648, 0.224762908, 0.231538148, 
0.238272961, 0.244966911, 0.251620354, 0.258234265, 0.264809649, 
0.271346664, 0.277849829, 0.284321318, 0.290763373, 0.297178251, 
0.303568182, 0.309935342, 0.316281835, 0.322609671, 0.328920763, 
0.335216916, 0.341499828, 0.347771086, 0.354032169, 0.360284449, 
0.366529195, 0.372767575, 0.379000659, 0.385228383, 0.391452659, 
0.397674379, 0.403894278, 0.410113015, 0.416331169, 0.422549249, 
0.428767696, 0.434986885, 0.441207124, 0.447428382, 0.453650614, 
0.459874623, 0.466100494, 0.472328255, 0.478557889, 0.484789325, 
0.491022448, 0.497257069, 0.503492698, 0.509729541, 0.515967304, 
0.522205646, 0.528444192, 0.534682523, 0.540920186, 0.547156706, 
0.553391649, 0.559624442, 0.565854477, 0.572081108, 0.578303656, 
0.584521407, 0.590733615, 0.596939751, 0.60313893, 0.609330184, 
0.615512627, 0.62168534, 0.627847374, 0.633997746, 0.640135447, 
0.646259648, 0.652369348, 0.658463166, 0.664539964, 0.670598572, 
0.676637795, 0.682656407, 0.688653158, 0.694626769, 0.700575937, 
0.706499709, 0.712396345, 0.718264447, 0.724102613, 0.729909422, 
0.735683432, 0.741423185, 0.747127207, 0.752794009, 0.75842209, 
0.76400994, 0.769556038, 0.775058888, 0.780517023, 0.785928794, 
0.791292674, 0.796607144, 0.801870689, 0.807081807, 0.812239008, 
0.817340818, 0.822385784, 0.827372474, 0.832299481, 0.837165425, 
0.841968959, 0.846708768, 0.851383572, 0.85599213, 0.860533241, 
0.865005747, 0.869408534, 0.87374053, 0.878000715, 0.882188112, 
0.886301795, 0.890340885, 0.894304553, 0.898192017, 0.902002544, 
0.905735448, 0.90939009, 0.912965874, 0.916462251, 0.91987871, 
0.923214783, 0.926470039, 0.929644083, 0.932736555, 0.935747126, 
0.938675494, 0.941521384, 0.944284543, 0.946964741, 0.949561766, 
0.952075421, 0.954505523, 0.956851903, 0.959114397, 0.96129285, 
0.96338711, 0.965397031, 0.967322465, 0.969163264, 0.970919277, 
0.972590351, 0.974176327, 0.975677038, 0.977092313, 0.978421971, 
0.979665824, 0.980823673, 0.981895311, 0.982880522, 0.983779081, 
0.984590755, 0.985315301, 0.985952471, 0.986502013, 0.98696367, 
0.987337182, 0.987622296, 0.987818759, 0.98792633, 0.987944783, 
0.98787391, 0.987713535, 0.987463516, 0.987123759, 0.986694229, 
0.98617497, 0.985565739, 0.984865203, 0.984075129, 0.983195992, 
0.982228463, 0.981173457, 0.980032178, 0.978806183, 0.977497453, 
0.976108474, 0.974637842, 0.973087939, 0.971467822, 0.969783146, 
0.968040817, 0.966242589, 0.964393924, 0.962516656, 0.960625545, 
0.958720088, 0.956834075, 0.954997177, 0.953215092, 0.951546225, 
0.950018481, 0.948683391, 0.947594362, 0.946809163, 0.946391536, 
0.946402951, 0.946902568, 0.947936825, 0.94954483, 0.951740304, 
0.954529281, 0.957896053, 0.96181202, 0.966248822, 0.971161622, 
0.976510983, 0.982257307, 0.988362068), G = c(0.000466127766, 
0.00126992553, 0.00224934863, 0.00339180156, 0.00469194561, 0.00613611626, 
0.00771344131, 0.00941675403, 0.0112247138, 0.0131362262, 0.0151325789, 
0.0171991484, 0.0193306298, 0.0215030771, 0.0237024271, 0.0259207864, 
0.0281385015, 0.0303236129, 0.0324736172, 0.0345691867, 0.0365900213, 
0.0385036268, 0.0402939095, 0.0419053329, 0.0433278666, 0.0445561662, 
0.0455829503, 0.0464018731, 0.0470080541, 0.0473986708, 0.047573592, 
0.0475360183, 0.0472930838, 0.0468563678, 0.0462422566, 0.0454676444, 
0.0445588056, 0.0435542881, 0.0424893149, 0.0414017089, 0.0403288858, 
0.0393088888, 0.0384001825, 0.0376322609, 0.0370296488, 0.0366146049, 
0.0364049901, 0.0364052511, 0.0366209949, 0.0370545017, 0.0377052832, 
0.0385706153, 0.0396468666, 0.0409215821, 0.0423528741, 0.0439325787, 
0.0456437598, 0.0474700293, 0.0493958927, 0.0514069729, 0.0534901321, 
0.0556335178, 0.0578265505, 0.0600598734, 0.0623252772, 0.06461561, 
0.0669246832, 0.0692471753, 0.0715785403, 0.0739149211, 0.0762530701, 
0.0785914864, 0.0809267058, 0.0832568129, 0.0855803445, 0.0878961593, 
0.0902033992, 0.0925014543, 0.0947899342, 0.0970686417, 0.099337551, 
0.101597079, 0.103847716, 0.106089165, 0.108321923, 0.110546584, 
0.112763831, 0.11497443, 0.117179219, 0.119379132, 0.121575414, 
0.123768654, 0.125959947, 0.128150439, 0.130341324, 0.132533845, 
0.134729286, 0.136928959, 0.139134147, 0.141346265, 0.143566769, 
0.14579715, 0.148038934, 0.150293679, 0.152562977, 0.154848232, 
0.157151161, 0.159473549, 0.161817111, 0.164183582, 0.166574724, 
0.168992314, 0.17143815, 0.173913876, 0.176421271, 0.178962399, 
0.181539111, 0.184153268, 0.186806728, 0.189501352, 0.192238994, 
0.1950215, 0.197850703, 0.200728196, 0.203656029, 0.206635993, 
0.209669834, 0.21275927, 0.215905976, 0.219111589, 0.222377697, 
0.225705837, 0.229097492, 0.232554083, 0.236076967, 0.239667435, 
0.24332672, 0.247055968, 0.250856232, 0.254728485, 0.25867361, 
0.262692401, 0.266785558, 0.270953688, 0.2751973, 0.279516805, 
0.283912516, 0.288384647, 0.292933312, 0.297558528, 0.302260213, 
0.307038188, 0.311892183, 0.316821833, 0.321826685, 0.326906201, 
0.33205976, 0.337286663, 0.342586137, 0.34795734, 0.353399363, 
0.35891124, 0.364491949, 0.370140419, 0.375855533, 0.381636138, 
0.387481044, 0.393389034, 0.399358867, 0.405389282, 0.411479007, 
0.417626756, 0.423831237, 0.430091162, 0.436405243, 0.442772199, 
0.449190757, 0.455659658, 0.462177656, 0.468743522, 0.475356048, 
0.482014044, 0.488716345, 0.495461806, 0.502249309, 0.509077761, 
0.515946092, 0.522853259, 0.529798246, 0.536780059, 0.543797733, 
0.550850323, 0.557936911, 0.5650566, 0.572208516, 0.579391803, 
0.586605627, 0.593849168, 0.601121626, 0.608422211, 0.615750147, 
0.623104667, 0.630485011, 0.637890424, 0.645320152, 0.652773439, 
0.660249526, 0.667747641, 0.675267, 0.682806802, 0.690366218, 
0.697944391, 0.705540424, 0.713153375, 0.72078246, 0.728427497, 
0.736086521, 0.743758326, 0.751441596, 0.759134892, 0.766836624, 
0.774545028, 0.782258138, 0.789973753, 0.797691563, 0.805409333, 
0.813121725, 0.820825143, 0.828515491, 0.836190976, 0.843848069, 
0.85147634, 0.859068716, 0.866624355, 0.874128569, 0.881568926, 
0.888942277, 0.896225909, 0.903409063, 0.910472964, 0.917399053, 
0.924168246, 0.930760752, 0.937158971, 0.943347775, 0.949317522, 
0.9550629, 0.960586693, 0.965895868, 0.97100333, 0.975924241, 
0.980678193, 0.985282161, 0.989753437, 0.994108844, 0.998364143
), B = c(0.01386552, 0.018570352, 0.0242390508, 0.0309092475, 
0.038557898, 0.0468360336, 0.0551430756, 0.063459808, 0.071861689, 
0.0802817951, 0.0887668094, 0.0973274383, 0.105929835, 0.114621328, 
0.123397286, 0.132232108, 0.141140519, 0.150163867, 0.159254277, 
0.168413539, 0.177642172, 0.186961588, 0.196353558, 0.205798788, 
0.215289113, 0.224813479, 0.234357604, 0.2439037, 0.2534303, 
0.262912235, 0.272320803, 0.28162417, 0.290788012, 0.299776404, 
0.30855291, 0.317085139, 0.325338414, 0.333276678, 0.340874188, 
0.348110606, 0.354971391, 0.361446945, 0.367534629, 0.373237557, 
0.378563264, 0.383522415, 0.388128944, 0.39240015, 0.396353388, 
0.400006615, 0.403377897, 0.406485031, 0.409345373, 0.411976086, 
0.414392106, 0.416607861, 0.418636756, 0.420491164, 0.422182449, 
0.423720999, 0.425116277, 0.426376869, 0.427510546, 0.42852432, 
0.429424503, 0.430216765, 0.430906186, 0.431497309, 0.431994185, 
0.432400419, 0.432719214, 0.432954973, 0.433108763, 0.433182647, 
0.433178526, 0.433098056, 0.432942678, 0.432713635, 0.432411996, 
0.432038673, 0.431594438, 0.431080497, 0.430497898, 0.429845789, 
0.429124507, 0.42833432, 0.427475431, 0.426547991, 0.425552106, 
0.424487908, 0.42335611, 0.422155676, 0.420886594, 0.419548848, 
0.418142411, 0.416667258, 0.415123366, 0.413510662, 0.411828882, 
0.410078028, 0.408258132, 0.406369246, 0.404411444, 0.402384829, 
0.400289528, 0.398124897, 0.395891308, 0.393589349, 0.391219295, 
0.388781456, 0.38627618, 0.383703854, 0.381064906, 0.378358969, 
0.375586209, 0.372748214, 0.369845599, 0.366879025, 0.363849195, 
0.360756856, 0.357602797, 0.354387853, 0.3511129, 0.347776863, 
0.344382594, 0.340931208, 0.337423766, 0.333861367, 0.330245147, 
0.326576275, 0.322855952, 0.31908541, 0.31526591, 0.311398734, 
0.307485188, 0.303526312, 0.299522665, 0.295476756, 0.291389943, 
0.287263585, 0.283099033, 0.278897629, 0.274660698, 0.270389545, 
0.266085445, 0.261749643, 0.257383341, 0.2529877, 0.248563825, 
0.244112767, 0.239635512, 0.235132978, 0.230606009, 0.226055368, 
0.221481734, 0.216885699, 0.212267762, 0.207628326, 0.202967696, 
0.19828608, 0.193583583, 0.188860212, 0.184115876, 0.179350388, 
0.174563472, 0.169754764, 0.164923826, 0.160070152, 0.155193185, 
0.150292329, 0.145366973, 0.140416519, 0.135440416, 0.130438175, 
0.12540944, 0.120354038, 0.115272059, 0.110163947, 0.105030614, 
0.0998735931, 0.0946952268, 0.0894989073, 0.0842893891, 0.0790731907, 
0.0738591143, 0.0686589199, 0.0634881971, 0.058367489, 0.0533237243, 
0.048392009, 0.0436177922, 0.0390500131, 0.0349306227, 0.0314091591, 
0.0285075931, 0.0262497353, 0.0246613416, 0.0237702263, 0.0236063833, 
0.0242021174, 0.0255921853, 0.0278139496, 0.0309075459, 0.0349160639, 
0.0398857472, 0.0455808037, 0.0517503867, 0.0583286889, 0.0652570167, 
0.072489233, 0.0799897176, 0.0877314215, 0.0956941797, 0.103863324, 
0.112228756, 0.120784651, 0.129526579, 0.138453063, 0.147564573, 
0.156863224, 0.166352544, 0.176037298, 0.185923357, 0.196017589, 
0.206331925, 0.216876839, 0.227658046, 0.238685942, 0.249971582, 
0.261533898, 0.273391112, 0.285545675, 0.298010219, 0.310820466, 
0.323973947, 0.337475479, 0.351368713, 0.365627005, 0.380271225, 
0.395289169, 0.410665194, 0.426373236, 0.442367495, 0.458591507, 
0.474969778, 0.491426053, 0.507859649, 0.524203026, 0.540360752, 
0.55627509, 0.571925382, 0.587205773, 0.60215433, 0.616760413, 
0.631017009, 0.644924005)), .Names = c("R", "G", "B"), class = "data.frame", row.names = c(NA, 
-256L))
clsC = structure(list(R = c(0.0503832136, 0.0635363639, 0.0753531234, 
0.0862217979, 0.0963786097, 0.105979704, 0.115123641, 0.123902903, 
0.13238072, 0.140603076, 0.148606527, 0.156420649, 0.164069722, 
0.171573925, 0.178950212, 0.186212958, 0.193374449, 0.20044526, 
0.207434551, 0.214350298, 0.22119675, 0.227982971, 0.234714537, 
0.241396253, 0.248032377, 0.25462669, 0.261182562, 0.267702993, 
0.274190665, 0.280647969, 0.287076059, 0.293477695, 0.299855122, 
0.306209825, 0.312543124, 0.318856183, 0.325150025, 0.331425547, 
0.337683446, 0.343924591, 0.350149699, 0.356359209, 0.362553473, 
0.368732762, 0.37489727, 0.381047116, 0.387182639, 0.39330401, 
0.399410821, 0.405502914, 0.411580082, 0.417642063, 0.423688549, 
0.429719186, 0.435733575, 0.441732123, 0.4477136, 0.453677394, 
0.459622938, 0.465549631, 0.471456847, 0.477343929, 0.483210198, 
0.489054951, 0.494877466, 0.500677687, 0.506454143, 0.512206035, 
0.51793258, 0.52363299, 0.529306474, 0.534952244, 0.54056951, 
0.546157494, 0.551715423, 0.557242538, 0.562738096, 0.568201372, 
0.573631859, 0.579028682, 0.584391137, 0.589718606, 0.595010505, 
0.600266283, 0.605485428, 0.610667469, 0.615811974, 0.620918555, 
0.625986869, 0.631016615, 0.636007543, 0.640959444, 0.645872158, 
0.650745571, 0.655579615, 0.660374266, 0.665129493, 0.669845385, 
0.67452206, 0.679159664, 0.683758384, 0.68831844, 0.692840088, 
0.697323615, 0.701769334, 0.70617759, 0.710548747, 0.714883195, 
0.719181339, 0.723443604, 0.727670428, 0.731862231, 0.736019424, 
0.740142557, 0.744232102, 0.748288533, 0.752312321, 0.756303937, 
0.760263849, 0.764192516, 0.768090391, 0.771957916, 0.775795522, 
0.779603614, 0.783382636, 0.787132978, 0.790855015, 0.794549101, 
0.798215577, 0.801854758, 0.805466945, 0.809052419, 0.812611506, 
0.816144382, 0.819651255, 0.823132309, 0.826587706, 0.830017584, 
0.833422053, 0.836801237, 0.840155276, 0.843484103, 0.846787726, 
0.850066132, 0.853319279, 0.856547103, 0.85974952, 0.862926559, 
0.86607792, 0.869203436, 0.872302917, 0.875376149, 0.878422895, 
0.881442916, 0.884435982, 0.887401682, 0.890339687, 0.893249647, 
0.896131191, 0.898983931, 0.901807455, 0.904601295, 0.907364995, 
0.910098088, 0.912800095, 0.915470518, 0.918108848, 0.920714383, 
0.92328666, 0.925825146, 0.928329275, 0.930798469, 0.93323214, 
0.935629684, 0.937990034, 0.940312939, 0.942597771, 0.944843893, 
0.947050662, 0.949217427, 0.95134353, 0.953427725, 0.95546964, 
0.95746877, 0.95942443, 0.96133593, 0.963202573, 0.965023656, 
0.96679847, 0.968525639, 0.970204593, 0.971835007, 0.973416145, 
0.974947262, 0.976427606, 0.977856416, 0.979232922, 0.980556344, 
0.98182589, 0.983040742, 0.984198924, 0.98530076, 0.986345421, 
0.987332067, 0.988259846, 0.989127893, 0.989935328, 0.990681261, 
0.991364787, 0.99198499, 0.992540939, 0.993031693, 0.993456302, 
0.993813802, 0.994103226, 0.994323596, 0.994473934, 0.99455326, 
0.994560594, 0.994494964, 0.994355411, 0.994140989, 0.993850778, 
0.99348219, 0.993033251, 0.992505214, 0.99189727, 0.99120868, 
0.990438793, 0.989587065, 0.988647741, 0.987620557, 0.986509366, 
0.985314198, 0.984031139, 0.98265282, 0.981190389, 0.979643637, 
0.977994918, 0.976264977, 0.974443038, 0.972530009, 0.970532932, 
0.968443477, 0.966271225, 0.964021057, 0.961681481, 0.959275646, 
0.956808068, 0.954286813, 0.951726083, 0.949150533, 0.94660227, 
0.944151742, 0.94189612, 0.940015097), G = c(0.0298028976, 0.0284259729, 
0.0272063728, 0.0261253206, 0.0251650976, 0.0243092436, 0.02355625, 
0.0228781011, 0.0222583774, 0.0216866674, 0.0211535876, 0.0206507174, 
0.0201705326, 0.0197063415, 0.0192522243, 0.0188029767, 0.0183540593, 
0.0179015512, 0.0174421086, 0.0169729276, 0.0164970484, 0.0160071509, 
0.0155015065, 0.0149791041, 0.0144393586, 0.0138820918, 0.0133075156, 
0.0127162163, 0.0121091423, 0.0114875915, 0.0108554862, 0.0102128849, 
0.00956079551, 0.00890185346, 0.00823900704, 0.00757551051, 0.00691491734, 
0.00626107379, 0.00561830889, 0.0049905308, 0.00438202557, 0.00379781761, 
0.00324319591, 0.00272370721, 0.00224514897, 0.00181356205, 0.00143446923, 
0.00111388259, 0.000859420809, 0.000678091517, 0.000577101735, 
0.000563847476, 0.00064590278, 0.000831008207, 0.00112705875, 
0.00153984779, 0.00207954744, 0.00275470302, 0.00357374415, 0.00454518084, 
0.00567758762, 0.00697958743, 0.00845983494, 0.0101269996, 0.0119897486, 
0.014055064, 0.0163333443, 0.0188332232, 0.0215631918, 0.0245316468, 
0.0277468735, 0.03121703, 0.034950131, 0.0389540334, 0.0431364795, 
0.0473307585, 0.0515448092, 0.0557776706, 0.0600281369, 0.0642955547, 
0.0685790261, 0.0728775875, 0.0771902878, 0.0815161895, 0.0858543713, 
0.0902039303, 0.0945639838, 0.0989336721, 0.10331216, 0.107698641, 
0.112092335, 0.116492495, 0.120898405, 0.125309384, 0.129724785, 
0.134143997, 0.138566428, 0.14299154, 0.147418835, 0.151847851, 
0.156278163, 0.160709387, 0.165141174, 0.169573215, 0.174005236, 
0.178437, 0.182868306, 0.187298986, 0.191728906, 0.196157962, 
0.200586086, 0.205013174, 0.209439071, 0.213863965, 0.218287899, 
0.222710942, 0.227133187, 0.231554749, 0.235975765, 0.240396394, 
0.244816813, 0.24923722, 0.253657797, 0.258078397, 0.262499662, 
0.266921859, 0.271345267, 0.275770179, 0.280196901, 0.28462575, 
0.289057057, 0.293491117, 0.297927865, 0.30236813, 0.306812282, 
0.311260703, 0.315713782, 0.320171913, 0.324635499, 0.329104836, 
0.333580106, 0.338062109, 0.342551272, 0.347048028, 0.351552815, 
0.356066072, 0.360588229, 0.365119408, 0.369660446, 0.374211795, 
0.37877391, 0.383347243, 0.387932249, 0.392529339, 0.397138877, 
0.401761511, 0.406397694, 0.411047871, 0.415712489, 0.420391986, 
0.425086807, 0.429797442, 0.434524335, 0.439267908, 0.444028574, 
0.448806744, 0.453602818, 0.45841742, 0.463250828, 0.468103387, 
0.472975465, 0.47786742, 0.482779603, 0.487712357, 0.492666544, 
0.497642038, 0.502639147, 0.507658169, 0.51269939, 0.517763087, 
0.522849522, 0.52795955, 0.533093083, 0.538250172, 0.543431038, 
0.54863589, 0.553864931, 0.559118349, 0.564396327, 0.569699633, 
0.57502827, 0.580382015, 0.585761012, 0.591165394, 0.596595287, 
0.602050811, 0.607532077, 0.61303919, 0.61857225, 0.624131362, 
0.629717516, 0.635329876, 0.640968508, 0.646633475, 0.652324832, 
0.65804263, 0.663786914, 0.66955772, 0.675355082, 0.681179025, 
0.687029567, 0.692906719, 0.698810484, 0.704740854, 0.710697814, 
0.716681336, 0.722691379, 0.72872789, 0.734790799, 0.74088002, 
0.746995448, 0.753136955, 0.75930439, 0.765498551, 0.771719833, 
0.777966775, 0.78423912, 0.790536569, 0.796858775, 0.803205337, 
0.809578605, 0.815977942, 0.82240062, 0.82884598, 0.83531536, 
0.84181173, 0.848328902, 0.854866468, 0.861432314, 0.868015998, 
0.874622194, 0.881250063, 0.887896125, 0.894563989, 0.901249365, 
0.907950379, 0.914672479, 0.921406537, 0.928152065, 0.93490773, 
0.941670605, 0.9484349, 0.95518986, 0.961916487, 0.968589814, 
0.975158357), B = c(0.527974883, 0.533123681, 0.538007001, 0.542657691, 
0.547103487, 0.551367851, 0.555467728, 0.55942348, 0.563250116, 
0.566959485, 0.570561711, 0.574065446, 0.577478074, 0.58080589, 
0.584054243, 0.587227661, 0.590329954, 0.593364304, 0.596333341, 
0.599239207, 0.602083323, 0.604867403, 0.607592438, 0.610259089, 
0.612867743, 0.615418537, 0.617911385, 0.620345997, 0.622721903, 
0.625038468, 0.627294975, 0.62949049, 0.631623923, 0.633694102, 
0.635699759, 0.637639537, 0.639512001, 0.641315649, 0.643048936, 
0.644710195, 0.646297711, 0.647809772, 0.649244641, 0.650600561, 
0.651875762, 0.653068467, 0.654176761, 0.655198755, 0.656132835, 
0.656977276, 0.65773038, 0.658390492, 0.658956004, 0.659425363, 
0.659797077, 0.660069009, 0.660240367, 0.660309966, 0.660276655, 
0.660139383, 0.65989721, 0.659549311, 0.659094989, 0.658533677, 
0.657864946, 0.657087561, 0.656202294, 0.655209222, 0.654108545, 
0.652900629, 0.65158601, 0.650165396, 0.648639668, 0.647009884, 
0.645277275, 0.64344325, 0.641509389, 0.63947744, 0.637348841, 
0.635126108, 0.632811608, 0.630407727, 0.627916992, 0.625342058, 
0.622685703, 0.619950811, 0.617140367, 0.61425744, 0.611305174, 
0.608286774, 0.605205491, 0.602064611, 0.598867442, 0.5956173, 
0.592317494, 0.588971318, 0.585582301, 0.582153572, 0.578688247, 
0.575189431, 0.571660158, 0.56810338, 0.564521958, 0.560918659, 
0.557296144, 0.55365697, 0.550003579, 0.546338299, 0.542663338, 
0.538980786, 0.535292612, 0.531600995, 0.527908434, 0.524215533, 
0.520523766, 0.516834495, 0.513148963, 0.509468305, 0.505793543, 
0.502125599, 0.49846529, 0.494813338, 0.491170517, 0.487539124, 
0.483917732, 0.480306702, 0.476706319, 0.473116798, 0.469538286, 
0.465970871, 0.46241458, 0.458869577, 0.455337565, 0.451816385, 
0.448305861, 0.444805781, 0.441315901, 0.437835947, 0.434365616, 
0.430905052, 0.427454836, 0.424013059, 0.420579333, 0.417153264, 
0.413734445, 0.410322469, 0.406916975, 0.403518809, 0.400126027, 
0.396738211, 0.393354947, 0.389975832, 0.386600468, 0.383228622, 
0.379860246, 0.376494232, 0.373130228, 0.369767893, 0.366406907, 
0.363046965, 0.359687758, 0.356328796, 0.352969777, 0.349610469, 
0.346250656, 0.342890148, 0.339528771, 0.336165582, 0.332800827, 
0.329434512, 0.32606655, 0.322696876, 0.319325444, 0.315952211, 
0.31257544, 0.309196628, 0.305815824, 0.302433101, 0.299048555, 
0.295662308, 0.292274506, 0.288883445, 0.285490391, 0.282096149, 
0.27870099, 0.275305214, 0.271909159, 0.2685132, 0.265117752, 
0.261721488, 0.258325424, 0.254931256, 0.251539615, 0.2481512, 
0.244766775, 0.241387186, 0.238013359, 0.234646316, 0.231287178, 
0.227937141, 0.224595006, 0.221264889, 0.217948456, 0.214647532, 
0.211364122, 0.208100426, 0.204858855, 0.201642049, 0.1984529, 
0.195294567, 0.1921705, 0.189084459, 0.186040537, 0.18304318, 
0.180097207, 0.177207826, 0.174380656, 0.171621733, 0.168937522, 
0.166334918, 0.163821243, 0.161404226, 0.159091984, 0.156890625, 
0.154807583, 0.152854862, 0.151041581, 0.149376885, 0.14786981, 
0.146529128, 0.145357284, 0.144362644, 0.143556679, 0.142945116, 
0.142528388, 0.142302653, 0.142278607, 0.142453425, 0.142808191, 
0.143350944, 0.144061156, 0.144922913, 0.145918663, 0.147014438, 
0.148179639, 0.149370428, 0.150520343, 0.151566019, 0.152409489, 
0.152921158, 0.152925363, 0.152177604, 0.150327944, 0.146860789, 
0.140955606, 0.131325517)), .Names = c("R", "G", "B"), class = "data.frame", row.names = c(NA, 
-256L))
clsD = structure(list(R = c(0.26700401, 0.26851048, 0.26994384, 0.27130489, 
0.27259384, 0.27380934, 0.27495242, 0.27602238, 0.2770184, 0.27794143, 
0.27879067, 0.2795655, 0.28026658, 0.28089358, 0.28144581, 0.28192358, 
0.28232739, 0.28265633, 0.28291049, 0.28309095, 0.28319704, 0.28322882, 
0.28318684, 0.283072, 0.28288389, 0.28262297, 0.28229037, 0.28188676, 
0.28141228, 0.28086773, 0.28025468, 0.27957399, 0.27882618, 0.27801236, 
0.27713437, 0.27619376, 0.27519116, 0.27412802, 0.27300596, 0.27182812, 
0.27059473, 0.26930756, 0.26796846, 0.26657984, 0.2651445, 0.2636632, 
0.26213801, 0.26057103, 0.25896451, 0.25732244, 0.25564519, 0.25393498, 
0.25219404, 0.25042462, 0.24862899, 0.2468114, 0.24497208, 0.24311324, 
0.24123708, 0.23934575, 0.23744138, 0.23552606, 0.23360277, 0.2316735, 
0.22973926, 0.22780192, 0.2258633, 0.22392515, 0.22198915, 0.22005691, 
0.21812995, 0.21620971, 0.21429757, 0.21239477, 0.2105031, 0.20862342, 
0.20675628, 0.20490257, 0.20306309, 0.20123854, 0.1994295, 0.1976365, 
0.19585993, 0.19410009, 0.19235719, 0.19063135, 0.18892259, 0.18723083, 
0.18555593, 0.18389763, 0.18225561, 0.18062949, 0.17901879, 0.17742298, 
0.17584148, 0.17427363, 0.17271876, 0.17117615, 0.16964573, 0.16812641, 
0.1666171, 0.16511703, 0.16362543, 0.16214155, 0.16066467, 0.15919413, 
0.15772933, 0.15626973, 0.15481488, 0.15336445, 0.1519182, 0.15047605, 
0.14903918, 0.14760731, 0.14618026, 0.14475863, 0.14334327, 0.14193527, 
0.14053599, 0.13914708, 0.13777048, 0.1364085, 0.13506561, 0.13374299, 
0.13244401, 0.13117249, 0.1299327, 0.12872938, 0.12756771, 0.12645338, 
0.12539383, 0.12439474, 0.12346281, 0.12260562, 0.12183122, 0.12114807, 
0.12056501, 0.12009154, 0.11973756, 0.11951163, 0.11942341, 0.11948255, 
0.11969858, 0.12008079, 0.12063824, 0.12137972, 0.12231244, 0.12344358, 
0.12477953, 0.12632581, 0.12808703, 0.13006688, 0.13226797, 0.13469183, 
0.13733921, 0.14020991, 0.14330291, 0.1466164, 0.15014782, 0.15389405, 
0.15785146, 0.16201598, 0.1663832, 0.1709484, 0.17570671, 0.18065314, 
0.18578266, 0.19109018, 0.19657063, 0.20221902, 0.20803045, 0.21400015, 
0.22012381, 0.2263969, 0.23281498, 0.2393739, 0.24606968, 0.25289851, 
0.25985676, 0.26694127, 0.27414922, 0.28147681, 0.28892102, 0.29647899, 
0.30414796, 0.31192534, 0.3198086, 0.3277958, 0.33588539, 0.34407411, 
0.35235985, 0.36074053, 0.3692142, 0.37777892, 0.38643282, 0.39517408, 
0.40400101, 0.4129135, 0.42190813, 0.43098317, 0.44013691, 0.44936763, 
0.45867362, 0.46805314, 0.47750446, 0.4870258, 0.49661536, 0.5062713, 
0.51599182, 0.52577622, 0.5356211, 0.5455244, 0.55548397, 0.5654976, 
0.57556297, 0.58567772, 0.59583934, 0.60604528, 0.61629283, 0.62657923, 
0.63690157, 0.64725685, 0.65764197, 0.66805369, 0.67848868, 0.68894351, 
0.69941463, 0.70989842, 0.72039115, 0.73088902, 0.74138803, 0.75188414, 
0.76237342, 0.77285183, 0.78331535, 0.79375994, 0.80418159, 0.81457634, 
0.82494028, 0.83526959, 0.84556056, 0.8558096, 0.86601325, 0.87616824, 
0.88627146, 0.89632002, 0.90631121, 0.91624212, 0.92610579, 0.93590444, 
0.94563626, 0.95529972, 0.96489353, 0.97441665, 0.98386829, 0.99324789
), G = c(0.00487433, 0.00960483, 0.01462494, 0.01994186, 0.02556309, 
0.03149748, 0.03775181, 0.04416723, 0.05034437, 0.05632444, 0.06214536, 
0.06783587, 0.07341724, 0.07890703, 0.0843197, 0.08966622, 0.09495545, 
0.10019576, 0.10539345, 0.11055307, 0.11567966, 0.12077701, 0.12584799, 
0.13089477, 0.13592005, 0.14092556, 0.14591233, 0.15088147, 0.15583425, 
0.16077132, 0.16569272, 0.17059884, 0.1754902, 0.18036684, 0.18522836, 
0.19007447, 0.1949054, 0.19972086, 0.20452049, 0.20930306, 0.21406899, 
0.21881782, 0.22354911, 0.2282621, 0.23295593, 0.23763078, 0.24228619, 
0.2469217, 0.25153685, 0.2561304, 0.26070284, 0.26525384, 0.26978306, 
0.27429024, 0.27877509, 0.28323662, 0.28767547, 0.29209154, 0.29648471, 
0.30085494, 0.30520222, 0.30952657, 0.31382773, 0.3181058, 0.32236127, 
0.32659432, 0.33080515, 0.334994, 0.33916114, 0.34330688, 0.34743154, 
0.35153548, 0.35561907, 0.35968273, 0.36372671, 0.36775151, 0.37175775, 
0.37574589, 0.37971644, 0.38366989, 0.38760678, 0.39152762, 0.39543297, 
0.39932336, 0.40319934, 0.40706148, 0.41091033, 0.41474645, 0.4185704, 
0.42238275, 0.42618405, 0.42997486, 0.43375572, 0.4375272, 0.44128981, 
0.4450441, 0.4487906, 0.4525298, 0.45626209, 0.45998802, 0.46370813, 
0.4674229, 0.47113278, 0.47483821, 0.47853961, 0.4822374, 0.48593197, 
0.4896237, 0.49331293, 0.49700003, 0.50068529, 0.50436904, 0.50805136, 
0.51173263, 0.51541316, 0.51909319, 0.52277292, 0.52645254, 0.53013219, 
0.53381201, 0.53749213, 0.54117264, 0.54485335, 0.54853458, 0.55221637, 
0.55589872, 0.55958162, 0.56326503, 0.56694891, 0.57063316, 0.57431754, 
0.57800205, 0.58168661, 0.58537105, 0.58905521, 0.59273889, 0.59642187, 
0.60010387, 0.60378459, 0.60746388, 0.61114146, 0.61481702, 0.61849025, 
0.62216081, 0.62582833, 0.62949242, 0.63315277, 0.63680899, 0.64046069, 
0.64410744, 0.64774881, 0.65138436, 0.65501363, 0.65863619, 0.66225157, 
0.66585927, 0.66945881, 0.67304968, 0.67663139, 0.68020343, 0.68376525, 
0.68731632, 0.69085611, 0.69438405, 0.6978996, 0.70140222, 0.70489133, 
0.70836635, 0.71182668, 0.71527175, 0.71870095, 0.72211371, 0.72550945, 
0.72888753, 0.73224735, 0.73558828, 0.73890972, 0.74221104, 0.74549162, 
0.74875084, 0.75198807, 0.75520266, 0.75839399, 0.76156142, 0.76470433, 
0.76782207, 0.77091403, 0.77397953, 0.7770179, 0.78002855, 0.78301086, 
0.78596419, 0.78888793, 0.79178146, 0.79464415, 0.79747541, 0.80027461, 
0.80304099, 0.80577412, 0.80847343, 0.81113836, 0.81376835, 0.81636288, 
0.81892143, 0.82144351, 0.82392862, 0.82637633, 0.82878621, 0.83115784, 
0.83349064, 0.83578452, 0.83803918, 0.84025437, 0.8424299, 0.84456561, 
0.84666139, 0.84871722, 0.8507331, 0.85270912, 0.85464543, 0.85654226, 
0.85839991, 0.86021878, 0.86199932, 0.86374211, 0.86544779, 0.86711711, 
0.86875092, 0.87035015, 0.87191584, 0.87344918, 0.87495143, 0.87642392, 
0.87786808, 0.87928545, 0.88067763, 0.88204632, 0.88339329, 0.88472036, 
0.88602943, 0.88732243, 0.88860134, 0.88986815, 0.89112487, 0.89237353, 
0.89361614, 0.89485467, 0.89609127, 0.89732977, 0.8985704, 0.899815, 
0.90106534, 0.90232311, 0.90358991, 0.90486726, 0.90615657), 
    B = c(0.32941519, 0.33542652, 0.34137895, 0.34726862, 0.35309303, 
    0.35885256, 0.36454323, 0.37016418, 0.37571452, 0.38119074, 
    0.38659204, 0.39191723, 0.39716349, 0.40232944, 0.40741404, 
    0.41241521, 0.41733086, 0.42216032, 0.42690202, 0.43155375, 
    0.43611482, 0.44058404, 0.44496, 0.44924127, 0.45342734, 
    0.45751726, 0.46150995, 0.46540474, 0.46920128, 0.47289909, 
    0.47649762, 0.47999675, 0.48339654, 0.48669702, 0.48989831, 
    0.49300074, 0.49600488, 0.49891131, 0.50172076, 0.50443413, 
    0.50705243, 0.50957678, 0.5120084, 0.5143487, 0.5165993, 
    0.51876163, 0.52083736, 0.52282822, 0.52473609, 0.52656332, 
    0.52831152, 0.52998273, 0.53157905, 0.53310261, 0.53455561, 
    0.53594093, 0.53726018, 0.53851561, 0.53970946, 0.54084398, 
    0.5419214, 0.54294396, 0.54391424, 0.54483444, 0.54570633, 
    0.546532, 0.54731353, 0.54805291, 0.54875211, 0.54941304, 
    0.55003755, 0.55062743, 0.5511844, 0.55171011, 0.55220646, 
    0.55267486, 0.55311653, 0.55353282, 0.55392505, 0.55429441, 
    0.55464205, 0.55496905, 0.55527637, 0.55556494, 0.55583559, 
    0.55608907, 0.55632606, 0.55654717, 0.55675292, 0.55694377, 
    0.5571201, 0.55728221, 0.55743035, 0.55756466, 0.55768526, 
    0.55779216, 0.55788532, 0.55796464, 0.55803034, 0.55808199, 
    0.55811913, 0.55814141, 0.55814842, 0.55813967, 0.55811466, 
    0.5580728, 0.55801347, 0.557936, 0.55783967, 0.55772371, 
    0.55758733, 0.55742968, 0.5572505, 0.55704861, 0.55682271, 
    0.55657181, 0.55629491, 0.55599097, 0.55565893, 0.55529773, 
    0.55490625, 0.55448339, 0.55402906, 0.55354108, 0.55301828, 
    0.55245948, 0.55186354, 0.55122927, 0.55055551, 0.5498411, 
    0.54908564, 0.5482874, 0.54744498, 0.54655722, 0.54562298, 
    0.54464114, 0.54361058, 0.54253043, 0.54139999, 0.54021751, 
    0.53898192, 0.53769219, 0.53634733, 0.53494633, 0.53348834, 
    0.53197275, 0.53039808, 0.52876343, 0.52706792, 0.52531069, 
    0.52349092, 0.52160791, 0.51966086, 0.5176488, 0.51557101, 
    0.5134268, 0.51121549, 0.50893644, 0.5065889, 0.50417217, 
    0.50168574, 0.49912906, 0.49650163, 0.49380294, 0.49103252, 
    0.48818938, 0.48527326, 0.48228395, 0.47922108, 0.47608431, 
    0.4728733, 0.46958774, 0.46622638, 0.46278934, 0.45927675, 
    0.45568838, 0.45202405, 0.44828355, 0.44446673, 0.44057284, 
    0.4366009, 0.43255207, 0.42842626, 0.42422341, 0.41994346, 
    0.41558638, 0.41115215, 0.40664011, 0.40204917, 0.39738103, 
    0.39263579, 0.38781353, 0.38291438, 0.3779385, 0.37288606, 
    0.36775726, 0.36255223, 0.35726893, 0.35191009, 0.34647607, 
    0.3409673, 0.33538426, 0.32972749, 0.32399761, 0.31819529, 
    0.31232133, 0.30637661, 0.30036211, 0.29427888, 0.2881265, 
    0.28190832, 0.27562602, 0.26928147, 0.26287683, 0.25641457, 
    0.24989748, 0.24332878, 0.23671214, 0.23005179, 0.22335258, 
    0.21662012, 0.20986086, 0.20308229, 0.19629307, 0.18950326, 
    0.18272455, 0.17597055, 0.16925712, 0.16260273, 0.15602894, 
    0.14956101, 0.14322828, 0.13706449, 0.13110864, 0.12540538, 
    0.12000532, 0.11496505, 0.11034678, 0.10621724, 0.1026459, 
    0.09970219, 0.09745186, 0.09595277, 0.09525046, 0.09537439, 
    0.09633538, 0.09812496, 0.1007168, 0.10407067, 0.10813094, 
    0.11283773, 0.11812832, 0.12394051, 0.13021494, 0.13689671, 
    0.1439362)), .Names = c("R", "G", "B"), class = "data.frame", row.names = c(NA, 
-256L))

colschemes = list(A=clsA, B=clsB, C=clsC, D=clsD)


viridis <- function(n, alpha = 1, begin = 0, end = 1, option = "D") {
    # https://github.com/sjmgarnier/viridis but w/o ggplot2 dependencies
  if (begin < 0 | end < 0 | begin > 1 | end > 1) {
    stop("begin and end must be in [0,1]")
  }

  option <- switch(option,
                   A = "A", magma = "A",
                   B = "B", inferno = "B",
                   C = "C", plasma = "C",
                   D = "D", viridis = "D",
                   {warning(paste0("Option '", option, "' does not exist. Defaulting to 'viridis'.")); "D"})

  # map <- viridis::viridis.map[viridis::viridis.map$opt == option, ]
  map = colschemes[[option]]
  map_cols <- grDevices::rgb(map$R, map$G, map$B)
  fn_cols <- grDevices::colorRamp(map_cols, space = "Lab", interpolate = "spline")
  cols <- fn_cols(seq(begin, end, length.out = n)) / 255
  grDevices::rgb(cols[, 1], cols[, 2], cols[, 3], alpha = alpha)
}

magma = function(n, alpha = 1, begin = 0, end = 1) {
  viridis(n, alpha, begin, end, option = "magma")
}

inferno = function(n, alpha = 1, begin = 0, end = 1) {
  viridis(n, alpha, begin, end, option = "inferno")
}

plasma = function(n, alpha = 1, begin = 0, end = 1) {
  viridis(n, alpha, begin, end, option = "plasma")
}

uured = '#F52A01' # the UU hexs seem off.
uuyel = '#FFCD00'
uublu = '#094D8E'
uugre = '#419702'
uupur = '#791D72'
uulbl = '#36A2C9'
uupnk = '#9A0000'
uuora = '#F08000'
uulgr = '#FEF200'
