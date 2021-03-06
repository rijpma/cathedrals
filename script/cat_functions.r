# functions and constants

overpass_baseurl <- "http://overpass-api.de/api/interpreter?data="

thinmargins <- c(4, 4, 3, 0.5)

m3y10lbl = "Church building per 10 years (m³)"
m3y20lbl = "Church building per 20 years (m³)"
m2y20lbl = "Church building per 20 years (m²)"
m3y20lblm = "Church building per 20 years (millions m³)"
m3y20pclbl = "Per capita church building per 20 years (m³)"
m3y20puclbl = "Per urban capita church building per 20 years (m³)"
m3y100lbl = "Church building per century (m³)"

cmap = c(
    it = "Italy",
    fr = "France",
    ch = "Switzerland",
    de = "Germany",
    be = "Belgium",
    nl = "Netherlands",
    uk = "Gt Britain"
)

axis1ks = function(side, ...){
    axis(side = side, 
        at = axTicks(2), 
        labels = formatC(
            x = axTicks(2),
            format = "d", 
            big.mark = ","))
}

coefse = function(m, regex, ...){
    cft = data.frame(lmtest::coeftest(m, ...)[])
    cft = cft[stringi::stri_subset_regex(rownames(cft), regex), 1:2]
    cft$up = cft[, 1] + 2*cft[, 2]
    cft$lo = cft[, 1] - 2*cft[, 2]

    return(cft)
}

annualised_growth = function(series, delta = 1){
    (series / data.table::shift(series)) ^ (1 / (delta)) - 1
}

add_loess = function(frm, dat, span=0.7, res=100, ...){
    # cannot handle NA atm
    pred = loess(frm, data=dat, span=span)
    dat$ftd = predict(pred, newdata=dat)
    fitfrm = update.formula(frm, ftd ~ .)
    lines(fitfrm, data=dat, ...)
}

rtnorm = function (n, mean = 0, sd = 1, min = -Inf, max = Inf) {
    # replace Trunc distributions from envstats

    # many deps so only take these two functions

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

add_borders = function(border=1, add=T){
    data(wrld_simpl)
    eur = wrld_simpl[wrld_simpl$REGION==150, ]
    eur = wrld_simpl[wrld_simpl$SUBREGION %in% c(39, 154, 155), ]
    eur = wrld_simpl[wrld_simpl$ISO3 %in% c("NLD", "BEL", "CHE", "FRA", "DEU", "GBR", "ITA"), ]
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

to_annual_obs = function(dyn){

    full = as.data.table(expand.grid(year = 0:2000, osmid = unique(dyn$osmid), stringsAsFactors = FALSE))
    data.table::setkey(full, osmid, year)
    data.table::setkey(dyn, osmid, year)
    full = dyn[full]

    full = full[order(osmid, -year), ]

    # number of observations per phase for safe interpolation
    full[, m2obs := sum(!is.na(m2)), by=osmid]
    full[, hgtobs := sum(!is.na(hgt)), by=osmid]
    full[, m3obs := sum(!is.na(m3)), by=osmid]
    full = full[!(m2obs <= 1 | hgtobs <= 1 | m3obs <= 1), ]

    # interpolation
    # default na.approx is rule = 1, so no interpolation outside of date range. 
    # This means first m2/m3 obs are not used for im2 or im3, good because we don't 
    # know when they started (or their starting dates)
    full[m2obs > 1, 
        im2 := zoo::na.approx(m2, method = 'constant', rule = 1, na.rm = F), 
        by = osmid]
    full[hgtobs > 1, 
        ihgt := zoo::na.approx(hgt, method = 'constant', rule = 1, na.rm = F), 
        by = osmid]
    full[m3obs > 1, 
        im3 :=  zoo::na.approx(m3, method = 'constant', rule = 1, na.rm = F), 
        by = osmid]

    full[, 
        inobs := zoo::na.approx(nobs, method='constant', rule = 1, na.rm = F), 
        by = osmid]
    full[m2obs > 1 & !is.na(inobs), iphaselength := zoo::na.approx(phaselength, method='constant', na.rm=F, rule=2:1), by=osmid]
    
    full[, 
        irestphase := zoo::na.approx(restphase, method='constant', na.rm=F, rule=1, f=0), 
        by = osmid]
    full[, im2_ann := im2 / iphaselength]
    full[irestphase==1, im2_ann := 0]
    full[firstobs==TRUE & !is.na(firstobs), im2_ann := 0]

    # check m3 treatment
    full[, im3_ann := im3/iphaselength]
    full[irestphase == 1, im3_ann := 0]
    full[firstobs == TRUE & !is.na(firstobs), im3_ann := 0]
    
    full = full[order(osmid, year), ]

    # cumulative 
    full[, 
        ibldindex := zoo::na.approx(bldindex, method = 'constant', rule = 1, na.rm = F), 
        by = osmid]
    full[is.na(newbld), newbld := FALSE]
    full[, osmid_buildindex := paste(osmid, ibldindex, sep = '_')]

    full[!is.na(im2_ann), 
        im2_cml := cumsum(im2_ann) + im2[newbld==TRUE], 
        by = osmid_buildindex]
    full[, 
        im2_cml := zoo::na.approx(im2_cml, method = 'constant', rule = 1:2, yleft = 0, na.rm = F), 
        by = osmid]
    full[!is.na(im3_ann), 
        im3_cml := cumsum(im3_ann) + im3[newbld == TRUE], 
        by = osmid_buildindex]
    full[, 
        im3_cml := zoo::na.approx(im3_cml, method = 'constant', rule = 1:2, yleft = 0, na.rm = F), 
        by = osmid]

    return(full)
}

to_dynobs = function(churchlist){
    dyn = do.call(rbind, lapply(churchlist, function(x) x$dynamic))
    dyn = data.table::as.data.table(dyn)

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

recombine_churches = function(churches, guesses=NULL, firstm2col = 5){
    
    fill = list()
    
    for (id in unique(churches$osmid[churches$osmid!=''])){
        church = as.data.frame(churches[osmid==id, ])
        dynvrbs = firstm2col:ncol(church)
        temp = data.frame(osmid = rep(id, length(dynvrbs)), 
                          year = integer(length(dynvrbs)),
                          m2 = integer(length(dynvrbs)),
                          hgt = integer(length(dynvrbs)),
                          gss_hgt = logical(length(dynvrbs)),
                          gss_m2 = logical(length(dynvrbs)))

        temp$year = unlist(as.numeric(church[2, dynvrbs]))
        temp$m2 = unlist(as.numeric(church[3, dynvrbs]))
        temp$hgt = unlist(as.numeric(church[4, dynvrbs]))

        if (is.null(guesses)){
            temp$gss_m2 = unlist(as.logical(as.numeric(church[6, dynvrbs])))
            temp$gss_hgt = unlist(as.logical(as.numeric(church[6, dynvrbs])))
        } else {
            temp$gss_hgt = unlist(guesses[osmid == id, ][4, dynvrbs, with=F]) == "guestimate"
            temp$gss_m2 = unlist(guesses[osmid == id, ][3, dynvrbs, with=F]) == "guestimate"            
        }
        temp = temp[!(is.na(temp$year) & is.na(temp$m2) & is.na(temp$hgt)), ]

        fill[[id]][["static"]] = church[1, 1:firstm2col]
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
            file=outfile, append=TRUE, col.names=FALSE, row.names=FALSE, sep=',')
        for (fillvrb in fillvrbs){
            write.table(cbind(dat@data[rw, c("osmid", "city", "osmname")], fillvrb), 
                file=outfile, append=TRUE, col.names=FALSE, row.names=FALSE, sep=',')
            
        }
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
    # rbind topo$ways$tags with topo$ways$relations
    osmtags <- rbind(topo$ways$tags, topo$relations$tags)

    # check uniqueness
    osmtags <- reshape(osmtags, direction='wide', idvar='id', timevar='k', v.names='v')
    osmtags <- factor2char(osmtags)
    
    names(osmtags) <- gsub("v[.]", "", names(osmtags))
    names(osmtags) <- paste0('osm', names(osmtags))
    
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
    
    rownames(polys@data)  = sapply(slot(polys, "polygons"), function(x) slot(x, "ID"))
    if (length(polylist) == 1){
        return(polylist[[1]])
    }
    for (i in 2:length(polylist)){
        polys4merge = polylist[[i]]
        duplpolys = polys4merge$id %in% polys$id
        duplpolys = duplpolys | duplicated(polys4merge$id)
        polys4merge$id[duplpolys] = paste0(polys4merge$id[duplpolys], '_', i)
        polys4merge = sp::spChFIDs(polys4merge, as.character(polys4merge$id))
        row.names(polys4merge@data) = as.character(polys4merge$id)

        # fix missing variables between spdfs
        polys4merge@data[, setdiff(names(polys), names(polys4merge))] = NA
        polys@data[, setdiff(names(polys4merge), names(polys))] = NA
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
    return(out)
}

filter_prox = function(ctr_gcd, siem_ctr, 
    xcoords = c("lon", "lat"),
    ycoords = c("east", "north")){
    failed = sapply(ctr_gcd, nrow) > 1
    ctr_2nd = ctr_gcd
    for (i in 1:sum(failed)){
        dsts = sp::spDistsN1(
            as.matrix(as.data.frame(ctr_gcd[failed][[i]])[, xcoords]),
            as.matrix(as.data.frame(siem_ctr[failed, ])[i, ycoords]), 
            longlat = T)
        if (min(dsts) < 2){
            ctr_gcd[failed][[i]] = ctr_gcd[failed][[i]][which.min(dsts), ]
        } else {
            cat(siem_ctr$city[failed][[i]], 'no deduplication < 2km \n\n')
        }
    }
    return(ctr_gcd)
}

check_geocodes = function(siem_ctr, ctr_gcd,
                          xcoords = c("lon", "lat"), 
                          ycoords = c("east", "north")){
    cmpr = data.frame(siem_ctr, do.call(rbind, ctr_gcd))
    dstmat = sp::spDists(
        as.matrix(as.data.frame(cmpr)[, xcoords]),
        as.matrix(as.data.frame(cmpr)[, ycoords]), longlat=T)
    cmpr$distance = diag(dstmat)
    return(cmpr)
}

grepr <- function(pattern, x, ...){
    idx <- grep(pattern, x, ...)
    return(x[idx])
}

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
    # original osm fetch used for French churches
    basequery <-   
        '[out:xml][timeout:900];
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
    basequery <-   
        '[out:xml][timeout:900];
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
        basequery <-   
        '[out:xml][timeout:900];
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

geocode <- function(loc, reg = "", bounds = "", apikey = NA){

    if (length(loc) > 1) loc <- loc[1] # geocode api takes only one location
    loc <- loc

    base <- "https://maps.googleapis.com/maps/api/geocode/json?address="
    request <- paste0(base, loc, '&region=', reg)
    request <- paste0(request, '&bounds=', bounds)
    if(!is.na(apikey)) request <- paste0(request, '&key=', apikey)

    request <- URLencode(request)
    result <- readLines(url(request))
    closeAllConnections()
    Sys.sleep(0.2) # max 5 calls per sec

    result <- paste(result, collapse='')
    result <- jsonlite::fromJSON(result)

    if (result$status != "OK"){
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

distmatch <- function(lonlat1, lonlat2, maxdist=0.1){
    distm <- sp::spDists(lonlat1, lonlat2, longlat=T)
    ids <- which(distm < maxdist)
    rws <- row(distm)[distm < maxdist]
    cls <- col(distm)[distm < maxdist]

    dd <- data.frame(lonlat1=rws, lonlat2=cls, dist=distm[ids])
    if (anyDuplicated(dd$lonlat1)) warning('multiple objects in radius')
    dd <- do.call(rbind, lapply(split(dd, dd$lonlat1), function(x) x[which.min(x$dist), ]))

    return(dd)
}

# old version used for French churches
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
