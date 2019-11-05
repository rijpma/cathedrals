# create rural sample dataset and compare with original

rm(list=ls())

setwd("~/dropbox/cathedrals/")
options(stringsAsFactors=FALSE)

library("maptools")
library("raster")
library("data.table")

source("script/cat_functions.r")

# urban church building data
fullobs_sp_urb = data.table::fread("gunzip -c dat/fullobs_sp.csv.gz")
fullobs_sp_urb = fullobs_sp_urb[ctr != "it"]
fullobs_sp_urb[, decade2:=(trunc((year - 1) / 50) + 1) * 50] # so 1500 = 1481-1500

M = 9 # imputations
impvrbs = names(fullobs_sp_urb)[grep("im3_ann\\d", names(fullobs_sp_urb))]

# city pop data
siem = data.table::fread("dat/siem_long_1500.csv", encoding="UTF-8")
siem = siem[country %in% c("France", "UK", "Netherlands", "Belgium", "Germany")] #, "Italy")]

# map data
data(wrld_simpl, package = "maptools")

nld = raster::getData("GADM", country='NLD', level=0)
fra = raster::getData("GADM", country='FRA', level=0)
che = raster::getData("GADM", country='CHE', level=0)
bel = raster::getData("GADM", country='BEL', level=0)
gbr = raster::getData("GADM", country='GBR', level=0)
deu = raster::getData("GADM", country='DEU', level=0)

# rural data
rur = rgdal::readOGR("dat/gis/rur.shp") # original source shapefil
rur_chr = data.table::fread("dat/rurchurches_eb.csv", skip=1)
setnames(rur_chr, "central city", "centre")

rur_chr[, osmwikipedia := iconv(osmwikipedia, from='macroman', to='utf8')]
rur_chr[, osmname := iconv(osmname, from='macroman', to='utf8')]
rur_chr[, city := iconv(city, from='macroman', to='utf8')]
rur_chr[, centre := iconv(centre, from='macroman', to='utf8')]
rur_chr[, osmid := as.character(osmid)]

if   (!all(validUTF8(rur_chr$city))
    | !all(validUTF8(rur_chr$osmname))
    | !all(validUTF8(rur_chr$osmwikipedia))){
    warning("encoding misery")
}
if (all(table(rur_chr$osmid[rur_chr$osmid!='']) != 6)){
    warning("not all entries 6 lines")
}

any(grepl("\\D|[.]", unlist(rur_chr[surface == 'height', 12:ncol(rur_chr)])))

chrlist_rur = rur_chrlist_rur = recombine_churches(churches=rur_chr, guesses=NULL, firstm2col = 12)
# NA warnings ok

# drop ruins < 1000
chrlist_rur = chrlist_rur[!names(chrlist_rur) %in% c("139892873", "60299707", "167116576", "117627973")]

statobs_rur = do.call(rbind, lapply(chrlist_rur, `[[`, 'static')) 
statobs_rur = data.table::as.data.table(statobs_rur)

dynobs_rur = to_dynobs(churchlist=chrlist_rur)

fullobs_rur = to_annual_obs(dyn=dynobs_rur)
fullobs_rur[, decade:=(trunc((year - 1) / 20) + 1) * 20] # so 1500 = 1481-1500

dynobs_rur_sp = merge(dynobs_rur, statobs_rur, by="osmid")
dim(fullobs_rur)
fullobs_sp_rur = merge(fullobs_rur, statobs_rur, by="osmid", all=T)
dim(fullobs_sp_rur)
fullobs_sp_rur = fullobs_sp_rur[!is.na(year), ]

if   (length(unique(table(fullobs_sp_rur$year, useNA='ifany'))) > 1
    | length(unique(table(fullobs_sp_rur$osmid, useNA='ifany'))) > 1){
    warning("unbalanced dataset")
}

rurcit = c("Peterborough (Medeshamstede)", "Chester", "Amiens",
    "Toulouse", "Dijon", "Maastricht", "Osnabrueck (Osnabrück)",
    "Nuernberg (Nürnberg)")
sqrs = siem[city %in% rurcit]
sqrs = sqrs[, .SD[1, ], by="city"]

sqrs[, top := lat + km2lat(50)]
sqrs[, bot := lat - km2lat(50)]
sqrs[, lft := lon - km2lon(50, lat=lat)]
sqrs[, rgt := lon + km2lon(50, lat=lat)]

siem[, top := lat + km2lat(5)]
siem[, bot := lat - km2lat(5)]
siem[, lft := lon - km2lon(5, lat=lat)]
siem[, rgt := lon + km2lon(5, lat=lat)]

png('figs/researcharea.png', width=1080, height=1080)
proj4string(rur) = sp::CRS("+proj=longlat +datum=WGS84 +no_defs ")
# plot(rur, border=1)
# points(lat ~ lon, data=siem, pch=3)
plot(nld, lwd = 0.5, 
    xlim = range(fullobs_sp_urb$lon),
    ylim = range(fullobs_sp_urb$lat))
lines(fra, lwd = 0.5)
lines(che, lwd = 0.5)
lines(bel, lwd = 0.5)
lines(gbr, lwd = 0.5)
lines(deu, lwd = 0.5)
# lines(ita, lwd = 0.5)

# remove norther ireland
rect(xleft = -8.3, ybottom = 54, xright = -5.3, ytop = 55.3, col = 'white', border = NA)

# urban data
points(lat ~ lon, data=fullobs_sp_urb, pch = 20, cex = 0.7, col = 'gray')

# rural data
points(coordinates(rur), pch = 20, cex = 0.8)
rect(xleft = siem$lft, ybottom = siem$bot, 
     xright = siem$rgt, ytop = siem$top, 
     lwd=NA, border='gray', col = 'gray')
rect(xleft = sqrs$lft, ybottom = sqrs$bot, 
     xright = sqrs$rgt, ytop = sqrs$top, lwd=1, border=1)
axis(1); axis(2)
dev.off()

# research squares into spatialpolys
sqrlist = list()
for (i in 1:nrow(sqrs)){
    sqrlist[[i]] = raster::extent(unlist(sqrs[i, list(lft, rgt, bot, top)]))
}
sqpols = lapply(sqrlist, as, "SpatialPolygons")

sfc_fr = geosphere::areaPolygon(wrld_simpl[wrld_simpl$UN==250, ])
sfc_de = geosphere::areaPolygon(wrld_simpl[wrld_simpl$UN==276, ])
sfc_nl = geosphere::areaPolygon(wrld_simpl[wrld_simpl$UN==528, ])
sfc_be = geosphere::areaPolygon(wrld_simpl[wrld_simpl$UN==56, ])
sfc_uk = geosphere::areaPolygon(wrld_simpl[wrld_simpl$UN==826, ])
sfc_ch = geosphere::areaPolygon(wrld_simpl[wrld_simpl$UN==756, ])

fr_sfc = data.frame(
    rural = (geosphere::areaPolygon(sqpols[[1]]) +  
             geosphere::areaPolygon(sqpols[[2]]) +
             geosphere::areaPolygon(sqpols[[3]])) / sfc_fr, 
    urban = (siem[country=="France", length(unique(city))] * 100 * 1e6) / sfc_fr)
uk_sfc = data.frame(
    rural = (geosphere::areaPolygon(sqpols[[4]]) + 
             geosphere::areaPolygon(sqpols[[5]])) / sfc_uk, 
    urban = (siem[country=="UK", length(unique(city))] * 100 * 1e6) / sfc_uk)
lc_sfc = data.frame(
    rural = (geosphere::areaPolygon(sqpols[[6]])) / sfc_nl, 
    urban = (siem[country %in% c("Netherlands", "Belgium"), length(unique(city))] * 100 * 1e6) / sfc_nl)
de_sfc = data.frame(
    rural = (geosphere::areaPolygon(sqpols[[7]]) + 
             geosphere::areaPolygon(sqpols[[8]])) / sfc_de, 
    urban = (siem[country=="Germany", length(unique(city))] * 100 * 1e6) / sfc_de)
al_sfc = data.frame(
    rural = sum(sapply(sqpols, geosphere::areaPolygon)) / (sfc_fr + sfc_de + sfc_nl + sfc_be + sfc_uk + sfc_ch),
    urban = siem[, length(unique(city))] * 100 * 1e6 / 
        (sfc_fr + sfc_de + sfc_nl + sfc_be + sfc_uk + sfc_ch)) # + sfc_it))

al = fullobs_sp_urb[, list(urban=sum(im3_ann, na.rm=T) / 1e6), by = decade]
al_rur = fullobs_sp_rur[centre %in% rurcit, list(rural=sum(im3_ann, na.rm=T) / 1e6), by = decade]
al = al[al_rur, on = 'decade'][data.table::between(decade, 700, 1500)]
al[, rural_mlp := (1 / al_sfc$rural) * rural]
al[, combined := rural_mlp + urban]

pdf("figs/ruralcorrections_eu.pdf", height = 3.5, width = 9)
par(mfrow = c(1, 3), font.main = 1, mar=c(4, 4, 1.5, 0.5))
plot(combined ~ decade, data = al, type = 'n', bty = 'l',
    ylab = m3y20lblm, xlab = '', main = "Urban")
lines(urban ~ decade, data = al, type = 'l')
lines(rural_mlp ~ decade, data = al, type = 'l', col = 'gray')
plot(combined ~ decade, data = al, type = 'n', bty = 'l',
    ylab = '', main = "Rural sample (scaled to full area)")
lines(rural_mlp ~ decade, data = al, type = 'l')
lines(urban ~ decade, data = al, type = 'l', col = 'gray')
plot(combined ~ decade, data = al, type = 'n', bty = 'l',
    ylab = '', xlab = '', main = "Combined")
lines(combined ~ decade, data = al, type = 'l')
dev.off()

# by country

# set to 50y period?
declength = 20
fullobs_sp_urb[, decade := (trunc((year - 1) / declength) + 1) * declength] # so 1500 = 1481-1500
fullobs_sp_rur[, decade := (trunc((year - 1) / declength) + 1) * declength] # so 1500 = 1481-1500

fr = fullobs_sp_urb[
    data.table::between(year, 700, 1500) & ctr=="fr", 
    list(urban = base::sum(.SD, na.rm=T) / M), 
    by = decade, 
    .SDcols = impvrbs]
de = fullobs_sp_urb[
    data.table::between(year, 700, 1500) & ctr=="de", 
    list(urban = base::sum(.SD, na.rm=T) / M), 
    by = decade, 
    .SDcols = impvrbs]
uk = fullobs_sp_urb[
    data.table::between(year, 700, 1500) & ctr=="uk", 
    list(urban = base::sum(.SD, na.rm=T) / M), 
    by = decade, 
    .SDcols = impvrbs]
lc = fullobs_sp_urb[
    data.table::between(year, 700, 1500) & ctr=="nl" | ctr=="be", 
    list(urban = base::sum(.SD, na.rm=T) / M), 
    by = decade, 
    .SDcols = impvrbs]

fr_rur = fullobs_sp_rur[
    data.table::between(year, 700, 1500) & centre %in% c("Toulouse", "Dijon", "Amiens"), 
    list(rural=sum(im3_ann, na.rm=T)), 
    by=decade]
de_rur = fullobs_sp_rur[
    data.table::between(year, 700, 1500) & centre %in% c("Nuernberg (Nürnberg)", "Osnabrueck (Osnabrück)"), 
    list(rural=sum(im3_ann, na.rm=T)), 
    by=decade]
uk_rur = fullobs_sp_rur[
    data.table::between(year, 700, 1500) & centre %in% c("Chester", "Peterborough (Medeshamstede)"), 
    list(rural=sum(im3_ann, na.rm=T)), 
    by=decade]
lc_rur = fullobs_sp_rur[
    data.table::between(year, 700, 1500) & centre %in% c("Maastricht"), 
    list(rural=sum(im3_ann, na.rm=T)), 
    by=decade]

fr = fr[fr_rur, on = 'decade']
de = de[de_rur, on = 'decade']
uk = uk[uk_rur, on = 'decade']
lc = lc[lc_rur, on = 'decade']

fr[, ctr := "fr"]
de[, ctr := "de"]
uk[, ctr := "uk"]
lc[, ctr := "lc"]

fr[, combined := urban + rural * 1 / fr_sfc[, 'rural']]
de[, combined := urban + rural * 1 / de_sfc[, 'rural']]
uk[, combined := urban + rural * 1 / uk_sfc[, 'rural']]
lc[, combined := urban + rural * 1 / lc_sfc[, 'rural']]

fr[, urban_sm := predict(loess(urban ~ decade, data = .SD))]
fr[, rural_sm := predict(loess(rural ~ decade, data = .SD))]
de[, urban_sm := predict(loess(urban ~ decade, data = .SD))]
de[, rural_sm := predict(loess(rural ~ decade, data = .SD))]
uk[, urban_sm := predict(loess(urban ~ decade, data = .SD))]
uk[, rural_sm := predict(loess(rural ~ decade, data = .SD))]
lc[, urban_sm := predict(loess(urban ~ decade, data = .SD))]
lc[, rural_sm := predict(loess(rural ~ decade, data = .SD))]

ctrobs = rbindlist(list(fr, de, uk, lc))
ctrobs[, lapply(.SD, sum), by=decade, .SDcols=c("urban", "rural", "combined")]

cat("overall rural urban correlation: ")
ctrobs[, cor(urban, rural)]

cat("rural urban correlation, by country: ")
ctrobs[, cor(urban, rural), by = ctr]

cat("rural urban correlation, smoothed, by country: ")
ctrobs[, cor(urban_sm, rural_sm), by = ctr]

cat("overall rural urban correlation, first diff: ")
ctrobs[, list(urban_diff = urban - data.table::shift(urban), 
        rural_diff = rural - data.table::shift(rural)), by = ctr][, 
    cor(urban_diff, rural_diff, use = 'pairwise.complete')]

cat("overall rural urban correlation, first diff by country ")
ctrobs[, 
    cor(urban - data.table::shift(urban), rural - data.table::shift(rural), use = 'pairwise.complete'), 
    by = ctr]

cat("overall rural urban correlation, first diff, smoothed,  by country ")
ctrobs[, 
    cor(urban_sm - data.table::shift(urban_sm), rural_sm - data.table::shift(rural_sm), use = 'pairwise.complete'), 
    by = ctr]

cat("correlation matrix")
al[, cor(.SD - data.table::shift(.SD), use = "pairwise.complete"), .SDcols = c("urban", "rural", "combined")]

pdf('figs/ruralcorrections_smt.pdf', height = 10, width = 8)
par(mfrow=c(4, 3), mar=c(4, 4, 0.5, 0.5), font.main = 1, bty = 'l')
plot(rural / 1e6 ~ decade, data=fr, type='l', col='gray', ylab = m3y20lblm, xlab = "")
title(main='France, rural', line = -0.8)
add_loess(rural / 1e6 ~ decade, dat=fr, span=0.5)
plot(urban / 1e6 ~ decade, data=fr, type='l', col='gray', ylab = "", xlab = "")
title(main='France, urban', line = -0.8)
add_loess(urban / 1e6 ~ decade, dat=fr, span=0.5)
plot(combined / 1e6 ~ decade, data=fr, type='l', col='gray', ylab = "", xlab = "")
title(main='France, comb.', line = -0.8)
add_loess(combined / 1e6 ~ decade, dat=fr, span=0.5)

plot(rural / 1e6 ~ decade, data=de, type='l', col='gray', ylab = m3y20lblm, xlab = "")
title(main='Germany, rural', line = -0.8)
add_loess(rural / 1e6 ~ decade, dat=de, span=0.5)
plot(urban / 1e6 ~ decade, data=de, type='l', col='gray', ylab = "", xlab = "")
title(main='Germany, urban', line = -0.8)
add_loess(urban / 1e6 ~ decade, dat=de, span=0.5)
plot(combined / 1e6 ~ decade, data=de, type='l', col='gray', ylab = "", xlab = "")
title(main='Germany, comb.', line = -0.8)
add_loess(combined / 1e6 ~ decade, dat=de, span=0.5)

plot(rural / 1e6 ~ decade, data=lc, type='l', col='gray', ylab = m3y20lblm, xlab = "year")
title(main='Low Countries, rural', line = -0.8)
add_loess(rural / 1e6 ~ decade, dat=lc, span=0.5)
plot(urban / 1e6 ~ decade, data=lc, type='l', col='gray', ylab = "", xlab = "year")
title(main='Low Countries, urban', line = -0.8)
add_loess(urban / 1e6 ~ decade, dat=lc, span=0.5)
plot(combined / 1e6 ~ decade, data=lc, type='l', col='gray', ylab = "", xlab = "year")
title(main='Low Countries, comb.', line = -0.8)
add_loess(combined / 1e6 ~ decade, dat=lc, span=0.5)

plot(rural / 1e6 ~ decade, data=uk, type='l', col='gray', ylab = m3y20lblm, xlab = "")
title(main='Gt Britain, rural', line = -0.8)
add_loess(rural / 1e6~ decade, dat=uk, span=0.5)
plot(urban / 1e6 ~ decade, data=uk, type='l', col='gray', ylab = "", xlab = "")
title(main='Gt Britain, urban', line = -0.8)
add_loess(urban / 1e6~ decade, dat=uk, span=0.5)
plot(combined / 1e6 ~ decade, data=uk, type='l', col='gray', ylab = "", xlab = "")
title(main='Gt Britain, comb.', line = -0.8)
add_loess(combined / 1e6 ~ decade, dat=uk, span=0.5)

dev.off()
