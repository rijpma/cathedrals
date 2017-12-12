rm(list=ls())
setwd("~/dropbox/cathedrals/")
options(stringsAsFactors=FALSE)

library("maptools")
library("raster")
library("data.table")

source("script/cat_functions.r")

M = 9 # imputations

rur_chr = data.table::fread("dat/rurchurches_eb.csv", skip=1)
setnames(rur_chr, "central city", "centre")

rur_chr[, osmwikipedia:=iconv(osmwikipedia, from='macroman', to='utf8')]
rur_chr[, osmname:=iconv(osmname, from='macroman', to='utf8')]
rur_chr[, city:=iconv(city, from='macroman', to='utf8')]
rur_chr[, centre:=iconv(centre, from='macroman', to='utf8')]

grep("√", rur_chr$city)
grep("xyz", rur_chr$osmname)
grep("xyz", rur_chr$city)
grep("xyz", rur_chr$osmwikipedia)

table(table(rur_chr$osmid[rur_chr$osmid!='']))
duplids = names(table(rur_chr$osmid[rur_chr$osmid!='']))[table(rur_chr$osmid[rur_chr$osmid!='']) > 6]
rur_chr[osmid %in% duplids, ]
rur_chr[, osmid := as.character(osmid)]

sort(unique(unlist(rur_chr[surface == 'height', 12:ncol(rur_chr)])))
sort(unique(unlist(rur_chr[surface == 'surface', 12:ncol(rur_chr)])))
sort(unique(unlist(rur_chr[surface == 'year', 12:ncol(rur_chr)])))

chrlist_rur = rur_chrlist_rur = recombine_churches(churches=rur_chr, guesses=NULL, firstm2col = 12)

# drop ruins < 1000
chrlist_rur = chrlist_rur[!names(chrlist_rur) %in% c("139892873", "60299707", "167116576", "117627973")]

statobs_rur = do.call(rbind, lapply(chrlist_rur, `[[`, 'static')) 
statobs_rur = data.table::as.data.table(statobs_rur)

dynobs_rur = to_dynobs(churchlist=chrlist_rur)

fullobs_rur = to_annual_obs(dyn=dynobs_rur, churchlist=chrlist_rur)
fullobs_rur[, decade:=(trunc((year - 1) / 20) + 1) * 20] # so 1500 = 1481-1500
fullobs_rur[, im3_ann_cmc := im3_ann + im3_cml * 0.005]

citobs = to_city_obs(statobs=statobs_rur, fullobs=fullobs_rur)

dynobs_rur_sp = merge(dynobs_rur, statobs_rur, by="osmid")
dim(fullobs_rur)
fullobs_sp_rur = merge(fullobs_rur, statobs_rur, by="osmid", all=T)
dim(fullobs_sp_rur)
dynobs_rur[fullobs_sp_rur[is.na(year), osmid], ]
fullobs_sp_rur = fullobs_sp_rur[!is.na(year), ]
unique(table(fullobs_sp_rur$year, useNA='ifany'))

# fullobs_sp_rur[centre %in% c("Toulouse", "Dijon", "Amiens"), ctr := "fr"]
# fullobs_sp_rur[centre %in% c("Nuernberg (Nürnberg)", "Osnabrueck (Osnabrück)"), ctr := "de"]
# fullobs_sp_rur[centre %in% c("Chester", "Peterborough (Medeshamstede)"), ctr := "uk"]
# fullobs_sp_rur[centre %in% c("Maastricht"), ctr := "lc"]

fullobs_sp_urb = data.table::fread("cat dat/fullobs_sp.csv.gz | gunzip")
# fullobs_sp_urb[ctr %in% c("nl", "be"), ctr := "lc"]

fullobs_sp_urb[, decade2:=(trunc((year - 1) / 50) + 1) * 50] # so 1500 = 1481-1500
fullobs_sp_urb[, century:=(trunc((year - 1) / 100) + 1) * 100] # so 1500 = 1481-1500
fullobs_sp_rur[, century:=(trunc((year - 1) / 100) + 1) * 100] # so 1500 = 1481-1500

rur = maptools::readShapeSpatial("dat/gis/rur")
siem = data.table::fread("dat/siem_long.csv", encoding="UTF-8")
data(wrld_simpl)

nld = raster::getData("GADM", country='NLD', level=0)
fra = raster::getData("GADM", country='FRA', level=0)
che = raster::getData("GADM", country='CHE', level=0)
bel = raster::getData("GADM", country='BEL', level=0)
gbr = raster::getData("GADM", country='GBR', level=0)
deu = raster::getData("GADM", country='DEU', level=0)

sqrs = siem[city == "Peterborough (Medeshamstede)" | 
            city == "Chester" |
            city == "Amiens" |
            city == "Toulouse" |
            city == "Dijon" |
            city == "Maastricht" |
            city == "Osnabrueck (Osnabrück)" |
            city == "Nuernberg (Nürnberg)"]
sqrs = sqrs[, .SD[1, ], by="city"]

sqrs[, top := lat + km2lat(50)]
sqrs[, bot := lat - km2lat(50)]
sqrs[, lft := lon - km2lon(50, lat=lat)]
sqrs[, rgt := lon + km2lon(50, lat=lat)]
rurcit = c("Peterborough (Medeshamstede)", "Chester", "Amiens",
    "Toulouse", "Dijon", "Maastricht", "Osnabrueck (Osnabrück)",
    "Nuernberg (Nürnberg)")
# siem[, sqr := ifelse(city %in% rurcit, 50, 5)]
siem[, top := lat + km2lat(5)]
siem[, bot := lat - km2lat(5)]
siem[, lft := lon - km2lon(5, lat=lat)]
siem[, rgt := lon + km2lon(5, lat=lat)]

siem[city == "Peterborough (Medeshamstede)" | 
            city == "Chester" |
            city == "Amiens" |
            city == "Toulouse" |
            city == "Dijon" |
            city == "Maastricht" |
            city == "Osnabrueck (Osnabrück)" |
            city == "Nuernberg (Nürnberg)"]

png('figs/researcharea.png', width=720, height=720)
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

points(coordinates(rur), pch = 20, cex = 0.8)
# points(lat ~ lon, data=fullobs_sp_urb, pch='.')
# points(lat ~ lon, data=siem, pch=3)
rect(xleft = siem$lft[siem$country!="Italy"], ybottom = siem$bot[siem$country!="Italy"], 
     xright = siem$rgt[siem$country!="Italy"], ytop = siem$top[siem$country!="Italy"], 
     lwd=NA, border='gray', col = 'gray')
rect(xleft = sqrs$lft, ybottom = sqrs$bot, 
     xright = sqrs$rgt, ytop = sqrs$top, lwd=1, border=1)
axis(1); axis(2)
dev.off()

sqrlist = list()
for (i in 1:nrow(sqrs)){
    sqrlist[[i]] = raster::extent(unlist(sqrs[i, list(lft, rgt, bot, top)]))
    # assign(paste0("sq", i), raster::extent(unlist(sqrs[i, list(lft, rgt, bot, top)])))
}

p = raster::stack("~/downloads/data/hyde/700AD_pop/popc_700AD.asc",
    "~/downloads/data/hyde/800AD_pop/popc_800AD.asc",
    "~/downloads/data/hyde/900AD_pop/popc_900AD.asc",
    "~/downloads/data/hyde/1000AD_pop/popc_1000AD.asc",
    "~/downloads/data/hyde/1100AD_pop/popc_1100AD.asc",
    "~/downloads/data/hyde/1200AD_pop/popc_1200AD.asc",
    "~/downloads/data/hyde/1300AD_pop/popc_1300AD.asc",
    "~/downloads/data/hyde/1400AD_pop/popc_1400AD.asc",
    "~/downloads/data/hyde/1500AD_pop/popc_1500AD.asc",
    "~/downloads/data/hyde/1600AD_pop/popc_1600AD.asc")
p = raster::crop(p, raster::extent(-10, 20, 40, 60))

isor = raster::raster("~/downloads/data/hyde/iso_cr.asc")
isor = raster::crop(isor, raster::extent(-10, 20, 40, 60))


# sqrlist[[1]] = raster::extent(unlist(sqrs[1, list(lft, rgt, bot, top)]))

colSums(p[isor==250])

sqrs[, 1:2]
france  = colSums(raster::extract(x = p, y = sqrlist[[1]]), na.rm=T) + 
    colSums(raster::extract(x = p, y = sqrlist[[2]]), na.rm=T) + 
    colSums(raster::extract(x = p, y = sqrlist[[3]]), na.rm=T)
britain = colSums(raster::extract(x = p, y = sqrlist[[4]]), na.rm=T) + 
    colSums(raster::extract(x = p, y = sqrlist[[5]]), na.rm=T)
lowctr  = colSums(raster::extract(x = p, y = sqrlist[[6]]), na.rm=T)
germany = colSums(raster::extract(x = p, y = sqrlist[[7]]), na.rm=T) + 
    colSums(raster::extract(x = p, y = sqrlist[[8]]), na.rm=T)


# population
cbind(france, siem[country=="France" & year <= 1600, sum(inhab) * 1000, by=year])

fr_pop = data.frame(rural = france / colSums(p[isor==250]), 
    urban = siem[country=="France" & year <= 1600, sum(inhab) * 1000, by=year][, V1] / colSums(p[isor==250]))
de_pop = data.frame(rural = germany / colSums(p[isor==276]), 
    urban = siem[country=="Germany" & year <= 1600, sum(inhab) * 1000, by=year][, V1] / colSums(p[isor==276]))
uk_pop = data.frame(rural = britain / colSums(p[isor==826]), 
    urban = siem[country=="UK" & year <= 1600, sum(inhab) * 1000, by=year][, V1] / colSums(p[isor==826]))
lc_pop = data.frame(rural = lowctr / colSums(p[isor==528 | isor==56]), 
    urban = siem[country %in% c("Netherlands", "Belgium") & year <= 1600, sum(inhab) * 1000, by=year][, V1] / colSums(p[isor==528 | isor==56]))

sfc_fr = geosphere::areaPolygon(wrld_simpl[wrld_simpl$UN==250, ])
sfc_de = geosphere::areaPolygon(wrld_simpl[wrld_simpl$UN==276, ])
sfc_nl = geosphere::areaPolygon(wrld_simpl[wrld_simpl$UN==528, ])
sfc_be = geosphere::areaPolygon(wrld_simpl[wrld_simpl$UN==56, ])
sfc_uk = geosphere::areaPolygon(wrld_simpl[wrld_simpl$UN==826, ])
sfc_ch = geosphere::areaPolygon(wrld_simpl[wrld_simpl$UN==756, ])

sfc_fr / 1e6
sfc_de / 1e6
sfc_nl / 1e6
sfc_be / 1e6
sfc_uk / 1e6
sfc_ch / 1e6

sqpols = lapply(sqrlist, as, "SpatialPolygons")

fr_sfc = data.frame(rural = (geosphere::areaPolygon(sqpols[[1]]) +  
                             geosphere::areaPolygon(sqpols[[2]]) +
                             geosphere::areaPolygon(sqpols[[3]])) / sfc_fr, 
                    urban = (siem[country=="France", length(unique(city))] * 100 * 1e6) / sfc_fr)
uk_sfc = data.frame(rural = (geosphere::areaPolygon(sqpols[[4]]) + 
                             geosphere::areaPolygon(sqpols[[5]])) / sfc_uk, 
                    urban = (siem[country=="UK", length(unique(city))] * 100 * 1e6) / sfc_uk)
lc_sfc = data.frame(rural = (geosphere::areaPolygon(sqpols[[6]])) / sfc_nl, 
                    urban = (siem[country %in% c("Netherlands", "Belgium"), length(unique(city))] * 100 * 1e6) / sfc_nl)
de_sfc = data.frame(rural = (geosphere::areaPolygon(sqpols[[7]]) + 
                             geosphere::areaPolygon(sqpols[[8]])) / sfc_de, 
                    urban = (siem[country=="Germany", length(unique(city))] * 100 * 1e6) / sfc_de)

al_sfc = data.frame(rural = sum(sapply(sqpols, geosphere::areaPolygon)) / (sfc_fr + sfc_de + sfc_nl + sfc_be + sfc_uk + sfc_ch),
    urban = siem[country %in% c("France", "UK", "Netherlands", "Belgium", "Germany"), length(unique(city))] * 100 * 1e6 / 
        (sfc_fr + sfc_de + sfc_nl + sfc_be + sfc_uk + sfc_ch))

round(fr_pop, 3)
round(fr_sfc, 3)
round(de_pop, 3)
round(de_sfc, 3)
round(lc_pop, 3)
round(lc_sfc, 3)
round(uk_pop, 3)
round(uk_sfc, 3)

# fullobs = fullobs_sp_urb[ctr %in% c("fr", "de", "uk", "lc"), list(urban=sum(im2_ann, na.rm=T) + sum(im2_cml * 0.005, na.rm=T)), by=list(ctr, decade)][fullobs_sp_rur[, list(rural=sum(im2_ann, na.rm=T) + sum(im2_cml * 0.005, na.rm=T)), by=list(decade, ctr)], on=c("ctr", "decade")]

# fr = fullobs_sp_urb[ctr=="fr", list(urban=sum(im3_ann, na.rm=T) + sum(im3_cml * 0.005, na.rm=T)), by=century][fullobs_sp_rur[centre %in% c("Toulouse", "Dijon", "Amiens"), list(rural=sum(im3_ann, na.rm=T) + sum(im3_cml * 0.005, na.rm=T)), by=century], on='century']
# fullobs_sp_urb[ctr=="fr", list(urban=sum(im3_ann, na.rm=T)), by=decade]
# fr = fullobs_sp_urb[ctr=="fr", list(urban=sum(im3_ann, na.rm=T) + sum(im3_cml * 0.005, na.rm=T)), by=century][fullobs_sp_rur[centre %in% c("Toulouse", "Dijon", "Amiens"), list(rural=sum(im3_ann, na.rm=T) + sum(im3_cml * 0.005, na.rm=T)), by=century], on='century']

# plot(fullobs_sp_urb[ctr == "fr" & data.table::between(year, 700, 1500), list(base::sum(.SD, na.rm = T) / 9), .SDcols = grep("im3_ann\\d", names(fullobs_sp_urb)), by = decade])
# lines(fullobs_sp_urb[ctr=="fr" & data.table::between(year, 700, 1500), list(sum(im3_ann, na.rm=T)), by=decade])

# plot(fullobs_sp_urb[ctr == "fr" & data.table::between(year, 700, 1500), list(base::sum(.SD * 0.005, na.rm = T) / 9), .SDcols = grep("im3_cml\\d", names(fullobs_sp_urb)), by = decade])
# lines(fullobs_sp_urb[ctr == "fr" & data.table::between(year, 700, 1500), list(base::sum(.SD, na.rm = T) / 9), .SDcols = grep("im3_ann\\d", names(fullobs_sp_urb)), by = decade])

# fullobs_sp_urb[ctr=="fr", list(sum(im3_ann, na.rm=T)), by=decade]
# fullobs_sp_urb[ctr=="fr", list(sum(im3_ann, na.rm=T)), by=decade][fullobs_sp_rur[centre %in% c("Toulouse", "Dijon", "Amiens"), list(rural=sum(im3_ann, na.rm=T) + sum(im3_cml * 0.005, na.rm=T)), by=decade], on='decade']


# lines(fullobs_sp_urb[ctr=="fr" & data.table::between(year, 700, 1500), list(sum(im3_ann, na.rm=T) + sum(im3_cml * 0.005, na.rm=T)), by=decade][fullobs_sp_rur[centre %in% c("Toulouse", "Dijon", "Amiens"), list(rural=sum(im3_ann, na.rm=T) + sum(im3_cml * 0.005, na.rm=T)), by=decade], on='decade']


fr = fullobs_sp_urb[ctr=="fr", list(urban=base::sum(.SD, na.rm=T) / M), by = decade, .SDcols = grep("im3_ann\\d", names(fullobs_sp_urb))]
# fr_cml = fullobs_sp_urb[ctr=="fr", list(urban=(base::sum(.SD * 0.005, na.rm=T) / M)), by = decade, .SDcols = grep("im3_cml\\d", names(fullobs_sp_urb))]
# fr[, urban := urban + fr_cml$urban]
de = fullobs_sp_urb[ctr=="de", list(urban=base::sum(.SD, na.rm=T) / M), by = decade, .SDcols = grep("im3_ann\\d", names(fullobs_sp_urb))]
# de_cml = fullobs_sp_urb[ctr=="de", list(urban=(base::sum(.SD * 0.005, na.rm=T) / M)), by = decade, .SDcols = grep("im3_cml\\d", names(fullobs_sp_urb))]
# de[, urban := urban + fr_cml$urban]
uk = fullobs_sp_urb[ctr=="uk", list(urban=base::sum(.SD, na.rm=T) / M), by = decade, .SDcols = grep("im3_ann\\d", names(fullobs_sp_urb))]
# uk_cml = fullobs_sp_urb[ctr=="uk", list(urban=(base::sum(.SD * 0.005, na.rm=T) / M)), by = decade, .SDcols = grep("im3_cml\\d", names(fullobs_sp_urb))]
# uk[, urban := urban + uk_cml$urban]
lc = fullobs_sp_urb[ctr=="nl" | ctr=="be", list(urban=base::sum(.SD, na.rm=T) / M), by = decade, .SDcols = grep("im3_ann\\d", names(fullobs_sp_urb))]
# lc_cml = fullobs_sp_urb[ctr=="nl" | ctr=="be", list(urban=(base::sum(.SD * 0.005, na.rm=T) / M)), by = decade, .SDcols = grep("im3_cml\\d", names(fullobs_sp_urb))]
# lc[, urban := urban + lc_cml$urban]

al = fullobs_sp_urb[, list(urban=base::sum(.SD, na.rm=T) / M), by = decade, .SDcols = grep("im3_ann\\d", names(fullobs_sp_urb))]
al = fullobs_sp_urb[, list(urban=sum(im3_ann_cmc, na.rm=T)), by = decade]
al_rur = fullobs_sp_rur[centre %in% rurcit, list(rural=sum(im3_ann_cmc, na.rm=T)), by = decade]
al = al[al_rur, on = 'decade'][data.table::between(decade, 700, 1500)]
al[, rural_mlp := (1 / al_sfc$rural * rural)]
al[, combined := rural_mlp + urban]

pdf("figs/ruralcorrections_eu.pdf", height=4, width=10)
par(mfrow = c(1, 3), font.main = 1, mar=c(4, 4, 1.5, 0.5))
plot(rural ~ decade, data = al, type = 'l',
    ylab = 'm3/decade', main = "Rural sample")
plot(combined ~ decade, data = al, type = 'n',
    ylab = 'm3/decade', main = "Urban")
lines(urban ~ decade, data = al, type = 'l')
plot(combined ~ decade, data = al, type = 'n',
    ylab = 'm3/decade', main = "Combined")
lines(combined ~ decade, data = al, type = 'l')
dev.off()

fr_rur = fullobs_sp_rur[centre %in% c("Toulouse", "Dijon", "Amiens"), list(rural=sum(im3_ann, na.rm=T)), by=decade]
# fr_rur_cml = fullobs_sp_rur[centre %in% c("Toulouse", "Dijon", "Amiens"), list(rural=sum(im3_cml * 0.005, na.rm=T)), by=decade]
# fr_rur[, rural := rural + fr_rur_cml$rural]

de_rur = fullobs_sp_rur[centre %in% c("Nuernberg (Nürnberg)", "Osnabrueck (Osnabrück)"), list(rural=sum(im3_ann, na.rm=T)), by=decade]
# de_rur_cml = fullobs_sp_rur[centre %in% c("Nuernberg (Nürnberg)", "Osnabrueck (Osnabrück)"), list(rural=sum(im3_cml * 0.005, na.rm=T)), by=decade]
# de_rur[, rural := rural + de_rur_cml$rural]

uk_rur = fullobs_sp_rur[centre %in% c("Chester", "Peterborough (Medeshamstede)"), list(rural=sum(im3_ann, na.rm=T)), by=decade]
# uk_rur_cml = fullobs_sp_rur[centre %in% c("Chester", "Peterborough (Medeshamstede)"), list(rural=sum(im3_cml * 0.005, na.rm=T)), by=decade]
# uk_rur[, rural := rural + uk_rur_cml$rural]

lc_rur = fullobs_sp_rur[centre %in% c("Maastricht"), list(rural=sum(im3_ann, na.rm=T)), by=decade]
# lc_rur_cml = fullobs_sp_rur[centre %in% c("Maastricht"), list(rural=sum(im3_cml * 0.005, na.rm=T)), by=decade]
# lc_rur[, rural := rural + lc_rur_cml$rural]

fr = fr[fr_rur, on = 'decade']
de = de[de_rur, on = 'decade']
uk = uk[uk_rur, on = 'decade']
lc = lc[lc_rur, on = 'decade']

# fr = fullobs_sp_urb[ctr=="fr", list(urban=sum(im3_ann, na.rm=T) + sum(im3_cml * 0.005, na.rm=T)), by=decade][fullobs_sp_rur[centre %in% c("Toulouse", "Dijon", "Amiens"), list(rural=sum(im3_ann, na.rm=T) + sum(im3_cml * 0.005, na.rm=T)), by=decade], on='decade']
# de = fullobs_sp_urb[ctr=="de", list(urban=sum(im3_ann, na.rm=T) + sum(im3_cml * 0.005, na.rm=T)), by=decade][fullobs_sp_rur[centre %in% c("Nuernberg (Nürnberg)", "Osnabrueck (Osnabrück)"), list(rural=sum(im3_ann, na.rm=T) + sum(im3_cml * 0.005, na.rm=T)), by=decade], on='decade']
# uk = fullobs_sp_urb[ctr=="uk", list(urban=sum(im3_ann, na.rm=T) + sum(im3_cml * 0.005, na.rm=T)), by=decade][fullobs_sp_rur[centre %in% c("Chester", "Peterborough (Medeshamstede)"), list(rural=sum(im3_ann, na.rm=T) + sum(im3_cml * 0.005, na.rm=T)), by=decade], on='decade']
# lc = fullobs_sp_urb[ctr=="nl" | ctr=="be", list(urban=sum(im3_ann, na.rm=T) + sum(im3_cml * 0.005, na.rm=T)), by=decade][fullobs_sp_rur[centre %in% c("Maastricht"), list(rural=sum(im3_ann, na.rm=T) + sum(im3_cml * 0.005, na.rm=T)), by=decade], on='decade']

fr[, ctr := "fr"]
de[, ctr := "de"]
uk[, ctr := "uk"]
lc[, ctr := "lc"]

fr[, combined := urban + rural * 1 / fr_sfc[, 'rural']]
de[, combined := urban + rural * 1 / de_sfc[, 'rural']]
uk[, combined := urban + rural * 1 / uk_sfc[, 'rural']]
lc[, combined := urban + rural * 1 / lc_sfc[, 'rural']]

ctrobs = rbindlist(list(fr, de, uk, lc))
ctrobs[, lapply(.SD, sum), by=decade, .SDcols=c("urban", "rural", "combined")]

# out = rbindlist(list(ctrobs, ctrobs[, lapply(.SD, sum), by=decade, .SDcols=c("urban", "rural", "combined")]), fill=T)
# out[is.na(ctr), ctr := "all"]
# data.table::fwrite(out[decade <= 1500, ][order(ctr), ], "dat/forbruce1.csv")

# out = fullobs_sp_urb[, list(urban=sum(im3_ann, na.rm=T) + sum(im3_cml * 0.005, na.rm=T)), by=list(ctr2, decade)]
# out = rbindlist(list(out, out[, list(urban = sum(urban)), by=decade]), fill=T)
# out[is.na(ctr2), ctr2 := "all"]
# data.table::fwrite(out[decade <= 1500, ][order(ctr2), ], "dat/forbruce2.csv")


cor(fr[decade <= 1500, 2:3])
cor(fr[decade <= 1500, .SD - data.table::shift(.SD, type='lag'), .SDcols=2:3], use='pairwise')
cor(de[decade <= 1500, 2:3])
cor(de[decade <= 1500, .SD - data.table::shift(.SD, type='lag'), .SDcols=2:3], use='pairwise')
cor(uk[decade <= 1500, 2:3])
cor(uk[decade <= 1500, .SD - data.table::shift(.SD, type='lag'), .SDcols=2:3], use='pairwise')
cor(lc[decade <= 1500, 2:3])
cor(lc[decade <= 1500, .SD - data.table::shift(.SD, type='lag'), .SDcols=2:3], use='pairwise')


plot(fr[decade <= 1500, urban], type='l', ylab='urban', main='France')

par(mfrow=c(1, 3))
plot(fullobs_sp_urb[year <= 1500, sum(im3_ann, na.rm=T) + sum(im3_cml * 0.005, na.rm=T), 
    by=century], type='l', main="century")
plot(fullobs_sp_urb[year <= 1500, sum(im3_ann, na.rm=T) + sum(im3_cml * 0.005, na.rm=T), 
    by=decade2], type='l', main="50y")
plot(fullobs_sp_urb[year <= 1500, sum(im3_ann, na.rm=T) + sum(im3_cml * 0.005, na.rm=T), 
    by=decade], type='l', main="20y")
    
fullobs_sp_urb[year==1000, list(year, decade, decade2, century)]
fullobs_sp_urb[year==1001, list(year, decade, decade2, century)]

par(mfrow=c(3, 1))
plot(fullobs_sp_urb[year <= 1500, sum(im3_ann, na.rm=T) + sum(im3_cml * 0.005, na.rm=T), by=decade], type='l')
plot(fullobs_sp_rur[year <= 1500, sum(im3_ann, na.rm=T) + sum(im3_cml * 0.005, na.rm=T), by=decade], type='l')
plot(fullobs_sp_urb[year <= 1500, sum(im3_ann, na.rm=T) + sum(im3_cml * 0.005, na.rm=T), by=decade][, V1] + 
     fullobs_sp_rur[year <= 1500, sum(im3_ann, na.rm=T) + sum(im3_cml * 0.005, na.rm=T), by=decade][, V1] * 1/al_sfc[, 'rural'], type='l', ylab='')

pdf('figs/ruralcorrections.pdf', width=12)
par(mfrow=c(3, 4), mar=c(3, 4, 2, 0.5))
plot(urban ~ decade, data=fr[decade <= 1500, ], type='l', ylab='urban', main='France', col=2)
plot(urban ~ decade, data=de[decade <= 1500, ], type='l', main='Germany', col=2)
plot(urban ~ decade, data=uk[decade <= 1500, ], type='l', main='Britain', col=2)
plot(urban ~ decade, data=lc[decade <= 1500, ], type='l', main='Low Countries', col=2)

plot(rural ~ decade, data=fr[decade <= 1500, ], type='l', ylab='rural sample', col=2)
plot(rural ~ decade, data=de[decade <= 1500, ], type='l', col=2)
plot(rural ~ decade, data=uk[decade <= 1500, ], type='l', col=2)
plot(rural ~ decade, data=lc[decade <= 1500, ], type='l', col=2)

plot(combined ~ decade, data=fr[decade <= 1500, ], type='l', ylab='combined sample', col=2)
# lines(urban ~ decade, data=fr[decade <= 1500, ], type='l', col="gray70")
plot(combined ~ decade, data=de[decade <= 1500, ], type='l', col=2)
# lines(urban ~ decade, data=de[decade <= 1500, ], type='l', col="gray70")
plot(combined ~ decade, data=uk[decade <= 1500, ], type='l', col=2)
# lines(urban ~ decade, data=uk[decade <= 1500, ], type='l', col="gray70")
plot(combined ~ decade, data=lc[decade <= 1500, ], type='l', col=2)
# lines(urban ~ decade, data=lc[decade <= 1500, ], type='l', col="gray70")
dev.off()


pdf('figs/ruralcorrections_smt.pdf', height = 10, width = 8)
par(mfrow=c(4, 3), mar=c(3, 4, 2, 0.5), font.main = 1)
plot(urban ~ decade, data=fr[decade <= 1500, ], type='l', col='gray', ylab = 'im3/year')
title(main='France', line = -0.8)
add_loess(urban ~ decade, dat=fr[decade <= 1500, ], span=0.5)
plot(rural ~ decade, data=fr[decade <= 1500, ], type='l', col='gray')
title(main='France', line = -0.8)
add_loess(rural ~ decade, dat=fr[decade <= 1500, ], span=0.5)
plot(combined ~ decade, data=fr[decade <= 1500, ], type='l', col='gray')
title(main='France', line = -0.8)
add_loess(combined ~ decade, dat=fr[decade <= 1500, ], span=0.5)

plot(urban ~ decade, data=de[decade <= 1500, ], type='l', col='gray')
title(main='Germany', line = -0.8)
add_loess(urban ~ decade, dat=de[decade <= 1500, ], span=0.5)
plot(rural ~ decade, data=de[decade <= 1500, ], type='l', col='gray')
title(main='Germany', line = -0.8)
add_loess(rural ~ decade, dat=de[decade <= 1500, ], span=0.5)
plot(combined ~ decade, data=de[decade <= 1500, ], type='l', col='gray')
title(main='Germany', line = -0.8)
add_loess(combined ~ decade, dat=de[decade <= 1500, ], span=0.5)

plot(urban ~ decade, data=uk[decade <= 1500, ], type='l', col='gray')
title(main='Britain', line = -0.8)
add_loess(urban ~ decade, dat=uk[decade <= 1500, ], span=0.5)
plot(rural ~ decade, data=uk[decade <= 1500, ], type='l', col='gray')
title(main='Britain', line = -0.8)
add_loess(rural ~ decade, dat=uk[decade <= 1500, ], span=0.5)
plot(combined ~ decade, data=uk[decade <= 1500, ], type='l', col='gray')
title(main='Britain', line = -0.8)
add_loess(combined ~ decade, dat=uk[decade <= 1500, ], span=0.5)

plot(urban ~ decade, data=lc[decade <= 1500, ], type='l', col='gray')
title(main='Low Countries', line = -0.8)
add_loess(urban ~ decade, dat=lc[decade <= 1500, ], span=0.5)
plot(rural ~ decade, data=lc[decade <= 1500, ], type='l', col='gray')
title(main='Low Countries', line = -0.8)
add_loess(rural ~ decade, dat=lc[decade <= 1500, ], span=0.5)
plot(combined ~ decade, data=lc[decade <= 1500, ], type='l', col='gray')
title(main='Low Countries', line = -0.8)
add_loess(combined ~ decade, dat=lc[decade <= 1500, ], span=0.5)
dev.off()

fullobs_sp_urb[is.na(im3_ann), im3_ann:=0]
fullobs_sp_urb = fullobs_sp_urb[order(osmid, year), ]
fullobs_sp_urb[year <= 1500, im3_ann_smth5:=zoo::rollmean(im3_ann, k=5, fill=NA), by=osmid]
fullobs_sp_urb[year <= 1500, im3_ann_smth10:=zoo::rollmean(im3_ann, k=10, fill=NA), by=osmid]
# fullobs_sp_urb[year <= 1500, im3_ann_smth10:=zoo::rollapply(im3_ann, FUN=mean, na.rm=T, width=10), by=osmid]
fullobs_sp_urb[year <= 1500, im3_ann_smth20:=zoo::rollmean(im3_ann, k=20, fill=NA), by=osmid]
fullobs_sp_urb[year <= 1500, im3_ann_smth50:=zoo::rollmean(im3_ann, k=50, fill=NA), by=osmid]
fullobs_sp_urb[year <= 1500, im3_ann_smth100:=zoo::rollmean(im3_ann, k=100, fill=NA), by=osmid]

pdf("figs/movingavg.pdf", width=10)
par(mfrow=c(2, 3))
plot(fullobs_sp_urb[year <= 1500, sum(im3_ann, na.rm=F), by=list(decade)], type='l', xlim=c(700, 1500))
title(main="k=0")
plot(fullobs_sp_urb[year <= 1500, sum(im3_ann_smth5, na.rm=F), by=list(decade)], type='l', xlim=c(700, 1500))
title(main="k=5")
plot(fullobs_sp_urb[year <= 1500, sum(im3_ann_smth10, na.rm=F), by=list(decade)], type='l', xlim=c(700, 1500))
title(main="k=10")
plot(fullobs_sp_urb[year <= 1500, sum(im3_ann_smth20, na.rm=F), by=list(decade)], type='l', xlim=c(700, 1500))
title(main="k=20")
plot(fullobs_sp_urb[year <= 1500, sum(im3_ann_smth50, na.rm=F), by=list(decade)], type='l', xlim=c(700, 1500))
title(main="k=50")
plot(fullobs_sp_urb[year <= 1500, sum(im3_ann_smth100, na.rm=F), by=list(decade)], type='l', xlim=c(700, 1500))
title(main="k=100")
dev.off()

fullobs_sp_urb[osmid==310994417, ]

dynobs_rur[, length(m3), by=osmid][order(V1)]
