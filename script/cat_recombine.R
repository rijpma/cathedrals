rm(list=ls())
options(stringsAsFactors=FALSE)

setwd("~/dropbox/cathedrals/")
source("script/cat_functions.r")

library("data.table")
library("stringi")
library("plm")
library("raster") # for spatial splitting of data

chr = data.table::fread("dat/checkedchurches_eb_7.csv", 
    header=T, encoding="UTF-8", colClasses = "character")
chr = chr[, 1:29, with=F]
hgt = data.table::fread("dat/heights.csv", encoding="UTF-8")
sfc = data.table::fread("dat/backproj.csv", encoding="UTF-8")
siem = data.table::fread("dat/siem_long.csv", encoding="UTF-8")
ukgdp = data.table::fread("dat/engdp12001700.csv", skip=1, encoding="UTF-8")
gdp = data.table::fread("dat/maddison_gdp_old.csv")
cathedrals = read.csv("dat/cathedral_osmids.csv", header=F)

nanames = which(names(chr)=="NA")
setnames(chr, nanames, paste0("V", nanames))

# encoding
chr[, osmwikipedia := iconv(osmwikipedia, from='macroman', to='utf8')]
chr[, osmname := iconv(osmname, from='macroman', to='utf8')]
chr[, city := iconv(city, from='macroman', to='utf8')]

# should not give encoding errors
chr$city[grep("√", chr$city)]
grep("xyz", chr$osmname)
grep("xyz", chr$city)
grep("xyz", chr$osmwikipedia)

chr[, city := gsub("√∂", "ö", city)]
chr[, city := gsub("√º", "ü", city)]
chr[, city := gsub("√§", "ä", city)]
chr[, city := gsub("√©", "é", city)]
chr[, osmname := gsub("√©", "é", osmname)]
chr[, osmwikipedia := gsub("√©", "é", osmwikipedia)]

chr$osmname[grep("√", chr$osmname)]
chr$osmwikipedia[grep("√", chr$osmwikipedia)]
sample(chr$osmname[grep("é", chr$osmname)], 5)
sample(chr$osmname[grep("è", chr$osmname)], 5)
sample(chr$osmname[grep("ü", chr$osmname)], 5)
# etc.

# check for duplicate osmids
table(table(chr$osmid[chr$osmid!='']))
duplids = names(table(chr$osmid[chr$osmid!='']))[table(chr$osmid[chr$osmid!='']) > 6]
# chr[osmid %in% duplids, ]
duplids = names(table(chr$osmid[chr$osmid!='']))[table(chr$osmid[chr$osmid!='']) < 6]
match(duplids, chr$osmid)
 # chr[osmid %in% duplids, ]

# manual fixes
# halle should be two churches with unique osmids
chr[osmid=="217546683", "osmid"] = paste0(chr$osmid[chr$osmid=="217546683"], rep(c('a', 'b'), each=6))

# middelburg should have one id
chr[city=="Middelburg", osmid := osmid[1]]

# table(table(chr$osmid[chr$osmid!='']))

chr[city=="reading", city := "Reading"]
chr[city=="norwich", city := "Norwich"]

# fix height typos
chr[osmid == "32530870" & surface == "height", V19 := "15.8"]
chr[osmid == "69972010" & surface == "year", V17 := "1000"] # guess for now
chr[osmid == "136200148" & surface == "year", V23 := "1450"] # guess for now

chrlist = recombine_churches(churches=chr, guesses=NULL, firstm2col = 14)

statobs = do.call(rbind, lapply(chrlist, `[[`, 'static')) 
statobs = data.table::as.data.table(statobs)

## country splits north/south
statobs[, ctr2:=ctr]
statobs[lat > 46 & ctr=="fr", ctr2:="fr_north"]
statobs[lat <= 46 & ctr=="fr", ctr2:="fr_south"]
statobs[lat > 53 & ctr=="uk", ctr2:="uk_north"]
statobs[lat <= 53 & ctr=="uk", ctr2:="uk_south"]
statobs[lat > 50.5 & ctr=="de", ctr2:="de_north"]
statobs[lat <= 50.5 & ctr=="de", ctr2:="de_south"]

# country splits natural
statobs[ctr=="nl" | ctr == "be" | ctr == "lu", ctr3 := "lc"]
statobs[lat > 46 & ctr=="fr", ctr3:="fr_north"]
statobs[lat <= 46 & ctr=="fr", ctr3:="fr_south"]

lat2 = siem[city=="York", lat[1]] + km2lat(5)
lon2 = siem[city=="York", lon[1]] - km2lon(5, siem[city=="York", lat[1]])
lat1 = siem[city=="Bristol", lat[1]] + km2lat(5)
lon1 = siem[city=="Bristol", lon[1]] - km2lon(5, siem[city=="Bristol", lat[1]])
ln = lm(lat ~ lon, data=data.frame(lat=c(lat1, lat2), lon=c(lon1, lon2)))
north = predict(ln, newdata=statobs) < statobs[, lat]
statobs[ctr=='uk' & north==TRUE, ctr3 := "uk_nw"]
statobs[ctr=='uk' & north==FALSE, ctr3 := "uk_se"]

deu = raster::getData("GADM", country='DEU', level=1)
deu$region[grep("Bay|Thü|Sach|Bra|Ber|Meck", deu$NAME_1)] = "de_ne"
deu$region[is.na(deu$region)] = "de_sw"
cds = SpatialPoints(statobs[, list(lon, lat)])
proj4string(cds) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") # , +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0)
deregions = over(cds, deu)[, 'region']
statobs[!is.na(deregions), ctr3 := na.omit(deregions)]
statobs[ctr=="ch", ctr3 := "de_sw"]

### fix statobs city names to match siem 
fixes = lapply(gsub('-', ' ', setdiff(statobs$city, siem$city)), function(x) unique(siem$city)[grep(x, gsub('-', ' ', unique(siem$city)))])
fixes[sapply(fixes, length) == 0] = NA
fixes = unlist(fixes)
names(fixes) = setdiff(statobs$city, siem$city)

unique(fixes[match(statobs$city, names(fixes))])
statobs[, city2:=fixes[match(city, names(fixes))]]
statobs[!is.na(city2), city:=city2]
unique(statobs[!is.na(city2), list(city, city2)])
statobs[!is.na(city2), city:=city2][, city2:=NULL]

setdiff(statobs$city, siem$city)
all(unique(statobs$city) %in% siem$city)

# different city names in siem check, should be zero rows
statobs[!city %in% siem$city, ]
siem[, .SD[1], by = city][city %in% unique(statobs$city), ][duplicated(city)]

doubles = c("Strasbourg", "Strasbourg (Strassburg)", 
    "St Omer", "St Omer (Saint-Omer (Pas-de-Calais))", 
    "Chalons-sur-Marne", "Chalons-sur-Marne (Châlons-en-Champagne)")

doubles[!doubles %in% siem$city] # only worry about !
statobs[city %in% doubles[!doubles %in% siem$city], ]
siem[city %in% doubles, list(lat, lon, city)][order(city)]
siem[city %in% doubles, list(lat[1], lon[1]), by=city]

dynobs = to_dynobs(churchlist=chrlist)
dynobs[osmid == "26183417"]

# correct date heaping
table(as.numeric(stringi::stri_sub(dynobs$year, -1)))
table(as.numeric(stringi::stri_sub(dynobs$year, -2)))
dynobs[, ey:=as.numeric(stringi::stri_sub(dynobs$year, -2))]

pdf('figs/heaping2dig.pdf', height=6)
par(mfrow=c(1, 1))
hist(dynobs$ey, breaks=100)
dev.off()

pdf('figs/heaping3dig.pdf', height=6)
# hist(as.numeric(stringi::stri_sub(dynobs$year, -3)), breaks=1000)
hist(dynobs[year >= 500, year], breaks=1000)
dev.off()

dynobs[, year_lead := data.table::shift(year, type='lead', fill=Inf), by=osmid]
dynobs[, year_lag := data.table::shift(year, type='lag', fill=-Inf), by=osmid]
dynobs[, dyear := data.table::shift(year, type='lead') - year, by=osmid]

M = 9
for (j in 1:M){
    dynobs[, year_crc := year]
    dynobs[, heap100 := year %% 100 == 0 | year %% 100 == 1]
    # dynobs[((year -1) %% 100 == 0) & (year_lag %% 100 == 0), heap100]
    dynobs[, heap20 := ((year - 20) %% 100 == 0) | ((year + 20) %% 100 == 0) ]
    dynobs[, heap25 := ((year - 25) %% 100 == 0) | ((year + 25) %% 100 == 0) | ((year - 50) %% 100 == 0) ]
    dynobs[, heap10 := (year %% 10 == 0) & heap100 + heap20 + heap25 == 0]

    dynobs[heap100 == TRUE, sdev := 30]
    dynobs[heap20 == TRUE, sdev := 12]
    dynobs[heap25 == TRUE, sdev := 15]

    dynobs[heap100 == TRUE, delta := 50]
    dynobs[heap20 == TRUE, delta := 10]
    dynobs[heap25 == TRUE, delta := 12]
    dynobs[heap10 == TRUE, delta := 5]

    rsmpl = rtnorm(n = nrow(dynobs[!is.na(sdev)]), 
               mean = dynobs[!is.na(sdev), year],
               sd = dynobs[!is.na(sdev), sdev],
               min = dynobs[!is.na(sdev), year_lag] + 1,
               max = dynobs[!is.na(sdev), year_lead] - 1)
    dynobs[!is.na(sdev), year_crc := round(rsmpl)]

    # needs double resampling to prevent new heaping on 5
    # or just at runif(N) and round
    n = nrow(dynobs[heap10 == TRUE])
    dynobs[heap10 == TRUE, splt10 := rbinom(n, size=1, prob=0.5)]
    rsmpl4 = runif(n = sum(dynobs$splt10 == 1, na.rm=T),
                   min = dynobs[heap10 == TRUE & splt10 == 1, year - 4],
                   max = dynobs[heap10 == TRUE & splt10 == 1, year + 4])
    rsmpl5 = runif(n = sum(dynobs$splt10 == 0, na.rm=T),
                   min = dynobs[heap10 == TRUE, ][splt10 == 0, year - 5],
                   max = dynobs[heap10 == TRUE, ][splt10 == 0, year + 5])

    dynobs[heap10 == TRUE & splt10 == 1, year_crc := round(rsmpl4)]
    dynobs[heap10 == TRUE & splt10 == 0, year_crc := round(rsmpl5)]

    # swap if diff(year) < 0 ?
    # or mean to original observation?
    dynobs[, year_lead_crc := data.table::shift(year_crc, type='lead'), by=osmid]
    dynobs[, year_lag_crc := data.table::shift(year_crc, type='lag'), by=osmid]

    cat("Original: N swapped: ", sum(dynobs[, list(delta = diff(year_crc) < 0), by=osmid][, delta]), " - ")
    cat("N same: ",    sum(dynobs[, list(delta = diff(year_crc) == 0), by=osmid][, delta]), " - ")

    dynobs[(((year_lead_crc - year_crc) < 0) | ((year_crc - year_lag_crc) < 0)), year_crc := round((year + year_crc) / 2), by=osmid]
    dynobs[duplicated(paste0(osmid, year_crc)), year_crc := year_crc + 1]
    dynobs[duplicated(paste0(osmid, year_crc), fromLast=TRUE), year_crc := year_crc - 1]

    cat("After fix: N swapped: ", sum(dynobs[, list(delta = diff(year_crc) < 0), by=osmid][, delta]), " - ")
    cat("N same: ",    sum(dynobs[, list(delta = diff(year_crc) == 0), by=osmid][, delta]), "\n")

    setnames(dynobs, 'year_crc', paste0("year_crc", j))
}

pdf("figs/distrs_prepost_heap.pdf", width=11)
par(mfrow=c(1, 2))
hist(dynobs[, year], breaks=data.table::uniqueN(dynobs$year), main='heaping', xlab='year')
hist(dynobs[, year_crc1], breaks=data.table::uniqueN(dynobs$year_crc1), main='heaping resampled', xlab='year')
# imps = dynobs[, round(rowMeans(.SD)), .SDcols=grep('year_crc', names(dynobs))]
# hist(imps, uniqueN(imps), main='heaping resampled average')
dev.off()

tail(sort(table(dynobs[, year_crc1])), 10)

fullobs = to_annual_obs(dyn=dynobs, churchlist=chrlist)

unique(table(fullobs[, osmid]))
unique(table(fullobs[, year]))

fullobslist = list()
for (j in 1:M){
    dynobs_rs = data.table::copy(dynobs)
    dynobs_rs[, year := dynobs[, paste0("year_crc", j), with=F]]
    # fullobslist[[j]] = to_annual_obs(dynobs_rs, chrlist)    
    dynobs_rs[, prb := duplicated(year, fromLast=T), by=osmid]
    dynobs_rs[prb == TRUE & data.table::shift(year) != year, year := year - 1]

    tomerge = to_annual_obs(dyn = dynobs_rs, churchlist = chrlist)

    fullobs = merge(fullobs, tomerge[, list(osmid, year, im2_ann, im3_ann, im2_cml, im3_cml)], all.x=T, all.y=F, by=c("osmid", "year"), suffixes=c("", j))
    cat(j, ' - ', dim(fullobs), '\n')
    rm("tomerge")
    rm("dynobs_rs")
}

# do the imputations on dynobs on e.g. duration itself?
# still no city level dataset or panel dataset...
# do here, get standard errors, plug back in ?

fullobs[, decade := (trunc((year - 1) / 20) + 1) * 20] # so 1500 = 1481-1500

pdf("figs/heap_v_noheap_ann.pdf", height=6)
plot(fullobs[data.table::between(year, 700, 1500), sum(im3_ann, na.rm=T) * 1.1, by=year], type='n', ylab='m3/ann')
abline(v=(7:15)*100, col='gray70', lwd=0.8)
abline(v=c(1315, 1348), col='gray70', lwd=0.8)
lines(fullobs[data.table::between(year, 700, 1500), sum(im3_ann, na.rm=T), by=year], col='gray')
lines(fullobs[data.table::between(year, 700, 1500), sum(.SD, na.rm=T) / M, by=year, .SDcols=grep("im3_ann\\d", names(fullobs))], col=2)
legend('topleft', legend=c("heaping", "heaping corrected"), fill=c('gray70', 'red'))
dev.off()

pdf("figs/heap_v_noheap.pdf", width=11)
par(mfrow=c(1, 2))
plot(fullobs[data.table::between(year, 700, 1500), sum(im3_ann, na.rm=T), by=year], type='n', xlab = 'm3/year')
abline(v=(7:15)*100, col='gray', lwd=0.8)
abline(v=c(1315, 1348), col='gray', lwd=0.8)
lines(fullobs[data.table::between(year, 700, 1500), sum(im3_ann, na.rm=T), by=year])
lines(fullobs[data.table::between(year, 700, 1500), sum(.SD, na.rm=T) / M, by=year, .SDcols=grep("im3_ann\\d", names(fullobs))], col=2)

plot(fullobs[data.table::between(year, 700, 1500), sum(im3_ann, na.rm=T), by=decade], type='n', xlab = 'm3/dec')
abline(v=(7:15)*100, col='gray', lwd=0.8)
abline(v=c(1315, 1348), col='gray', lwd=0.8)
lines(fullobs[data.table::between(year, 700, 1500), sum(im3_ann, na.rm=T), by=decade], type='b')
lines(fullobs[data.table::between(year, 700, 1500), sum(.SD, na.rm=T) / M, by=decade, .SDcols=grep("im3_ann\\d", names(fullobs))], col=2, type='b')

legend('topleft', legend=c("heaping", "heaping corrected"), fill=1:2)
dev.off()

sum(fullobs[data.table::between(year, 700, 1500), sum(im2_ann, na.rm=T), by=year])
sum(fullobs[data.table::between(year, 700, 1500), sum(.SD, na.rm=T) / M, by=year, .SDcols=grep("im2_ann\\d", names(fullobs))])

citobs = to_city_obs(statobs=statobs, fullobs=fullobs)

dynobs_sp = merge(dynobs, statobs, by="osmid")
dim(fullobs)
fullobs_sp = merge(fullobs, statobs, by="osmid", all=T)
dim(fullobs_sp)
dynobs[fullobs_sp[is.na(year), osmid], ]
fullobs_sp = fullobs_sp[!is.na(year), ]
unique(table(fullobs_sp$year, useNA='ifany'))

# average end-size church
dynobs[, list(m2_end=sum(m2, na.rm=T)), by=paste(osmid, bldindex)][, mean(m2_end)]
dynobs[, list(m3_end=sum(m3, na.rm=T)), by=paste(osmid, bldindex)][, mean(m3_end)]
dynobs[, list(m2_end=sum(m2[bldindex==max(bldindex)], na.rm=T)), by=osmid][, mean(m2_end)]
dynobs[, list(m3_end=sum(m3[bldindex==max(bldindex)], na.rm=T)), by=osmid][, mean(m3_end)]

# average size church by year
plot(fullobs[, mean(im2_cml), by = year], type= 'l')
abline(v = 1500)
plot(fullobs[im2_ann == 0, mean(im2_cml), by = year][order(year)], type= 'l')
abline(v = 1500)

write.csv(dynobs_sp[ctr=='uk', ], 'dat/dynobs_uk.csv', row.names=F)

write.csv(dynobs, "dat/dynobs.csv", row.names=F)
write.csv(statobs, "dat/statobs.csv", row.names=F)
write.csv(citobs, "dat/citobs.csv", row.names=F)

outfile = gzfile("dat/fullobs_sp.csv.gz", 'w')
write.csv(fullobs_sp, outfile)
close(outfile)

siem[, ctr:=tolower(siem$tld)]
siem[ctr=='gb', ctr:="uk"]

plot(hgt ~ m2, data=dynobs[m2 >0, ])
curve(0.4542*sqrt(x), add=T, col=2)

### descriptives ###
####################

pdf('figs/phasedistr_hc.pdf', height=6, width=8)
par(mfrow=c(2, 3), mar=c(2,2,3,1), font.main=1)
x = dynobs_sp[, grep('ctr$|year_crc', names(dynobs_sp)), with=F]
for (country in unique(x$ctr)){
    histmat = as.matrix(x[ctr == country, grep("year", names(x)), with=F])
    histmat[histmat < 700] = 700
    histmat[histmat > 1600] = 1600
    hi = hist(histmat, breaks=M, plot=F)
    hi$counts = hi$counts / M
    plot(hi, main='')
    title(main=country, line=-0.2)
}
dev.off()

pdf('figs/phasedistr.pdf', height=6, width=8)
par(mfrow=c(2, 3), mar=c(2,2,3,1), font.main=1)
x = dynobs_sp[, list(ctr, year)]
x[year > 1600, year:=1600]
x[year < 700, year:=700]
hist(x$year[x$ctr=='de'], breaks=9, main='')
title(main='de', line=-0.2)
hist(x$year[x$ctr=='uk'], breaks=9, main='')
title(main='uk', line=-0.2)
hist(x$year[x$ctr=='fr'], breaks=9, main='')
title(main='fr', line=-0.2)
hist(x$year[x$ctr=='be'], breaks=9, main='')
title(main='be', line=-0.2)
hist(x$year[x$ctr=='ch'], breaks=9, main='')
title(main='ch', line=-0.2)
hist(x$year[x$ctr=='nl'], breaks=9, main='')
title(main='nl', line=-0.2)
dev.off()

siem_cities = aggregate(city ~ ctr, data=siem[year==1500, ], length)
church_cities = aggregate(city ~ ctr, data=unique(statobs[, list(city, ctr)]), length)
phases_pre1000 = aggregate(city ~ ctr, data=dynobs_sp[year < 1000, ], length)
phases_pre1200 = aggregate(city ~ ctr, data=dynobs_sp[year < 1200, ], length)
phases_all = aggregate(city ~ ctr, data=dynobs_sp, length)
churches_by_ctr = aggregate(osmid ~ ctr, data=statobs, length)

siem_cities = siem_cities[siem_cities$ctr!='it', ]
ss_out = data.frame(siem_cities, cities_w_church=church_cities[,-1],
    n_churches=churches_by_ctr[, -1], n_phases=phases_all[,-1],
    n_phases_pre1000=phases_pre1000[,-1], n_phases_pre1200=phases_pre1200[, -1])
ss_out = rbind(ss_out, c('all', colSums(ss_out[, -1])))

write.csv(ss_out, 'tab/sumstats.csv', row.names=F)

# beware:
unique(stringi::stri_enc_mark(siem$city))
unique(stringi::stri_enc_mark(citobs$city))

# regular churches v. cathedrals
cathedrals
cathedrals$V1[is.na(as.numeric(cathedrals$V1))]

# replace with

impvrbs = grepr('im3_ann\\d', names(fullobs_sp))

pdf('figs/cath_v_allchurches_hc.pdf', height=4, width=9)
par(mfrow=c(1, 3), font.main=1)
plot(fullobs_sp[category=="cathedral" & data.table::between(year, 700, 1500), sum(.SD, na.rm=T) / M, by=decade, .SDcols=impvrbs], 
    type='l', ylab = 'm3/ann')
plot(fullobs_sp[category!="cathedral" & data.table::between(year, 700, 1500), sum(.SD, na.rm=T) / M, by=decade, .SDcols=impvrbs], 
    type='l', ylab = 'm3/ann')
plot(fullobs_sp[category!="cathedral" & data.table::between(year, 700, 1500), sum(.SD, na.rm=T) / M, by=decade, .SDcols=impvrbs], 
    type='l', ylab = 'm3/ann')
lines(fullobs_sp[category=="cathedral" & data.table::between(year, 700, 1500), sum(.SD, na.rm=T) / M, by=decade, .SDcols=impvrbs], 
    type='l', col=2)
text(c(1200, 1300), c(5e5, 16e5), c('cathedrals', 'all churches'), col=2:1)
dev.off()

pdf('figs/cath_v_allchurches.pdf', height=4, width=9)
par(mfrow=c(1, 3), font.main=1)
plot(fullobs_sp[category=="cathedral" & data.table::between(year, 700, 1500), sum(im3_ann, na.rm=T), by=decade], 
    type='l', ylab = 'm3/ann')
plot(fullobs_sp[category!="cathedral" & data.table::between(year, 700, 1500), sum(im3_ann, na.rm=T), by=decade], 
    type='l', ylab = 'm3/ann')
plot(fullobs_sp[category!="cathedral" & data.table::between(year, 700, 1500), sum(im3_ann, na.rm=T), by=decade], 
    type='l', ylab = 'm3/ann')
lines(fullobs_sp[category=="cathedral" & data.table::between(year, 700, 1500), sum(im3_ann, na.rm=T), by=decade], 
    type='l', col=2)
text(c(1200, 1050), c(4e5, 11.5e5), c('cathedrals', 'all churches'), col=2:1)
dev.off()

pdf("figs/bytype_hc.pdf")
bytype = fullobs_sp[data.table::between(year, 700, 1500), base::sum(.SD, na.rm=T) / M, by=list(decade, category), .SDcols = impvrbs]
cmladd = fullobs_sp[data.table::between(year, 700, 1500), base::sum(.SD * 0.005, na.rm=T) / M, by=list(decade, category), .SDcols = grepr('im3_cml\\d', names(fullobs_sp))]
bytype[, V1 := V1 + cmladd$V1]
par(mfrow=c(2, 2), mar=c(4,3,1.5,0.5), font.main=1)
plot(V1 ~ decade, data=bytype[category=="cathedral", ], 
    type='l', ylim=range(bytype$V1), bty='l', col=2, lwd=1.5, ylab = 'm3/ann')
    lines(V1 ~ decade, data=bytype[category=="parish", ], 
        type='l', ylim=range(bytype$V1), col='lightgray', ylab = 'm3/ann')
    lines(V1 ~ decade, data=bytype[category=="other", ], 
        type='l', ylim=range(bytype$V1), col='lightgray', ylab = 'm3/ann')
    lines(V1 ~ decade, data=bytype[category=="conventual", ], 
        type='l', ylim=range(bytype$V1), col='lightgray', ylab = 'm3/ann')
title(main="cathedral", line=-1)
plot(V1 ~ decade, data=bytype[category=="conventual", ], 
    type='l', ylim=range(bytype$V1), bty='l', col=2, lwd=1.5, ylab = 'm3/ann')
    lines(V1 ~ decade, data=bytype[category=="parish", ], 
        type='l', ylim=range(bytype$V1), col='lightgray', ylab = 'm3/ann')
    lines(V1 ~ decade, data=bytype[category=="cathedral", ], 
        type='l', ylim=range(bytype$V1), col='lightgray', ylab = 'm3/ann')
    lines(V1 ~ decade, data=bytype[category=="other", ], 
        type='l', ylim=range(bytype$V1), col='lightgray', ylab = 'm3/ann')
title(main="conventual", line=-1)
plot(V1 ~ decade, data=bytype[category=="parish", ], 
    type='l', ylim=range(bytype$V1), bty='l', col=2, lwd=1.5, ylab = 'm3/ann')
    lines(V1 ~ decade, data=bytype[category=="cathedral", ], 
        type='l', ylim=range(bytype$V1), col='lightgray', ylab = 'm3/ann')
    lines(V1 ~ decade, data=bytype[category=="other", ], 
        type='l', ylim=range(bytype$V1), col='lightgray', ylab = 'm3/ann')
    lines(V1 ~ decade, data=bytype[category=="conventual", ], 
        type='l', ylim=range(bytype$V1), col='lightgray', ylab = 'm3/ann')
title(main="parish", line=-1)
plot(V1 ~ decade, data=bytype[category=="other", ], 
    type='l', ylim=range(bytype$V1), bty='l', col=2, lwd=1.5, ylab = 'm3/ann')
    lines(V1 ~ decade, data=bytype[category=="parish", ], 
        type='l', ylim=range(bytype$V1), col='lightgray', ylab = 'm3/ann')
    lines(V1 ~ decade, data=bytype[category=="cathedral", ], 
        type='l', ylim=range(bytype$V1), col='lightgray', ylab = 'm3/ann')
    lines(V1 ~ decade, data=bytype[category=="conventual", ], 
        type='l', ylim=range(bytype$V1), col='lightgray', ylab = 'm3/ann')
title(main="other", line=-1)
dev.off()

pdf('figs/mendicants_hc.pdf', height = 4)
bytype = fullobs_sp[data.table::between(year, 700, 1500), base::sum(.SD, na.rm=T) / M, by=list(decade, category, mendicants), .SDcols = impvrbs]
cmladd = fullobs_sp[data.table::between(year, 700, 1500), base::sum(.SD * 0.005, na.rm=T) / M, by=list(decade, category, mendicants), .SDcols = grepr('im3_cml\\d', names(fullobs_sp))]
bytype = bytype[, V1 := V1 + cmladd$V1][category == "conventual"]
par(mfrow = c(1, 2))
plot(V1 ~ decade, data=bytype[category=="conventual" & mendicants == 1, ], 
    type='l', ylim=range(bytype$V1), bty='l', col=2, lwd=1.5, ylab = 'm3/ann', main = 'Mendicant')
plot(V1 ~ decade, data=bytype[category=="conventual" & mendicants == 0, ], 
    type='l', ylim=range(bytype$V1), bty='l', col=2, lwd=1.5, ylab = 'm3/ann', main = 'Other')
dev.off()

pdf("figs/bytype.pdf")
bytype = fullobs_sp[data.table::between(year, 700, 1500), sum(im3_ann, na.rm=T) + sum(im3_cml * 0.005, na.rm=T), by=list(decade, category)]
par(mfrow=c(2, 2), mar=c(4,3,1.5,0.5), font.main=1)
plot(V1 ~ decade, data=bytype[category=="cathedral", ], type='l', ylim=range(bytype$V1), bty='l', col=2, lwd=1.5, xlab= ' m3/ann')
    lines(V1 ~ decade, data=bytype[category=="parish", ], type='l', ylim=range(bytype$V1), col='lightgray')
    lines(V1 ~ decade, data=bytype[category=="other", ], type='l', ylim=range(bytype$V1), col='lightgray')
    lines(V1 ~ decade, data=bytype[category=="conventual", ], type='l', ylim=range(bytype$V1), col='lightgray')
title(main="cathedral", line=-1)
plot(V1 ~ decade, data=bytype[category=="parish", ], type='l', ylim=range(bytype$V1), bty='l', col=2, lwd=1.5, xlab= ' m3/ann')
    lines(V1 ~ decade, data=bytype[category=="cathedral", ], type='l', ylim=range(bytype$V1), col='lightgray')
    lines(V1 ~ decade, data=bytype[category=="other", ], type='l', ylim=range(bytype$V1), col='lightgray')
    lines(V1 ~ decade, data=bytype[category=="conventual", ], type='l', ylim=range(bytype$V1), col='lightgray')
title(main="parish", line=-1)
plot(V1 ~ decade, data=bytype[category=="other", ], type='l', ylim=range(bytype$V1), bty='l', col=2, lwd=1.5, xlab= ' m3/ann')
    lines(V1 ~ decade, data=bytype[category=="parish", ], type='l', ylim=range(bytype$V1), col='lightgray')
    lines(V1 ~ decade, data=bytype[category=="cathedral", ], type='l', ylim=range(bytype$V1), col='lightgray')
    lines(V1 ~ decade, data=bytype[category=="conventual", ], type='l', ylim=range(bytype$V1), col='lightgray')
title(main="other", line=-1)
plot(V1 ~ decade, data=bytype[category=="conventual", ], type='l', ylim=range(bytype$V1), bty='l', col=2, lwd=1.5, xlab= ' m3/ann')
    lines(V1 ~ decade, data=bytype[category=="parish", ], type='l', ylim=range(bytype$V1), col='lightgray')
    lines(V1 ~ decade, data=bytype[category=="cathedral", ], type='l', ylim=range(bytype$V1), col='lightgray')
    lines(V1 ~ decade, data=bytype[category=="other", ], type='l', ylim=range(bytype$V1), col='lightgray')
title(main="conventual", line=-1)
dev.off()

bytype[, dtotal := sum(V1), by=decade]
bytype[category=="other", list(decade, category, V1 / dtotal)]

# write.csv(statobs[osmid %in% cathedrals$V1, list(city, osmid, osmname, osmwikipedia)], "dat/cathedrals.csv", row.names=F)

# exploratory regs

# citobs_full_and = merge(citobs, siem, by=c('city', 'year'))
x = statobs[fullobs, on="osmid"]
x[, year := round(year / 100)  * 100] # for compatability w. century
x = x[data.table::between(year, 650, 1550), list(im3_cnt = base::sum(abs(.SD), na.rm=T) / M), by=list(city, year), .SDcols=paste0("im3_ann", 1:M)]

pcitobs = merge(x, siem, by=c('city', 'year'), all.x=T)
# why mean? already total by city, now take mean for kind of city per
pdf("figs/geography_hc.pdf", height=3, width=8)
par(mfrow=c(1, 3), font.main=1)
plot(pcitobs[year <=1500 & rivercanal==1, mean(im3_cnt), by=year], 
    type='n', col='lightgray', ylab='mean m3 per city')
lines(V1 ~ year, data=pcitobs[rivercanal==1, mean(im3_cnt), by=year], type='b', col='lightgray')
lines(V1 ~ year, data=pcitobs[landlocked==1, mean(im3_cnt), by=year], type='b', col='lightgray')
lines(V1 ~ year, data=pcitobs[coastal==1, mean(im3_cnt), by=year], type='b', col='red')
title(main='Coastal')
plot(pcitobs[rivercanal==1, mean(im3_cnt), by=year], 
    type='n', col='lightgray', ylab='mean m3 per city')
lines(V1 ~ year, data=pcitobs[coastal==1, mean(im3_cnt), by=year], type='b', col='lightgray')
lines(V1 ~ year, data=pcitobs[landlocked==1, mean(im3_cnt), by=year], type='b', col='lightgray')
lines(V1 ~ year, data=pcitobs[rivercanal==1, mean(im3_cnt), by=year], type='b', col='red')
title(main='River/canal')
plot(pcitobs[rivercanal==1, mean(im3_cnt), by=year], 
    type='n', col='lightgray', ylab='mean m3 per city')
lines(V1 ~ year, data=pcitobs[rivercanal==1, mean(im3_cnt), by=year], type='b', col='lightgray')
lines(V1 ~ year, data=pcitobs[coastal==1, mean(im3_cnt), by=year], type='b', col='lightgray')
lines(V1 ~ year, data=pcitobs[landlocked==1, mean(im3_cnt), by=year], type='b', col='red')
title(main='Landlocked')
dev.off()

x = pcitobs[data.table::between(year, 650, 1550), sum(im3_cnt), by=list(year, rivercanal, coastal, landlocked)]
# why mean? already total by city, now take mean for kind of city per
pdf("figs/geography.pdf", height=3, width=8)
par(mfrow=c(1, 3), font.main=1)
plot(pcitobs[year <=1500 & coastal==1, mean(im3_cnt), by=year], 
    type='n', col='lightgray', ylab='mean m3 per city')
lines(V1 ~ year, data=pcitobs[rivercanal==1, mean(im3_cnt), by=year], type='b', col='lightgray')
lines(V1 ~ year, data=pcitobs[landlocked==1, mean(im3_cnt), by=year], type='b', col='lightgray')
lines(V1 ~ year, data=pcitobs[coastal==1, mean(im3_cnt), by=year], type='b', col='red')
title(main='Coastal')
plot(pcitobs[coastal==1, mean(im3_cnt), by=year], 
    type='n', col='lightgray', ylab='mean m3 per city')
lines(V1 ~ year, data=pcitobs[coastal==1, mean(im3_cnt), by=year], type='b', col='lightgray')
lines(V1 ~ year, data=pcitobs[landlocked==1, mean(im3_cnt), by=year], type='b', col='lightgray')
lines(V1 ~ year, data=pcitobs[rivercanal==1, mean(im3_cnt), by=year], type='b', col='red')
title(main='River/canal')
plot(pcitobs[coastal==1, mean(im3_cnt), by=year], 
    type='n', col='lightgray', ylab='mean m3 per city')
lines(V1 ~ year, data=pcitobs[rivercanal==1, mean(im3_cnt), by=year], type='b', col='lightgray')
lines(V1 ~ year, data=pcitobs[coastal==1, mean(im3_cnt), by=year], type='b', col='lightgray')
lines(V1 ~ year, data=pcitobs[landlocked==1, mean(im3_cnt), by=year], type='b', col='red')
title(main='Landlocked')
dev.off()

# setdiff(citobs_full_and[, paste(city, year)], pcitobs[, paste(city, year)])
# setdiff(pcitobs[, paste(city, year)], citobs_full_and[, paste(city, year)])

pcitobs = pcitobs[data.table::between(year, 700, 1500), ]
plot(log(inhab) ~ log(im3_cnt), data=pcitobs)
summary(lm(log1p(inhab) ~ log1p(im3_cnt), data=pcitobs))

pcitobs = pdata.frame(pcitobs, c("city", "year"))
m_base <- plm::plm(log1p(inhab) ~ lag(log1p(im3_cnt)), data=pcitobs)
m_lag <- plm::plm(log1p(inhab) ~ lag(log1p(im3_cnt)) + lag(log1p(inhab)), data=pcitobs)
m_yd <- plm::plm(log1p(inhab) ~ lag(log1p(im3_cnt)) + year, data=pcitobs)
m_trend <- plm::plm(log1p(inhab) ~ lag(log1p(im3_cnt)) + as.numeric(year), data=pcitobs)

texreg::screenreg(list(m_base, m_trend, m_lag, m_yd))

pdf("figs/inhab_v_churchm2.pdf")
plot(log1p(inhab) ~ lag(log1p(im3_cnt)), data=pcitobs)
abline(plm::plm(log1p(inhab) ~ lag(log1p(im3_cnt)), data=pcitobs), col=2)
abline(lm(log1p(inhab) ~ lag(log1p(im3_cnt)), data=pcitobs), col=2)
text(x=c(1, 8), y=c(1, 0.4), c('pooled', 'within'), col=2)
dev.off()

# m3 per 20y period en country
pdf('figs/europetotal_hc.pdf', height=6)
plot(fullobs[data.table::between(decade, 700, 1500), base::sum(.SD, na.rm=T) / M, by=decade, .SDcols = impvrbs], 
    type='n', bty='l', ylab = 'm3/20 year')
abline(v=c(768, 1315, 1000, 1348), col='gray')
lines(fullobs[data.table::between(decade, 700, 1500), base::sum(.SD, na.rm=T) / M, by=decade, .SDcols = impvrbs], type='b', col=2, lwd=1.5, pch=1, cex=0.9)
dev.off()

pdf('figs/europetotal.pdf', height=6)
plot(fullobs[data.table::between(decade, 700, 1500), sum(im3_ann, na.rm=T), by=decade], 
    type='n', bty='l', ylab = 'm3/20 year')
abline(v=c(768, 1315, 1000, 1348), col='gray')
lines(fullobs[data.table::between(decade, 700, 1500), sum(im3_ann, na.rm=T), by=decade], type='b', col=2, lwd=1.5, pch=1, cex=0.9)
dev.off()

# for bruce
# data.table::fwrite(fullobs_sp[year <= 1500, list(im2_dec=sum(im2_ann, na.rm=T), im3_dec=sum(im3_ann, na.rm=T)), by=list(decade, ctr2, category)], "dat/fullobs20yctr2cat.csv")
# data.table::fwrite(fullobs_sp[year <= 1500, list(im2_dec=sum(im2_ann, na.rm=T), im3_dec=sum(im3_ann, na.rm=T)), by=list(decade, ctr2)], "dat/fullobs20yctr2.csv")
# data.table::fwrite(fullobs_sp[year <= 1500, list(im2_dec=sum(im2_ann, na.rm=T), im3_dec=sum(im3_ann, na.rm=T)), by=list(decade, ctr)], "dat/fullobs20yctr.csv")

x = fullobs_sp[data.table::between(decade, 700, 1500), list(im2_dec=sum(im2_ann, na.rm=T), im3_dec=sum(im3_ann, na.rm=T)), by=list(decade, ctr)]

pdf("figs/m3bycountry.pdf", height=9)
par(mfrow=c(3, 2))
plot(im3_dec ~ decade, data=x[ctr=="de", ], type='l', col=2, lwd=1.5, bty='l', xlim=c(700, 1500), main="de")
points(x=c(1, 1000, 1500), y=gdp[V1 <= 1500, Germany / Germany[V1==1500]] * x[ctr=="de" & decade==1500, im3_dec])

plot(im3_dec ~ decade, data=x[ctr=="uk", ], type='l', col=2, lwd=1.5, bty='l', xlim=c(700, 1500), main="uk")
points(x=c(1, 1000, 1500), y=gdp[V1 <= 1500, UK / UK[V1==1500]] * x[ctr=="uk" & decade==1500, im3_dec])

plot(im3_dec ~ decade, data=x[ctr=="fr", ], type='l', col=2, lwd=1.5, bty='l', xlim=c(700, 1500), main="fr")
points(x=c(1, 1000, 1500), y=gdp[V1 <= 1500, France / France[V1==1500]] * x[ctr=="fr" & decade==1500, im3_dec])

plot(im3_dec ~ decade, data=x[ctr=="be", ], type='l', col=2, lwd=1.5, bty='l', xlim=c(700, 1500), main="be")
points(x=c(1, 1000, 1500), y=gdp[V1 <= 1500, Belgium / Belgium[V1==1500]] * x[ctr=="be" & decade==1500, im3_dec])

plot(im3_dec ~ decade, data=x[ctr=="nl", ], type='l', col=2, lwd=1.5, bty='l', xlim=c(700, 1500), main="nl")
points(x=c(1, 1000, 1500), y=gdp[V1 <= 1500, Netherlands / Netherlands[V1==1500]] * x[ctr=="nl" & decade==1500, im3_dec])

plot(im3_dec ~ decade, data=x[ctr=="ch", ], type='l', col=2, lwd=1.5, bty='l', xlim=c(700, 1500), main="ch")
points(x=c(1, 1000, 1500), y=gdp[V1 <= 1500, Switzerland / Switzerland[V1==1500]] * x[ctr=="ch" & decade==1500, im3_dec])
dev.off()

# m3 per region

out = fullobs_sp[data.table::between(year, 700, 1500), list(im2 = sum(im2_ann, na.rm=T), 
                                    im3 = sum(im3_ann, na.rm=T), 
                                    im2_tot = max(im2_cml, na.rm=T),
                                    im3_tot = max(im3_cml, na.rm=T)), 
    by=list(ctr, city, decade, category)][order(ctr, city, category, decade), ]
write.csv(out, "dat/fullobs_sp_20y.csv", row.names=F)

x = merge(fullobs_sp, siem[, list(inhab, lat, lon, city, year)], by=c('city', 'year'), all.x=T)
x = x[!is.na(decade) & data.table:::between(decade, 700, 1500), ]


x1 = x[, list(im3_dec = base::sum(.SD, na.rm=T) / M, inhab=sum(inhab, na.rm=T)), by=list(decade, ctr), .SDcols = impvrbs]
x2 = x[, list(im3_dec = base::sum(.SD, na.rm=T) / M, inhab=sum(inhab, na.rm=T)), by=list(decade, ctr2), .SDcols = impvrbs]
x3 = x[, list(im3_dec = base::sum(.SD, na.rm=T) / M, inhab=sum(inhab, na.rm=T)), by=list(decade, ctr3), .SDcols = impvrbs]
x3_im3 = x[, list(im3_dec = base::sum(.SD, na.rm=T) / M), by=list(decade, ctr3), .SDcols = grep("im3_ann", names(x))]
x3 = x3[x3_im3, on = list(decade, ctr3)]
eu = x[, list(im3_dec = base::sum(.SD, na.rm=T) / M, inhab = sum(inhab, na.rm=T)), by=decade, .SDcols = impvrbs]

x1[decade %% 100!=0, inhab:=NA] # fix zeroes
x2[decade %% 100!=0, inhab:=NA]
x3[decade %% 100!=0, inhab:=NA]
eu[decade %% 100 != 0, inhab := NA]
x1[, iinhab:=exp(zoo::na.approx(log(inhab))), by=ctr]
x2[, iinhab:=exp(zoo::na.approx(log(inhab))), by=ctr2]
x3[, iinhab:=exp(zoo::na.approx(log(inhab))), by=ctr3]
eu[, iinhab := exp(zoo::na.approx(log(inhab)))]

write.csv(x3, "~/dropbox/cathedrals/dat/countryseries.csv", row.names=F)

x1[, im3_dec_smth:=predict(loess(im3_dec / iinhab ~ decade), newdata=decade), by=ctr]
x2[, im3_dec_smth:=predict(loess(im3_dec / iinhab ~ decade), newdata=decade), by=ctr2]
x3[, im3_dec_smth:=predict(loess(im3_dec / iinhab ~ decade), newdata=decade), by=ctr3]
x3[, im3_dec_smth:=predict(loess(im3_dec / iinhab ~ decade), newdata=decade), by=ctr3]
eu[, im3_dec_smth:=predict(loess(im3_dec / iinhab ~ decade), newdata=decade)]


pdf('figs/europetotal_puc_hc.pdf', height=6)
par(mfrow=c(1, 1))
plot(im3_dec / iinhab ~ decade, data=eu, type='n', bty='l')
abline(v=c(768, 1315, 1000, 1348), col='gray')
lines(im3_dec / iinhab ~ decade, data=eu, type='b', col=2, lwd=1.5, pch=1, cex=0.9)
dev.off()

pdf("figs/pucpanel.pdf", width=8)
par(mfrow=c(2, 2), mar=c(4, 4, 1.5, 0.5), font.main=1)
plot(im3_dec / iinhab ~ decade, data=x1[ctr=='fr', ], type='l', 
    bty='l', ylab='m3/dec', xlab='', main='France', col=2)
lines(im3_dec / iinhab ~ decade, data=eu, type='l', col='gray70')
plot(im3_dec / iinhab ~ decade, data=x1[ctr=='de', ], type='l', 
    bty='l', ylab='m3/dec', xlab='', main='Germany', col=2)
lines(im3_dec / iinhab ~ decade, data=eu, type='l', col='gray70')
plot(im3_dec / iinhab ~ decade, data=x1[ctr=='uk', ], type='l', 
    bty='l', ylab='m3/dec', main='Britain', col=2)
lines(im3_dec / iinhab ~ decade, data=eu, type='l', col='gray70')
plot(im3_dec / iinhab ~ decade, data=x3[ctr3=='lc', ], type='l', 
    bty='l', ylab='m3/dec', main='Low Countries', col=2)
lines(im3_dec / iinhab ~ decade, data=eu, type='l', col='gray70')
dev.off()


pdf('figs/france_panel_hc.pdf', height=4, width=9)
par(mfrow=c(1, 3), font.main=1)
yl = range(x1[ctr=='fr', im3_dec])
plot(im3_dec ~ decade, data=x1[ctr=='fr', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Fr., total', col=2)
plot(im3_dec ~ decade, data=x3[ctr3=='fr_south', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Fr., south', col=2)
lines(im3_dec ~ decade, data=x1[ctr=='fr', ], col='gray70')
plot(im3_dec ~ decade, data=x3[ctr3=='fr_north', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Fr., north', col=2)
lines(im3_dec ~ decade, data=x1[ctr=='fr', ], col='gray70')
dev.off()

# pdf('figs/france_panel_hc.pdf', height=4, width=9)
par(mfrow=c(1, 3), font.main=1)
yl = range(x3[ctr3=='fr_south' | ctr3=="fr_north", sum(im3_dec), by = decade][, V1])
plot(V1 ~ decade, data=x3[ctr3=='fr_south' | ctr3=="fr_north", sum(im3_dec), by = decade], ylim=yl, type='l', bty='l', ylab='m2/dec', main='Fr., south', col=2)
plot(im3_dec ~ decade, data=x3[ctr3=='fr_south', ], ylim=yl, type='l', bty='l', ylab='m2/dec', main='Fr., south', col=2)
lines(V1 ~ decade, data=x3[ctr3=='fr_south' | ctr3=="fr_north", sum(im3_dec), by = decade], type='l', col='gray70')
plot(im3_dec ~ decade, data=x3[ctr3=='fr_north', ], ylim=yl, type='l', bty='l', ylab='m2/dec', main='Fr., north', col=2)
lines(V1 ~ decade, data=x3[ctr3=='fr_south' | ctr3=="fr_north", sum(im3_dec), by = decade], type='l', col='gray70')
# dev.off()

pdf('figs/france_panel_puc_hc.pdf', height=4, width=9)
par(mfrow=c(1, 3), font.main=1)
yl = range(x1[ctr=='fr', im3_dec / iinhab]) * 1.1
plot(im3_dec / iinhab ~ decade, data=x1[ctr=='fr', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Fr., total', col=2)
plot(im3_dec / iinhab ~ decade, data=x3[ctr3=='fr_south', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Fr., south', col=2)
lines(im3_dec / iinhab ~ decade, data=x1[ctr=='fr', ], col='gray70')
plot(im3_dec / iinhab ~ decade, data=x3[ctr3=='fr_north', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Fr., north', col=2)
lines(im3_dec / iinhab ~ decade, data=x1[ctr=='fr', ], col='gray70')
dev.off()

pdf('figs/germany_panel_hc.pdf', height=4, width=9)
par(mfrow=c(1, 3), font.main=1)
yl = range(x1[grep('de', ctr), im3_dec])
plot(im3_dec ~ decade, data=x1[ctr=='de', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='de., total', col=2)
plot(im3_dec ~ decade, data=x3[ctr3=='de_sw', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='de., south-west', col=2)
lines(im3_dec ~ decade, data=x1[ctr=='de', ], col='gray70')
plot(im3_dec ~ decade, data=x3[ctr3=='de_ne', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='de., north-east', col=2)
lines(im3_dec ~ decade, data=x1[ctr=='de', ], col='gray70')
dev.off()

pdf('figs/germany_panel_puc_hc.pdf', height=4, width=9)
par(mfrow=c(1, 3), font.main=1)
yl = range(x3[grep('de', ctr3), im3_dec / iinhab])
plot(im3_dec / iinhab ~ decade, data=x1[ctr=='de', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='de., total', col=2)
plot(im3_dec / iinhab ~ decade, data=x3[ctr3=='de_sw', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='de., south-west', col=2)
lines(im3_dec / iinhab ~ decade, data=x1[ctr=='de', ], col='gray70')
plot(im3_dec / iinhab ~ decade, data=x3[ctr3=='de_ne', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='de., north-east', col=2)
lines(im3_dec / iinhab ~ decade, data=x1[ctr=='de', ], col='gray70')
dev.off()

pdf('figs/britain_panel_hc.pdf', height=4, width=9)
par(mfrow=c(1, 3), font.main=1)
yl = range(x1[grep('uk', ctr), im3_dec])
plot(im3_dec ~ decade, data=x1[ctr=='uk', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='UK, total', col=2)
plot(im3_dec ~ decade, data=x3[ctr3=='uk_se', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='UK, south-east', col=2)
lines(im3_dec ~ decade, data=x1[ctr=='uk', ], col='gray70')
plot(im3_dec ~ decade, data=x3[ctr3=='uk_nw', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='UK, north-west', col=2)
lines(im3_dec ~ decade, data=x1[ctr=='uk', ], col='gray70')
dev.off()

pdf('figs/britain_panel_puc_hc.pdf', height=4, width=9)
par(mfrow=c(1, 3), font.main=1)
yl = range(x3[grep('uk', ctr3), im3_dec / iinhab])
plot(im3_dec / iinhab ~ decade, data=x1[ctr=='uk', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='UK, total', col=2)
plot(im3_dec / iinhab ~ decade, data=x3[ctr3=='uk_se', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='UK, south-east', col=2)
lines(im3_dec / iinhab ~ decade, data=x1[ctr=='uk', ], col='gray70')
plot(im3_dec / iinhab ~ decade, data=x3[ctr3=='uk_nw', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='UK, north-west', col=2)
lines(im3_dec / iinhab ~ decade, data=x1[ctr=='uk', ], col='gray70')
dev.off()


pdf('figs/lowctr_panel_hc.pdf', height=4, width=9)
par(mfrow=c(1, 3), font.main=1)
yl = range(x3[grep('lc', ctr3), im3_dec])
plot(im3_dec ~ decade, data=x3[ctr3=='lc', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='LC, total', col=2)
plot(im3_dec ~ decade, data=x1[ctr=='be', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Belgium', col=2)
lines(im3_dec ~ decade, data=x3[ctr3=='lc', ], col='gray70')
plot(im3_dec ~ decade, data=x1[ctr=='nl', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Netherlands', col=2)
lines(im3_dec ~ decade, data=x3[ctr3=='lc', ], col='gray70')
dev.off()

pdf('figs/lowctr_panel_puc_hc.pdf', height=4, width=9)
par(mfrow=c(1, 3), font.main=1)
yl = range(x1[grep('nl|be', ctr), im3_dec / iinhab])
plot(im3_dec / iinhab ~ decade, data=x3[ctr3=='lc', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='LC, total', col=2)
plot(im3_dec / iinhab ~ decade, data=x1[ctr=='be', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Belgium', col=2)
lines(im3_dec / iinhab ~ decade, data=x3[ctr3=='lc', ], col='gray70')
plot(im3_dec / iinhab ~ decade, data=x1[ctr=='nl', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Netherlands', col=2)
lines(im3_dec / iinhab ~ decade, data=x3[ctr3=='lc', ], col='gray70')
dev.off()

x1 = x[, list(im2_dec=sum(im2_ann, na.rm=T), im3_dec=sum(im3_ann, na.rm=T), inhab=sum(inhab, na.rm=T)), by=list(decade, ctr)]
x2 = x[, list(im2_dec=sum(im2_ann, na.rm=T), im3_dec=sum(im3_ann, na.rm=T), inhab=sum(inhab, na.rm=T)), by=list(decade, ctr2)]
x3 = x[, list(im2_dec=sum(im2_ann, na.rm=T), im3_dec=sum(im3_ann, na.rm=T), inhab=sum(inhab, na.rm=T)), by=list(decade, ctr3)]
x1[decade %% 100!=0, inhab:=NA] # fix zeroes
x2[decade %% 100!=0, inhab:=NA]
x3[decade %% 100!=0, inhab:=NA]
x1[, iinhab:=exp(zoo::na.approx(log(inhab))), by=ctr]
x2[, iinhab:=exp(zoo::na.approx(log(inhab))), by=ctr2]
x3[, iinhab:=exp(zoo::na.approx(log(inhab))), by=ctr3]

x1[, im2_dec_smth:=predict(loess(im2_dec / iinhab ~ decade), newdata=decade), by=ctr]
x2[, im2_dec_smth:=predict(loess(im2_dec / iinhab ~ decade), newdata=decade), by=ctr2]
x3[, im2_dec_smth:=predict(loess(im2_dec / iinhab ~ decade), newdata=decade), by=ctr3]

x1[, im3_dec_smth:=predict(loess(im3_dec / iinhab ~ decade), newdata=decade), by=ctr]
x2[, im3_dec_smth:=predict(loess(im3_dec / iinhab ~ decade), newdata=decade), by=ctr2]
x3[, im3_dec_smth:=predict(loess(im3_dec / iinhab ~ decade), newdata=decade), by=ctr3]

pdf('figs/france_panel.pdf', height=4, width=9)
par(mfrow=c(1, 3), font.main=1)
yl = range(x1[ctr=='fr', im3_dec])
plot(im3_dec ~ decade, data=x1[ctr=='fr', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Fr., total', col=2)
plot(im3_dec ~ decade, data=x2[ctr2=='fr_south', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Fr., south', col=2)
lines(im3_dec ~ decade, data=x1[ctr=='fr', ], col='gray70')
plot(im3_dec ~ decade, data=x2[ctr2=='fr_north', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Fr., north', col=2)
lines(im3_dec ~ decade, data=x1[ctr=='fr', ], col='gray70')
dev.off()

# pdf('figs/germany_panel.pdf', height=4, width=9)
# par(mfrow=c(1, 3), font.main=1)
# yl = range(x1[ctr=='de', im2_dec])
# plot(im2_dec ~ decade, data=x1[ctr=='de', ], type='l', ylim=yl, bty='l', ylab='m2/dec', main='de., total', col=2)
# plot(im2_dec ~ decade, data=x2[ctr2=='de_south', ], type='l', ylim=yl, bty='l', ylab='m2/dec', main='de., south', col=2)
# lines(im2_dec ~ decade, data=x1[ctr=='de', ], col='gray70')
# plot(im2_dec ~ decade, data=x2[ctr2=='de_north', ], type='l', ylim=yl, bty='l', ylab='m2/dec', main='de., north', col=2)
# lines(im2_dec ~ decade, data=x1[ctr=='de', ], col='gray70')
# dev.off()

pdf('figs/germany_panel_alt.pdf', height=4, width=9)
par(mfrow=c(1, 3), font.main=1)
yl = range(x1[ctr=='de', im3_dec])
plot(im3_dec ~ decade, data=x1[ctr=='de', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='de., total', col=2)
plot(im3_dec ~ decade, data=x3[ctr3=='de_sw', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='de., south-west', col=2)
lines(im3_dec ~ decade, data=x1[ctr=='de', ], col='gray70')
plot(im3_dec ~ decade, data=x3[ctr3=='de_ne', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='de., north-east', col=2)
lines(im3_dec ~ decade, data=x1[ctr=='de', ], col='gray70')
dev.off()

# pdf('figs/britain_panel.pdf', height=4, width=9)
# par(mfrow=c(1, 3), font.main=1)
# yl = range(x1[ctr=='uk', im2_dec])
# plot(im2_dec ~ decade, data=x1[ctr=='uk', ], type='l', ylim=yl, bty='l', ylab='m2/dec', main='uk., total', col=2)
# plot(im2_dec ~ decade, data=x2[ctr2=='uk_south', ], type='l', ylim=yl, bty='l', ylab='m2/dec', main='uk., south', col=2)
# lines(im2_dec ~ decade, data=x1[ctr=='uk', ], col='gray70')
# plot(im2_dec ~ decade, data=x2[ctr2=='uk_north', ], type='l', ylim=yl, bty='l', ylab='m2/dec', main='uk., north', col=2)
# lines(im2_dec ~ decade, data=x1[ctr=='uk', ], col='gray70')
# dev.off()

pdf('figs/britain_panel_alt.pdf', height=4, width=9)
par(mfrow=c(1, 3), font.main=1)
yl = range(x1[ctr=='uk', im3_dec])
plot(im3_dec ~ decade, data=x1[ctr=='uk', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='GB, total', col=2)
plot(im3_dec ~ decade, data=x3[ctr3=='uk_se', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='GB, south-east', col=2)
lines(im3_dec ~ decade, data=x1[ctr=='uk', ], col='gray70')
plot(im3_dec ~ decade, data=x3[ctr3=='uk_nw', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='GB, north-west', col=2)
lines(im3_dec ~ decade, data=x1[ctr=='uk', ], col='gray70')
dev.off()

# pdf('figs/smallctr_panel.pdf', height=4, width=9)
# par(mfrow=c(1, 3), font.main=1)
# yl = range(x1[ctr %in% c('nl', 'ch', 'be'), im2_dec])
# plot(im2_dec ~ decade, data=x1[ctr=='ch', ], type='l', ylim=yl, bty='l', ylab='m2/dec', main='ch', col=2)
# lines(im2_dec ~ decade, data=x1[ctr=='be', ], col='gray70')
# plot(im2_dec ~ decade, data=x2[ctr2=='be', ], type='l', ylim=yl, bty='l', ylab='m2/dec', main='be', col=2)
# plot(im2_dec ~ decade, data=x2[ctr2=='nl', ], type='l', ylim=yl, bty='l', ylab='m2/dec', main='nl', col=2)
# lines(im2_dec ~ decade, data=x1[ctr=='be', ], col='gray70')
# dev.off()

pdf('figs/lowctr_panel.pdf', height=4, width=9)
par(mfrow=c(1, 3), font.main=1)
yl = range(x3[ctr3 %in% c('lc'), im3_dec])
plot(im3_dec ~ decade, data=x3[ctr3=='lc', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='low ctr', col=2)  
plot(im3_dec ~ decade, data=x3[ctr3=='lc', ], type='l', ylim=yl, bty='l', ylab='m3/dec', col='gray70', main='belgium')
lines(im3_dec ~ decade, data=x2[ctr2=='be', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='be', col=2)
plot(im3_dec ~ decade, data=x3[ctr3=='lc', ], type='l', ylim=yl, bty='l', ylab='m3/dec', col='gray70', main='netherlands')
lines(im3_dec ~ decade, data=x2[ctr2=='nl', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='nl', col=2)
dev.off()

pdf('figs/francetrends.pdf', height=6)
par(mfrow=c(1, 1), font.main=1)
yl = range(x1[ctr=='fr', im3_dec])
plot(im3_dec ~ decade, data=x1[ctr=='fr', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='France')
lines(im3_dec ~ decade, data=x2[ctr2=='fr_south', ], col=uured)
lines(im3_dec ~ decade, data=x2[ctr2=='fr_north', ], col=uublu)
text(c(1450, 1450, 1470), c(17e4, 75e4, 40e4), c('South', "All", "North"), col=c(uured, 1, uublu), cex=0.8)
# legend('topleft', legend=c("All", "North", "South"), fill=c(1, uublu, uured))
dev.off()

pdf('figs/britaintrends.pdf', height=6)
par(mfrow=c(1, 1), font.main=1)
yl = range(x1[ctr=='uk', im3_dec])
plot(im3_dec ~ decade, data=x1[ctr=='uk', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='UK')
lines(im3_dec ~ decade, data=x2[ctr2=='uk_south', ], col=uured)
lines(im3_dec ~ decade, data=x2[ctr2=='uk_north', ], col=uublu)
text(c(1430 , 1450, 1410), c(11e4, 18e4, 5e4), c('South', "All", "North"), col=c(uured, 1, uublu), cex=0.8)
# legend('topleft', legend=c("All", "North", "South"), fill=c(1, uublu, uured))
dev.off()

pdf('figs/germanytrends.pdf', height=6)
par(mfrow=c(1, 1), font.main=1)
yl = range(x1[ctr=='de', im3_dec])
plot(im3_dec ~ decade, data=x1[ctr=='de', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Germany')
lines(im3_dec ~ decade, data=x2[ctr2=='de_south', ], col=uured)
lines(im3_dec ~ decade, data=x2[ctr2=='de_north', ], col=uublu)
text(c(1450, 1450, 1470), c(22e4, 75e4, 42e4), c('South', "All", "North"), col=c(uured, 1, uublu), cex=0.8)
# legend('topleft', legend=c("All", "North", "South"), fill=c(1, uublu, uured))
dev.off()

pdf('figs/benlchtrends.pdf', height=6)
par(mfrow=c(1, 1), font.main=1)
yl = range(x1[ctr=='uk', im3_dec])
plot(im3_dec ~ decade, data=x1[ctr=='ch', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Small ctr.')
lines(im3_dec ~ decade, data=x1[ctr=='be', ], col=uured)
lines(im3_dec ~ decade, data=x1[ctr=='nl', ], col=uublu)
text(c(1450, 1450, 1420), c(18e4, 5e4, 30e4), c('Be', "Ch", "Nl"), col=c(uured, 1, uublu), cex=0.8)
# legend('topleft', legend=c("Be", "Ch", "Nl"), fill=c(1, uublu, uured))
dev.off()

# gdp comparisons

x = fullobs_sp[data.table::between(decade, 700, 1500), list(im2_dec=sum(im2_ann, na.rm=T), im3_dec=sum(im3_ann, na.rm=T)), by=list(decade, ctr)]
trends = matrix(NA, nrow=6, ncol=2)
rownames(trends) = c("de", "uk", "fr", "be", "nl", "ch")
colnames(trends) = c("m3", "gdp")

trends[1, 1] = lm(im3_dec ~ decade, data=x[ctr=="de", ])$coef["decade"]
trends[2, 1] = lm(im3_dec ~ decade, data=x[ctr=="uk", ])$coef["decade"]
trends[3, 1] = lm(im3_dec ~ decade, data=x[ctr=="fr", ])$coef["decade"]
trends[4, 1] = lm(im3_dec ~ decade, data=x[ctr=="be", ])$coef["decade"]
trends[5, 1] = lm(im3_dec ~ decade, data=x[ctr=="nl", ])$coef["decade"]
trends[6, 1] = lm(im3_dec ~ decade, data=x[ctr=="ch", ])$coef["decade"]

trends[1, 2] = lm(Germany ~ V1, data=gdp[V1 > 1 & V1 < 1600, ])$coef["V1"]
trends[2, 2] = lm(UK ~ V1, data=gdp[V1 > 1 & V1 < 1600, ])$coef["V1"]
trends[3, 2] = lm(France ~ V1, data=gdp[V1 > 1 & V1 < 1600, ])$coef["V1"]
trends[4, 2] = lm(Belgium ~ V1, data=gdp[V1 > 1 & V1 < 1600, ])$coef["V1"]
trends[5, 2] = lm(Netherlands ~ V1, data=gdp[V1 > 1 & V1 < 1600, ])$coef["V1"]
trends[6, 2] = lm(Switzerland ~ V1, data=gdp[V1 > 1 & V1 < 1600, ])$coef["V1"]

plot(trends, type='n', log='xy', bty='l', xlab="m3/decade", ylab="GDP/decade")
text(trends[,'m3'], trends[,'gdp'], rownames(trends), col=2)