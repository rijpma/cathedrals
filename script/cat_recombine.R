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
chr[osmid == "2322752", "lat"][1] = 46.4349 # cluny

chr[osmid == "66636479", "osmlink"][1] <- "http://www.openstreetmap.org/way/66636479"
chr[osmid == "66636479", "osmwikipedia"][1] <- ""
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
statobs[, lat := as.numeric(lat)]
statobs[, lon := as.numeric(lon)]

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


doubles = c("Strasbourg", "Strasbourg (Strassburg)", 
    "St Omer", "St Omer (Saint-Omer (Pas-de-Calais))", 
    "Chalons-sur-Marne", "Chalons-sur-Marne (Châlons-en-Champagne)")

doubles[!doubles %in% siem$city] # only worry about !
statobs[city %in% doubles[!doubles %in% siem$city], ]
siem[city %in% doubles, list(lat, lon, city, year)][order(city)]
siem[city %in% doubles, list(lat[1], lon[1]), by=city]

dynobs = to_dynobs(churchlist=chrlist)

# correct date heaping

pdf('figs/heaping2dig.pdf', height=6)
par(mfrow=c(1, 1))
hist(as.numeric(stringi::stri_sub(dynobs$year, -2)), breaks=100)
dev.off()

dynobs[, year_lead := data.table::shift(year, type='lead', fill=Inf), by=osmid]
dynobs[, year_lag := data.table::shift(year, type='lag', fill=-Inf), by=osmid]
dynobs[, dyear := data.table::shift(year, type='lead') - year, by=osmid]

M = 9
for (j in 1:M){
    dynobs[, year_crc := year]

    dynobs[, heap100 := year %% 100 == 0 | year %% 100 == 1]
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

    # uniform double resampling to prevent new heaping on 5
    # note that this currently causes underheaping at 5
    # because 2/3 of times neighbour is twenty years away
    # maybe simple approach preferable because smaller pbolem?
    n = nrow(dynobs[heap10 == TRUE])
    dynobs[heap10 == TRUE, splt10 := rbinom(n, size=1, prob=0.5)]
    # rsmpl = runif(n = n,
    #                min = dynobs[heap10 == TRUE, year - 4.75],
    #                max = dynobs[heap10 == TRUE, year + 4.75])
    rsmpl4 = runif(n = sum(dynobs$splt10 == 1, na.rm=T),
                   min = dynobs[heap10 == TRUE & splt10 == 1, year - 4],
                   max = dynobs[heap10 == TRUE & splt10 == 1, year + 4])
    rsmpl5 = runif(n = sum(dynobs$splt10 == 0, na.rm=T),
                   min = dynobs[heap10 == TRUE, ][splt10 == 0, year - 5],
                   max = dynobs[heap10 == TRUE, ][splt10 == 0, year + 5])

    # dynobs[heap10 == TRUE, year_crc := round(rsmpl)]
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

h_heap = hist(dynobs[, year], breaks=data.table::uniqueN(dynobs$year), plot = F)
h_unheap = hist(dynobs[, year_crc1], breaks=data.table::uniqueN(dynobs$year_crc1), plot = F)
h_heap$counts = log1p(h_heap$counts)
h_unheap$counts = log1p(h_unheap$counts)

pdf("figs/distrs_prepost_heap.pdf", width=11, height = 6)
par(mfrow=c(1, 2), font.main = 1)
plot(h_heap, main='heaping', xlab='year', yaxt = 'n')
axis(2, labels = c(1, 10, 100), at = log(c(1, 10, 100)))
plot(h_heap, main='heaping resampled', xlab='year', yaxt = 'n', border = NA)
lines(h_unheap)
axis(2, labels = c(1, 10, 100), at = log(c(1, 10, 100)))
# imps = dynobs[, round(rowMeans(.SD)), .SDcols=grep('year_crc', names(dynobs))]
# hist(imps, uniqueN(imps), main='heaping resampled average')
dev.off()

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

citobs = to_city_obs(statobs=statobs, fullobs=fullobs)

dynobs_sp = merge(dynobs, statobs, by="osmid")
dim(fullobs)
fullobs_sp = merge(fullobs, statobs, by="osmid", all=T)
dim(fullobs_sp)
dynobs[fullobs_sp[is.na(year), osmid], ] # are one obs churches in 700
fullobs_sp = fullobs_sp[!is.na(year), ]
unique(table(fullobs_sp$year, useNA='ifany'))


write.csv(dynobs, "dat/dynobs.csv", row.names=F)
write.csv(statobs, "dat/statobs.csv", row.names=F)
write.csv(citobs, "dat/citobs.csv", row.names=F)

outfile = gzfile("dat/fullobs_sp.csv.gz", 'w')
write.csv(fullobs_sp, outfile)
close(outfile)

# beware:
unique(stringi::stri_enc_mark(siem$city))
unique(stringi::stri_enc_mark(citobs$city))


# for bruce
# data.table::fwrite(fullobs_sp[year <= 1500, list(im2_dec=sum(im2_ann, na.rm=T), im3_dec=sum(im3_ann, na.rm=T)), by=list(decade, ctr2, category)], "dat/fullobs20yctr2cat.csv")
# data.table::fwrite(fullobs_sp[year <= 1500, list(im2_dec=sum(im2_ann, na.rm=T), im3_dec=sum(im3_ann, na.rm=T)), by=list(decade, ctr2)], "dat/fullobs20yctr2.csv")
# data.table::fwrite(fullobs_sp[year <= 1500, list(im2_dec=sum(im2_ann, na.rm=T), im3_dec=sum(im3_ann, na.rm=T)), by=list(decade, ctr)], "dat/fullobs20yctr.csv")


# m3 per region

out = fullobs_sp[data.table::between(year, 700, 1500), list(im2 = sum(im2_ann, na.rm=T), 
                                    im3 = sum(im3_ann, na.rm=T), 
                                    im2_tot = max(im2_cml, na.rm=T),
                                    im3_tot = max(im3_cml, na.rm=T)), 
    by=list(ctr, city, decade, category)][order(ctr, city, category, decade), ]
write.csv(out, "dat/fullobs_sp_20y.csv", row.names=F)


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