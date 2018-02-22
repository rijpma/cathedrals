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

dynobs[, heap100 := (year %% 100 == 0) | (year %% 100 == 1)]
dynobs[, heap20 := ((year - 20) %% 100 == 0) | ((year + 20) %% 100 == 0) ]
dynobs[, heap25 := ((year - 25) %% 100 == 0) | ((year + 25) %% 100 == 0) | ((year - 50) %% 100 == 0) ]
dynobs[, heap10 := (year %% 10 == 0) & (heap100 + heap20 + heap25) == 0]

dynobs[heap100 == TRUE, sdev := 30]
dynobs[heap20 == TRUE, sdev := 12]
dynobs[heap25 == TRUE, sdev := 15]

dynobs[heap100 == TRUE, delta := 50]
dynobs[heap20 == TRUE, delta := 10]
dynobs[heap25 == TRUE, delta := 12]
dynobs[heap10 == TRUE, delta := 5]

dynobs[, heap := FALSE]
dynobs[heap100 == TRUE | heap10 == TRUE | heap25 == TRUE | heap20 == TRUE, heap := TRUE]

set.seed(121314)
M = 9
for (j in 1:M){
    dynobs[, year_crc := year]

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

im3crc = fullobs[, .SD, .SDcols = grep('im3_ann\\d', names(fullobs))] +
    fullobs[, .SD * 0.005, .SDcols = grep('im3_cml\\d', names(fullobs))]
im2crc = fullobs[, .SD, .SDcols = grep('im2_ann\\d', names(fullobs))] +
    fullobs[, .SD * 0.005, .SDcols = grep('im2_cml\\d', names(fullobs))]
fullobs[, paste0("im3_ann_cmc", 1:M) := im3crc]
fullobs[, paste0("im2_ann_cmc", 1:M) := im2crc]
fullobs[, im3_ann_cmc := im3_ann + im3_cml * 0.005]
fullobs[, im2_ann_cmc := im2_ann + im2_cml * 0.005]

# do the imputations on dynobs on e.g. duration itself?
# still no city level dataset or panel dataset...
# do here, get standard errors, plug back in ?

fullobs[, decade := (trunc((year - 1) / 20) + 1) * 20] # so 1500 = 1481-1500

citobs = to_city_obs(statobs=statobs, fullobs=fullobs)

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

# m3 per region

out = fullobs_sp[data.table::between(year, 700, 1500), list(im2 = sum(im2_ann, na.rm=T), 
                                    im3 = sum(im3_ann, na.rm=T), 
                                    im2_tot = max(im2_cml, na.rm=T),
                                    im3_tot = max(im3_cml, na.rm=T)), 
    by=list(ctr, city, decade, category)][order(ctr, city, category, decade), ]
write.csv(out, "dat/fullobs_sp_20y.csv", row.names=F)
