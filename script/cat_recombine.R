rm(list=ls())
options(stringsAsFactors=FALSE)
setwd('~/dropbox/cathedrals/')

library("data.table")
library("zoo")
library("osmar")
library("sp")
library("maptools")
library("raster")

source('script/cat_functions.r')

# germany
#--------
chr_de = data.table::fread("dat/deucities_eb.csv", skip=1, colClasses="character")
gss_de = data.table::fread("dat/deucities_ebg.csv", skip=1, colClasses="character")

chr_de[, city:=iconv(city, from='macroman', to='utf8')]
gss_de[, city:=iconv(city, from='macroman', to='utf8')]

chr_de[grepl('[A-z]', city), 2:ncol(chr_de):=chr_de[grepl('[A-z]', city), -ncol(chr_de), with=F], with=F]
gss_de[grepl('[A-z]', city), 2:ncol(gss_de):=gss_de[grepl('[A-z]', city), -ncol(gss_de), with=F], with=F]
setnames(chr_de, names(chr_de), c('osmid', names(chr_de)[1:(ncol(chr_de) - 1)]))
setnames(gss_de, names(gss_de), c('osmid', names(gss_de)[1:(ncol(gss_de) - 1)]))

chr_de[, osmid:=ifelse(grepl('[A-z]', osmid), shift(osmid, type='lead'), osmid)]
gss_de[, osmid:=ifelse(grepl('[A-z]', osmid), shift(osmid, type='lead'), osmid)]

chr_de[, osmwikipedia:=iconv(osmwikipedia, from='macroman', to='utf8')]
chr_de[, osmname:=iconv(osmname, from='macroman', to='utf8')]
chr_de[, city:=iconv(city, from='macroman', to='utf8')]
chr_de[ ,city:=gsub("√∂", "ö", city)]
chr_de[ ,city:=gsub("√º", "ü", city)]
chr_de[ ,city:=gsub("√§", "ä", city)]

gss_de[, osmwikipedia:=iconv(osmwikipedia, from='macroman', to='utf8')]
gss_de[, osmname:=iconv(osmname, from='macroman', to='utf8')]
gss_de[, city:=iconv(city, from='macroman', to='utf8')]
gss_de[ ,city:=gsub("√∂", "ö", city)]
gss_de[ ,city:=gsub("√º", "ü", city)]
gss_de[ ,city:=gsub("√§", "ä", city)]

table(table(gss_de$osmid[gss_de$osmid!='']))
duplids = names(table(gss_de$osmid[gss_de$osmid!='']))[table(gss_de$osmid[gss_de$osmid!='']) > 6]

as.data.frame(chr_de[osmid%in% duplids, list(osmid, lon, V8, V9)])
chr_de = chr_de[!(osmid=="26441576" & city=="Duesseldorf (Düsseldorf)"), ]
chr_de = chr_de[!(osmid=="130432939" & city=="Ludwigshafen"), ]
chr_de = chr_de[!(osmid=="34869135" & city=="Altona"), ]
chr_de = chr_de[!(osmid=="113302207" & city=="Werden (now part of Essen)"), ]
chr_de = chr_de[!(osmid=="91328768" & (city=="Barmen (Wuppertal)"|city=="Wuppertal (in 1929 from Eberfeld, Barmen and three others)")), ]
chr_de[osmid=="217546683", 1:10, with=F] # ask eltjo

chrlist_de = recombine_churches(churches=chr_de, guesses=gss_de)
# yes = which(sapply(chrlist_de, function(x) nrow(x$dynamic)) > 0)
# no = which(sapply(chrlist_de, function(x) nrow(x$dynamic)) == 0)
# lapply(chrlist_de[sample(yes, 10)], `[[`, 'dynamic')
# sample(no, 10)

statobs_de = do.call(rbind, lapply(chrlist_de, `[[`, 'static')) 
statobs_de = data.table::as.data.table(statobs_de)
statobs_de$ctr = "de"

dynobs_de = to_dynobs(churchlist=chrlist_de)
fullobs_de = to_annual_obs(dyn=dynobs_de, churchlist=chrlist_de)
citobs_de = to_city_obs(statobs=statobs_de, fullobs=fullobs_de)


chk = checks(full=fullobs_de, dyn=dynobs_de, churchlist=chrlist_de)
str(chk)
chrlist_de[chk$totaloutl]

plot(fullobs_de[, sum(im2_ann, na.rm=T), by=year], type='l', xlim=c(800, 1500))

# britain
#--------
chr_gb = data.table::fread("dat/gbrcities_eb.csv", skip=1, colClasses="character")
gss_gb = data.table::fread("dat/gbrcities_ebg.csv", skip=1, colClasses="character")
names(chr_gb)[1] = names(gss_gb)[1] = 'osmid'

chr_gb_rel = data.table::fread("dat/gbrcities_rels_eb.csv", skip=1, colClasses="character")
gss_gb_rel = data.table::fread("dat/gbrcities_rels_ebg.csv", skip=1, colClasses="character")

setdiff(names(chr_gb), names(chr_gb_rel)) # is ok
setdiff(names(chr_gb_rel), names(chr_gb))
chr_gb = rbind(chr_gb_rel, chr_gb, fill=TRUE)
gss_gb = rbind(gss_gb_rel, gss_gb, fill=TRUE)

chr_gb[, osmwikipedia:=iconv(osmwikipedia, from='macroman', to='utf8')]
chr_gb[, osmname:=iconv(osmname, from='macroman', to='utf8')]
chr_gb[, city:=iconv(city, from='macroman', to='utf8')]
gss_gb[, osmwikipedia:=iconv(osmwikipedia, from='macroman', to='utf8')]
gss_gb[, osmname:=iconv(osmname, from='macroman', to='utf8')]
gss_gb[, city:=iconv(city, from='macroman', to='utf8')]

as.data.frame(chr_gb[osmid%in% duplids, list(osmid, lon, V9, V10)])

chrlist_gb = recombine_churches(churches=chr_gb, guesses=gss_gb)
# yes = which(sapply(chrlist_gb, function(x) nrow(x$dynamic)) > 0)
# no = which(sapply(chrlist_gb, function(x) nrow(x$dynamic)) == 0)
# lapply(chrlist_gb[sample(yes, 10)], `[[`, 'dynamic')
# sample(no, 10)


chrlist_gb[["96409899"]]$static$surface = 1000 # crude fix

statobs_gb = do.call(rbind, lapply(chrlist_gb, function(x) x$static))
statobs_gb = data.table::as.data.table(statobs_gb)
statobs_gb$ctr = "uk"

dynobs_gb = to_dynobs(churchlist=chrlist_gb)
fullobs_gb = to_annual_obs(dyn=dynobs_gb, churchlist=chrlist_gb)
citobs_gb = to_city_obs(statobs=statobs_gb, fullobs=fullobs_gb)

chk = checks(full=fullobs_gb, dyn=dynobs_gb, churchlist=chrlist_gb)

plot(fullobs_gb[, sum(im2_ann, na.rm=T), by=year], type='l', xlim=c(800, 1500))

# luxemburg
#------------
# no medieval churches found so far

# netherlands
#------------
chr_nl = data.table::fread("dat/nldcities_eb.csv", skip=1, colClasses="character")
chr_nl_rel = data.table::fread("dat/nldcities_rels_eb.csv", skip=1, colClasses="character")
gss_nl = data.table::fread("dat/nldcities_ebg.csv", skip=1, colClasses="character")
gss_nl_rel = data.table::fread("dat/nldcities_rels_eb.csv", skip=1, colClasses="character")

setdiff(names(chr_nl), names(chr_nl_rel))
setdiff(names(chr_nl_rel), names(chr_nl))
chr_nl = rbind(chr_nl_rel, chr_nl, fill=TRUE)
gss_nl = rbind(gss_nl_rel, gss_nl, fill=TRUE)

chr_nl[, osmwikipedia:=iconv(osmwikipedia, from='macroman', to='utf8')]
chr_nl[, osmname:=iconv(osmname, from='macroman', to='utf8')]
chr_nl[, city:=iconv(city, from='macroman', to='utf8')]
gss_nl[, osmwikipedia:=iconv(osmwikipedia, from='macroman', to='utf8')]
gss_nl[, osmname:=iconv(osmname, from='macroman', to='utf8')]
gss_nl[, city:=iconv(city, from='macroman', to='utf8')]

as.data.frame(chr_nl[osmid%in% duplids, list(osmid, lon, V9, V10)])

chrlist_nl = recombine_churches(churches=chr_nl, guesses=gss_nl)
# yes = which(sapply(chrlist_nl, function(x) nrow(x$dynamic)) > 0)
# no = which(sapply(chrlist_nl, function(x) nrow(x$dynamic)) == 0)
# lapply(chrlist_nl[sample(yes, 10)], `[[`, 'dynamic')
# sample(no, 10)

statobs_nl = do.call(rbind, lapply(chrlist_nl, function(x) x$static))
statobs_nl = data.table::as.data.table(statobs_nl)
statobs_nl$ctr = "nl"

dynobs_nl = to_dynobs(churchlist=chrlist_nl)
fullobs_nl = to_annual_obs(dyn=dynobs_nl, churchlist=chrlist_nl)
citobs_nl = to_city_obs(statobs=statobs_nl, fullobs=fullobs_nl)

chk = checks(full=fullobs_nl, dyn=dynobs_nl, churchlist=chrlist_nl)

plot(fullobs_nl[, sum(im2_ann, na.rm=T), by=year], type='l', xlim=c(800, 1500))

# belgium
#--------
chr_be = fread('dat/belgiancities_eb.csv', skip=1, colClasses="character")
chr_be_rel = fread('dat/belgiancities_rels_eb.csv', skip=1, colClasses="character")
gss_be = data.table::fread("dat/belgiancities_ebg.csv", skip=1, colClasses="character")
gss_be_rel = data.table::fread("dat/belgiancities_rels_eb.csv", skip=1, colClasses="character")

setdiff(names(chr_be_rel), names(chr_be))
setdiff(names(chr_be), names(chr_be_rel))

chr_be = rbind(chr_be, chr_be_rel, fill=TRUE)
gss_be = rbind(gss_be, gss_be_rel, fill=TRUE)

chr_be[, osmwikipedia:=iconv(osmwikipedia, from='macroman', to='utf8')]
chr_be[, osmname:=iconv(osmname, from='macroman', to='utf8')]
chr_be[, city:=iconv(city, from='macroman', to='utf8')]
gss_be[, osmwikipedia:=iconv(osmwikipedia, from='macroman', to='utf8')]
gss_be[, osmname:=iconv(osmname, from='macroman', to='utf8')]
gss_be[, city:=iconv(city, from='macroman', to='utf8')]

chr_be[city=="Lede", city:="Aalst (Alost)"]
chr_be[city=="rumpelbeke", city:="Roeselare"]
chr_be[city=="damme", city:="Brugge (Bruges)"]
chr_be[city=="elverdinge", city:="Ieper (Ypres)"]

as.data.frame(chr_be[osmid%in% duplids, list(osmid, lon, V9, V10)])

chrlist_be = recombine_churches(churches=chr_be, guesses=gss_be)
# yes = which(sapply(chrlist_be, function(x) nrow(x$dynamic)) > 0)
# no = which(sapply(chrlist_be, function(x) nrow(x$dynamic)) == 0)
# lapply(chrlist_be[sample(yes, 10)], `[[`, 'dynamic')
# sample(no, 10)

chrlist_be[["94553693"]]$dynamic = data.frame(osmid=94553693, year=c(1224, 1240, 1400, 1500),
                                        m2=c(0, 525, 0, 1050),
                                        hgt=c(10, 10, 14, 14),
                                        gss_hgt=c(T, T, T, T),
                                        gss_m2=c(T, T, T, T))
chrlist_be[["105194545"]]$dynamic = data.frame(osmid=105194545, year=c(1100, 1200, 1201, 1500),
                                        m2=c(0, 1300, 0 , 1300),
                                        hgt=c(16, 16, 23, 23),
                                        gss_hgt=c(F, F, F, F),
                                        gss_m2=c(F, F, F, F))

statobs_be = do.call(rbind, lapply(chrlist_be, function(x) x$static))
statobs_be = data.table::as.data.table(statobs_be)
statobs_be$ctr = "be"

dynobs_be = to_dynobs(churchlist=chrlist_be)
fullobs_be = to_annual_obs(dyn=dynobs_be, churchlist=chrlist_be)
citobs_be = to_city_obs(statobs=statobs_be, fullobs=fullobs_be)

chk = checks(full=fullobs_be, dyn=dynobs_be, churchlist=chrlist_be)

plot(fullobs_be[, sum(im2_ann, na.rm=T), by=year], type='l', xlim=c(800, 1500))

# switzerland
#------------
chr_ch = fread('dat/swisscities_eb.csv', skip=1, colClasses="character")
gss_ch = fread('dat/swisscities_ebg.csv', skip=1, colClasses="character")
chr_rel_ch = fread('dat/swisscities_rels_eb.csv', skip=1, colClasses="character")
gss_rel_ch = fread('dat/swisscities_rels_ebg.csv', skip=1, colClasses="character")

chr_ch = rbind(chr_rel_ch, chr_ch)
gss_ch = rbind(gss_rel_ch, gss_ch)

chr_ch[, osmwikipedia:=iconv(osmwikipedia, from='macroman', to='utf8')]
chr_ch[, osmname:=iconv(osmname, from='macroman', to='utf8')]
chr_ch[, city:=iconv(city, from='macroman', to='utf8')]
gss_ch[, osmwikipedia:=iconv(osmwikipedia, from='macroman', to='utf8')]
gss_ch[, osmname:=iconv(osmname, from='macroman', to='utf8')]
gss_ch[, city:=iconv(city, from='macroman', to='utf8')]

unique(chr_ch$city)
unique(gss_ch$city) # zurich encoding problem only in gss

as.data.frame(chr_ch[osmid%in% duplids, list(osmid, lon, V9, V10)])

chrlist_ch = recombine_churches(churches=chr_ch, guesses=gss_ch)
unique(sapply(chrlist_ch, function(x) x$static$city))
# yes = which(sapply(chrlist_ch, function(x) nrow(x$dynamic)) > 0)
# no = which(sapply(chrlist_ch, function(x) nrow(x$dynamic)) == 0)
# lapply(chrlist_ch[sample(yes, 10)], `[[`, 'dynamic')
# sample(no, 10)

chrlist_ch[["16082412"]]$dynamic = data.frame(osmid=16082412, year=c(850, 900, 1180, 1320),
                                        m2=c(0, 300, 0, 600),
                                        hgt=c(8, 8, 11, 11),
                                        gss_hgt=c(T, T, T, T),
                                        gss_m2=c(T, T, T, T))

statobs_ch = do.call(rbind, lapply(chrlist_ch, function(x) x$static))
statobs_ch = data.table::as.data.table(statobs_ch)
statobs_ch$ctr = "ch"

dynobs_ch = to_dynobs(chrlist_ch)
fullobs_ch = to_annual_obs(dyn=dynobs_ch, churchlist=chrlist_ch)
citobs_ch = to_city_obs(statobs=statobs_ch, fullobs=fullobs_ch)

checks(full=fullobs_ch, dyn=dynobs_ch, churchlist=chrlist_ch)

plot(fullobs_ch[, sum(im2_ann, na.rm=T), by=year], type='l', xlim=c(700, 1500))

# france
# ------

chr_fr = data.table::fread("dat/frenchcities_eb.csv", skip=1, colClasses='character')
chr_fr_ex = data.table::fread("dat/frenchcities_extra_eb.csv", skip=1, colClasses='character')
chr_fr_exm = data.table::fread("dat/frenchcities_extra_more_eb.csv", skip=1, colClasses='character')
chr_rel_fr = data.table::fread("dat/frenchcities_rels_eb.csv", colClasses="character")

setnames(chr_fr, names(chr_fr)[1], 'osmid')

setdiff(names(chr_fr), names(chr_rel_fr))
setdiff(names(chr_rel_fr), names(chr_fr))
chr_fr = data.table::rbindlist(list(chr_fr, chr_fr_ex, chr_fr_exm, chr_rel_fr), fill=TRUE)

gss_fr = data.table::fread("dat/frenchcities_ebg.csv", skip=1, colClasses='character')
gss_fr_ex = data.table::fread("dat/frenchcities_extra_ebg.csv", skip=1, colClasses='character')
gss_fr_exm = data.table::fread("dat/frenchcities_extra_more_ebg.csv", skip=1, colClasses='character')
gss_rel_fr = data.table::fread("dat/frenchcities_rels_ebg.csv", colClasses='character')

setnames(gss_fr, names(gss_fr)[1], 'osmid')
setnames(gss_fr_exm, 1:ncol(gss_fr_exm), names(gss_fr_ex))

setdiff(names(gss_fr), names(gss_rel_fr))
setdiff(names(gss_rel_fr), names(gss_fr))

gss_fr = data.table::rbindlist(list(gss_fr, gss_fr_ex, gss_fr_exm, gss_rel_fr), fill=TRUE)

chr_fr_crc = fread("dat/frenchcities_crc_eb.csv", colClasses='character')
gss_fr_crc = fread("dat/frenchcities_crc_ebg.csv", colClasses='character')

setnames(chr_fr_crc, names(chr_fr_crc), names(chr_fr)[1:ncol(chr_fr_crc)])
setnames(gss_fr_crc, names(gss_fr_crc), names(gss_fr)[1:ncol(gss_fr_crc)])
chr_fr_crc = chr_fr_crc[osmid!='', ]
gss_fr_crc = gss_fr_crc[osmid!='', ]

chr_fr_crc = chr_fr_crc[match(paste(chr_fr$osmid[chr_fr$osmid %in% chr_fr_crc$osmid], 1:5),
    paste(chr_fr_crc$osmid, 1:5)),]
gss_fr_crc = gss_fr_crc[match(paste(gss_fr$osmid[gss_fr$osmid %in% gss_fr_crc$osmid], 1:5),
    paste(gss_fr_crc$osmid, 1:5)),]

cbind(chr_fr_crc[, 1, with=F], chr_fr[chr_fr$osmid %in% chr_fr_crc$osmid, 1, with=F])
chr_fr[chr_fr$osmid %in% chr_fr_crc$osmid, 1:ncol(chr_fr_crc):= chr_fr_crc[, 1:ncol(chr_fr_crc), with=F], with=F]
cbind(gss_fr_crc[, 1, with=F], gss_fr[gss_fr$osmid %in% gss_fr_crc$osmid, 1, with=F])
gss_fr[gss_fr$osmid %in% gss_fr_crc$osmid, 1:ncol(gss_fr_crc):= gss_fr_crc[, 1:ncol(gss_fr_crc), with=F], with=F]

chr_fr[, osmwikipedia:=iconv(osmwikipedia, from='macroman', to='utf8')]
chr_fr[, osmname:=iconv(osmname, from='macroman', to='utf8')]
chr_fr[, city:=iconv(city, from='macroman', to='utf8')]
gss_fr[, osmwikipedia:=iconv(osmwikipedia, from='macroman', to='utf8')]
gss_fr[, osmname:=iconv(osmname, from='macroman', to='utf8')]
gss_fr[, city:=iconv(city, from='macroman', to='utf8')]

as.data.frame(chr_fr[osmid%in% duplids, list(osmid, lon, V9, V10)])
chrlist_fr = recombine_churches(churches=chr_fr, guesses=gss_fr)
unique(sapply(chrlist_fr, function(x) x$static$city))
# check perigeux
# yes = which(sapply(chrlist_fr, function(x) nrow(x$dynamic)) > 0)
# no = which(sapply(chrlist_fr, function(x) nrow(x$dynamic)) == 0)
# lapply(chrlist_fr[sample(yes, 10)], `[[`, 'dynamic')
# sample(no, 10)

rws = unlist(lapply(chrlist_fr, function(x) nrow(x[["dynamic"]])))
length(chrlist_fr)
chrlist_fr[names(rws)[rws == 1]]
# chrlist_fr = chrlist_fr[names(rws)[rws > 1]]
chrlist_fr = chrlist_fr[names(rws)[rws > 0]]
length(chrlist_fr)
table(unlist(lapply(chrlist_fr, function(x) nrow(x[["dynamic"]]))))

# corrections
chrlist_fr[["144278109"]][["dynamic"]][, 3:4] <- chrlist_fr[["144278109"]][["dynamic"]][, 2:3]
chrlist_fr[["309089736"]][["dynamic"]][, 3:4] <- chrlist_fr[["309089736"]][["dynamic"]][, 2:3]
chrlist_fr[["82048485"]][["dynamic"]][, 3:4] <- chrlist_fr[["82048485"]][["dynamic"]][, 2:3]
chrlist_fr[["65309666"]][["dynamic"]][, 3:4] <- chrlist_fr[["65309666"]][["dynamic"]][, 2:3]
chrlist_fr[["73122051"]][["dynamic"]][, 3:4] <- chrlist_fr[["73122051"]][["dynamic"]][, 2:3]
chrlist_fr[["88612586"]][["dynamic"]][, 3:4] <- chrlist_fr[["88612586"]][["dynamic"]][, 2:3]
chrlist_fr[["113692150"]][["dynamic"]][, 3:4] <- chrlist_fr[["113692150"]][["dynamic"]][, 2:3]
chrlist_fr[["26471260"]][["dynamic"]][, 2:3] <- chrlist_fr[["26471260"]][["dynamic"]][, 3:4]

chrlist_fr[["144278109"]][["dynamic"]]$year <- as.numeric(unlist(chr_fr[osmid=="144278109", ][1, -c(1:8), with=F])[1:nrow(chrlist_fr[["144278109"]][["dynamic"]])])
chrlist_fr[["309089736"]][["dynamic"]]$year <- as.numeric(unlist(chr_fr[osmid=="309089736", ][1, -c(1:8), with=F])[1:nrow(chrlist_fr[["309089736"]][["dynamic"]])])
chrlist_fr[["82048485"]][["dynamic"]]$year <- as.numeric(unlist(chr_fr[osmid=="82048485", ][1, -c(1:8), with=F])[1:nrow(chrlist_fr[["82048485"]][["dynamic"]])])
chrlist_fr[["65309666"]][["dynamic"]]$year <- as.numeric(unlist(chr_fr[osmid=="65309666", ][1, -c(1:8), with=F])[1:nrow(chrlist_fr[["65309666"]][["dynamic"]])])
chrlist_fr[["73122051"]][["dynamic"]]$year <- as.numeric(unlist(chr_fr[osmid=="73122051", ][1, -c(1:8), with=F])[1:nrow(chrlist_fr[["73122051"]][["dynamic"]])])
chrlist_fr[["88612586"]][["dynamic"]]$year <- as.numeric(unlist(chr_fr[osmid=="88612586", ][1, -c(1:8), with=F])[1:nrow(chrlist_fr[["88612586"]][["dynamic"]])])
chrlist_fr[["113692150"]][["dynamic"]]$year <- as.numeric(unlist(chr_fr[osmid=="113692150", ][1, -c(1:8), with=F])[1:nrow(chrlist_fr[["113692150"]][["dynamic"]])])
chrlist_fr[["26471260"]][["dynamic"]]$hgt <- as.numeric(unlist(chr_fr[osmid=="26471260", ][5, -c(1:8), with=F])[1:nrow(chrlist_fr[["26471260"]][["dynamic"]])])

names(chrlist_fr[sapply(chrlist_fr, function(x) any(x$dynamic$gss_hgt!=x$dynamic$gss_m2))])
idx = c("86359613", "41552515", "92294792", "63420359")
# chrlist_fr[names(chrlist_fr)[names(chrlist_fr) %in% idx]]
# chrlist_fr[names(chrlist_fr)[!names(chrlist_fr) %in% idx]]

# lapply(chrlist_fr[names(chrlist_fr)[!names(chrlist_fr) %in% idx]], function(x) x$dynamic$gss_hgt | x$dynamic$gss_m2)

chrlist_fr[["119815440"]][["dynamic"]]$year[1] <- 1152
chrlist_fr[["104688596"]][["dynamic"]]$hgt <- 23
chrlist_fr[["292205686"]] <- NULL
chrlist_fr[["40461850"]][["dynamic"]]$year[2] <- 1534
chrlist_fr[["80615783"]] <- NULL
chrlist_fr[["39540475"]][["dynamic"]]$year[4] <- 1000
chrlist_fr[["88613054"]][["dynamic"]]$year[9:10] <- c(1285, 1425)
chrlist_fr[["130311744"]][["dynamic"]]$m2[2] <- 125
chrlist_fr[["92295579"]][["dynamic"]]$m2[2] <- 25
chrlist_fr[["55458284"]][["dynamic"]]$m2[2] <- 25
chrlist_fr[["61430628"]] <- NULL # double
chrlist_fr[["126913675"]][["dynamic"]]$year[5] <- 980
chrlist_fr[["252586198"]][["dynamic"]]$year[6] <- 1336
chrlist_fr[["44773604"]][["dynamic"]]$year[5] <- 1110
chrlist_fr[["36689922"]][["static"]]$city <- "Blois"
chrlist_fr[["121570436"]][["dynamic"]]$year[12] = 1380

statobs_fr = do.call(rbind, lapply(chrlist_fr, function(x) x$static))
statobs_fr = data.table::as.data.table(statobs_fr)
statobs_fr$ctr = "fr"

dynobs_fr = to_dynobs(chrlist_fr)
dynobs_fr[!osmid %in% idx, c("gss_hgt", "gss_m2"):=gss_m2|gss_hgt]
fullobs_fr = to_annual_obs(dyn=dynobs_fr, churchlist=chrlist_fr)
citobs_fr = to_city_obs(statobs=statobs_fr, fullobs=fullobs_fr)

checks(full=fullobs_fr, dyn=dynobs_fr, churchlist=chrlist_fr)

plot(fullobs_fr[, sum(im2_ann, na.rm=T), by=year], type='l', xlim=c(700, 1500))

# b2l expanded data
#------------------
siem = data.table::fread("dat/siem.csv")
data.table::setnames(siem, names(siem)[!grepl('^V\\d+$', names(siem))], 
    c('inhab', 'coord', 'yr', 'cargoratio', 'rivercanal', 'rivertoll', 
    'sqrtsetdens', 'parliament', 'manuscipt', 'commune', 'bishopric',
    'arcbishopric', 'seatoll', 'coastal', 'riverXcoast', 'landlocked', 
    'atlantic', 'whitesea', 'blacksea', 'northsea', 'caspian', 'mediterranean', 'baltic'))
for (i in 5:ncol(siem)){
    if (grepl("^V\\d+$", names(siem)[i])) names(siem)[i] = names(siem)[i - 1]
}
names(siem) = paste0(gsub('^V\\d+$', '', names(siem)), siem[2, ])

siem_fr = data.table::fread("dat/siem_france.csv")
data.table::setnames(siem_fr, 1:ncol(siem), names(siem))
siem_fr[, distance2:=NULL]
siem_ch = data.table::fread("dat/siem_swiss.csv")
data.table::setnames(siem_ch, 1:ncol(siem), names(siem))
siem_be = data.table::fread("dat/siem_belgium.csv")
data.table::setnames(siem_be, 1:ncol(siem), names(siem))
siem_nl = data.table::fread("dat/siem_nl.csv")[, -1, with=F]
data.table::setnames(siem_nl, 1:ncol(siem), names(siem))
siem_uk = data.table::fread("dat/siem_uk.csv")[, -1, with=F]
data.table::setnames(siem_uk, 1:ncol(siem), names(siem))
siem_de = data.table::fread("dat/siem_de.csv")[, -1, with=F]
data.table::setnames(siem_de, 1:ncol(siem), names(siem))

siem = data.table::rbindlist(list(siem_fr, siem_be, siem_ch, siem_uk, siem_nl, siem_de))
siem = siem[, !sapply(siem, function(x) all(is.na(x))), with=F]

measures = unique(gsub('\\d.*', '', names(siem)[grep('\\d{3}', names(siem))]))
measureslist = lapply(measures, function(x) grepr(paste0('^', x), names(siem)))
siem = data.table::melt(siem, measure=measureslist, 
    id.vars=setdiff(names(siem), unlist(measureslist)),
    variable.name="year", variable.factor=F)
siem$year = 600 + as.numeric(siem$year) * 100
setnames(siem, grep('value', names(siem)), measures)
siem[, (measures):=lapply(.SD, as.numeric), .SDcols=measures]

citobs = data.table::rbindlist(list(citobs_fr, citobs_ch, citobs_be, citobs_gb, citobs_nl, citobs_de))

fixes = lapply(gsub('-', ' ', setdiff(citobs$city, siem$city)), function(x) unique(siem$city)[grep(x, gsub('-', ' ', unique(siem$city)))])
fixes[sapply(fixes, length) == 0] = NA
fixes = unlist(fixes)
names(fixes) = setdiff(citobs$city, siem$city)

unique(fixes[match(citobs$city, names(fixes))])
citobs[, city2:=fixes[match(city, names(fixes))]]
citobs[!is.na(city2), city:=city2]
unique(citobs[!is.na(city2), list(city, city2)])
citobs[!is.na(city2), city:=city2][, city2:=NULL]

# fix excel encoding mess still necessary?
citobs[grep("√", city),]
statobs[grep("√", city),]

setdiff(citobs$city, siem$city)
setdiff(siem$city, citobs$city)

siem[, im2_cnt := citobs$im2_cnt[match(paste0(siem$city, siem$year), paste0(citobs$city, citobs$year))]]
siem[, im3_cnt := citobs$im3_cnt[match(paste0(siem$city, siem$year), paste0(citobs$city, citobs$year))]]

statobs = data.table::rbindlist(list(statobs_be, statobs_fr, statobs_ch, statobs_gb, statobs_nl, statobs_de), fill=T)
dynobs = data.table::rbindlist(list(dynobs_be, dynobs_fr, dynobs_ch, dynobs_gb, dynobs_nl, dynobs_de))
fullobs = data.table::rbindlist(list(fullobs_be, fullobs_fr, fullobs_ch, fullobs_gb, fullobs_nl, fullobs_de))

eltjo = copy(dynobs)
eltjo[, gss:=as.numeric(gss_m2|gss_hgt)]
eltjo[, m3:=NA]
eltjo[is.na(year),]

osm = copy(statobs)
osm = osm[osmid %in% unique(eltjo$osmid), ][order(ctr, city, osmname), ]

# check encoding
sapply(osm, function(s) all(stringi::stri_enc_isascii(na.omit(s))))
sapply(osm, function(s) all(stringi::stri_enc_isutf8(na.omit(s))))

fill = NULL
baseinfo = c("osmid", "city", "osmname", "surface", "osmwikipedia",
             "osmlink", "ctr", "lblink", "rmlink", "lat", "lon")
for (i in 1:nrow(osm)){
    t1 = data.frame(eltjo[osmid==osm$osmid[i], list(year, m2, hgt, m3, gss)])
    out = as.data.frame(matrix(nrow=length(baseinfo) + 20, ncol=6))
    out[, 1] = c(unlist(osm[i, baseinfo, with=F]), rep('x', 20)) # make sure order matches original!
    out[out$V1 %in% 'x', 2:(1 + ncol(t1))][1:nrow(t1), ] = t1
    out[4, 2:6] = c("year", "surface", "height", "m3", "guestimate")
    fill = rbind(fill, t(out), rep('', nrow(out)))
}
fill = as.data.frame(fill)
fill[, 1:3] = lapply(osm[, list(osmid, city, osmname)], rep, each=7)
fill[1:nrow(fill) %% 7 == 0, ] = ''
names(fill) = baseinfo

truiden3 = get_osm_data_church(3207528, what="relation")
smungo = get_osm_data_church(5761459, what="relation")
durham = get_osm_data_church(3584722, what="relation")

cluny = get_osm_data_church(2322752, what="relation")
cluny_rns = osmar::get_osm(osmar::way(174041154), full=TRUE)
cluny_rns_lines = osmar::as_sp(cluny_rns, what="lines")
cluny_rns_poly = Polygon(cluny_rns_lines@lines[[1]]@Lines[[1]])
cluny_rns_polys = Polygons(list(cluny_rns_poly), ID=cluny_rns_lines@lines[[1]]@ID)
cluny_rns_spolys = sp::SpatialPolygons(list(cluny_rns_polys), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
cluny = polylist2df(list(cluny, sp::SpatialPolygonsDataFrame(cluny_rns_spolys, data=cluny@data, match.ID=FALSE)))

maximin = get_osm_data_church(130385489, what="way")
maximin@data$surface = geosphere::areaPolygon(maximin)
maximin@data$lon = sp::coordinates(maximin)[, 1]
maximin@data$lat = sp::coordinates(maximin)[, 2]
maximin@data$city = "Trier"
maximin@data[setdiff(baseinfo, names(maximin))] = NA

additions = polylist2df(list(truiden3, smungo, durham, cluny))
additions@data$surface = geosphere::areaPolygon(additions)
additions@data$surface[additions@data$role=="inner"] = -1 * additions@data$surface[additions@data$role=="inner"]
additions@data$lon = sp::coordinates(additions)[, 1]
additions@data$lat = sp::coordinates(additions)[, 2]
additions = aggregate_multipolys(additions)
additions@data$city = c("Cluny", "Gent (Gand, Ghent)", "Durham", "Glasgow")
additions@data$ctr = c('fr', 'be', 'uk', 'uk')
additions@data[setdiff(baseinfo, names(additions))] = NA

write.csv(fill, file="dat/checkfile.csv", row.names=F, na='')
write_filltable(additions, baseinfo=baseinfo, outfile='dat/additions.csv')
write_filltable(maximin, baseinfo=baseinfo, outfile='dat/maximin.csv')

# hgt v m2
plot(hgt ~ m2, data=dynobs[m2 > 0 & hgt > 0, ])
curve(sqrt(x/ 5), add=T, col=2)

sum(table(statobs$ctr))
all_by_ctr = data.frame(table(statobs$ctr))

sum(table(statobs[osmid %in% dynobs$osmid, ctr]))
all_by_ctr_b1500 = data.frame(table(statobs[osmid %in% dynobs$osmid, ctr]))

sum(table(statobs[match(dynobs$osmid, osmid), ctr]))
all_bldphsase_by_ctr = data.frame(table(statobs[match(dynobs$osmid, osmid), ctr]))

write.csv(file="dat/nobs.csv",
    x= data.frame(ctr=all_by_ctr$Var1,
                  all_churches=all_by_ctr$Freq,
                  all_churches_b1500=all_by_ctr_b1500$Freq,
                  all_bld_phases=all_bldphsase_by_ctr$Freq))


dupls = find_duplicated_ids(unique(dynobs$osmid))

dpls = merge(statobs, unique(siem[, list(city, lat, lon)]), by='city', all.x=T, suffixes=c('_city', '_church'))
dpls = dpls[match(dupls, osmid), ]
dpls$osmid_original = gsub('_.*', '', dpls$osmid)
dpls[, lat_city:=as.numeric(lat_city)]
dpls[, lon_city:=as.numeric(lon_city)]
dpls[, lat_church:=as.numeric(lat_church)]
dpls[, lon_church:=as.numeric(lon_church)]
dpls = dpls[complete.cases(dpls[, grepr('lat|lon', names(dpls)), with=F]), ]  
dpls[, distance:=sp::spDists(x=cbind(lon_church, lat_church),  
                          y=cbind(lon_city, lat_city), 
                          diagonal=T, longlat=T)]
dpls[, mindist:=which.min(distance) == 1:length(distance),  by=osmid_original]

# not a huge issue so far...
"64400991" # should be nulled
"64451862" # should be nulled
"103890401_40" # should be nulled
# etc.

fullobs_sp = merge(fullobs, statobs, by='osmid', all=T)
fullobs_sp[, decade:=trunc(year / 10) * 10]

pdf('figs/building_byctr_dec.pdf', paper='a4')
par(mfrow=c(4, 2), mar=c(4.5, 3.5, 01.5, 0.5), font.main=1)
plot(im2_ann ~ decade,
    data=fullobs_sp[ctr=="fr" & lat > 46, list(im2_ann=sum(im2_ann, na.rm=T)), by=list(decade)],
    xlim=c(800, 1500),
    type='l', bty='l', col=2)
title(main="France (North)", line=-0.6)
plot(im2_ann ~ decade,
    data=fullobs_sp[ctr=="fr" & lat <= 46, list(im2_ann=sum(im2_ann, na.rm=T)), by=list(decade)],
    xlim=c(800, 1500),
    type='l', bty='l', col=2)
title(main="France (South)", line=-0.6)
plot(im2_ann ~ decade,
    data=fullobs_sp[ctr=="uk" & lat > 53, list(im2_ann=sum(im2_ann, na.rm=T)), by=list(decade)],
    xlim=c(800, 1500),
    type='l', bty='l', col=2)
title(main="Britain (North)", line=-0.6)
plot(im2_ann ~ decade,
    data=fullobs_sp[ctr=="uk" & lat <= 53, list(im2_ann=sum(im2_ann, na.rm=T)), by=list(decade)],
    xlim=c(800, 1500),
    type='l', bty='l', col=2)
title(main="Britain (South)", line=-0.6)
plot(im2_ann ~ decade,
    data=fullobs_sp[ctr=="ch", list(im2_ann=sum(im2_ann, na.rm=T)), by=list(decade)],
    xlim=c(800, 1500),
    type='l', bty='l', col=2)
title(main="Switzerland", line=-0.6)
plot(im2_ann ~ decade,
    data=fullobs_sp[ctr=="be", list(im2_ann=sum(im2_ann, na.rm=T)), by=list(decade)],
    xlim=c(800, 1500),
    type='l', bty='l', col=2)
title(main="Belgium", line=-0.6)
plot(im2_ann ~ decade,
    data=fullobs_sp[ctr=="nl", list(im2_ann=sum(im2_ann, na.rm=T)), by=list(decade)],
    xlim=c(800, 1500),
    type='l', bty='l', col=2)
title(main="Netherlands", line=-0.6)
plot(im2_ann ~ decade,
    data=fullobs_sp[ctr=="de", list(im2_ann=sum(im2_ann, na.rm=T)), by=list(decade)],
    xlim=c(800, 1500),
    type='l', bty='l', col=2)
abline(v=c(887, 1348, 1500, 919), col='gray')
title(main="Germany", line=-0.6)
# plot(im2_ann ~ decade,
#     data=fullobs_sp[, list(im2_ann=sum(im2_ann, na.rm=T)), by=list(decade)],
#     xlim=c(800, 1500),
#     type='l', bty='l', col=2)
# title(main="All", line=-0.6)
dev.off()

fullobs_sp[, lat:=as.numeric(lat)]
fullobs_sp[, lon:=as.numeric(lon)]

# check these
fullobs_sp[is.na(year), ]
fullobs_sp[is.na(lat), ]
fullobs_sp[is.na(lon), ]
fullobs_sp[is.na(as.numeric(lat)), ]

fullobs_sp[, gridx:=floor(lon)]
fullobs_sp[, gridy:=floor(lat)]
fullobs_sp[, grid:=paste0(gridx, 'x', floor(lat))]

plot(V1 ~ year, data=fullobs_sp[grid=="5x50", sum(im2_ann, na.rm=T), by=list(year)], type='l')

fullobs_sp[, sum(im2_ann, na.rm=T), by=list(year, grid)]

fullobs_sp[, im2_dec:=sum(abs(im2_ann), na.rm=T), by=list(osmid, decade)]
# fullobs_spd = fullobs_sp[, list(im2_dec=sum(abs(im2_ann), na.rm=T), lat, lon), by=list(osmid, decade)]
# fullobs_spd[, lim2_dec:=ifelse(is.na(im2_dec) | im2_dec<=0, 0, log(im2_dec))]

fill = list()
for (i in seq(from=800, to=1500, by=10)){
    d = fullobs_sp[year==i & !is.na(year), ]
    ds = raster::rasterize(x=as.matrix(d[, list(lon, lat)]), y=r, field=abs(d$im2_dec), fun=sum, na.rm=T)
    # used to say lim2
    fill[[as.character(i)]] = ds
    cat(i, '-')
}
r = raster::stack(fill)

# replace with library("maps")?
data(wrld_simpl)
eur = wrld_simpl[wrld_simpl$REGION==150, ]
eur = wrld_simpl[wrld_simpl$SUBREGION %in% c(39, 154, 155), ]
add_borders = function(){
    plot(eur, add=T)
}
animation::saveGIF(raster::animate(r, n=1, addfun=add_borders), 
    movie.name='grid.gif', clean=F,
    interval=0.5, imgdir='figs')

plot(r, breaks=seq(min(raster::minValue( r )),max(raster::maxValue(r)),length.out=100))


palsize = 100
pal = colorRampPalette(c('blue', 'red'))(palsize)
cuts <- seq(from=0, to=500, length.out=palsize)
    jpeg(paste0('figs/gif/grid', sprintf("%04d", i), '.jpg'), width=720, height=720)
    d = fullobs_sp[year==i & !is.na(year), ]
    ds = raster::rasterize(x=as.matrix(d[, list(lon, lat)]), y=r, field=d$lim2_dec, fun=sum, na.rm=T)
    print(max(ds@data@values, na.rm=T))
    polys5km <- raster::rasterToPolygons(ds)
    polys5km$col <- pal[as.numeric(cut(log1p(polys5km$layer), cuts))]
    plot(polys5km, col=polys5km$col, lwd=0.1, main=i)
    dev.off()
}