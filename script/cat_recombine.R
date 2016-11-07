rm(list=ls())
options(stringsAsFactors=FALSE)

setwd("~/dropbox/cathedrals/")
source("script/cat_functions.r")

library("data.table")
library("stringi")
library("plm")
source("dat/cathedrallist.r")


chr = data.table::fread("dat/checkedchurches_eb_2.csv", header=T, encoding="UTF-8")
# compare with "dat/checkedchurches_eb.csv" to find new height imputations
chr = chr[, 1:27, with=F]
hgt = data.table::fread("dat/heights.csv", encoding="UTF-8")
sfc = data.table::fread("dat/backproj.csv", encoding="UTF-8")
siem = data.table::fread("dat/siem_long.csv", encoding="UTF-8")
ukgdp = data.table::fread("dat/engdp12001700.csv", skip=1, encoding="UTF-8")
gdp = data.table::fread("dat/maddison_gdp_old.csv")
cathedrals = read.csv("dat/cathedral_osmids.csv", header=F)

chr[, osmwikipedia:=iconv(osmwikipedia, from='macroman', to='utf8')]
chr[, osmname:=iconv(osmname, from='macroman', to='utf8')]
chr[, city:=iconv(city, from='macroman', to='utf8')]

# check for encoding issues
chr$city[grep("√", chr$city)]
grep("xyz", chr$osmname)
grep("xyz", chr$city)
grep("xyz", chr$osmwikipedia)

chr[ ,city:=gsub("√∂", "ö", city)]
chr[ ,city:=gsub("√º", "ü", city)]
chr[ ,city:=gsub("√§", "ä", city)]
chr[ ,city:=gsub("√©", "é", city)]
chr[ ,osmname:=gsub("√©", "é", osmname)]
chr[ ,osmwikipedia:=gsub("√©", "é", osmwikipedia)]

chr$osmname[grep("√", chr$osmname)]
chr$osmwikipedia[grep("√", chr$osmwikipedia)]
chr$osmname[grep("é", chr$osmname)]

table(table(chr$osmid[chr$osmid!='']))
duplids = names(table(chr$osmid[chr$osmid!='']))[table(chr$osmid[chr$osmid!='']) > 6]
chr[osmid %in% duplids, ]

chrlist = recombine_churches(churches=chr, guesses=NULL)

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

doubles = c("Strasbourg", "Strasbourg (Strassburg)", 
    "St Omer", "St Omer (Saint-Omer (Pas-de-Calais))", 
    "Chalons-sur-Marne", "Chalons-sur-Marne (Châlons-en-Champagne)")

doubles[!doubles %in% siem$city] # only worry about !
statobs[city %in% doubles[!doubles %in% siem$city], ]
siem[city %in% doubles, list(lat[1], lon[1]), by=city]

dynobs = to_dynobs(churchlist=chrlist)
fullobs = to_annual_obs(dyn=dynobs, churchlist=chrlist)
fullobs[, decade:=(trunc((year - 1) / 20) + 1) * 20] # so 1500 = 1481-1500
citobs = to_city_obs(statobs=statobs, fullobs=fullobs)
dynobs_sp = merge(dynobs, statobs, by="osmid")
dim(fullobs)
fullobs_sp = merge(fullobs, statobs, by="osmid", all=T)
dim(fullobs_sp)
dynobs[fullobs_sp[is.na(year), osmid], ]
fullobs_sp = fullobs_sp[!is.na(year), ]
unique(table(fullobs_sp$year, useNA='ifany'))

plot(fullobs_sp[year <= 1500, sum(im2_ann, na.rm=T) + sum(im2_cml * 0.005, na.rm=T), by=decade], type='l')

# gemiddele eindgrote kerk
dynobs[, list(m2_end=sum(m2, na.rm=T)), by=paste(osmid, bldindex)]
dynobs[, list(m2_end=sum(m2[bldindex==max(bldindex)], na.rm=T)), by=osmid]

write.csv(statobs, "dat/statobs.csv", row.names=F)
write.csv(citobs, "dat/citobs.csv", row.names=F)
outfile = gzfile("dat/fullobs_sp.csv.gz", 'w')
write.csv(fullobs_sp, outfile)
close(outfile)

siem[, ctr:=tolower(siem$tld)]
siem[ctr=='gb', ctr:="uk"]

statobs
fullobs
citobs

plot(hgt ~ m2, data=dynobs[m2 >0, ])
curve(0.4542*sqrt(x), add=T, col=2)

### descriptives ###
####################

pdf('figs/phasedistr.pdf', height=6, width=8)
par(mfrow=c(2, 3), mar=c(2,2,3,1), font.main=1)
# hist(dynobs_sp$year, breaks=16)
x = dynobs_sp[, list(ctr, year)]
x[year > 1600, year:=1600]
x[year < 700, year:=700]
hist(x$year[x$ctr=='de'], breaks=9, main='')
title(main='de', line=-0.2)
hist(x$year[x$ctr=='uk'], breaks=9, main='')
title(main='uk', line=-0.2)
hist(x$year[x$ctr=='fr'], breaks=9, main='')
title(main='fr', line=-0.2)
hist(x$year[x$ctr=='ch'], breaks=9, main='')
title(main='ch', line=-0.2)
hist(x$year[x$ctr=='be'], breaks=9, main='')
title(main='be', line=-0.2)
hist(x$year[x$ctr=='nl'], breaks=9, main='')
title(main='nl', line=-0.2)
dev.off()

par(mfrow=c(2, 3), mar=c(2,2,3,1), font.main=1)
hist(x$year[x$ctr=='de'], breaks=9, main='', ylim=c(0, 300))
title(main='de', line=-0.2)
hist(x$year[x$ctr=='uk'], breaks=9, main='', ylim=c(0, 300))
title(main='uk', line=-0.2)
hist(x$year[x$ctr=='fr'], breaks=9, main='', ylim=c(0, 300))
title(main='fr', line=-0.2)
hist(x$year[x$ctr=='ch'], breaks=9, main='', ylim=c(0, 300))
title(main='ch', line=-0.2)
hist(x$year[x$ctr=='be'], breaks=9, main='', ylim=c(0, 300))
title(main='be', line=-0.2)
hist(x$year[x$ctr=='nl'], breaks=9, main='', ylim=c(0, 300))
title(main='nl', line=-0.2)

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

table(as.numeric(stringi::stri_sub(dynobs$year, -1)))
table(as.numeric(stringi::stri_sub(dynobs$year, -2)))
par(mfrow=c(1, 1))
hist(as.numeric(stringi::stri_sub(dynobs$year, -2)), breaks=100)

# siem[, im2_cnt := citobs$im2_cnt[match(paste0(siem$city, siem$year), paste0(citobs$city, citobs$year))]]
# siem[, im3_cnt := citobs$im3_cnt[match(paste0(siem$city, siem$year), paste0(citobs$city, citobs$year))]]

# beware:
unique(stringi::stri_enc_mark(siem$city))
unique(stringi::stri_enc_mark(citobs$city))

# regular churches v. cathedrals
cathedrals
cathedrals$V1[is.na(as.numeric(cathedrals$V1))]

pdf('figs/cath_v_allchurches.pdf', height=4, width=9)
par(mfrow=c(1, 3), font.main=1)
plot(fullobs[year <= 1500 & year >= 700, sum(im2_ann, na.rm=T),by=decade], type='l', bty='l', col=1, main='all churches')
plot(fullobs[year <= 1500 & year >= 700 & osmid %in% cathedrals$V1, sum(im2_ann, na.rm=T),by=decade], type='l', bty='l', col=2, main='cathedrals only')
plot(fullobs[year <= 1500 & year >= 700, sum(im2_ann, na.rm=T),by=decade], type='l', bty='l', col=1)
lines(fullobs[year <= 1500 & year >= 700 & osmid %in% cathedrals$V1, sum(im2_ann, na.rm=T),by=decade], col=2)
text(c(1410, 1410), c(25e3, 60e3), c('cathedrals', 'all churches'), col=2:1)
dev.off()

sum(fullobs[osmid %in% cathedrals$V1, im2_ann], na.rm=T) / sum(fullobs[, im2_ann], na.rm=T)

write.csv(statobs[osmid %in% cathedrals$V1, list(city, osmid, osmname, osmwikipedia)], "dat/cathedrals.csv", row.names=F)

# exploratory regs

# citobs_full_and = merge(citobs, siem, by=c('city', 'year'))
pcitobs = merge(citobs, siem, by=c('city', 'year'), all.x=T)
# setdiff(citobs_full_and[, paste(city, year)], pcitobs[, paste(city, year)])
# setdiff(pcitobs[, paste(city, year)], citobs_full_and[, paste(city, year)])
pcitobs = pcitobs[year <= 1500, ]

plot(log(inhab) ~ log(im2_cnt), data=pcitobs)
summary(lm(log1p(inhab) ~ log1p(im2_cnt), data=pcitobs))

pcitobs = pdata.frame(pcitobs, c("city", "year"))
m_base <- plm::plm(log1p(inhab) ~ lag(log1p(im2_cnt)), data=pcitobs)
m_lag <- plm::plm(log1p(inhab) ~ lag(log1p(im2_cnt)) + lag(log1p(inhab)), data=pcitobs)
m_yd <- plm::plm(log1p(inhab) ~ lag(log1p(im2_cnt)) + year, data=pcitobs)
m_trend <- plm::plm(log1p(inhab) ~ lag(log1p(im2_cnt)) + as.numeric(year), data=pcitobs)

texreg::screenreg(list(m_base, m_trend, m_lag, m_yd))

pdf("figs/inhab_v_churchm2.pdf")
plot(log1p(inhab) ~ lag(log1p(im2_cnt)), data=pcitobs)
abline(plm::plm(log1p(inhab) ~ lag(log1p(im2_cnt)), data=pcitobs), col=2)
abline(lm(log1p(inhab) ~ lag(log1p(im2_cnt)), data=pcitobs), col=2)
text(x=c(1, 8), y=c(1, 0.4), c('pooled', 'within'), col=2)
dev.off()

# m2 per 20y period en country
pdf('figs/europetotal.pdf', height=6)
plot(fullobs[decade <= 1500, sum(im2_ann, na.rm=T), by=decade], type='n', bty='l')
abline(v=c(768, 1315, 1000, 1348), col='gray')
lines(fullobs[decade <= 1500, sum(im2_ann, na.rm=T), by=decade], type='b', col=2, lwd=1.5, pch=1, cex=0.9)
dev.off()

x = fullobs_sp[, list(im2_dec=sum(im2_ann, na.rm=T), im3_dec=sum(im3_ann, na.rm=T)), by=list(decade, ctr)]

pdf("figs/m2bycountry.pdf", height=9)
par(mfrow=c(3, 2))
plot(im2_dec ~ decade, data=x[ctr=="de" & decade <=1500, ], type='l', col=2, lwd=1.5, bty='l', xlim=c(700, 1500), main="de")
points(x=c(1, 1000, 1500), y=gdp[V1 <= 1500, Germany / Germany[V1==1500]] * x[ctr=="de" & decade==1500, im2_dec])
# lines(x=c(1, 1000, 1500), y=gdp[V1 <= 1500, Germany / Germany[V1==1000]] * x[ctr=="de" & decade==1000, im2_dec])

plot(im2_dec ~ decade, data=x[ctr=="uk" & decade <=1500, ], type='l', col=2, lwd=1.5, bty='l', xlim=c(700, 1500), main="uk")
points(x=c(1, 1000, 1500), y=gdp[V1 <= 1500, UK / UK[V1==1500]] * x[ctr=="uk" & decade==1500, im2_dec])
# lines(x=c(1, 1000, 1500), y=gdp[V1 <= 1500, UK / UK[V1==1000]] * x[ctr=="uk" & decade==1000, im2_dec])

plot(im2_dec ~ decade, data=x[ctr=="fr" & decade <=1500, ], type='l', col=2, lwd=1.5, bty='l', xlim=c(700, 1500), main="fr")
points(x=c(1, 1000, 1500), y=gdp[V1 <= 1500, France / France[V1==1500]] * x[ctr=="fr" & decade==1500, im2_dec])
# lines(x=c(1, 1000, 1500), y=gdp[V1 <= 1500, France / France[V1==1000]] * x[ctr=="fr" & decade==1000, im2_dec])

plot(im2_dec ~ decade, data=x[ctr=="be" & decade <=1500, ], type='l', col=2, lwd=1.5, bty='l', xlim=c(700, 1500), main="be")
points(x=c(1, 1000, 1500), y=gdp[V1 <= 1500, Belgium / Belgium[V1==1500]] * x[ctr=="be" & decade==1500, im2_dec])
# lines(x=c(1, 1000, 1500), y=gdp[V1 <= 1500, Belgium / Belgium[V1==1000]] * x[ctr=="be" & decade==1000, im2_dec])

plot(im2_dec ~ decade, data=x[ctr=="nl" & decade <=1500, ], type='l', col=2, lwd=1.5, bty='l', xlim=c(700, 1500), main="nl")
points(x=c(1, 1000, 1500), y=gdp[V1 <= 1500, Netherlands / Netherlands[V1==1500]] * x[ctr=="nl" & decade==1500, im2_dec])
# lines(x=c(1, 1000, 1500), y=gdp[V1 <= 1500, Netherlands / Netherlands[V1==1000]] * x[ctr=="nl" & decade==1000, im2_dec])

plot(im2_dec ~ decade, data=x[ctr=="ch" & decade <=1500, ], type='l', col=2, lwd=1.5, bty='l', xlim=c(700, 1500), main="ch")
points(x=c(1, 1000, 1500), y=gdp[V1 <= 1500, Switzerland / Switzerland[V1==1500]] * x[ctr=="ch" & decade==1500, im2_dec])
# lines(x=c(1, 1000, 1500), y=gdp[V1 <= 1500, Switzerland / Switzerland[V1==1000]] * x[ctr=="ch" & decade==1000, im2_dec])
dev.off()

# m2 per region

fullobs_sp[, ctr2:=ctr]
fullobs_sp[lat > 46 & ctr=="fr", ctr2:="fr_north"]
fullobs_sp[lat <= 46 & ctr=="fr", ctr2:="fr_south"]
fullobs_sp[lat > 53 & ctr=="uk", ctr2:="uk_north"]
fullobs_sp[lat <= 53 & ctr=="uk", ctr2:="uk_south"]
fullobs_sp[lat > 50.5 & ctr=="de", ctr2:="de_north"]
fullobs_sp[lat <= 50.5 & ctr=="de", ctr2:="de_south"]

x = merge(fullobs_sp, siem[, list(inhab, lat, lon, city, year)], by=c('city', 'year'), all.x=T)
x = x[!is.na(decade) & decade <= 1500, ]
x1 = x[, list(im2_dec=sum(im2_ann, na.rm=T), im3_dec=sum(im3_ann, na.rm=T), inhab=sum(inhab, na.rm=T)), by=list(decade, ctr)]
x2 = x[, list(im2_dec=sum(im2_ann, na.rm=T), im3_dec=sum(im3_ann, na.rm=T), inhab=sum(inhab, na.rm=T)), by=list(decade, ctr2)]
x1[decade %% 100!=0, inhab:=NA]
x2[decade %% 100!=0, inhab:=NA]
x1[, iinhab:=exp(zoo::na.approx(log(inhab))), by=ctr]
x2[, iinhab:=exp(zoo::na.approx(log(inhab))), by=ctr2]

x1[, im2_dec_smth:=predict(loess(im2_dec / iinhab ~ decade), newdata=decade), by=ctr]
x2[, im2_dec_smth:=predict(loess(im2_dec / iinhab ~ decade), newdata=decade), by=ctr2]

pdf('figs/france_panel.pdf', height=4, width=9)
par(mfrow=c(1, 3), font.main=1)
yl = range(x1[ctr=='fr', im2_dec])
plot(im2_dec ~ decade, data=x1[ctr=='fr', ], type='l', ylim=yl, bty='l', ylab='m2/dec', main='Fr., total')
plot(im2_dec ~ decade, data=x2[ctr2=='fr_south', ], type='l', ylim=yl, bty='l', ylab='m2/dec', main='Fr., south')
lines(im2_dec ~ decade, data=x1[ctr=='fr', ], col='gray70')
plot(im2_dec ~ decade, data=x2[ctr2=='fr_north', ], type='l', ylim=yl, bty='l', ylab='m2/dec', main='Fr., north')
lines(im2_dec ~ decade, data=x1[ctr=='fr', ], col='gray70')
dev.off()

pdf('figs/germany_panel.pdf', height=4, width=9)
par(mfrow=c(1, 3), font.main=1)
yl = range(x1[ctr=='de', im2_dec])
plot(im2_dec ~ decade, data=x1[ctr=='de', ], type='l', ylim=yl, bty='l', ylab='m2/dec', main='de., total')
plot(im2_dec ~ decade, data=x2[ctr2=='de_south', ], type='l', ylim=yl, bty='l', ylab='m2/dec', main='de., south')
lines(im2_dec ~ decade, data=x1[ctr=='de', ], col='gray70')
plot(im2_dec ~ decade, data=x2[ctr2=='de_north', ], type='l', ylim=yl, bty='l', ylab='m2/dec', main='de., north')
lines(im2_dec ~ decade, data=x1[ctr=='de', ], col='gray70')
dev.off()

pdf('figs/britain_panel.pdf', height=4, width=9)
par(mfrow=c(1, 3), font.main=1)
yl = range(x1[ctr=='uk', im2_dec])
plot(im2_dec ~ decade, data=x1[ctr=='uk', ], type='l', ylim=yl, bty='l', ylab='m2/dec', main='uk., total')
plot(im2_dec ~ decade, data=x2[ctr2=='uk_south', ], type='l', ylim=yl, bty='l', ylab='m2/dec', main='uk., south')
lines(im2_dec ~ decade, data=x1[ctr=='uk', ], col='gray70')
plot(im2_dec ~ decade, data=x2[ctr2=='uk_north', ], type='l', ylim=yl, bty='l', ylab='m2/dec', main='uk., north')
lines(im2_dec ~ decade, data=x1[ctr=='uk', ], col='gray70')
dev.off()

pdf('figs/smallctr_panel.pdf', height=4, width=9)
par(mfrow=c(1, 3), font.main=1)
yl = range(x1[ctr %in% c('nl', 'ch', 'be'), im2_dec])
plot(im2_dec ~ decade, data=x1[ctr=='ch', ], type='l', ylim=yl, bty='l', ylab='m2/dec', main='ch')
lines(im2_dec ~ decade, data=x1[ctr=='be', ], col='gray70')
plot(im2_dec ~ decade, data=x2[ctr2=='be', ], type='l', ylim=yl, bty='l', ylab='m2/dec', main='be')
plot(im2_dec ~ decade, data=x2[ctr2=='nl', ], type='l', ylim=yl, bty='l', ylab='m2/dec', main='nl')
lines(im2_dec ~ decade, data=x1[ctr=='be', ], col='gray70')
dev.off()

pdf('figs/francetrends.pdf', height=6)
par(mfrow=c(1, 1), font.main=1)
yl = range(x1[ctr=='fr', im2_dec])
plot(im2_dec ~ decade, data=x1[ctr=='fr', ], type='l', ylim=yl, bty='l', ylab='m2/dec', main='France')
lines(im2_dec ~ decade, data=x2[ctr2=='fr_south', ], col=uured)
lines(im2_dec ~ decade, data=x2[ctr2=='fr_north', ], col=uublu)
text(c(1450, 1450, 1470), c(5000, 35000, 19000), c('South', "All", "North"), col=c(uured, 1, uublu), cex=0.8)
# legend('topleft', legend=c("All", "North", "South"), fill=c(1, uublu, uured))
dev.off()

pdf('figs/britaintrends.pdf', height=6)
par(mfrow=c(1, 1), font.main=1)
yl = range(x1[ctr=='uk', im2_dec])
plot(im2_dec ~ decade, data=x1[ctr=='uk', ], type='l', ylim=yl, bty='l', ylab='m2/dec', main='UK')
lines(im2_dec ~ decade, data=x2[ctr2=='uk_south', ], col=uured)
lines(im2_dec ~ decade, data=x2[ctr2=='uk_north', ], col=uublu)
text(c(1420 , 1450, 1420), c(5300, 9000, 20e2), c('South', "All", "North"), col=c(uured, 1, uublu), cex=0.8)
# legend('topleft', legend=c("All", "North", "South"), fill=c(1, uublu, uured))
dev.off()

pdf('figs/germanytrends.pdf', height=6)
par(mfrow=c(1, 1), font.main=1)
yl = range(x1[ctr=='de', im2_dec])
plot(im2_dec ~ decade, data=x1[ctr=='de', ], type='l', ylim=yl, bty='l', ylab='m2/dec', main='Germany')
lines(im2_dec ~ decade, data=x2[ctr2=='de_south', ], col=uured)
lines(im2_dec ~ decade, data=x2[ctr2=='de_north', ], col=uublu)
text(c(1450, 1450, 1470), c(5000, 35000, 19000), c('South', "All", "North"), col=c(uured, 1, uublu), cex=0.8)
# legend('topleft', legend=c("All", "North", "South"), fill=c(1, uublu, uured))
dev.off()

pdf('figs/benlchtrends.pdf', height=6)
par(mfrow=c(1, 1), font.main=1)
yl = range(x1[ctr=='uk', im2_dec])
plot(im2_dec ~ decade, data=x1[ctr=='ch', ], type='l', ylim=yl, bty='l', ylab='m2/dec', main='Small ctr.')
lines(im2_dec ~ decade, data=x1[ctr=='be', ], col=uured)
lines(im2_dec ~ decade, data=x1[ctr=='nl', ], col=uublu)
text(c(1450, 1450, 1420), c(8e3, 2.2e3, 14e3), c('Be', "Ch", "Nl"), col=c(uured, 1, uublu), cex=0.8)
# legend('topleft', legend=c("Be", "Ch", "Nl"), fill=c(1, uublu, uured))
dev.off()

# gdp comparisons

x = fullobs_sp[, list(im2_dec=sum(im2_ann, na.rm=T), im3_dec=sum(im3_ann, na.rm=T)), by=list(decade, ctr)]
trends = matrix(NA, nrow=6, ncol=2)
rownames(trends) = c("de", "uk", "fr", "be", "nl", "ch")
colnames(trends) = c("m2", "gdp")

trends[1, 1] = lm(im2_dec ~ decade, data=x[ctr=="de" & decade <=1500, ])$coef["decade"]
trends[2, 1] = lm(im2_dec ~ decade, data=x[ctr=="uk" & decade <=1500, ])$coef["decade"]
trends[3, 1] = lm(im2_dec ~ decade, data=x[ctr=="fr" & decade <=1500, ])$coef["decade"]
trends[4, 1] = lm(im2_dec ~ decade, data=x[ctr=="be" & decade <=1500, ])$coef["decade"]
trends[5, 1] = lm(im2_dec ~ decade, data=x[ctr=="nl" & decade <=1500, ])$coef["decade"]
trends[6, 1] = lm(im2_dec ~ decade, data=x[ctr=="ch" & decade <=1500, ])$coef["decade"]

trends[1, 2] = lm(Germany ~ V1, data=gdp[V1 > 1 & V1 < 1600, ])$coef["V1"]
trends[2, 2] = lm(UK ~ V1, data=gdp[V1 > 1 & V1 < 1600, ])$coef["V1"]
trends[3, 2] = lm(France ~ V1, data=gdp[V1 > 1 & V1 < 1600, ])$coef["V1"]
trends[4, 2] = lm(Belgium ~ V1, data=gdp[V1 > 1 & V1 < 1600, ])$coef["V1"]
trends[5, 2] = lm(Netherlands ~ V1, data=gdp[V1 > 1 & V1 < 1600, ])$coef["V1"]
trends[6, 2] = lm(Switzerland ~ V1, data=gdp[V1 > 1 & V1 < 1600, ])$coef["V1"]

plot(trends, type='n', log='xy', bty='l', xlab="m2/decade", ylab="GDP/decade")
text(trends[,'m2'], trends[,'gdp'], rownames(trends), col=2)