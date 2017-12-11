rm(list = ls())
setwd("~/dropbox/cathedrals")
source("script/cat_functions.r")

library("data.table")

M = 9 # number of imputations

dynobs = data.table::fread("dat/dynobs.csv")
statobs = data.table::fread("dat/statobs.csv")
dynobs_sp = merge(dynobs, statobs, by="osmid")

citobs = data.table::fread("dat/citobs.csv")
fullobs_sp = data.table::fread("gunzip -c  dat/fullobs_sp.csv.gz")

impvrbs = grepr('im3_ann_cmc\\d', names(fullobs_sp))

im3crc = fullobs_sp[, .SD, .SDcols = grep('im3_ann\\d', names(fullobs_sp))] +
    fullobs_sp[, .SD * 0.005, .SDcols = grep('im3_cml\\d', names(fullobs_sp))]
im2crc = fullobs_sp[, .SD, .SDcols = grep('im2_ann\\d', names(fullobs_sp))] +
    fullobs_sp[, .SD * 0.005, .SDcols = grep('im2_cml\\d', names(fullobs_sp))]
fullobs_sp[, paste0("im3_ann_cmc", 1:M) := im3crc]
fullobs_sp[, paste0("im2_ann_cmc", 1:M) := im2crc]
fullobs_sp[, im3_ann_cmc := im3_ann + im3_cml * 0.005]
fullobs_sp[, im2_ann_cmc := im2_ann + im2_cml * 0.005]

fullobs = fullobs_sp[, -names(statobs)[-1], with = F]

ukgdp = data.table::fread("dat/engdp12001700.csv", skip=1, encoding="UTF-8")
gdp = data.table::fread("dat/maddison_gdp_old.csv")
siem = data.table::fread("dat/siem_long.csv", encoding="UTF-8")

siem[, ctr:=tolower(siem$tld)]
siem[ctr=='gb', ctr:="uk"]

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

# total diffs
sum(fullobs[data.table::between(year, 700, 1500), sum(im3_ann, na.rm=T), by=year])
sum(fullobs[data.table::between(year, 700, 1500), sum(.SD, na.rm=T) / M, by=year, .SDcols=grep("im3_ann\\d", names(fullobs))])

# average end-size church
dynobs[, list(m3_end=sum(m3, na.rm=T)), by=paste(osmid, bldindex)][, mean(m3_end)]
dynobs[, list(m3_end=sum(m3[bldindex==max(bldindex)], na.rm=T)), by=osmid][, mean(m3_end)]

# average size church by year
plot(fullobs[, mean(im3_cml), by = year], type= 'l')
abline(v = 1500)
plot(fullobs[im3_ann == 0, mean(im3_cml), by = year][order(year)], type= 'l')
abline(v = 1500)

# distribution observations
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

# sumstats
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
ss_out = ss_out[order(-as.numeric(ss_out$city)), ]
ss_out = rbind(ss_out, c('all', colSums(ss_out[, -1])))

write.csv(ss_out, 'tab/sumstats.csv', row.names=F)

# height imputation
plot(hgt ~ m2, data=dynobs[m2 >0, ])
curve(0.4542*sqrt(x), add=T, col=2)


pdf('figs/cath_v_allchurches_hc.pdf', height=4, width=9)
par(mfrow=c(1, 3), font.main=1)
plot(fullobs_sp[category=="cathedral" & data.table::between(year, 700, 1500), base::sum(.SD, na.rm=T) / M, by=decade, .SDcols=impvrbs],
    type='l', ylab = 'm3/ann')
plot(fullobs_sp[category!="cathedral" & data.table::between(year, 700, 1500), base::sum(.SD, na.rm=T) / M, by=decade, .SDcols=impvrbs],
    type='l', ylab = 'm3/ann')
plot(fullobs_sp[category!="cathedral" & data.table::between(year, 700, 1500), base::sum(.SD, na.rm=T) / M, by=decade, .SDcols=impvrbs],
    type='l', ylab = 'm3/ann')
lines(fullobs_sp[category=="cathedral" & data.table::between(year, 700, 1500), base::sum(.SD, na.rm=T) / M, by=decade, .SDcols=impvrbs],
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
# cmladd = fullobs_sp[data.table::between(year, 700, 1500), base::sum(.SD * 0.005, na.rm=T) / M, by=list(decade, category), .SDcols = grepr('im3_cml\\d', names(fullobs_sp))]
# bytype[, V1 := V1 + cmladd$V1]
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
# cmladd = fullobs_sp[data.table::between(year, 700, 1500), base::sum(.SD * 0.005, na.rm=T) / M, by=list(decade, category, mendicants), .SDcols = grepr('im3_cml\\d', names(fullobs_sp))]
# bytype = bytype[, V1 := V1 + cmladd$V1][category == "conventual"]
par(mfrow = c(1, 2))
plot(V1 ~ decade, data=bytype[category=="conventual" & mendicants == 0, ],
    type='n', bty='l', col=2, lwd=1.5, ylab = 'm3/ann', main = 'Other')
lines(V1 ~ decade, data=bytype[category=="conventual" & mendicants == 1, ],
    type='l', bty='l', col=2, lwd=1.5, ylab = 'm3/ann', main = 'Mendicant')
plot(V1 ~ decade, data=bytype[category=="conventual" & mendicants == 0, ],
    type='l', bty='l', col=2, lwd=1.5, ylab = 'm3/ann', main = 'Other')
dev.off()

pdf("figs/bytype.pdf")
bytype = fullobs_sp[data.table::between(year, 700, 1500), sum(im3_ann, na.rm=T) + sum(im3_cml * 0.005, na.rm=T), by=list(decade, category)]
par(mfrow=c(2, 2), mar=c(4,3,1.5,0.5), font.main=1)
plot(V1 ~ decade, data=bytype[category=="cathedral", ], type='l', ylim=range(bytype$V1), bty='l', col=2, lwd=1.5, ylab = ' m3/ann')
    lines(V1 ~ decade, data=bytype[category=="parish", ], type='l', ylim=range(bytype$V1), col='lightgray')
    lines(V1 ~ decade, data=bytype[category=="other", ], type='l', ylim=range(bytype$V1), col='lightgray')
    lines(V1 ~ decade, data=bytype[category=="conventual", ], type='l', ylim=range(bytype$V1), col='lightgray')
title(main="cathedral", line=-1)
plot(V1 ~ decade, data=bytype[category=="parish", ], type='l', ylim=range(bytype$V1), bty='l', col=2, lwd=1.5, ylab = ' m3/ann')
    lines(V1 ~ decade, data=bytype[category=="cathedral", ], type='l', ylim=range(bytype$V1), col='lightgray')
    lines(V1 ~ decade, data=bytype[category=="other", ], type='l', ylim=range(bytype$V1), col='lightgray')
    lines(V1 ~ decade, data=bytype[category=="conventual", ], type='l', ylim=range(bytype$V1), col='lightgray')
title(main="parish", line=-1)
plot(V1 ~ decade, data=bytype[category=="other", ], type='l', ylim=range(bytype$V1), bty='l', col=2, lwd=1.5, ylab = ' m3/ann')
    lines(V1 ~ decade, data=bytype[category=="parish", ], type='l', ylim=range(bytype$V1), col='lightgray')
    lines(V1 ~ decade, data=bytype[category=="cathedral", ], type='l', ylim=range(bytype$V1), col='lightgray')
    lines(V1 ~ decade, data=bytype[category=="conventual", ], type='l', ylim=range(bytype$V1), col='lightgray')
title(main="other", line=-1)
plot(V1 ~ decade, data=bytype[category=="conventual", ], type='l', ylim=range(bytype$V1), bty='l', col=2, lwd=1.5, ylab = ' m3/ann')
    lines(V1 ~ decade, data=bytype[category=="parish", ], type='l', ylim=range(bytype$V1), col='lightgray')
    lines(V1 ~ decade, data=bytype[category=="cathedral", ], type='l', ylim=range(bytype$V1), col='lightgray')
    lines(V1 ~ decade, data=bytype[category=="other", ], type='l', ylim=range(bytype$V1), col='lightgray')
title(main="conventual", line=-1)
dev.off()

bytype[, dtotal := sum(V1), by=decade]
bytype[category=="other", list(decade, category, V1 / dtotal)]


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


x = fullobs_sp
x[, year := round(year / 100)  * 100] # for compatability w. century
x = x[data.table::between(year, 650, 1550), list(im3_cnt = base::sum(abs(.SD), na.rm=T) / M), by=list(city, year), .SDcols=impvrbs]

pcitobs = merge(x, siem, by=c('city', 'year'), all.x=T)

pdf("figs/geography_hc.pdf", height=3, width=8)
par(mfrow=c(1, 3), font.main=1)
# why mean? already total by city, now take mean for kind of city per
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

x = pcitobs[data.table::between(year, 650, 1550), list(im3_cnt = mean(im3_cnt)), by=list(year, rivercanal, coastal, landlocked)]
# why mean? already total by city, now take mean for kind of city per
pdf("figs/geography.pdf", height=3, width=8)
par(mfrow=c(1, 3), font.main=1)
plot(x[year <=1500 & rivercanal==1, mean(im3_cnt), by=year],
    type='n', col='lightgray', ylab='mean m3 per city')
lines(V1 ~ year, data=x[rivercanal==1, mean(im3_cnt), by=year], type='b', col='lightgray')
lines(V1 ~ year, data=x[landlocked==1, mean(im3_cnt), by=year], type='b', col='lightgray')
lines(V1 ~ year, data=x[coastal==1, mean(im3_cnt), by=year], type='b', col='red')
title(main='Coastal')
plot(x[year <=1500 & rivercanal==1, mean(im3_cnt), by=year],
    type='n', col='lightgray', ylab='mean m3 per city')
lines(V1 ~ year, data=x[coastal==1, mean(im3_cnt), by=year], type='b', col='lightgray')
lines(V1 ~ year, data=x[landlocked==1, mean(im3_cnt), by=year], type='b', col='lightgray')
lines(V1 ~ year, data=x[rivercanal==1, mean(im3_cnt), by=year], type='b', col='red')
title(main='River/canal')
plot(x[year <=1500 & rivercanal==1, mean(im3_cnt), by=year],
    type='n', col='lightgray', ylab='mean m3 per city')
lines(V1 ~ year, data=x[rivercanal==1, mean(im3_cnt), by=year], type='b', col='lightgray')
lines(V1 ~ year, data=x[coastal==1, mean(im3_cnt), by=year], type='b', col='lightgray')
lines(V1 ~ year, data=x[landlocked==1, mean(im3_cnt), by=year], type='b', col='red')
title(main='Landlocked')
dev.off()



# exploratory regs
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
