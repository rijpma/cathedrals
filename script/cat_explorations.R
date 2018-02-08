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
fullobs = fullobs_sp[, -names(statobs)[-1], with = F]

impvrbs = grepr('im3_ann_cmc\\d', names(fullobs_sp))

ukgdp = data.table::fread("dat/engdp12001700.csv", skip=1, encoding="UTF-8")
gdp = data.table::fread("dat/maddison_gdp_old.csv")
siem = data.table::fread("dat/siem_long.csv", encoding="UTF-8")

siem[, ctr:=tolower(siem$tld)]
siem[ctr=='gb', ctr:="uk"]

# heaping and its correction
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

# distribution observations
pdf('figs/phasedistr_hc.pdf', height=6, width=8)
par(mfrow=c(2, 3), mar=c(4,4,3,1), font.main=1)
x = dynobs_sp[, grep('ctr$|year_crc', names(dynobs_sp)), with=F]
for (country in unique(x$ctr)){
    histmat = as.matrix(x[ctr == country, grep("year", names(x)), with=F])
    histmat[histmat < 700] = 700
    histmat[histmat > 1600] = 1600
    hi = hist(histmat, breaks=M, plot=F)
    hi$counts = hi$counts / M
    plot(hi, main='', xlab = 'year')
    title(main=country, line = -0.1)
}
dev.off()

pdf('figs/phasedistr.pdf', height=6, width=8)
par(mfrow=c(2, 3), mar=c(2,2,3,1), font.main=1)
x = dynobs_sp[, list(ctr, year)]
x[year > 1600, year:=1600]
x[year < 700, year:=700]
for (country in unique(x$ctr)){
    hist(x[ctr == country, year], breaks = 9, main = '')
    title(main = country, line = -0.1)
}
dev.off()

# sumstats
siem_cities = aggregate(city ~ ctr, data=siem[year==1500 & ctr != 'it', ], length)
church_cities = aggregate(city ~ ctr, data=unique(statobs[, list(city, ctr)]), length)
phases_pre1000 = aggregate(city ~ ctr, data=dynobs_sp[year < 1000, ], length)
phases_pre1200 = aggregate(city ~ ctr, data=dynobs_sp[year < 1200, ], length)
phases_all = aggregate(city ~ ctr, data=dynobs_sp, length)
churches_by_ctr = aggregate(osmid ~ ctr, data=statobs, length)


ss_out = data.frame(siem_cities, 
    cities_w_church=church_cities[, -1],
    n_churches=churches_by_ctr[, -1], n_phases=phases_all[,-1],
    n_phases_pre1000=phases_pre1000[,-1], n_phases_pre1200=phases_pre1200[, -1])

ss_out = ss_out[order(-as.numeric(ss_out$city)), ]
ss_out = rbind(ss_out, c('all', colSums(ss_out[, -1])))

write.csv(ss_out, 'tab/sumstats.csv', row.names=F)

byctr = Reduce(merge, list(
    siem[year == 1500 & ctr != 'it', .N, by = ctr],
    unique(statobs[, list(city, 1)])[siem[year == 1500 & ctr != 'it'], on = c("city")][, list(`% cities w. large church` = mean(!is.na(V2))), by = ctr],
    unique(statobs[, list(city, osmid)])[siem[year == 1500 & ctr != 'it'], on = c("city"), allow.cartesian = T][, list(nchurch = sum(!is.na(osmid))), by = list(city, ctr)][, list(`Large churches/city` = mean(nchurch)), by = ctr],
    dynobs_sp[, .N, by = list(osmid, ctr)][, list(`Building phases / church` = mean(N)), by = ctr],
    dynobs_sp[, list(`% phases < 1000` = mean(year < 1000)), by = ctr],
    dynobs_sp[, list(`% phases < 1200` = mean(year < 1200)), by = ctr]))
tot = Reduce(merge, list(
    siem[year == 1500 & ctr != 'it', .N],
    unique(statobs[, list(city, 1)])[siem[year == 1500 & ctr != 'it'], on = c("city")][, list(`% cities w. large church` = mean(!is.na(V2)))],
    unique(statobs[, list(city, osmid)])[siem[year == 1500 & ctr != 'it'], on = c("city"), allow.cartesian = T][, list(nchurch = sum(!is.na(osmid))), by = list(city, ctr)][, list(`Large churches/city` = mean(nchurch))],
    dynobs_sp[, .N, by = list(osmid, ctr)][, list(`Building phases / church` = mean(N))],
    dynobs_sp[, list(`% phases < 1000` = mean(year < 1000))],
    dynobs_sp[, list(`% phases < 1200` = mean(year < 1200))]))
out = rbind(byctr, tot, fill = T)
# out[, grep('\\%', names(out)) := round(.SD * 100), .SDcols = grep('\\%', names(out))]
out[, grep('\\%', names(out)) := lapply(.SD, function(x) as.integer(round(x * 100))), .SDcols = grep('\\%', names(out))]
# out[, grep('\\%', names(out), invert = T) := round(.SD, 1), .SDcols = grep('\\%', names(out), invert = T)]
write.csv(format(out[order(-N)], digits = 1, nsmall = 1), "tab/sumstats_perc.csv", row.names = F)

# catheral building v. other churches
pdf('figs/cath_v_allchurches_hc.pdf', height=4, width=9)
par(mfrow=c(1, 3), font.main=1)
plot(fullobs_sp[category=="cathedral" & data.table::between(year, 700, 1500), base::sum(.SD, na.rm=T) / M, by=decade, .SDcols=impvrbs],
    type='l', ylab = 'm3/ann', main = 'cathedrals')
plot(fullobs_sp[category!="cathedral" & data.table::between(year, 700, 1500), base::sum(.SD, na.rm=T) / M, by=decade, .SDcols=impvrbs],
    type='l', ylab = 'm3/ann', main = 'other churches')
plot(fullobs_sp[category!="cathedral" & data.table::between(year, 700, 1500), base::sum(.SD, na.rm=T) / M, by=decade, .SDcols=impvrbs],
    type='l', ylab = 'm3/ann')
lines(fullobs_sp[category=="cathedral" & data.table::between(year, 700, 1500), base::sum(.SD, na.rm=T) / M, by=decade, .SDcols=impvrbs],
    type='l', col=2)
text(c(1200, 1000), c(5e5, 20e5), c('cathedrals', 'other churches'), col=2:1)
dev.off()

pdf('figs/cath_v_allchurches.pdf', height=4, width=9)
par(mfrow=c(1, 3), font.main=1)
plot(fullobs_sp[category=="cathedral" & data.table::between(year, 700, 1500), sum(im3_ann, na.rm=T), by=decade],
    type='l', ylab = 'm3/ann', main = 'cathedrals')
plot(fullobs_sp[category!="cathedral" & data.table::between(year, 700, 1500), sum(im3_ann, na.rm=T), by=decade],
    type='l', ylab = 'm3/ann', main = 'other churches')
plot(fullobs_sp[category!="cathedral" & data.table::between(year, 700, 1500), sum(im3_ann, na.rm=T), by=decade],
    type='l', ylab = 'm3/ann')
lines(fullobs_sp[category=="cathedral" & data.table::between(year, 700, 1500), sum(im3_ann, na.rm=T), by=decade],
    type='l', col=2)
text(c(1200, 1050), c(4e5, 11.5e5), c('cathedrals', 'other churches'), col=2:1)
dev.off()

pdf("figs/bytype_hc.pdf", width = 8)
bytype = fullobs_sp[data.table::between(year, 700, 1500), base::sum(.SD, na.rm=T) / M, by=list(decade, category), .SDcols = impvrbs]
# cmladd = fullobs_sp[data.table::between(year, 700, 1500), base::sum(.SD * 0.005, na.rm=T) / M, by=list(decade, category), .SDcols = grepr('im3_cml\\d', names(fullobs_sp))]
# bytype[, V1 := V1 + cmladd$V1]
par(mfrow=c(2, 2), mar=c(4, 4, 1.5, 0.5), font.main=1)
plot(V1 ~ decade, data=bytype[category=="cathedral", ],
    type='l', ylim=range(bytype$V1), bty='l', lwd=1.5, ylab = 'm3/20y')
    lines(V1 ~ decade, data=bytype[category=="parish", ],
            type='l', ylim=range(bytype$V1), col='gray', ylab = 'm3/20y')
    lines(V1 ~ decade, data=bytype[category=="other", ],
            type='l', ylim=range(bytype$V1), col='gray', ylab = 'm3/20y')
    lines(V1 ~ decade, data=bytype[category=="conventual", ],
            type='l', ylim=range(bytype$V1), col='gray', ylab = 'm3/20y')
title(main="cathedral", line=-1)
plot(V1 ~ decade, data=bytype[category=="conventual", ],
    type='l', ylim=range(bytype$V1), bty='l', lwd=1.5, ylab = 'm3/20y')
    lines(V1 ~ decade, data=bytype[category=="parish", ],
            type='l', ylim=range(bytype$V1), col='gray', ylab = 'm3/20y')
    lines(V1 ~ decade, data=bytype[category=="cathedral", ],
            type='l', ylim=range(bytype$V1), col='gray', ylab = 'm3/20y')
    lines(V1 ~ decade, data=bytype[category=="other", ],
            type='l', ylim=range(bytype$V1), col='gray', ylab = 'm3/20y')
title(main="conventual", line=-1)
plot(V1 ~ decade, data=bytype[category=="parish", ],
    type='l', ylim=range(bytype$V1), bty='l', lwd=1.5, ylab = 'm3/20y')
    lines(V1 ~ decade, data=bytype[category=="cathedral", ],
            type='l', ylim=range(bytype$V1), col='gray', ylab = 'm3/20y')
    lines(V1 ~ decade, data=bytype[category=="other", ],
            type='l', ylim=range(bytype$V1), col='gray', ylab = 'm3/20y')
    lines(V1 ~ decade, data=bytype[category=="conventual", ],
            type='l', ylim=range(bytype$V1), col='gray', ylab = 'm3/20y')
title(main="parish", line=-1)
plot(V1 ~ decade, data=bytype[category=="other", ],
    type='l', ylim=range(bytype$V1), bty='l', lwd=1.5, ylab = 'm3/20y')
    lines(V1 ~ decade, data=bytype[category=="parish", ],
            type='l', ylim=range(bytype$V1), col='lightgray', ylab = 'm3/20y')
    lines(V1 ~ decade, data=bytype[category=="cathedral", ],
            type='l', ylim=range(bytype$V1), col='lightgray', ylab = 'm3/20y')
    lines(V1 ~ decade, data=bytype[category=="conventual", ],
            type='l', ylim=range(bytype$V1), col='lightgray', ylab = 'm3/20y')
title(main="other", line=-1)
dev.off()

pdf('figs/mendicants_hc.pdf', height = 4)
bytype = fullobs_sp[data.table::between(year, 700, 1500), base::sum(.SD, na.rm=T) / M, by=list(decade, category, mendicants), .SDcols = impvrbs]
# cmladd = fullobs_sp[data.table::between(year, 700, 1500), base::sum(.SD * 0.005, na.rm=T) / M, by=list(decade, category, mendicants), .SDcols = grepr('im3_cml\\d', names(fullobs_sp))]
# bytype = bytype[, V1 := V1 + cmladd$V1][category == "conventual"]
par(mfrow = c(1, 2))
plot(V1 ~ decade, data=bytype[category=="conventual" & mendicants == 0, ],
    type='n', bty='l', col=2, lwd=1.5, ylab = 'm3/ann', main = 'Mendicant')
lines(V1 ~ decade, data=bytype[category=="conventual" & mendicants == 1, ],
    type='l', bty='l', col=2, lwd=1.5)
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

midcent = copy(fullobs_sp)
midcent[, year := round(year / 100)  * 100] # for compatability w. century
midcent = midcent[data.table::between(year, 650, 1550), list(im3_cnt = base::sum(abs(.SD), na.rm=T) / M), by=list(city, year), .SDcols=impvrbs]
pcitobs = merge(midcent, siem, by=c('city', 'year'), all.x=T)

pdf("figs/geography_hc.pdf", height=3, width=8)
par(mfrow=c(1, 3), font.main=1, mar = c(4.5, 4, 1.5, 0.2))
# why mean? already total by city, now take mean for kind of city per
plot(pcitobs[rivercanal==1, mean(im3_cnt), by=year],
    type='n', col='gray', ylab='mean ann. city m3')
lines(V1 ~ year, data=pcitobs[rivercanal==1, mean(im3_cnt), by=year], type='b', col='gray')
lines(V1 ~ year, data=pcitobs[landlocked==1, mean(im3_cnt), by=year], type='b', col='gray')
lines(V1 ~ year, data=pcitobs[coastal==1, mean(im3_cnt), by=year], type='b', lwd = 1.5)
title(main='Coastal')
plot(pcitobs[rivercanal==1, mean(im3_cnt), by=year], type='n', col='gray', ylab = '')
lines(V1 ~ year, data=pcitobs[coastal==1, mean(im3_cnt), by=year], type='b', col='gray')
lines(V1 ~ year, data=pcitobs[landlocked==1, mean(im3_cnt), by=year], type='b', col='gray')
lines(V1 ~ year, data=pcitobs[rivercanal==1, mean(im3_cnt), by=year], type='b', lwd = 1.5)
title(main='River/canal')
plot(pcitobs[rivercanal==1, mean(im3_cnt), by=year], type='n', col='gray', ylab = '')
lines(V1 ~ year, data=pcitobs[rivercanal==1, mean(im3_cnt), by=year], type='b', col='gray')
lines(V1 ~ year, data=pcitobs[coastal==1, mean(im3_cnt), by=year], type='b', col='gray')
lines(V1 ~ year, data=pcitobs[landlocked==1, mean(im3_cnt), by=year], type='b', lwd = 1.5)
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

# yearinhab = dcast(pcitobs, year ~ inhab, mean, na.rm = T, value.var = 'im3_cnt')
# yearinhab[sapply(yearinhab, is.nan)] = 0
# filled.contour(as.numeric(as.character(yearinhab[, 1])), 
#     log1p(as.numeric(colnames(yearinhab)[-1])), 
#     log1p(as.matrix(yearinhab[, -1])))

pcitobs = plm::pdata.frame(pcitobs, c("city", "year"))
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


# x = fullobs_sp[data.table::between(decade, 700, 1500), list(im2_dec=sum(im2_ann, na.rm=T), im3_dec=sum(im3_ann, na.rm=T)), by=list(decade, ctr)]

# pdf("figs/m3bycountry.pdf", height=9)
# par(mfrow=c(3, 2))
# plot(im3_dec ~ decade, data=x[ctr=="de", ], type='l', col=2, lwd=1.5, bty='l', xlim=c(700, 1500), main="de")
# points(x=c(1, 1000, 1500), y=gdp[V1 <= 1500, Germany / Germany[V1==1500]] * x[ctr=="de" & decade==1500, im3_dec])

# plot(im3_dec ~ decade, data=x[ctr=="uk", ], type='l', col=2, lwd=1.5, bty='l', xlim=c(700, 1500), main="uk")
# points(x=c(1, 1000, 1500), y=gdp[V1 <= 1500, UK / UK[V1==1500]] * x[ctr=="uk" & decade==1500, im3_dec])

# plot(im3_dec ~ decade, data=x[ctr=="fr", ], type='l', col=2, lwd=1.5, bty='l', xlim=c(700, 1500), main="fr")
# points(x=c(1, 1000, 1500), y=gdp[V1 <= 1500, France / France[V1==1500]] * x[ctr=="fr" & decade==1500, im3_dec])

# plot(im3_dec ~ decade, data=x[ctr=="be", ], type='l', col=2, lwd=1.5, bty='l', xlim=c(700, 1500), main="be")
# points(x=c(1, 1000, 1500), y=gdp[V1 <= 1500, Belgium / Belgium[V1==1500]] * x[ctr=="be" & decade==1500, im3_dec])

# plot(im3_dec ~ decade, data=x[ctr=="nl", ], type='l', col=2, lwd=1.5, bty='l', xlim=c(700, 1500), main="nl")
# points(x=c(1, 1000, 1500), y=gdp[V1 <= 1500, Netherlands / Netherlands[V1==1500]] * x[ctr=="nl" & decade==1500, im3_dec])

# plot(im3_dec ~ decade, data=x[ctr=="ch", ], type='l', col=2, lwd=1.5, bty='l', xlim=c(700, 1500), main="ch")
# points(x=c(1, 1000, 1500), y=gdp[V1 <= 1500, Switzerland / Switzerland[V1==1500]] * x[ctr=="ch" & decade==1500, im3_dec])
# dev.off()

# add ctr2 and ctr3 to siem
dim(siem)
siem = unique(statobs[, list(ctr2, ctr3, city)])[siem, on = "city"]
dim(siem)

# siem = pop[decade %% 100 == 0][siem[, c(.SD, list(decade = year))], on = c('ctr', 'decade')]

# x = fullobs_sp[!is.na(decade) & data.table:::between(decade, 700, 1500), ]
# first aggregate to city, then add siem
citobs_dec = fullobs_sp[data.table::between(decade, 700, 1500), list(m3dec = base::sum(.SD, na.rm=T) / M), by=list(decade, city, ctr, ctr2, ctr3), .SDcols = impvrbs]
dim(citobs_dec)
citobs_dec = siem[, list(inhab, lat, lon, city, decade = year)][citobs_dec, on = c("city", "decade")]
dim(citobs_dec)

x1 = citobs_dec[, list(im3_dec = sum(m3dec), urb_inhab = sum(inhab)), by=list(decade, ctr)]
x2 = citobs_dec[, list(im3_dec = sum(m3dec), urb_inhab = sum(inhab)), by=list(decade, ctr2)]
x3 = citobs_dec[, list(im3_dec = sum(m3dec), urb_inhab = sum(inhab)), by=list(decade, ctr3)]
eu = citobs_dec[, list(im3_dec = sum(m3dec), urb_inhab = sum(inhab)), by=list(decade)]

# for total urbpop
# x1 = citobs_dec[, list(im3_dec = sum(m3dec)), by=list(decade, ctr)]
# x1 = siem[, list(urb_inhab = sum(inhab)), by = list(ctr, decade = year)][x1, on = c('ctr', 'decade')]
# x2 = citobs_dec[, list(im3_dec = sum(m3dec)), by=list(decade, ctr2)]
# x2 = siem[, list(urb_inhab = sum(inhab)), by = list(ctr2, decade = year)][x2, on = c('ctr2', 'decade')]
# x3 = citobs_dec[, list(im3_dec = sum(m3dec)), by=list(decade, ctr3)]
# x3 = siem[, list(urb_inhab = sum(inhab)), by = list(ctr3, decade = year)][x3, on = c('ctr3', 'decade')]
# eu = citobs_dec[, list(im3_dec = sum(m3dec)), by=list(decade)]
# eu = siem[grep("fr|be|ch|uk|nl|de", ctr), list(urb_inhab = sum(inhab)), by = list(decade = year)][eu, on = c('decade')]

x1[decade %% 100!=0, urb_inhab:=NA] # fix zeroes
x2[decade %% 100!=0, urb_inhab:=NA]
x3[decade %% 100!=0, urb_inhab:=NA]
eu[decade %% 100 != 0, urb_inhab := NA]
x1[, urb_inhab_i := round(exp(zoo::na.approx(log(urb_inhab)))), by=ctr]
x2[, urb_inhab_i := round(exp(zoo::na.approx(log(urb_inhab)))), by=ctr2]
x3[, urb_inhab_i := round(exp(zoo::na.approx(log(urb_inhab)))), by=ctr3]
eu[, urb_inhab_i := round(exp(zoo::na.approx(log(urb_inhab))))]

pop = data.table::fread("dat/totpop.csv", encoding = "UTF-8")
x1 = pop[x1, on = c('ctr', 'decade')]
eu = pop[, list(totpop = sum(totpop)), by = decade][eu, on = 'decade']

x1[, sum(urb_inhab_i) / sum(totpop), by = ctr]
x1[, sum(urb_inhab_i) / sum(totpop), by = list(ctr, decade)][ctr=='nl']
x1[, sum(urb_inhab_i) / sum(totpop), by = list(ctr, decade)][ctr=='be']
x1[!is.na(urb_inhab), sum(urb_inhab) / sum(totpop), by = ctr]

write.csv(x3, "~/dropbox/cathedrals/dat/countryseries.csv", row.names=F)

x1[, im3_dec_smth:=predict(loess(im3_dec / urb_inhab_i ~ decade), newdata=decade), by=ctr]
x2[, im3_dec_smth:=predict(loess(im3_dec / urb_inhab_i ~ decade), newdata=decade), by=ctr2]
x3[, im3_dec_smth:=predict(loess(im3_dec / urb_inhab_i ~ decade), newdata=decade), by=ctr3]
eu[, im3_dec_smth:=predict(loess(im3_dec / urb_inhab_i ~ decade), newdata=decade)]

pdf('figs/europetotal_puc_hc.pdf', height=6)
par(mfrow=c(1, 1))
plot(im3_dec / urb_inhab_i ~ decade, data=eu, type='n', bty='l')
abline(v=c(768, 1315, 1000, 1348), col='gray')
lines(im3_dec / urb_inhab_i ~ decade, data=eu, type='b', col=2, lwd=1.5, pch=1, cex=0.9)
dev.off()

pdf('figs/europetotal_pc_hc.pdf', height=6)
par(mfrow=c(1, 1))
plot(im3_dec / totpop ~ decade, data=eu, type='n', bty='l')
abline(v=c(768, 1315, 1000, 1348), col='gray')
lines(im3_dec / totpop ~ decade, data=eu, type='b', col=2, lwd=1.5, pch=1, cex=0.9)
dev.off()

pdf("figs/pucpanel.pdf", width=8)
par(mfrow=c(2, 2), mar=c(4, 4, 1.5, 0.5), font.main=1)
plot(im3_dec / urb_inhab_i ~ decade, data=x1[ctr=='fr', ], type='l',
    bty='l', ylab='m3/dec', xlab='', main='France')
lines(im3_dec / urb_inhab_i ~ decade, data=eu, type='l', col='gray70')
# plot(im3_dec / urb_inhab_i ~ decade, data=x1[ctr=='de', ], type='l',
plot(im3_dec / urb_inhab_i ~ decade, data=x1[ctr %in% c('de', 'ch'), list(im3_dec = sum(im3_dec), urb_inhab_i = sum(urb_inhab_i)), by = decade], type='l',
    bty='l', ylab='m3/dec', xlab='', main='Germany (incl. Switz.)')
lines(im3_dec / urb_inhab_i ~ decade, data=eu, type='l', col='gray70')
plot(im3_dec / urb_inhab_i ~ decade, data=x1[ctr=='uk', ], type='l',
    bty='l', ylab='m3/dec', main='Britain')
lines(im3_dec / urb_inhab_i ~ decade, data=eu, type='l', col='gray70')
plot(im3_dec / urb_inhab_i ~ decade, data=x3[ctr3=='lc', ], type='l',
    bty='l', ylab='m3/dec', main='Low Countries')
lines(im3_dec / urb_inhab_i ~ decade, data=eu, type='l', col='gray70')
dev.off()

pdf("figs/pcpanel.pdf", height = 11, width = 8)
par(mfrow=c(3, 2), mar=c(4, 4, 1.5, 0.5), font.main=1)
plot(im3_dec / totpop ~ decade, data=x1[ctr=='fr', ], type='l',
    bty='l', ylab='m3/dec', xlab='', main='France')
lines(im3_dec / totpop ~ decade, data=eu, type='l', col='gray70')
plot(im3_dec / totpop ~ decade, data=x1[ctr %in% c('de'), list(im3_dec = sum(im3_dec), totpop = sum(totpop)), by = decade], type='l',
    bty='l', ylab='m3/dec', xlab='', main='Germany')
lines(im3_dec / totpop ~ decade, data=eu, type='l', col='gray70')
plot(im3_dec / totpop ~ decade, data=x1[ctr %in% c('ch'), list(im3_dec = sum(im3_dec), totpop = sum(totpop)), by = decade], type='l',
    bty='l', ylab='m3/dec', xlab='', main='Switzerland')
lines(im3_dec / totpop ~ decade, data=eu, type='l', col='gray70')
plot(im3_dec / totpop ~ decade, data=x1[ctr=='uk', ], type='l',
    bty='l', ylab='m3/dec', main='Britain')
lines(im3_dec / totpop ~ decade, data=eu, type='l', col='gray70')
plot(im3_dec / totpop ~ decade, data=x1[ctr %in% c("be"), list(im3_dec = sum(im3_dec), totpop = sum(totpop)), by = decade], type='l',
    bty='l', ylab='m3/dec', main='Belgium')
lines(im3_dec / totpop ~ decade, data=eu, type='l', col='gray70')
plot(im3_dec / totpop ~ decade, data=x1[ctr %in% c("nl"), list(im3_dec = sum(im3_dec), totpop = sum(totpop)), by = decade], type='l',
    bty='l', ylab='m3/dec', main='Netherlands')
lines(im3_dec / totpop ~ decade, data=eu, type='l', col='gray70')
dev.off()

data.table::fwrite(rbindlist(list(x1, x1[ctr %in% c("nl", "be"), list(im3_dec = sum(im3_dec), totpop = sum(totpop)), by = decade], eu[, c(.SD, list(ctr = 'eu'))]), 
    fill = T), "dat/totpcpuc.csv")
data.table::fwrite(x3, "dat/totpuc_regions.csv")

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
yl = range(x1[ctr=='fr', im3_dec / urb_inhab_i]) * 1.1
plot(im3_dec / urb_inhab_i ~ decade, data=x1[ctr=='fr', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Fr., total', col=2)
plot(im3_dec / urb_inhab_i ~ decade, data=x3[ctr3=='fr_south', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Fr., south', col=2)
lines(im3_dec / urb_inhab_i ~ decade, data=x1[ctr=='fr', ], col='gray70')
plot(im3_dec / urb_inhab_i ~ decade, data=x3[ctr3=='fr_north', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Fr., north', col=2)
lines(im3_dec / urb_inhab_i ~ decade, data=x1[ctr=='fr', ], col='gray70')
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
yl = range(x3[grep('de', ctr3), im3_dec / urb_inhab_i])
plot(im3_dec / urb_inhab_i ~ decade, data=x1[ctr=='de', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='de., total', col=2)
plot(im3_dec / urb_inhab_i ~ decade, data=x3[ctr3=='de_sw', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='de., south-west', col=2)
lines(im3_dec / urb_inhab_i ~ decade, data=x1[ctr=='de', ], col='gray70')
plot(im3_dec / urb_inhab_i ~ decade, data=x3[ctr3=='de_ne', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='de., north-east', col=2)
lines(im3_dec / urb_inhab_i ~ decade, data=x1[ctr=='de', ], col='gray70')
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
yl = range(x3[grep('uk', ctr3), im3_dec / urb_inhab_i])
plot(im3_dec / urb_inhab_i ~ decade, data=x1[ctr=='uk', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='UK, total', col=2)
plot(im3_dec / urb_inhab_i ~ decade, data=x3[ctr3=='uk_se', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='UK, south-east', col=2)
lines(im3_dec / urb_inhab_i ~ decade, data=x1[ctr=='uk', ], col='gray70')
plot(im3_dec / urb_inhab_i ~ decade, data=x3[ctr3=='uk_nw', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='UK, north-west', col=2)
lines(im3_dec / urb_inhab_i ~ decade, data=x1[ctr=='uk', ], col='gray70')
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
yl = range(x1[grep('nl|be', ctr), im3_dec / urb_inhab_i])
plot(im3_dec / urb_inhab_i ~ decade, data=x3[ctr3=='lc', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='LC, total', col=2)
plot(im3_dec / urb_inhab_i ~ decade, data=x1[ctr=='be', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Belgium', col=2)
lines(im3_dec / urb_inhab_i ~ decade, data=x3[ctr3=='lc', ], col='gray70')
plot(im3_dec / urb_inhab_i ~ decade, data=x1[ctr=='nl', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Netherlands', col=2)
lines(im3_dec / urb_inhab_i ~ decade, data=x3[ctr3=='lc', ], col='gray70')
dev.off()

x1 = x[, list(im2_dec=sum(im2_ann, na.rm=T), im3_dec=sum(im3_ann, na.rm=T), inhab=sum(inhab, na.rm=T)), by=list(decade, ctr)]
x2 = x[, list(im2_dec=sum(im2_ann, na.rm=T), im3_dec=sum(im3_ann, na.rm=T), inhab=sum(inhab, na.rm=T)), by=list(decade, ctr2)]
x3 = x[, list(im2_dec=sum(im2_ann, na.rm=T), im3_dec=sum(im3_ann, na.rm=T), inhab=sum(inhab, na.rm=T)), by=list(decade, ctr3)]

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
