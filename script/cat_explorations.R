rm(list = ls())
library("data.table")

dynobs = data.table::fread("dat/dynobs.csv")
statobs = data.table::fread("dat/statobs.csv")
citobs = data.table::fread("dat/citobs.csv")
fullobs_sp = data.table::fread("gunzip -c  dat/fullobs_sp.csv.gz")
fullobs = fullobs_sp[, 1:grep("decade", names(fullobs_sp)), with = F]

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
sum(fullobs[data.table::between(year, 700, 1500), sum(im2_ann, na.rm=T), by=year])
sum(fullobs[data.table::between(year, 700, 1500), sum(.SD, na.rm=T) / M, by=year, .SDcols=grep("im2_ann\\d", names(fullobs))])

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
