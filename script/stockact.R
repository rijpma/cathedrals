# relation between building activity and stock of churches

rm(list=ls())
setwd("~/dropbox/cathedrals")

library("data.table")

source('script/cat_functions.r')

dynobs = data.table::fread("dat/dynobs.csv")
fullobs_sp = data.table::fread("gunzip -c dat/fullobs_sp.csv.gz")

M = 9
impvrbs = names(fullobs_sp)[grep('im3_ann\\d', names(fullobs_sp))]

fullobs_sp[, century := round(year / 100)  * 100] # for compatability w. century in siem

# if by phase
endsizes = dynobs[, list(endsize = sum(m3, na.rm = T), year = tail(year, 1)), by = list(osmid, bldindex)]
fullobs_sp = endsizes[fullobs_sp, on = list(osmid, year)]
fullobs_sp = fullobs_sp[order(city, osmid, year)]
fullobs_sp[, endsize := zoo::na.approx(endsize, method = 'constant', rule = 1:2, na.rm = F), by = osmid]

endsizes = fullobs_sp[data.table::between(year, 701, 1500), mean(endsize, na.rm = T), by = list(city, osmid, century)][, list(endsize = sum(V1, na.rm = T)), by = list(city, century)]
activity = fullobs_sp[data.table::between(year, 701, 1500), list(activity = base::sum(abs(.SD), na.rm = T)), .SDcols = impvrbs, by = list(city, century)]
stock = endsizes[activity, on = list(city, century)]
stock[, endsize_lag := data.table::shift(endsize), by = city]
stock[, endsize_lag2 := data.table::shift(endsize, 2), by = city]
stock[, endsize_lag3 := data.table::shift(endsize, 3), by = city]

m = lm(log1p(activity) ~ log1p(endsize), data = stock)
m_lag = lm(log1p(activity) ~ log1p(endsize_lag), data = stock)

pdf("figs/completedstock_activity.pdf", width = 10, height = 6)
par(mfrow=c(1, 2), font.main=1, mar = c(4.5, 4, 1.5, 0.2))
plot(log1p(activity) ~ log1p(endsize), data = stock, col = 'gray30', bty = 'l',
    xlab = "completed church stock (log m³)", 
    ylab = "church building per century (log m³)")
abline(m, col = 1, lwd = 1.5)
plot(log1p(activity) ~ log1p(endsize_lag), data = stock, col = 'gray30', bty = 'l',
    xlab = "lag completed church stock (log m³)", ylab = "")
abline(m_lag, col = 1, lwd = 1.5)
dev.off()
