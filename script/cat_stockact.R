rm(list=ls())
setwd("~/dropbox/cathedrals")

library("data.table")

source('script/cat_functions.r')

statobs = data.table::fread("dat/statobs.csv")
citobs = data.table::fread("dat/citobs.csv")
dynobs = data.table::fread("dat/dynobs.csv")
fullobs_sp = data.table::fread("gunzip -c dat/fullobs_sp.csv.gz")
siem = data.table::fread("dat/siem_long.csv", encoding="UTF-8")

M = 9

# fullobs_sp[, century := ceiling(year / 100)  * 100] # for compatability w. century
fullobs_sp[, century := round(year / 100)  * 100] # for compatability w. century
# fullobs_sp[, century := (trunc((year - 1) / 100) + 1) * 100] # so 1500 = 1401-1500

# endsizes = dynobs[, list(endsize = cumsum(m3), year), by = list(osmid)]
# if by phase
endsizes = dynobs[, list(endsize = sum(m3, na.rm = T), year = tail(year, 1)), by = list(osmid, bldindex)]
fullobs_sp = endsizes[fullobs_sp, on = list(osmid, year)]
fullobs_sp = fullobs_sp[order(city, osmid, year)]
fullobs_sp[, endsize := zoo::na.approx(endsize, method = 'constant', rule = 1:2, na.rm = F), by = osmid]

endsizes = fullobs_sp[data.table::between(year, 701, 1500), mean(endsize, na.rm = T), by = list(city, osmid, century)][, list(endsize = sum(V1, na.rm = T)), by = list(city, century)]
activity = fullobs_sp[data.table::between(year, 701, 1500), list(activity = sum(abs(im3_ann), na.rm = T)), by = list(city, century)]
stock = endsizes[activity, on = list(city, century)]
stock[, endsize_lag := data.table::shift(endsize), by = city]
stock[, endsize_lag2 := data.table::shift(endsize, 2), by = city]
stock[, endsize_lag3 := data.table::shift(endsize, 3), by = city]

m = lm(log1p(activity) ~ log1p(endsize), data = stock)
m_lag = lm(log1p(activity) ~ log1p(endsize_lag), data = stock)

pdf("figs/completedstock_activity.pdf", width = 10, height = 6)
par(mfrow = c(1, 2))
plot(log1p(activity) ~ log1p(endsize), data = stock, col = 'gray50', bty = 'l',
    xlab = "compl. stock (log m3)", ylab = "activity (log m3/century)")
abline(m, col = 1)
plot(log1p(activity) ~ log1p(endsize_lag), data = stock, col = 'gray50', bty = 'l',
    xlab = "lag compl. stock (log m3)", ylab = "activity (log m3/century)")
abline(m_lag, col = 1)
dev.off()

m_city = lm(log1p(activity) ~ log1p(endsize) + factor(century) + factor(city), data = stock)
m_lag_city = lm(log1p(activity) ~ log1p(endsize_lag) + factor(century), data = stock)

texreg::screenreg(list(m, m_lag, m_city, m_lag_city), omit.coef = 'city')
