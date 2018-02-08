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

plot(log1p(activity) ~ log1p(endsize_lag3), data = stock)
abline(lm(log1p(activity) ~ log1p(endsize_lag3), data = stock))

stock[, mean(activity), by = round(log1p(endsize))]
plot(stock[, mean(activity), by = round(log1p(endsize))])

m = lm(log1p(activity) ~ log1p(endsize) + factor(century) + factor(city), data = stock)
m_lag = lm(log1p(activity) ~ log1p(endsize_lag) + factor(century), data = stock)

texreg::screenreg(list(m, m_lag), omit.coef = 'city')

plot(log(activity) ~ log(endsize), data = stock)
plot(log(activity) ~ log(endsize_lag), data = stock)


as.data.frame(fullobs_sp[city == 'Aachen' & decade == 720, list(osmid, decade, endsize)])
unique(fullobs_sp[city == 'Aachen' & decade == 720, list(osmid, endsize)])[, sum(endsize, na.rm = T)]

# ideally, actually, look at building phases (assuming in use after phase)
# and possibly exclude m3/ann of the churches that 
# also redo with heaping correction






x = statobs[fullobs_sp, on="osmid"]

endsizes = x[data.table::between(year, 701, 1500), list(endsize = mean(im3_cml, na.rm = T)), by = list(city, osmid, decade)][, list(endsize = sum(endsize, na.rm = T)), by = list(city, decade)]
activity = x[data.table::between(year, 701, 1500), list(activity = sum(abs(im3_ann), na.rm = T)), by = list(city, decade)]
endsizes = endsizes[activity, on = list(city, decade)]

plot(log1p(activity) ~ log1p(endsize), data = endsizes)
abline(lm(log1p(activity) ~ log1p(endsize), data = endsizes), col = 2)

endsizes = x[data.table::between(year, 701, 1500), list(endsize = mean(im3_cml, na.rm = T)), by = list(city, osmid, century)][, list(endsize = sum(endsize, na.rm = T)), by = list(city, century)]
activity = x[data.table::between(year, 701, 1500), list(activity = sum(abs(im3_ann), na.rm = T)), by = list(city, century)]
endsizes = endsizes[activity, on = list(city, century)]
endsizes[order(city, century), endsize_lag := data.table::shift(endsize), by = city]

plot(log1p(activity) ~ log1p(endsize), data = endsizes)
abline(lm(log1p(activity) ~ log1p(endsize), data = endsizes), col = 2)

plot(log1p(activity) ~ log1p(endsize_lag), data = endsizes)
abline(lm(log1p(activity) ~ log1p(endsize_lag), data = endsizes), col = 2)




m3 = x[year <= 1500, list(im3_cnt = base::sum(abs(.SD), na.rm=T) / M), by=list(city, century), .SDcols=paste0("im3_ann", 1:M)]
stock = x[year <= 1500, list(im3_stock = (base::sum(.SD, na.rm=T) / M) / length(unique(year))), by=list(city, century), .SDcols=paste0("im3_cml", 1:M)]

x[year <= 1500, mean(rowMeans(.SD)), by = list(city, osmid, century), .SDcols=paste0("im3_cml", 1:M)][, sum(V1), by = list(city, century)]
x[year <= 1500, mean(im3_cml), by = list(city, osmid, century)][, sum(V1), by = list(city, century)]


# works but nasty



x = dynobs[, list(endsize = sum(m3, na.rm=T), year = max(year)), by = list(osmid, bldindex)]
# merge this in, then approx 1:2

x = x[fullobs_sp, on = list(osmid, year)]
x[, endsize := zoo::na.approx(endsize, method = 'constant', rule = 1:2, na.rm=F)]
x[, century := round(year / 100)  * 100] # for compatability w. century
plot(log1p(V1) ~ log1p(V2), data = x[data.table::between(century, 700, 1500), list(sum(abs(im3_ann), na.rm = T), mean(endsize, na.rm = T)), by = list(city, century)])

as.data.frame(x[city == "Mainz" & year <= 850, list(year, osmid, im3_cml)])

as.data.frame(fullobs_sp[city == 'Mainz' & year < 800, list(osmid, year, im3_cml)])
as.data.frame(x[city == 'Mainz' & century == 700, list(osmid, year, im3_cml)])
x[year <= 1500, list(im3_stock = (base::sum(im3_cml, na.rm=T)) / .N), by=list(city, century)][city == "Mainz"]
x[year <= 1500, list(im3_stock = (base::sum(im3_cml, na.rm=T)) / length(unique(year))), by=list(city, century)][city == "Mainz"]


builddyn = m3[stock, on = c("city", "year")][order(city, year)][year <= 1500, ]
builddyn[, stock_lag := data.table::shift(im3_stock), by = city]

pdf('figs/stock_activity.pdf')
par(mfrow = c(2, 2))
plot(log(im3_cnt) ~ log(im3_stock), data = builddyn[im3_cnt > 0 & im3_stock > 0])
abline(lm(log(im3_cnt) ~ log(im3_stock), data = builddyn[im3_cnt > 0 & im3_stock > 0]), col = 2)
plot(log(im3_cnt) ~ log(stock_lag), data = builddyn[im3_cnt > 0 & im3_stock > 0])
abline(lm(log(im3_cnt) ~ log(stock_lag), data = builddyn[im3_cnt > 0 & stock_lag > 0]), col = 2)

m_base = lm(log1p(im3_cnt) ~ log1p(im3_stock), data = builddyn)
m_base_lag = lm(log1p(im3_cnt) ~ log1p(stock_lag), data = builddyn)
plot(log1p(im3_cnt) ~ log1p(im3_stock), data = builddyn)
    abline(m_base, col = 2)
plot(log1p(im3_cnt) ~ log1p(stock_lag), data = builddyn)
    abline(m_base_lag, col = 2)
dev.off()
# very crudely: no inverse relation
# but: part of this is picking up building continuing after some stock
# has been created; need stock of completed churches

endsizes = fullobs_sp[!is.na(bldindex), list(endsize = tail(im3_cml, 1), year = tail(year, 1)), by = list(osmid, bldindex)]
fullobs_sp = endsizes[fullobs_sp, on = c("osmid", "bldindex", "year")]
fullobs_sp[, finished_stock := zoo::na.approx(endsize, method = 'constant', rule = 1:2, na.rm = FALSE), by = osmid]
fullobs_sp[, century := round(year / 100)  * 100]
x = fullobs_sp[data.table::between(year, 701, 1500), list(sum(abs(im3_ann), na.rm=T), mean(finished_stock, na.rm=T)), by = list(city, century)]

pdf("figs/complstock_activity.pdf")
par(mfrow = c(2, 2))
x[, stock_lag := data.table::shift(V2), by = city]

plot(log(V1) ~ log(V2), data = x[V1 > 0 & V2 > 0])
    abline(lm(log(V1) ~ log(V2), data = x[V1 > 0 & V2 > 0]), col = 2)
plot(log(V1) ~ log(stock_lag), data = x[V1 > 0 & stock_lag > 0])
    abline(lm(log(V1) ~ log(stock_lag), data = x[V1 > 0 & stock_lag > 0]), col = 2)
m_complstock = lm(log1p(V1) ~ log1p(V2), data = x)
m_complstock_lag = lm(log1p(V1) ~ log1p(stock_lag), data = x)
plot(log1p(V1) ~ log1p(V2), data = x)
    abline(m_complstock, col = 2)
plot(log1p(V1) ~ log1p(stock_lag), data = x)
    abline(m_complstock_lag, col = 2)
dev.off()
# lag here is a bit ill-defined and should probably be century

# maybe also commencement as total end size as function of 
x = dynobs[, list(totalm3 = tail(cumsum(abs(m3)), 1), endyear = tail(year, 1), startyear = head(year, 1)), by = list(osmid, bldindex)]
x = x[order(osmid, endyear), ][startyear <= 1500]
x[, totalm3_previous := shift(totalm3), by = osmid]
plot(totalm3 ~ totalm3_previous, data = x, log = 'xy') #  church is rarely smaller than previous one
curve(1 * x, add = T, col = 2)
# so should be aggregated to city...
hist(dynobs[, diff(range(year)), by = list(osmid, bldindex)][, V1])

builddyn = x[statobs, on = "osmid"][, list(totalnew = sum(totalm3, na.rm = T), totalold = sum(totalm3_previous, na.rm = T)), by = list(city, endyear)]
builddyn = builddyn[order(city, endyear)][startyear <= 1500]
builddyn[city == "Liege (Luik, Luettich)"]
builddyn[totalold == 0, totalold := NA]
builddyn[totalnew == 0, totalnew := NA]
builddyn[, totalold := as.numeric(zoo::na.approx(totalold, method = 'constant', rule = 1:2, na.rm = F)), by = city]
# builddyn[is.na(totalold), totalold := 0]
builddyn[is.na(totalnew), totalnew := 0]
builddyn[, totalold_lag := shift(totalold), by = city]

pdf("figs/complstock_complact.pdf")
par(mfrow = c(2, 2))
plot(log(totalnew) ~ log(totalold), data = builddyn[totalold > 0 & totalnew > 0])
    abline(lm(log(totalnew) ~ log(totalold), data = builddyn[totalold > 0 & totalnew > 0]), col = 2)
plot(log(totalnew) ~ log(totalold_lag), data = builddyn[totalold > 0 & totalold_lag > 0])
    abline(lm(log(totalnew) ~ log(totalold_lag), data = builddyn[totalnew > 0 & totalold_lag > 0]), col = 2)
m_complstock_newproj = lm(log1p(totalnew) ~ log1p(totalold), data = builddyn)
m_complstock_newproj_lag = lm(log1p(totalnew) ~ log1p(totalold_lag), data = builddyn)
plot(log1p(totalnew) ~ log1p(totalold), data = builddyn)
abline(m_complstock_newproj, col = 2)
plot(log1p(totalnew) ~ log1p(totalold_lag), data = builddyn)
abline(m_complstock_newproj_lag, col = 2)
dev.off()

texreg::screenreg(list(m_base, m_base_lag, 
    m_complstock, m_complstock_lag, 
    m_complstock_newproj, m_complstock_newproj_lag))


summary(lm(log1p(V1) ~ log1p(stock_lag) + century, data = x))
summary(lm(log1p(V1) ~ log1p(stock_lag) + factor(century), data = x))
