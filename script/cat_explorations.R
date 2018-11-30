rm(list = ls())
setwd("~/dropbox/cathedrals")
source("script/cat_functions.r")

library("data.table")

M = 9 # number of imputations

dynobs = data.table::fread("dat/dynobs.csv")
statobs = data.table::fread("dat/statobs.csv")
dynobs_sp = merge(dynobs, statobs, by="osmid")
citobs = data.table::fread("dat/citobs.csv")
fullobs_sp = data.table::fread("gunzip -c dat/fullobs_sp.csv.gz")
fullobs = fullobs_sp[, -names(statobs)[-1], with = F]

# impvrbs = grepr('im3_ann_cmc\\d', names(fullobs_sp))
impvrbs = names(fullobs_sp)[grepl('im3_ann\\d', names(fullobs_sp))]

ukgdp = data.table::fread("dat/engdp12001700.csv", skip=1, encoding="UTF-8")
gdp = data.table::fread("dat/maddison_gdp_old.csv")

gdp_new = readxl::read_excel("excels/mpd2018.xlsx", sheet = "Full data")
setDT(gdp_new)
gdp_new = gdp_new[year <= 1500]
gdp_new[, iso2c := ifelse(countrycode == 'GBR', "UK",
    countrycode::countrycode(countrycode, 'iso3c', 'iso2c'))]
gdp_new = gdp_new[iso2c %in% toupper(unique(fullobs_sp$ctr)), ]
holland_england = readxl::read_excel("excels/mpd2018.xlsx",
    sheet = "Partial countries", skip = 1)
setDT(holland_england)
holland_england = holland_england[year <= 1500]
holland_england = rbindlist(list(
    holland_england[, list(
        year = year, iso2c = "UK", cgdppc_part = cgdppc_England, rgdpnapc_part = rgdpnapc_England)],
    holland_england[, list(
        year = year, iso2c = "NL", cgdppc_part = cgdppc_Holland, rgdpnapc_part = rgdpnapc_Holland)]))

gdp_new = merge(gdp_new, holland_england, on = c("iso2c", "year"), all = T)

siem = data.table::fread("dat/siem_long.csv", encoding="UTF-8")

pop_mj = data.table::fread("dat/mcevedyjones.csv", encoding = "UTF-8")
pop_mj[, decade := year]
pop_mj = pop_mj[as.data.table(expand.grid(ctr = unique(pop_mj$ctr), decade = unique(fullobs$decade))), on = c("ctr", "decade")]
pop_mj[ctr == 'uk', broadberry := ukgdp$population[match(decade, ukgdp$V1)]]
pop_mj[!is.na(broadberry), pop := broadberry / 1e6]
pop_mj[, pop := pop * 1e3]
pop_mj[, bd := ifelse(decade < 1348, "pre", "post")]

pop_mj[decade >= 500, pop_spl := exp(zoo::na.spline(log(pop), x = decade, na.rm = F)), by = .(ctr, bd)]
pdf("figs/popinterpols.pdf", width = 9)
par(mfrow = c(2, 3), mar = c(4,3,2,1))
for (i in c("it", "fr", "de", "be", "nl", "uk")){
    plot(pop ~ decade, data = pop_mj[ctr == i], main = i)
    lines(pop_spl ~ decade, data = pop_mj[ctr == i], col = 2)
    abline(v = c(700, 1348, 1500), col = 'gray')
}
dev.off()
writexl::write_xlsx(pop_mj, "excels/popinterpols.xlsx")

pop_mj[ctr=='nl']
lines(pop_spl ~ decade, data = pop_mj[ctr == 'de'], col = 2)
abline(v = 1348)

plot(log(pop) ~ decade, data = pop_mj[ctr == 'nl'], type = 'b')
lines(pop_mj[ctr=='nl' & bd == 'pre', .(decade, ap = na.spline(log(pop), x = decade, na.rm = F))])
abline(v = 1348)
lines(pop_mj[ctr=='nl' & bd == 'post', .(decade, ap = na.spline(log(pop), x = decade, na.rm = F))], col = 2)


plot(pop_mj[ctr == 'nl' & bd == 'pre', exp(predict(loess(log(pop) ~ decade, surface = 'direct'), newdata = data.frame(decade)))])
plot(pop_mj[ctr == 'nl' & bd == 'post', exp(predict(loess(log(pop) ~ decade, surface = 'direct', span = 0.8), newdata = data.frame(decade)))])

pop_mj[, pop_l := exp(predict(loess(log(pop) ~ decade, surface = 'direct', span = 0.8), newdata = data.frame(decade))), by = .(ctr, bd)]

plot(pop ~ decade, data = pop_mj[ctr == 'nl'], type = 'b')
lines(pop_l ~ decade, data = pop_mj[ctr == 'nl'], type = 'l')
pop_mj[ctr=='nl']

lm(log(pop) ~ decade, data =)

pop_mj[, pop_i := exp(predict(lm(log(pop) ~ decade), newdata = data.frame(decade))), by = .(ctr, bd)]

lines(exp(crazy) ~ decade, data = pop_mj[ctr == 'nl'], type = 'l')

siem[, ctr:=tolower(siem$tld)]
siem[ctr=='gb', ctr:="uk"]

siem[, tokeep := any(inhab[year <= 1500] > 0), by = city]
siem[, .N, by = tokeep]
siem = siem[tokeep == T]
siem[, tokeep := NULL]

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

pdf("figs/heap_v_noheap.pdf", width=11)
par(mfrow=c(1, 2))
heaping_annual = fullobs[data.table::between(year, 700, 1500),
    list(heaping_corrected = sum(.SD, na.rm=T) / M,
        heaped = sum(im3_ann, na.rm=T)),
    by=year,
    .SDcols=grep("im3_ann\\d", names(fullobs))]
heaping_20y = fullobs[data.table::between(year, 700, 1500),
    list(heaping_corrected = sum(.SD, na.rm=T) / M,
        heaped = sum(im3_ann, na.rm=T)),
    by=decade,
    .SDcols=grep("im3_ann\\d", names(fullobs))]

plot(heaping_annual[, max(heaping_corrected, heaped), by = year], type='n', xlab = 'm3/year')
abline(v=(7:15)*100, col='gray', lwd=0.8)
abline(v=c(1140, 1315, 1348), col='gray', lwd=0.8)
lines(heaped ~ year, data = heaping_annual)
lines(heaping_corrected ~ year, data = heaping_annual, col = 'red')

plot(heaping_20y[, max(heaping_corrected, heaped), by = decade], type='n', xlab = 'm3/20y')
abline(v=(7:15)*100, col='gray', lwd=0.8)
abline(v=c(1140, 1315, 1348), col='gray', lwd=0.8)
lines(heaped ~ decade, data = heaping_20y, type = 'b')
lines(heaping_corrected ~ decade, data = heaping_20y, col = 'red', type = 'b')

legend('topleft', legend=c("heaping", "heaping corrected"), fill=1:2)
dev.off()

# total difference between heaped and non-heaped series
sum(fullobs[data.table::between(year, 700, 1500), sum(im3_ann, na.rm=T), by=year])
sum(fullobs[data.table::between(year, 700, 1500), sum(.SD, na.rm=T) / M, by=year, .SDcols=grep("im3_ann\\d", names(fullobs))])

# average end-size church
dynobs[, list(m3_end=sum(m3, na.rm=T)), by=paste(osmid, bldindex)][, mean(m3_end)]
dynobs[, list(m3_end=sum(m3[bldindex==max(bldindex)], na.rm=T)), by=osmid][, mean(m3_end)]

# distribution observations
pdf('figs/phasedistr_hc.pdf', height = 6, width = 10)
par(mfrow=c(2, 4), mar=c(4, 4, 2, 1), font.main = 1)
x = dynobs_sp[, grep('ctr$|year_crc', names(dynobs_sp)), with = F]
for (country in unique(x$ctr)){
    histmat = as.matrix(x[ctr == country, grep("year", names(x)), with = F])
    histmat[histmat < 700] = 700
    histmat[histmat > 1600] = 1600
    hi = hist(histmat, breaks = M, plot = F)
    hi$counts = hi$counts / M
    plot(hi, main='', xlab = 'year')
    title(main=country, line = -0.1)
}
dev.off()

pdf('figs/phasedistr.pdf', height = 6, width = 10)
par(mfrow=c(2, 4), mar = c(2, 2, 3, 1), font.main = 1)
x = dynobs_sp[, list(ctr, year)]
x[year > 1600, year := 1600]
x[year < 700, year := 700]
for (country in unique(x$ctr)){
    hist(x[ctr == country, year], breaks = 9, main = '')
    title(main = country, line = -0.1)
}
dev.off()

# sumstats
siem_cities = aggregate(city ~ ctr, data=siem[year==1500 & ctr != 'lu', ], length)
church_cities = aggregate(city ~ ctr, data=unique(statobs[, list(city, ctr)]), length)
phases_pre1000 = aggregate(city ~ ctr, data=dynobs_sp[year < 1000, ], length)
phases_pre1200 = aggregate(city ~ ctr, data=dynobs_sp[year < 1200, ], length)
phases_all = aggregate(city ~ ctr, data=dynobs_sp, length)
churches_by_ctr = aggregate(osmid ~ ctr, data=statobs, length)

ss_out = data.table(siem_cities,
    `N cities w. large church` = church_cities[, -1],
    `N churches` = churches_by_ctr[, -1],
    `N phases` = phases_all[,-1],
    `N phases pre1000` = phases_pre1000[,-1], 
    `N phases pre1200` = phases_pre1200[, -1])

ss_out = ss_out[order(-as.numeric(city)), ]
ss_out = rbind(ss_out, c(ctr = 'all', as.list(colSums(ss_out[, -1]))))

writeLines(
    knitr::kable(ss_out, format = "html", row.names = FALSE),
    "tab/sumstats.html")

byctr = Reduce(merge, list(
    siem[year == 1500,
        .N,
        by = ctr],
    unique(statobs[, list(city, 1)])[
        siem[year == 1500],
        on = c("city")][,
            list(`% cities w. large church` = mean(!is.na(V2))),
            by = ctr],
    unique(statobs[, list(city, osmid)])[
        siem[year == 1500],
        on = c("city"),
        allow.cartesian = T][,
            list(nchurch = sum(!is.na(osmid))),
            by = list(city, ctr)][,
                list(`Large churches/city` = mean(nchurch)),
                by = ctr],
    dynobs_sp[,
        .N,
        by = list(osmid, ctr)][,
            list(`Building phases / church` = mean(N)),
            by = ctr],
    dynobs_sp[,
        list(`% phases < 1000` = mean(year < 1000)),
        by = ctr],
    dynobs_sp[,
        list(`% phases < 1200` = mean(year < 1200)),
        by = ctr]))
tot = Reduce(merge, list(
    siem[year == 1500, .N],
    unique(statobs[, list(city, 1)])[
        siem[year == 1500 ],
        on = c("city")][,
            list(`% cities w. large church` = mean(!is.na(V2)))],
    unique(statobs[, list(city, osmid)])[
        siem[year == 1500 ],
        on = c("city"),
        allow.cartesian = T][,
            list(nchurch = sum(!is.na(osmid))),
            by = list(city, ctr)][,
                list(`Large churches/city` = mean(nchurch))],
    dynobs_sp[,
        .N,
        by = list(osmid, ctr)][,
            list(`Building phases / church` = mean(N))],
    dynobs_sp[, list(`% phases < 1000` = mean(year < 1000))],
    dynobs_sp[, list(`% phases < 1200` = mean(year < 1200))]))
setnames(tot, 'x', "N")
out = rbind(byctr, data.table(ctr = 'all', tot))
out[, grep('\\%', names(out)) := lapply(.SD, function(x) as.integer(round(x * 100))), .SDcols = grep('\\%', names(out))]
writeLines(knitr::kable(out, digits = 1, format = 'html'), "tab/sumstats_perc.html")

data.table::fwrite(
    statobs[city %in% siem[year <= 1500, city[bishopric == 1 | arcbishopric == 1]] & ctr == 'it'][order(city)],
    "tab/italianbishoprics.csv")

source("dat/itcathedrals.R")
# catheral building v. other churches

fullobs_sp[osmid %in% itcathedrals, category := "cathedral"]
fullobs_sp[is.na(category), category := ""]

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

pdf("figs/bytype_hc.pdf", width = 8)
bytype = fullobs_sp[data.table::between(year, 700, 1500), base::sum(.SD, na.rm=T) / M, by=list(decade, category), .SDcols = impvrbs]
# cmladd = fullobs_sp[data.table::between(year, 700, 1500), base::sum(.SD * 0.005, na.rm=T) / M, by=list(decade, category), .SDcols = grepr('im3_cml\\d', names(fullobs_sp))]
# bytype[, V1 := V1 + cmladd$V1]
bytype[, sum(V1), by = list(century = round(decade, -2), category)][, list(category, V1 / sum(V1)), by = century][category == "cathedral"]
bytype[, sum(V1), by = list(century = round(decade, -2), category)][, list(category, V1 / sum(V1)), by = century][category == "other"]

par(mfrow=c(2, 2), mar=c(4, 4, 1.5, 0.5), font.main=1)
plot(V1 ~ decade, data=bytype[category=="cathedral", ],
    type='l', ylim=range(bytype$V1), bty='l', lwd=1.5, ylab = 'm3/20 year')
    lines(V1 ~ decade, data=bytype[category=="parish", ],
            type='l', ylim=range(bytype$V1), col='gray')
    lines(V1 ~ decade, data=bytype[category=="other", ],
            type='l', ylim=range(bytype$V1), col='gray')
    lines(V1 ~ decade, data=bytype[category=="conventual", ],
            type='l', ylim=range(bytype$V1), col='gray')
title(main="cathedral", line=-1)
plot(V1 ~ decade, data=bytype[category=="conventual", ],
    type='l', ylim=range(bytype$V1), bty='l', lwd=1.5, ylab = 'm3/20 year')
    lines(V1 ~ decade, data=bytype[category=="parish", ],
            type='l', ylim=range(bytype$V1), col='gray')
    lines(V1 ~ decade, data=bytype[category=="cathedral", ],
            type='l', ylim=range(bytype$V1), col='gray')
    lines(V1 ~ decade, data=bytype[category=="other", ],
            type='l', ylim=range(bytype$V1), col='gray')
title(main="conventual", line=-1)
plot(V1 ~ decade, data=bytype[category=="parish", ],
    type='l', ylim=range(bytype$V1), bty='l', lwd=1.5, ylab = 'm3/20 year')
    lines(V1 ~ decade, data=bytype[category=="cathedral", ],
            type='l', ylim=range(bytype$V1), col='gray')
    lines(V1 ~ decade, data=bytype[category=="other", ],
            type='l', ylim=range(bytype$V1), col='gray')
    lines(V1 ~ decade, data=bytype[category=="conventual", ],
            type='l', ylim=range(bytype$V1), col='gray')
title(main="parish", line=-1)
plot(V1 ~ decade, data=bytype[category=="other", ],
    type='l', ylim=range(bytype$V1), bty='l', lwd=1.5, ylab = 'm3/20 year')
    lines(V1 ~ decade, data=bytype[category=="parish", ],
            type='l', ylim=range(bytype$V1), col='lightgray')
    lines(V1 ~ decade, data=bytype[category=="cathedral", ],
            type='l', ylim=range(bytype$V1), col='lightgray')
    lines(V1 ~ decade, data=bytype[category=="conventual", ],
            type='l', ylim=range(bytype$V1), col='lightgray')
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

bytype[, dtotal := sum(V1), by=decade]
bytype[category=="other", list(decade, category, V1 / dtotal)]

eutotal = fullobs[data.table::between(decade, 700, 1500), list(im3y20 = base::sum(.SD, na.rm=T) / M), by=decade, .SDcols = impvrbs]

# m3 per 20y period en country
pdf('figs/europetotal_hc.pdf', height=6)
plot(eutotal, type='n', bty='l', ylab = 'm3/20 year')
abline(v=c(768, 1140, 1315, 1000, 1348), col='gray')
lines(eutotal, type='b', lwd=1.5, pch=1, cex=0.9)
dev.off()

out = fullobs[, list(im3c_imp = base::sum(.SD, na.rm=T) / M), by=list(T = (trunc((year - 1) / 100) + 1) * 100), .SDcols = impvrbs][
    fullobs[, list(im3c_noimp = base::sum(im3_ann, na.rm = T)), by=list(T = (trunc((year - 1) / 100) + 1) * 100)],
        on = 'T'][
    fullobs[, list(im3y20_imp = base::sum(.SD, na.rm=T) / M), by=list(T = decade), .SDcols = impvrbs],
        on = 'T'][
    fullobs[, list(im3y20_noimp = base::sum(im3_ann, na.rm = T)), by=list(T = decade)],
        on = 'T']
data.table::fwrite(out, "dat/eutotalcompare.csv")


eutotal[, anngrowth := annualised_growth(im3y20, delta = 19)]
# plot(anngrowth*100 ~ decade, data = eutotal, type = 'b')
# abline(h = c(0.5, 1), v = c(920, 960, 1060, 1180), col = 'gray')
annualised_growth(eutotal[decade == 920 | decade == 1320, im3y20], delta = 1320 - 921)
annualised_growth(eutotal[decade == 960 | decade == 1040, im3y20], delta = 1040 - 961)
annualised_growth(eutotal[decade == 1060 | decade == 1160, im3y20], delta = 1160 - 1061)
annualised_growth(eutotal[decade == 1180 | decade == 1240, im3y20], delta = 1240 - 1181)
annualised_growth(eutotal[decade == 1320 | decade == 1380, im3y20], delta = 1380 - 1321)
annualised_growth(eutotal[decade == 1380 | decade == 1440, im3y20], delta = 1440 - 1381)
annualised_growth(eutotal[decade == 1440 | decade == 1500, im3y20], delta = 1500 - 1441)

data.table::fwrite(eutotal, "dat/eutotals.csv")

midcent = copy(fullobs_sp)
midcent[, year := round(year / 100)  * 100] # for compatability w. century
midcent = midcent[data.table::between(year, 650, 1550), list(im3_cnt = base::sum(abs(.SD), na.rm=T) / M), by=list(city, year), .SDcols=impvrbs]
pcitobs = merge(midcent, siem, by=c('city', 'year'), all.x=T)

pdf("figs/geography_hc.pdf", height=3, width=8)
par(mfrow=c(1, 3), font.main=1, mar = c(4.5, 4, 1.5, 0.2))
# why mean? already total by city, now take mean for kind of city per
plot(pcitobs[coastal==1, mean(im3_cnt), by=year],
    bty = 'l', type='n', col='gray', ylab='mean m3/century', xlab = '')
lines(V1 ~ year, data=pcitobs[rivercanal==1, mean(im3_cnt), by=year], type='b', col='gray')
lines(V1 ~ year, data=pcitobs[landlocked==1, mean(im3_cnt), by=year], type='b', col='gray')
lines(V1 ~ year, data=pcitobs[coastal==1, mean(im3_cnt), by=year], type='b', lwd = 1.5)
title(main='Coastal', line = -0.3)
plot(pcitobs[coastal==1, mean(im3_cnt), by=year],
    bty = 'l', type='n', col='gray', ylab = '', xlab = 'century')
lines(V1 ~ year, data=pcitobs[coastal==1, mean(im3_cnt), by=year], type='b', col='gray')
lines(V1 ~ year, data=pcitobs[landlocked==1, mean(im3_cnt), by=year], type='b', col='gray')
lines(V1 ~ year, data=pcitobs[rivercanal==1, mean(im3_cnt), by=year], type='b', lwd = 1.5)
title(main='River/canal', line = -0.3)
plot(pcitobs[coastal==1, mean(im3_cnt), by=year],
    bty = 'l', type='n', col='gray', ylab = '', xlab = '')
lines(V1 ~ year, data=pcitobs[rivercanal==1, mean(im3_cnt), by=year], type='b', col='gray')
lines(V1 ~ year, data=pcitobs[coastal==1, mean(im3_cnt), by=year], type='b', col='gray')
lines(V1 ~ year, data=pcitobs[landlocked==1, mean(im3_cnt), by=year], type='b', lwd = 1.5)
title(main='Landlocked', line = -0.3)
dev.off()

pcitobs[rivercanal==1, mean(im3_cnt), by=year][year %in% c(900, 1200), list(year, annualised_growth(V1, delta = 300))]
pcitobs[coastal==1, mean(im3_cnt), by=year][year %in% c(900, 1200), list(year, annualised_growth(V1, delta = 300))]
pcitobs[landlocked==1, mean(im3_cnt), by=year][year %in% c(900, 1200), list(year, annualised_growth(V1, delta = 300))]

pcitobs[rivercanal==1, mean(im3_cnt), by=year][year %in% c(1200, 1500), list(year, annualised_growth(V1, delta = 300))]
pcitobs[coastal==1, mean(im3_cnt), by=year][year %in% c(1200, 1500), list(year, annualised_growth(V1, delta = 300))]
pcitobs[landlocked==1, mean(im3_cnt), by=year][year %in% c(1200, 1500), list(year, annualised_growth(V1, delta = 300))]

# exploratory regs
pcitobs = pcitobs[data.table::between(year, 700, 1500), ]
# plot(log(inhab) ~ log(im3_cnt), data=pcitobs)
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

# add ctr2 and ctr3 to siem
# note: not complete to all cities, should be redone
siem = unique(statobs[, list(ctr2, ctr3, city)])[siem, on = "city"]

# siem = pop[decade %% 100 == 0][siem[, c(.SD, list(decade = year))], on = c('ctr', 'decade')]

# x = fullobs_sp[!is.na(decade) & data.table:::between(decade, 700, 1500), ]
# first aggregate to city, then add siem
citobs_dec = fullobs_sp[data.table::between(decade, 700, 1500), list(m3dec = base::sum(.SD, na.rm=T) / M), by=list(decade, city, ctr, ctr2, ctr3), .SDcols = impvrbs]
# citobs_dec_cml = fullobs_sp[data.table::between(decade, 700, 1500), list(m3cml = sum(.SD, na.rm=T) / M), by=list(decade, city, ctr, ctr2, ctr3), .SDcols = grepr('im3_ann_cmc\\d', names(fullobs_sp))]
# d = citobs_dec_cml[citobs_dec, on = c('decade', 'city', 'ctr', 'ctr2', 'ctr3')]
# d[, stock := (m3cml - m3dec) * 200] # check citobs madness

# siem]
dim(citobs_dec)
citobs_dec = siem[, list(inhab, lat, lon, city, decade = year)][citobs_dec, on = c("city", "decade")]
dim(citobs_dec)

x1 = citobs_dec[, list(im3_dec = sum(m3dec), urb_inhab = sum(inhab)), by=list(decade, ctr)]
x2 = citobs_dec[, list(im3_dec = sum(m3dec), urb_inhab = sum(inhab)), by=list(decade, ctr2)]
x3 = citobs_dec[, list(im3_dec = sum(m3dec), urb_inhab = sum(inhab)), by=list(decade, ctr3)]
eu = citobs_dec[, list(im3_dec = sum(m3dec), urb_inhab = sum(inhab)), by=list(decade)]

# note: luxembourg already included under belgian total population
# fix urban population, if use total urbpop

# for total urbpop
# x1 = citobs_dec[, list(im3_dec = sum(m3dec)), by=list(decade, ctr)]
# x1 = siem[, list(urb_inhab = sum(inhab)), by = list(ctr, decade = year)][x1, on = c('ctr', 'decade')]
# x2 = citobs_dec[, list(im3_dec = sum(m3dec)), by=list(decade, ctr2)]
# x2 = siem[, list(urb_inhab = sum(inhab)), by = list(ctr2, decade = year)][x2, on = c('ctr2', 'decade')]
# x3 = citobs_dec[, list(im3_dec = sum(m3dec)), by=list(decade, ctr3)]
# x3 = siem[, list(urb_inhab = sum(inhab)), by = list(ctr3, decade = year)][x3, on = c('ctr3', 'decade')]
# eu = citobs_dec[, list(im3_dec = sum(m3dec)), by=list(decade)]
# eu = siem[grep("fr|be|ch|uk|nl|de", ctr), list(urb_inhab = sum(inhab)), by = list(decade = year)][eu, on = c('decade')]

x1 = pop_mj[, list(ctr, decade, pop)][x1, on = c('ctr', 'decade')]
eu = pop_mj[, list(ctr, decade, pop)][, list(pop = sum(pop)), by = decade][eu, on = 'decade']

x1 = gdp_new[year %% 20 == 0, list(ctr = tolower(iso2c), decade = year, cgdppc, rgdpnapc,
    pop_madison = pop, cgdppc_part, rgdpnapc_part)][
    x1, on = c('ctr', 'decade')]

x1[decade %% 100!=0, urb_inhab:=NA] # fix zeroes
x2[decade %% 100!=0, urb_inhab:=NA]
x3[decade %% 100!=0, urb_inhab:=NA]
eu[decade %% 100 != 0, urb_inhab := NA]
x1[, urb_inhab_i := round(exp(zoo::na.approx(log(urb_inhab)))), by=ctr]
x2[, urb_inhab_i := round(exp(zoo::na.approx(log(urb_inhab)))), by=ctr2]
x3[, urb_inhab_i := round(exp(zoo::na.approx(log(urb_inhab)))), by=ctr3]
eu[, urb_inhab_i := round(exp(zoo::na.approx(log(urb_inhab))))]

x1[, pop_i := exp(zoo::na.approx(log(pop))), by=ctr]
eu[, pop_i := exp(zoo::na.approx(log(pop)))]

x1[, sum(urb_inhab_i) / sum(pop_i), by = ctr]
x1[, sum(urb_inhab_i) / sum(pop_i), by = list(ctr, decade)][ctr=='nl']
x1[, sum(urb_inhab_i) / sum(pop_i), by = list(ctr, decade)][ctr=='be']
x1[!is.na(urb_inhab), sum(urb_inhab) / sum(pop), by = ctr]

out = rbindlist(list(x1, cbind(eu, ctr = 'eu')), fill = T)
writexl::write_xlsx(out, "excels/countryseries.xlsx")

write.csv(x3, "~/dropbox/cathedrals/dat/countryseries.csv", row.names=F)

x1[, im3_dec_smth:=predict(loess(im3_dec / urb_inhab_i ~ decade), newdata=decade), by=ctr]
x2[, im3_dec_smth:=predict(loess(im3_dec / urb_inhab_i ~ decade), newdata=decade), by=ctr2]
x3[, im3_dec_smth:=predict(loess(im3_dec / urb_inhab_i ~ decade), newdata=decade), by=ctr3]
eu[, im3_dec_smth:=predict(loess(im3_dec / urb_inhab_i ~ decade), newdata=decade)]

pdf('figs/europetotal_puc_hc.pdf', height=6)
par(mfrow=c(1, 1))
plot(im3_dec / urb_inhab_i ~ decade, data=eu, type='n', bty='l')
abline(v=c(768, 1000, 1140, 1315, 1348), col='gray')
lines(im3_dec / urb_inhab_i ~ decade, data=eu, type='b', col=2, lwd=1.5, pch=1, cex=0.9)
dev.off()

pdf('figs/europetotal_pc_hc.pdf', height=6)
par(mfrow=c(1, 1))
plot(im3_dec / pop_i ~ decade, data=eu, type='n', bty='l')
abline(v=c(768, 1000, 1140, 1315, 1348), col='gray')
lines(im3_dec / pop_i ~ decade, data=eu, type='b', col=2, lwd=1.5, pch=1, cex=0.9)
dev.off()

eucompare = eu[, list(decade, im3_dec, im3_dec_per_urb_capita = im3_dec / urb_inhab_i, im3_dec_per_capita = im3_dec / pop_i)]
eucompare[, 2:4] = eucompare[, lapply(.SD, function(x) x / x[decade==1200]), .SDcols = -1]

pdf("figs/eucompare.pdf", height=3, width=8)
par(mfrow=c(1, 3), font.main=1, mar = c(4.5, 4, 1.5, 0.2))
matplot(eucompare[, decade], eucompare[, 2:4], main = "Total",
    type = 'l', lty = 1, col = 'gray', bty = 'l',
    ylab = 'Building activity (index 1200 = 1)', xlab = 'decade')
lines(im3_dec ~ decade, data = eucompare, lwd = 1.5)
matplot(eucompare[, decade], eucompare[, 2:4], main = "Per capita",
    type = 'l', lty = 1, col = 'gray', bty = 'l',
    ylab = '', xlab = 'decade')
lines(im3_dec_per_capita ~ decade, data = eucompare, lwd = 1.5)
matplot(eucompare[, decade], eucompare[, 2:4], main = "Per urban capita",
    type = 'l', lty = 1, col = 'gray', bty = 'l',
    ylab = '', xlab = 'decade')
lines(im3_dec_per_urb_capita ~ decade, data = eucompare, lwd = 1.5)
dev.off()

yl = range(x1[ctr %in% c("fr", "de"), im3_dec / urb_inhab_i],
           x3[ctr3 %in% c("lc"), im3_dec / urb_inhab_i])
pdf("figs/pucpanel.pdf", width=8, height = 5)
par(mfrow=c(2, 3), mar=c(4, 4, 1.5, 0.5), font.main=1)
plot(im3_dec / urb_inhab_i ~ decade, data=x1[ctr=='fr', ], type='l', lwd = 1.5,
    bty='l', ylab='m3/20 year', xlab='', main='France', ylim = yl)
lines(im3_dec / urb_inhab_i ~ decade, data=eu, type='l', col='gray')
plot(im3_dec / urb_inhab_i ~ decade, data=x1[ctr %in% c('de', 'ch'), list(im3_dec = sum(im3_dec), urb_inhab_i = sum(urb_inhab_i)), by = decade], type='l', lwd = 1.5,
    bty='l', ylab='', xlab='', main='Germany (incl. Switzerland)', ylim = yl)
lines(im3_dec / urb_inhab_i ~ decade, data=eu, type='l', col='gray')
plot(im3_dec / urb_inhab_i ~ decade, data=x1[ctr=='uk', ], type='l', lwd = 1.5,
    bty='l', ylab='m3/20 year', main='Great Britain')
lines(im3_dec / urb_inhab_i ~ decade, data=eu, type='l', col='gray')
plot(im3_dec / urb_inhab_i ~ decade, data=x3[ctr3=='lc', ], type='l', lwd = 1.5,
    bty='l', ylab='', main='Low Countries', ylim = yl)
lines(im3_dec / urb_inhab_i ~ decade, data=eu, type='l', col='gray')
plot(im3_dec / urb_inhab_i ~ decade, data=x1[ctr=='it', ], type='l', lwd = 1.5,
    bty='l', ylab='m3/20 year', xlab='', main='Italy', ylim = yl)
lines(im3_dec / urb_inhab_i ~ decade, data=eu, type='l', col='gray')
dev.off()

yl1 = x1[ctr %in% c("fr", "de", "ch", "uk"), range(im3_dec / pop_i)]
# yl2 = x1[!ctr %in% c("fr", "de", "ch", "uk"), range(im3_dec / pop_i)]
yl2 = c(0, 320)
pdf("figs/pcpanel.pdf", width=8, height = 5)
par(mfrow=c(2, 3), mar=c(4, 4, 1.5, 0.5), font.main=1)
plot(im3_dec / pop_i ~ decade, data=x1[ctr=='fr', ], type='l',
    ylim = yl1, lwd = 1.5, bty='l',
    ylab='m3/20 years', xlab='', main='France')
lines(im3_dec / pop_i ~ decade, data=eu, type='l', col='gray')
plot(im3_dec / pop_i ~ decade, data=x1[ctr %in% c('de', "ch"), list(im3_dec = sum(im3_dec), pop_i = sum(pop_i)), by = decade], type='l',
    ylim = yl1, lwd = 1.5, bty='l',
    xlab='', ylab = '', main='Germany (incl. Switzerland)')
lines(im3_dec / pop_i ~ decade, data=eu, type='l', col='gray')
plot(im3_dec / pop_i ~ decade, data=x1[ctr=='uk', ], type='l',
    ylim = yl1, lwd = 1.5, bty='l',
    xlab = 'decade', ylab = 'm3/20 years', main='Great Britain')
lines(im3_dec / pop_i ~ decade, data=eu, type='l', col='gray')
plot(im3_dec / pop_i ~ decade, data=x1[ctr %in% c("nl", "be"), list(im3_dec = sum(im3_dec), pop_i = sum(pop_i)), by = decade], type='l',
    ylim = yl2, lwd = 1.5, bty='l',
    xlab = 'decade', ylab = '', main='Low Countries')
lines(im3_dec / pop_i ~ decade, data=eu, type='l', col='gray')
plot(im3_dec / pop_i ~ decade, data=x1[ctr=='it', ], type='l',
    ylim = yl2, lwd = 1.5, bty='l',
    ylab='m3/20 years', xlab='', main='Italy')
lines(im3_dec / pop_i ~ decade, data=eu, type='l', col='gray')
dev.off()

data.table::fwrite(rbindlist(list(x1, x1[ctr %in% c("nl", "be"), list(im3_dec = sum(im3_dec), pop_i = sum(pop_i)), by = decade], eu[, c(.SD, list(ctr = 'eu'))]),
    fill = T), "dat/totpcpuc.csv")
data.table::fwrite(x3, "dat/totpuc_regions.csv")

pdf('figs/italy_panel_hc.pdf', height=4, width=9)
par(mfrow=c(1, 3), font.main=1)
yl = c(0, max(x1[ctr=='it', im3_dec]))
plot(im3_dec ~ decade, data=x1[ctr=='it', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Italy, total', col=2)
plot(im3_dec ~ decade, data=x3[ctr3=='it_south', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Italy, south', col=2)
lines(im3_dec ~ decade, data=x1[ctr=='it', ], col='gray70')
plot(im3_dec ~ decade, data=x3[ctr3=='it_north', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Italy, north', col=2)
lines(im3_dec ~ decade, data=x1[ctr=='fr', ], col='gray70')
dev.off()

pdf('figs/italy_panel_puc_hc.pdf', height=4, width=9)
par(mfrow=c(1, 3), font.main=1)
yl = range(x3[grep('it', ctr3), im3_dec / urb_inhab_i])
plot(im3_dec / urb_inhab_i ~ decade, data=x1[ctr=='it', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Italy, total', col=2)
plot(im3_dec / urb_inhab_i ~ decade, data=x3[ctr3=='it_south', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Italy, south', col=2)
lines(im3_dec / urb_inhab_i ~ decade, data=x1[ctr=='it', ], col='gray70')
plot(im3_dec / urb_inhab_i ~ decade, data=x3[ctr3=='it_north', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Italy, north', col=2)
lines(im3_dec / urb_inhab_i ~ decade, data=x1[ctr=='fr', ], col='gray70')
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
