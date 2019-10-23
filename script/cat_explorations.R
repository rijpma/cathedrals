# time series plots of church building activity

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

impvrbs = names(fullobs_sp)[grepl('im3_ann\\d', names(fullobs_sp))]

ukgdp = data.table::fread("dat/engdp12001700.csv", skip=1, encoding="UTF-8")   
gdp = data.table::fread("dat/maddison_gdp_old.csv")
gdp_new = data.table::fread("dat/gddnew.csv")
pop_mj = data.table::fread("dat/pop_mj.csv")

siem = data.table::fread("dat/siem_long_1500.csv", encoding="UTF-8")

# add ctr2 and ctr3 to siem
# note: does not cover all cities (only those w churches)
siem = unique(statobs[, list(ctr2, ctr3, city)])[siem, on = "city"]

# heaping and its correction
# --------------------------
h_heap = hist(dynobs[between(year, 700, 1500)][, year], breaks=data.table::uniqueN(dynobs[between(year, 700, 1500)]$year), plot = F)
h_unheap = hist(dynobs[between(year, 700, 1500)][, year_crc1], breaks=data.table::uniqueN(dynobs[between(year, 700, 1500)]$year_crc1), plot = F)
h_heap$counts = log1p(h_heap$counts)
h_unheap$counts = log1p(h_unheap$counts)

pdf("figs/distrs_prepost_heap.pdf", width=11, height = 6)
par(mfrow=c(1, 2), font.main = 1)
plot(h_heap, main='heaping', xlab='year', yaxt = 'n')
axis(2, labels = c(1, 10, 100), at = log(c(1, 10, 100)))
plot(h_heap, main='heaping resampled', xlab='year', yaxt = 'n', border = NA)
lines(h_unheap)
axis(2, labels = c(1, 10, 100), at = log(c(1, 10, 100)))
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

cat("heaped and non-heaped 700-1500: ")
sum(fullobs[data.table::between(year, 700, 1500), sum(im3_ann, na.rm=T), by=year])
sum(fullobs[data.table::between(year, 700, 1500), sum(.SD, na.rm=T) / M, by=year, .SDcols=grep("im3_ann\\d", names(fullobs))])
cat("heaped and non-heaped 700-1800: ")
sum(fullobs[, sum(im3_ann, na.rm=T), by=year])
sum(fullobs[, sum(.SD, na.rm=T) / M, by=year, .SDcols=grep("im3_ann\\d", names(fullobs))])

# data range from imputations
x = fullobs[data.table::between(year, 700, 1500), 
    lapply(.SD, sum, na.rm = TRUE), 
    .SDcols = impvrbs, 
    by = decade ][, 
        .(base::min(.SD), base::max(.SD)), 
        .SDcols = impvrbs, 
        by = decade]

cat("average end-size church: \n")
dynobs[, list(m3_end=sum(m3, na.rm=T)), by=paste(osmid, bldindex)][, mean(m3_end)]
cat("average end-size final church: \n")
dynobs[, list(m3_end=sum(m3[bldindex==max(bldindex)], na.rm=T)), by=osmid][, mean(m3_end)]

# distribution observations
# -------------------------
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

# w. european totals
eutotal = fullobs[data.table::between(decade, 700, 1500), list(im3y20 = base::sum(.SD, na.rm=T) / M / 1e6), by=decade, .SDcols = impvrbs]

# m3 per 20y period en country
pdf('figs/europetotal_hc.pdf', height = 6)
plot(eutotal, type='n', bty='l', ylab = m3y20lblm)
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

# growth rates
cat("annualised growth rate 960-1260: ")
annualised_growth(eutotal[decade == 960 | decade == 1260, im3y20], delta = 1260 - 961)
cat("annualised growth rate 960-1040: ")
annualised_growth(eutotal[decade == 960 | decade == 1040, im3y20], delta = 1040 - 961)
cat("annualised growth rate 1060-1140: ")
annualised_growth(eutotal[decade == 1060 | decade == 1140, im3y20], delta = 1140 - 1061)
cat("annualised growth rate 1180-1260: ")
annualised_growth(eutotal[decade == 1180 | decade == 1260, im3y20], delta = 1260 - 1181)
cat("annualised growth rate 1320-1400: ")
annualised_growth(eutotal[decade == 1320 | decade == 1400, im3y20], delta = 1400 - 1321)
cat("annualised growth rate 1380-1460: ")
annualised_growth(eutotal[decade == 1380 | decade == 1460, im3y20], delta = 1460 - 1381)
cat("annualised growth rate 1460-1500: ")
annualised_growth(eutotal[decade == 1460 | decade == 1500, im3y20], delta = 1500 - 1461)

data.table::fwrite(eutotal, "dat/eutotals.csv")

# by type of church
# -----------------

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

bytype = fullobs_sp[
    data.table::between(year, 700, 1500),
    list(
        cathedral = base::sum(.SD[category == "cathedral"], na.rm=T) / M / 1e6,
        parish = base::sum(.SD[category == "parish"], na.rm=T) / M / 1e6,
        conventual = base::sum(.SD[category == "conventual"], na.rm=T) / M / 1e6,
        other = base::sum(.SD[category == "other"], na.rm=T) / M / 1e6),
    by = list(decade), 
    .SDcols = impvrbs]

# cmladd = fullobs_sp[data.table::between(year, 700, 1500), base::sum(.SD * 0.005, na.rm=T) / M, by=list(decade, category), .SDcols = grepr('im3_cml\\d', names(fullobs_sp))]
# bytype[, V1 := V1 + cmladd$V1]
cat("percentages church building by type:\n")
bytype[decade %% 100 == 0, .SD / rowSums(.SD), by = decade]

pdf("figs/bytype_hc.pdf", width = 8)
par(mfrow=c(2, 2), mar=c(4, 4, 1.5, 0.5), font.main=1)
matplot(bytype[, 1], bytype[, -1], lty = 1, type='l', bty = 'l',
    xlab = 'year', ylab = m3y20lblm, col='gray')
lines(cathedral ~ decade, data = bytype, lwd=1.5)
title(main="cathedral", line=-1)

matplot(bytype[, 1], bytype[, -1], lty = 1, type='l', bty = 'l',
    xlab = 'year', ylab = "", col='gray')
lines(parish ~ decade, data = bytype, lwd=1.5)
title(main="parish", line=-1)

matplot(bytype[, 1], bytype[, -1], lty = 1, type='l', bty = 'l',
    xlab = 'year', ylab = m3y20lblm, col='gray')
lines(conventual ~ decade, data = bytype, lwd=1.5)
title(main="conventual", line=-1)

matplot(bytype[, 1], bytype[, -1], lty = 1, type='l', bty = 'l',
    xlab = 'year', ylab = "", col='gray')
lines(other ~ decade, data = bytype, lwd=1.5)
title(main="other", line=-1)
dev.off()

pdf('figs/mendicants_hc.pdf', height = 4)
bytype = fullobs_sp[data.table::between(year, 700, 1500) & category == "conventual", base::sum(.SD, na.rm=T) / M, by=list(decade, category, mendicants), .SDcols = impvrbs]
# cmladd = fullobs_sp[data.table::between(year, 700, 1500), base::sum(.SD * 0.005, na.rm=T) / M, by=list(decade, category, mendicants), .SDcols = grepr('im3_cml\\d', names(fullobs_sp))]
# bytype = bytype[, V1 := V1 + cmladd$V1][category == "conventual"]
par(mfrow = c(1, 2), mar=c(4, 4, 1.5, 0.5))
plot(V1 ~ decade, data=bytype[mendicants == 1, ],
    type='n', bty='l', col=2, lwd=1.5, ylab = 'm3/20y', main = 'Mendicant')
lines(V1 ~ decade, data=bytype[mendicants == 1, ],
    type='l', bty='l', col=2, lwd=1.5)
plot(V1 ~ decade, data=bytype[mendicants == 1, ],
    type='n', bty='l', col=2, lwd=1.5, ylab = 'm3/20y', main = 'Other')
lines(V1 ~ decade, data=bytype[mendicants == 0, ],
    type='l', bty='l', col=2, lwd=1.5)
dev.off()

# building by city geography
# --------------------------

midcent = copy(fullobs_sp)
midcent[, year := round(year / 100)  * 100] # for compatability w. century
midcent = midcent[data.table::between(year, 650, 1550), list(im3_cnt = base::sum(abs(.SD), na.rm=T) / M), by=list(city, year), .SDcols=impvrbs]
pcitobs = merge(midcent, siem, by=c('city', 'year'), all.x=T)

geography = pcitobs[, list(rivercanal = mean(im3_cnt[rivercanal == 1], na.rm = T),
              coastal = mean(im3_cnt[coastal == 1], na.rm = T),
              landlocked = mean(im3_cnt[landlocked == 1], na.rm = T)), by = year]

# correct that some italian cities have no basin (all others do)
pcitobs[ctr == "it" &  mediterranean == 0, mediterranean := 1]
geography_north = pcitobs[, list(
              `Landlocked, Northern` = mean(im3_cnt[landlocked == 1 & mediterranean == 0], na.rm = T),
              `Rivercanal, Northern` = mean(im3_cnt[rivercanal == 1 & mediterranean == 0], na.rm = T),
              `Coastal, Northern` = mean(im3_cnt[coastal == 1 & mediterranean == 0], na.rm = T)),
    by = list(year)]
geography_south = pcitobs[, list(
              `Landlocked, Mediterranean` = mean(im3_cnt[landlocked == 1 & mediterranean == 1], na.rm = T),
              `Rivercanal, Mediterranean` = mean(im3_cnt[rivercanal == 1 & mediterranean == 1], na.rm = T),
              `Coastal, Mediterranean` = mean(im3_cnt[coastal == 1 & mediterranean == 1], na.rm = T)),
    by = list(year)]

pdf("figs/geography_hc.pdf", height=3, width=8)
par(mfrow=c(1, 3), font.main=1, mar = c(4.5, 4, 1.5, 0.2))
matplot(geography[, 1], geography[, -1], 
    bty = 'l', type='b', col='gray', lty = 1, pch = 1,
    ylab = m3y100lbl, 
    xlab = '')
lines(coastal ~ year, data = geography, type='b', lwd = 1.5)
title(main='Coastal', line = -0.3)
matplot(geography[, 1], geography[, -1], 
    bty = 'l', type='b', col='gray', lty = 1, pch = 1,
    # ylab = m3y100lbl, 
    xlab = 'century')
lines(rivercanal ~ year, data = geography, type='b', lwd = 1.5)
title(main='River/canal', line = -0.3)
matplot(geography[, 1], geography[, -1], 
    bty = 'l', type='b', col='gray', lty = 1, pch = 1,
    # ylab = m3y100lbl, 
    xlab = '')
lines(landlocked ~ year, data = geography, type='b', lwd = 1.5)
title(main='Landlocked', line = -0.3)
dev.off()

cat("annualised growth 900-1200:\n")
geography[year %in% c(900, 1200), 
    lapply(.SD, annualised_growth, delta = 300),
    .SDcols = -"year"]
cat("annualised growth North 900-1200:\n")
geography_north[year %in% c(900, 1200), 
    lapply(.SD, annualised_growth, delta = 300),
    .SDcols = -"year"]
cat("annualised growth South 900-1200:\n")
geography_south[year %in% c(900, 1200), 
    lapply(.SD, annualised_growth, delta = 300),
    .SDcols = -"year"]
cat("annualised growth 900-1200:\n")
geography[year %in% c(1200, 1500), 
    lapply(.SD, annualised_growth, delta = 300),
    .SDcols = -"year"]

pdf("figs/bygeography_split.pdf", width = 9, height = 6)
yl = range(c(geography_north[, -1], geography_south[, -1]))
par(mfrow = c(2, 3), mar = c(4.5, 4, 1.5, 0.2), font.main = 1)
for (geo in names(geography_north)[-1]){
    matplot(geography_north[, year], geography_north[, -1, with = F], 
        bty = 'l', type='l', col='gray', lty = 1, ylim = yl,
        yaxt = "n",
        ylab = ifelse(geo == names(geography_north)[-1][1], m3y100lbl, ""),
        xlab = 'century', 
        main = geo)
    lines(geography_north[, .(year, get(geo))], lwd = 1.5, type = 'b')
    axis1ks(side = 2)
}
for (geo in names(geography_south)[-1]){
    matplot(geography_south[, year], geography_south[, -1, with = F], 
        bty = 'l', type='l', col='gray', lty = 1, ylim = yl,
        yaxt = "n",
        ylab = ifelse(geo == names(geography_south)[-1][1], m3y100lbl, ""), 
        xlab = 'century', 
        main = geo)
    lines(geography_south[, .(year, get(geo))], lwd = 1.5, type = 'b')
    axis1ks(side = 2)
}
dev.off()

# totals per region
# -----------------

# first aggregate to city, then add siem
citobs_dec = fullobs_sp[data.table::between(decade, 700, 1500), list(m3dec = base::sum(.SD, na.rm=T) / M), by=list(decade, city, ctr, ctr2, ctr3), .SDcols = impvrbs]

dim(citobs_dec)
citobs_dec = siem[, list(inhab, lat, lon, city, decade = year)][citobs_dec, on = c("city", "decade")]
dim(citobs_dec)

byregion = citobs_dec[data.table::between(decade, 701, 1500), 
    list(
        `S Italy` = sum(m3dec[ctr3 == "it_south"]), 
        `N Italy` = sum(m3dec[ctr3 == "it_north"]),
        `South France` = sum(m3dec[ctr3 == "fr_south"]), 
        `North France` = sum(m3dec[ctr3 == "fr_north"]), 
        `Switzerland` = sum(m3dec[ctr == "ch"]), 
        `SW Germany` = sum(m3dec[ctr3 == "de_sw"]), 
        `NE Germany` = sum(m3dec[ctr3 == "de_ne"]), 
        `Belgium + Luxembourg` = sum(m3dec[ctr == "be"]), 
        `Netherlands` = sum(m3dec[ctr == "nl"]), 
        `SE England` = sum(m3dec[ctr3 == "uk_se"]), 
        `Rest Great Britain` = sum(m3dec[ctr3 == "uk_nw"])), 
    by = list(century = paste0(century = (trunc((decade - 1) / 100) + 1), "th"))]
byregion[, `SW Germany` := `SW Germany` - Switzerland]
byregion = rapply(byregion, function(x) x / 1e6, class = "numeric", how = "replace")
centuries = byregion$century
byregion = byregion[, data.table(t(.SD), keep.rownames = T), .SDcols = -"century"]
setnames(byregion, names(byregion), c("Region", centuries))
byregion = rbindlist(list(
        byregion, 
        byregion[, lapply(.SD, sum), .SDcols = -1]),
    fill = TRUE)

writeLines(knitr::kable(byregion, digits = 1, format = "html"),
    "tab/regiontable.html")

mean11001500 = byregion[, mean(unlist(.SD)), .SDcols = c("11th", "12th", "13th", "14th", "15th"), by = Region]
byregionindices = mean11001500[byregion, on = "Region"][, .SD / V1 * 100, by = Region]
colmean11001500 = byregion[, mean(colSums(.SD)), .SDcols = c("11th", "12th", "13th", "14th", "15th")]

writeLines(knitr::kable(byregionindices[, -"V1"], digits = 0, format = "html"),
    "tab/regionindices.html")

# per (urban) capita series
# -------------------------
byctr = citobs_dec[, list(im3_dec = sum(m3dec), urb_inhab = sum(inhab)), by=list(decade, ctr)]
byctr2 = citobs_dec[, list(im3_dec = sum(m3dec), urb_inhab = sum(inhab)), by=list(decade, ctr2)]
byctr3 = citobs_dec[, list(im3_dec = sum(m3dec), urb_inhab = sum(inhab)), by=list(decade, ctr3)]
eu = citobs_dec[, list(im3_dec = sum(m3dec), urb_inhab = sum(inhab)), by=list(decade)]
# luxembourg already included under belgian total population

# citobs above has total pop for cities w. church only
# for total urbpop of country:
# byctr = citobs_dec[, list(im3_dec = sum(m3dec)), by=list(decade, ctr)]
# byctr = siem[, list(urb_inhab = sum(inhab)), by = list(ctr, decade = year)][byctr, on = c('ctr', 'decade')]
# byctr2 = citobs_dec[, list(im3_dec = sum(m3dec)), by=list(decade, ctr2)]
# byctr2 = siem[, list(urb_inhab = sum(inhab)), by = list(ctr2, decade = year)][byctr2, on = c('ctr2', 'decade')]
# byctr3 = citobs_dec[, list(im3_dec = sum(m3dec)), by=list(decade, ctr3)]
# byctr3 = siem[, list(urb_inhab = sum(inhab)), by = list(ctr3, decade = year)][byctr3, on = c('ctr3', 'decade')]
# eu = citobs_dec[, list(im3_dec = sum(m3dec)), by=list(decade)]
# eu = siem[grep("fr|be|ch|uk|nl|de", ctr), list(urb_inhab = sum(inhab)), by = list(decade = year)][eu, on = c('decade')]

byctr = pop_mj[data.table::between(decade, 700, 1500), list(ctr, decade, pop, pop_spl)][byctr, on = c('ctr', 'decade')]
eu = pop_mj[data.table::between(decade, 700, 1500), list(ctr, decade, pop, pop_spl)][, list(pop = sum(pop), pop_spl = sum(pop_spl)), by = decade][eu, on = 'decade']

byctr = gdp_new[year %% 20 == 0, list(ctr = tolower(iso2c), decade = year, cgdppc, rgdpnapc,
    pop_madison = pop, cgdppc_part, rgdpnapc_part)][
    byctr, on = c('ctr', 'decade')]

byctr[decade %% 100 != 0, urb_inhab := NA] # fix zeroes
byctr2[decade %% 100 != 0, urb_inhab := NA]
byctr3[decade %% 100 != 0, urb_inhab := NA]
eu[decade %% 100 != 0, urb_inhab := NA]

byctr[, urb_inhab_i := round(exp(zoo::na.approx(log(urb_inhab)))), by=ctr]
byctr2[, urb_inhab_i := round(exp(zoo::na.approx(log(urb_inhab)))), by=ctr2]
byctr3[, urb_inhab_i := round(exp(zoo::na.approx(log(urb_inhab)))), by=ctr3]
eu[, urb_inhab_i := round(exp(zoo::na.approx(log(urb_inhab))))]

byctr[, pop_i := exp(zoo::na.approx(log(pop))), by=ctr]
eu[, pop_i := exp(zoo::na.approx(log(pop)))]

out = rbindlist(list(byctr, cbind(eu, ctr = 'eu')), fill = T)
writexl::write_xlsx(
    out[, list(ctr, decade, im3_dec, pop, pop_i, urb_inhab, urb_inhab_i)], 
    "excels/countryseries.xlsx")

write.csv(byctr3, "~/dropbox/cathedrals/dat/countryseries.csv", row.names=F)

byctr[, im3_dec_smth:=predict(loess(im3_dec / urb_inhab_i ~ decade), newdata=decade), by=ctr]
byctr2[, im3_dec_smth:=predict(loess(im3_dec / urb_inhab_i ~ decade), newdata=decade), by=ctr2]
byctr3[, im3_dec_smth:=predict(loess(im3_dec / urb_inhab_i ~ decade), newdata=decade), by=ctr3]
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

pdf("figs/pucpanel.pdf", width = 9, height = 6)
yl = range(byctr[ctr %in% c("fr", "de", "it"), im3_dec / urb_inhab_i],
           byctr3[ctr3 %in% c("lc"), im3_dec / urb_inhab_i])
par(mfrow=c(2, 3), mar=c(4, 4, 1.5, 0.5), font.main=1)
plot(im3_dec / urb_inhab_i ~ decade, data=byctr[ctr=='it', ], type='l', lwd = 1.5,
    bty='l', yaxt = "n",
    ylab = m3y20puclbl, main='Italy', ylim = yl)
axis1ks(2)
lines(im3_dec / urb_inhab_i ~ decade, data=eu, type='l', col='gray')
plot(im3_dec / urb_inhab_i ~ decade, data=byctr[ctr=='fr', ], type='l', lwd = 1.5,
    bty='l', yaxt = "n",
    ylab = "", main='France', ylim = yl)
axis1ks(2)
lines(im3_dec / urb_inhab_i ~ decade, data=eu, type='l', col='gray')
plot(im3_dec / urb_inhab_i ~ decade, data=byctr[ctr %in% c('de', 'ch'), list(im3_dec = sum(im3_dec), urb_inhab_i = sum(urb_inhab_i)), by = decade], type='l', lwd = 1.5,
    bty='l', yaxt = "n",
    ylab = "", main='Germany (incl. Switzerland)', ylim = yl)
axis1ks(2)
lines(im3_dec / urb_inhab_i ~ decade, data=eu, type='l', col='gray')

plot(im3_dec / urb_inhab_i ~ decade, data=byctr3[ctr3=='lc', ], type='l', lwd = 1.5,
    bty='l', yaxt = "n",
    ylab = m3y20puclbl, main = 'Low Countries', ylim = yl)
axis1ks(2)
lines(im3_dec / urb_inhab_i ~ decade, data=eu, type='l', col='gray')
plot(im3_dec / urb_inhab_i ~ decade, data=byctr[ctr=='uk', ], type='l', lwd = 1.5,
    bty='l', yaxt = "n",
    ylab = "", main='Great Britain')
axis1ks(2)
lines(im3_dec / urb_inhab_i ~ decade, data=eu, type='l', col='gray')
plot(im3_dec / urb_inhab_i ~ decade, data=eu, type='l', lwd = 1.5,
    bty='l', yaxt = "n",
    ylab = "", main='W. Europe', ylim = yl)
axis1ks(2)
dev.off()

pdf("figs/pcpanel.pdf", width = 9, height = 6)
yl1 = byctr[ctr %in% c("fr", "de", "ch", "uk"), range(im3_dec / pop_i)]
yl2 = byctr[ctr %in% c("nl", "be"), sum(im3_dec) / sum(pop_i), by = decade][, range(V1)]
par(mfrow=c(2, 3), mar=c(4, 4, 1.5, 0.5), font.main=1)
plot(im3_dec / pop_i ~ decade, 
    data = byctr[ctr=='it', ], type='l',
    ylim = yl2, lwd = 1.5, bty='l',
    ylab = m3y20pclbl, main='Italy')
lines(im3_dec / pop_i ~ decade, data=eu, type='l', col='gray')
plot(im3_dec / pop_i ~ decade, 
    data = byctr[ctr=='fr', ], type='l',
    ylim = yl1, lwd = 1.5, bty='l',
    ylab = "", main='France')
lines(im3_dec / pop_i ~ decade, data=eu, type='l', col='gray')
plot(im3_dec / pop_i ~ decade, 
    data = byctr[ctr %in% c('de', "ch"), 
        list(im3_dec = sum(im3_dec), pop_i = sum(pop_i)), 
        by = decade], type='l',
    ylim = yl1, lwd = 1.5, bty='l',
    ylab = "", main = 'Germany (incl. Switzerland)')
lines(im3_dec / pop_i ~ decade, data=eu, type='l', col='gray')
plot(im3_dec / pop_i ~ decade, 
    data = byctr[ctr %in% c("nl", "be"), 
        list(im3_dec = sum(im3_dec), pop_i = sum(pop_i)), 
        by = decade], type='l',
    ylim = yl2, lwd = 1.5, bty='l',
    ylab = m3y20pclbl, main='Low Countries')
lines(im3_dec / pop_i ~ decade, data=eu, type='l', col='gray')
plot(im3_dec / pop_i ~ decade, 
    data = byctr[ctr=='uk', ], type='l',
    ylim = yl1, lwd = 1.5, bty='l',
    ylab = "", main='Great Britain')
lines(im3_dec / pop_i ~ decade, data=eu, type='l', col='gray')
plot(im3_dec / pop_i ~ decade, data=eu, type='l',
    ylim = yl1, lwd = 1.5, bty='l',
    ylab = "", main='W. Europe')
dev.off()

data.table::fwrite(
    rbindlist(
        list(byctr, 
            byctr[ctr %in% c("nl", "be"), 
                list(im3_dec = sum(im3_dec), pop_i = sum(pop_i)), 
                by = decade], 
            eu[, c(.SD, list(ctr = 'eu'))]),
        fill = T), 
    file = "dat/totpcpuc.csv")
data.table::fwrite(byctr3, "dat/totpuc_regions.csv")

pdf('figs/italy_panel_hc.pdf', height=4, width=9)
par(mfrow=c(1, 3), font.main=1)
yl = c(0, max(byctr[ctr=='it', im3_dec]))
plot(im3_dec ~ decade, data=byctr[ctr=='it', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Italy, total', col=2)
plot(im3_dec ~ decade, data=byctr3[ctr3=='it_south', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Italy, south', col=2)
lines(im3_dec ~ decade, data=byctr[ctr=='it', ], col='gray70')
plot(im3_dec ~ decade, data=byctr3[ctr3=='it_north', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Italy, north', col=2)
lines(im3_dec ~ decade, data=byctr[ctr=='fr', ], col='gray70')
dev.off()

pdf('figs/italy_panel_puc_hc.pdf', height=4, width=9)
par(mfrow=c(1, 3), font.main=1)
yl = range(byctr3[grep('it', ctr3), im3_dec / urb_inhab_i])
plot(im3_dec / urb_inhab_i ~ decade, data=byctr[ctr=='it', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Italy, total', col=2)
plot(im3_dec / urb_inhab_i ~ decade, data=byctr3[ctr3=='it_south', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Italy, south', col=2)
lines(im3_dec / urb_inhab_i ~ decade, data=byctr[ctr=='it', ], col='gray70')
plot(im3_dec / urb_inhab_i ~ decade, data=byctr3[ctr3=='it_north', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Italy, north', col=2)
lines(im3_dec / urb_inhab_i ~ decade, data=byctr[ctr=='fr', ], col='gray70')
dev.off()

pdf('figs/france_panel_hc.pdf', height=4, width=9)
par(mfrow=c(1, 3), font.main=1)
yl = range(byctr[ctr=='fr', im3_dec])
plot(im3_dec ~ decade, data=byctr[ctr=='fr', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Fr., total', col=2)
plot(im3_dec ~ decade, data=byctr3[ctr3=='fr_south', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Fr., south', col=2)
lines(im3_dec ~ decade, data=byctr[ctr=='fr', ], col='gray70')
plot(im3_dec ~ decade, data=byctr3[ctr3=='fr_north', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Fr., north', col=2)
lines(im3_dec ~ decade, data=byctr[ctr=='fr', ], col='gray70')
dev.off()

pdf('figs/france_panel_puc_hc.pdf', height=4, width=9)
par(mfrow=c(1, 3), font.main=1)
yl = range(byctr[ctr=='fr', im3_dec / urb_inhab_i]) * 1.1
plot(im3_dec / urb_inhab_i ~ decade, data=byctr[ctr=='fr', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Fr., total', col=2)
plot(im3_dec / urb_inhab_i ~ decade, data=byctr3[ctr3=='fr_south', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Fr., south', col=2)
lines(im3_dec / urb_inhab_i ~ decade, data=byctr[ctr=='fr', ], col='gray70')
plot(im3_dec / urb_inhab_i ~ decade, data=byctr3[ctr3=='fr_north', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Fr., north', col=2)
lines(im3_dec / urb_inhab_i ~ decade, data=byctr[ctr=='fr', ], col='gray70')
dev.off()

pdf('figs/germany_panel_hc.pdf', height=4, width=9)
par(mfrow=c(1, 3), font.main=1)
yl = range(byctr[grep('de', ctr), im3_dec])
plot(im3_dec ~ decade, data=byctr[ctr=='de', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='de., total', col=2)
plot(im3_dec ~ decade, data=byctr3[ctr3=='de_sw', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='de., south-west', col=2)
lines(im3_dec ~ decade, data=byctr[ctr=='de', ], col='gray70')
plot(im3_dec ~ decade, data=byctr3[ctr3=='de_ne', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='de., north-east', col=2)
lines(im3_dec ~ decade, data=byctr[ctr=='de', ], col='gray70')
dev.off()

pdf('figs/germany_panel_puc_hc.pdf', height=4, width=9)
par(mfrow=c(1, 3), font.main=1)
yl = range(byctr3[grep('de', ctr3), im3_dec / urb_inhab_i])
plot(im3_dec / urb_inhab_i ~ decade, data=byctr[ctr=='de', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='de., total', col=2)
plot(im3_dec / urb_inhab_i ~ decade, data=byctr3[ctr3=='de_sw', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='de., south-west', col=2)
lines(im3_dec / urb_inhab_i ~ decade, data=byctr[ctr=='de', ], col='gray70')
plot(im3_dec / urb_inhab_i ~ decade, data=byctr3[ctr3=='de_ne', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='de., north-east', col=2)
lines(im3_dec / urb_inhab_i ~ decade, data=byctr[ctr=='de', ], col='gray70')
dev.off()

pdf('figs/britain_panel_hc.pdf', height=4, width=9)
par(mfrow=c(1, 3), font.main=1)
yl = range(byctr[grep('uk', ctr), im3_dec])
plot(im3_dec ~ decade, data=byctr[ctr=='uk', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='UK, total', col=2)
plot(im3_dec ~ decade, data=byctr3[ctr3=='uk_se', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='UK, south-east', col=2)
lines(im3_dec ~ decade, data=byctr[ctr=='uk', ], col='gray70')
plot(im3_dec ~ decade, data=byctr3[ctr3=='uk_nw', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='UK, north-west', col=2)
lines(im3_dec ~ decade, data=byctr[ctr=='uk', ], col='gray70')
dev.off()

pdf('figs/britain_panel_puc_hc.pdf', height=4, width=9)
par(mfrow=c(1, 3), font.main=1)
yl = range(byctr3[grep('uk', ctr3), im3_dec / urb_inhab_i])
plot(im3_dec / urb_inhab_i ~ decade, data=byctr[ctr=='uk', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='UK, total', col=2)
plot(im3_dec / urb_inhab_i ~ decade, data=byctr3[ctr3=='uk_se', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='UK, south-east', col=2)
lines(im3_dec / urb_inhab_i ~ decade, data=byctr[ctr=='uk', ], col='gray70')
plot(im3_dec / urb_inhab_i ~ decade, data=byctr3[ctr3=='uk_nw', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='UK, north-west', col=2)
lines(im3_dec / urb_inhab_i ~ decade, data=byctr[ctr=='uk', ], col='gray70')
dev.off()

pdf('figs/lowctr_panel_hc.pdf', height=4, width=9)
par(mfrow=c(1, 3), font.main=1)
yl = range(byctr3[grep('lc', ctr3), im3_dec])
plot(im3_dec ~ decade, data=byctr3[ctr3=='lc', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='LC, total', col=2)
plot(im3_dec ~ decade, data=byctr[ctr=='be', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Belgium', col=2)
lines(im3_dec ~ decade, data=byctr3[ctr3=='lc', ], col='gray70')
plot(im3_dec ~ decade, data=byctr[ctr=='nl', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Netherlands', col=2)
lines(im3_dec ~ decade, data=byctr3[ctr3=='lc', ], col='gray70')
dev.off()

pdf('figs/lowctr_panel_puc_hc.pdf', height=4, width=9)
par(mfrow=c(1, 3), font.main=1)
yl = range(byctr[grep('nl|be', ctr), im3_dec / urb_inhab_i])
plot(im3_dec / urb_inhab_i ~ decade, data=byctr3[ctr3=='lc', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='LC, total', col=2)
plot(im3_dec / urb_inhab_i ~ decade, data=byctr[ctr=='be', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Belgium', col=2)
lines(im3_dec / urb_inhab_i ~ decade, data=byctr3[ctr3=='lc', ], col='gray70')
plot(im3_dec / urb_inhab_i ~ decade, data=byctr[ctr=='nl', ], type='l', ylim=yl, bty='l', ylab='m3/dec', main='Netherlands', col=2)
lines(im3_dec / urb_inhab_i ~ decade, data=byctr3[ctr3=='lc', ], col='gray70')
dev.off()
