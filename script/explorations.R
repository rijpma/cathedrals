# time series plots of church building activity

rm(list = ls())
setwd("~/dropbox/cathedrals")
source("script/cat_functions.r")

library("data.table")

# data in
# -------
dynobs = data.table::fread("dat/dynobs.csv")
statobs = data.table::fread("dat/statobs.csv")
dynobs_sp = merge(dynobs, statobs, by="osmid")
citobs = data.table::fread("dat/citobs.csv")
# fullobs_sp = data.table::fread("gunzip -c dat/fullobs_sp.csv.gz")
fullobs_sp = data.table::fread("gunzip -c ~/data/churches/fullobs_sp.csv.gz")
fullobs = fullobs_sp[, -names(statobs)[-1], with = F]

M = 9 # number of imputations
impvrbs = names(fullobs_sp)[grepl('im3_ann\\d', names(fullobs_sp))]

ukgdp = data.table::fread("dat/engdp12001700.csv", skip=1, encoding="UTF-8")   
gdp = data.table::fread("dat/maddison_gdp_old.csv")
pop_mj = data.table::fread("dat/pop_mj.csv")

siem = data.table::fread("dat/siem_long_1500.csv", encoding="UTF-8")

# add ctr2 and ctr3 to siem
# note: does not cover all cities in siem (only those w churches)
siem = unique(statobs[, list(ctr2, ctr3, city)])[siem, on = "city"]

# heaping and its correction
# --------------------------
h_heaped = hist(dynobs[between(year, 700, 1500)][, year], breaks=data.table::uniqueN(dynobs[between(year, 700, 1500)]$year), plot = F)
h_unheaped = hist(dynobs[between(year, 700, 1500)][, year_crc1], breaks=data.table::uniqueN(dynobs[between(year, 700, 1500)]$year_crc1), plot = F)
h_heaped$counts = log1p(h_heaped$counts)
h_unheaped$counts = log1p(h_unheaped$counts)

pdf("figs/distrs_prepost_heap.pdf", width=11, height = 6)
par(mfrow=c(1, 2), font.main = 1)
plot(h_heaped, main='heaping', xlab='year', yaxt = 'n')
axis(2, labels = c(1, 10, 100), at = log(c(1, 10, 100)))
plot(h_heaped, main='heaping resampled', xlab='year', yaxt = 'n', border = NA)
lines(h_unheaped)
axis(2, labels = c(1, 10, 100), at = log(c(1, 10, 100)))
dev.off()

cat("heaped and non-heaped 700-1500: ")
sum(fullobs[data.table::between(year, 700, 1500), sum(im3_ann, na.rm=TRUE), by=year])
sum(fullobs[data.table::between(year, 700, 1500), sum(.SD, na.rm=TRUE) / M, by=year, .SDcols=grep("im3_ann\\d", names(fullobs))])
cat("heaped and non-heaped 700-1800: ")
sum(fullobs[, sum(im3_ann, na.rm=TRUE), by=year])
sum(fullobs[, sum(.SD, na.rm=TRUE) / M, by=year, .SDcols=grep("im3_ann\\d", names(fullobs))])

# data range from imputations
x = fullobs[data.table::between(year, 700, 1500), 
    lapply(.SD, sum, na.rm = TRUE), 
    .SDcols = impvrbs, 
    by = decade ][, 
        .(base::min(.SD), base::max(.SD)), 
        .SDcols = impvrbs, 
        by = decade]

cat("average end-size church: \n")
dynobs[, list(m3_end=sum(m3, na.rm=TRUE)), by=paste(osmid, bldindex)][, mean(m3_end)]
cat("average end-size final church: \n")
dynobs[, list(m3_end=sum(m3[bldindex==max(bldindex)], na.rm=TRUE)), by=osmid][, mean(m3_end)]

# w. european totals
# ------------------
eutotal = fullobs[data.table::between(decade, 700, 1500), list(im3y20 = base::sum(.SD, na.rm=TRUE) / M / 1e6), by=decade, .SDcols = impvrbs]

# m3 per 20y period en country
pdf('figs/europetotal_hc.pdf', height = 6)
plot(eutotal, type='n', bty='l', ylab = m3y20lblm)
abline(v=c(768, 1140, 1315, 1000, 1348), col='gray')
lines(eutotal, type='b', lwd=1.5, pch=1, cex=0.9)
dev.off()

fwrite(eutotal, "dat/figure3.csv")

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

# by type of church
# -----------------
bytype = fullobs_sp[
    data.table::between(year, 700, 1500),
    list(
        cathedral = base::sum(.SD[category == "cathedral"], na.rm=TRUE) / M / 1e6,
        parish = base::sum(.SD[category == "parish"], na.rm=TRUE) / M / 1e6,
        conventual = base::sum(.SD[category == "conventual"], na.rm=TRUE) / M / 1e6,
        other = base::sum(.SD[category == "other"], na.rm=TRUE) / M / 1e6),
    by = list(decade), 
    .SDcols = impvrbs]

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

fwrite(bytype, "dat/figure8.csv")

# building by city geography
# --------------------------

midcent = copy(fullobs_sp)
midcent[, year := round(year / 100)  * 100] # for compatability w. century
midcent = midcent[data.table::between(year, 650, 1550), 
     list(im3_cnt = base::sum(abs(.SD), na.rm=TRUE) / M), 
     by = list(city, year), 
     .SDcols = impvrbs]
pcitobs = merge(midcent, siem, by=c('city', 'year'), all.x=TRUE)

out = pcitobs[, 
    list(city, loc_frmtd, country, tld,
        year,
        lat, lon, 
        `transport location`, 
        rivercanal, coastal, landlocked,
        atlantic, whitesea, blacksea, northsea, caspian, mediterranean, baltic,
        inhab, im3_cnt)]
writexl::write_xlsx(out, "excels/cities_geography.xlsx")

geography = pcitobs[, 
    list(rivercanal = mean(im3_cnt[rivercanal == 1], na.rm = TRUE),
         coastal = mean(im3_cnt[coastal == 1], na.rm = TRUE),
         landlocked = mean(im3_cnt[landlocked == 1], na.rm = TRUE)), 
    by = year]

# correct that some italian cities have no basin (all others do)
pcitobs[ctr == "it" &  mediterranean == 0, mediterranean := 1]
geography_north = pcitobs[, 
    list(`Landlocked, Northern` = mean(im3_cnt[landlocked == 1 & mediterranean == 0], na.rm = TRUE),
         `River/canal, Northern` = mean(im3_cnt[rivercanal == 1 & mediterranean == 0], na.rm = TRUE),
         `Coastal, Northern` = mean(im3_cnt[coastal == 1 & mediterranean == 0], na.rm = TRUE)),
    by = list(year)]
geography_south = pcitobs[, 
    list(`Landlocked, Mediterranean` = mean(im3_cnt[landlocked == 1 & mediterranean == 1], na.rm = TRUE),
         `River/canal, Mediterranean` = mean(im3_cnt[rivercanal == 1 & mediterranean == 1], na.rm = TRUE),
         `Coastal, Mediterranean` = mean(im3_cnt[coastal == 1 & mediterranean == 1], na.rm = TRUE)),
    by = list(year)]

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
out = rbindlist(list(
        north = setNames(geography_north, c("year", "landlocked", "rivercanal", "coastal")),
        south = setNames(geography_south, c("year", "landlocked", "rivercanal", "coastal"))),
    idcol = TRUE)
fwrite(out, "dat/figure9.csv")

# totals per region
# -----------------

# first aggregate to city, then add siem
citobs_dec = fullobs_sp[
    data.table::between(decade, 700, 1500), 
    list(m3dec = base::sum(.SD, na.rm=TRUE) / M), 
    by = list(decade, city, ctr, ctr2, ctr3), 
    .SDcols = impvrbs]
citobs_dec = siem[, list(inhab, lat, lon, city, decade = year)][citobs_dec, on = c("city", "decade")]

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
byregion = byregion[, data.table(t(.SD), keep.rownames = TRUE), .SDcols = -"century"]
setnames(byregion, names(byregion), c("Region", centuries))
byregion = rbindlist(list(
        byregion, 
        byregion[, lapply(.SD, sum), .SDcols = -1]),
    fill = TRUE)

writeLines(knitr::kable(byregion, digits = 1, format = "html"),
    "tab/regiontable.html")

# index figures
mean11001500 = byregion[, mean(unlist(.SD)), .SDcols = c("11th", "12th", "13th", "14th", "15th"), by = Region]
byregionindices = mean11001500[byregion, on = "Region"][, .SD / V1 * 100, by = Region]
colmean11001500 = byregion[, mean(colSums(.SD)), .SDcols = c("11th", "12th", "13th", "14th", "15th")]

writeLines(knitr::kable(byregionindices[, -"V1"], digits = 0, format = "html"),
    "tab/regionindices.html")

# per (urban) capita series
# -------------------------
byctr = citobs_dec[, 
    list(im3_dec = sum(m3dec), urb_inhab = sum(inhab)), 
    by = list(decade, ctr)]
byctr2 = citobs_dec[, 
    list(im3_dec = sum(m3dec), urb_inhab = sum(inhab)), 
    by = list(decade, ctr2)]
byctr3 = citobs_dec[, 
    list(im3_dec = sum(m3dec), urb_inhab = sum(inhab)), 
    by = list(decade, ctr3)]
eu = citobs_dec[, 
    list(im3_dec = sum(m3dec), urb_inhab = sum(inhab)), 
    by = list(decade)]
# luxembourg already included under belgian total population

# citobs above has total pop for cities w. church only
# for total urbpop of country do:
# byctr = citobs_dec[, list(im3_dec = sum(m3dec)), by=list(decade, ctr)]
# byctr = siem[, list(urb_inhab = sum(inhab)), by = list(ctr, decade = year)][byctr, on = c('ctr', 'decade')]
# byctr2 = citobs_dec[, list(im3_dec = sum(m3dec)), by=list(decade, ctr2)]
# byctr2 = siem[, list(urb_inhab = sum(inhab)), by = list(ctr2, decade = year)][byctr2, on = c('ctr2', 'decade')]
# byctr3 = citobs_dec[, list(im3_dec = sum(m3dec)), by=list(decade, ctr3)]
# byctr3 = siem[, list(urb_inhab = sum(inhab)), by = list(ctr3, decade = year)][byctr3, on = c('ctr3', 'decade')]
# eu = citobs_dec[, list(im3_dec = sum(m3dec)), by=list(decade)]
# eu = siem[grep("fr|be|ch|uk|nl|de", ctr), list(urb_inhab = sum(inhab)), by = list(decade = year)][eu, on = c('decade')]

byctr = pop_mj[
    data.table::between(decade, 700, 1500), 
    list(ctr, decade, pop, pop_spl) ][ 
        byctr, on = c('ctr', 'decade')]
eu = pop_mj[
    data.table::between(decade, 700, 1500), 
    list(ctr, decade, pop, pop_spl)][, 
        list(pop = sum(pop), pop_spl = sum(pop_spl)), 
        by = decade][eu, on = 'decade']

 # fix zeroes
byctr[decade %% 100 != 0, urb_inhab := NA]
byctr2[decade %% 100 != 0, urb_inhab := NA]
byctr3[decade %% 100 != 0, urb_inhab := NA]
eu[decade %% 100 != 0, urb_inhab := NA]

fwrite(byctr[, list(ctr, decade, pop, urb_inhab, im3_dec = round(im3_dec))], 
    "dat/countrydata.csv")

# interpolate pop
byctr[, urb_inhab_i := round(exp(zoo::na.approx(log(urb_inhab)))), 
    by = ctr]
byctr2[, urb_inhab_i := round(exp(zoo::na.approx(log(urb_inhab)))), 
    by = ctr2]
byctr3[, urb_inhab_i := round(exp(zoo::na.approx(log(urb_inhab)))), 
    by = ctr3]
eu[, urb_inhab_i := round(exp(zoo::na.approx(log(urb_inhab))))]

byctr[, pop_i := exp(zoo::na.approx(log(pop))), by=ctr]
eu[, pop_i := exp(zoo::na.approx(log(pop)))]

out = rbindlist(list(byctr, cbind(eu, ctr = 'eu')), fill = TRUE)
# writexl::write_xlsx(
#     out[, list(ctr, decade, im3_dec, pop, pop_i, urb_inhab, urb_inhab_i)], 
#     "excels/countryseries.xlsx")

byctr[, im3_dec_smth := predict(loess(im3_dec / urb_inhab_i ~ decade), newdata=decade), 
    by = ctr]
byctr2[, im3_dec_smth := predict(loess(im3_dec / urb_inhab_i ~ decade), newdata=decade), 
    by = ctr2]
byctr3[, im3_dec_smth := predict(loess(im3_dec / urb_inhab_i ~ decade), newdata=decade), 
    by = ctr3]
eu[, im3_dec_smth := predict(loess(im3_dec / urb_inhab_i ~ decade), newdata=decade)]

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

out = rbindlist(
    list(byctr, 
         byctr[ctr %in% c("nl", "be"), 
             list(im3_dec = sum(im3_dec), pop_i = sum(pop_i)), 
             by = decade], 
         eu[, c(.SD, list(ctr = 'eu'))]),
    fill = TRUE)
