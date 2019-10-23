# summary statistics tables

setwd("~/dropbox/cathedrals")

library("data.table")
library("knitr")

dynobs = data.table::fread("dat/dynobs.csv")
statobs = data.table::fread("dat/statobs.csv")
dynobs_sp = merge(dynobs, statobs, by="osmid")
siem = data.table::fread("dat/siem_long_1500.csv", encoding="UTF-8")

siem[, inhab_before1100 := any(inhab[year <= 1100] > 0), by = city]
siem[, inhab_1100_1500 := any(inhab[year > 1100 & year <= 1500] > 0), by = city]

siem1100 = siem[inhab_before1100 == TRUE & year <= 1100]
dynobs_sp1100 = dynobs_sp[year <= 1100]

siem11001500 = siem[inhab_1100_1500 == TRUE & year > 1100 & year <= 1500]
dynobs_sp11001500 = dynobs_sp[year > 1100 & year <= 1500]

byctr = Reduce(merge, list(
    siem[year == 1500,
        .N,
        by = list(ctr)],
    unique(dynobs_sp[, list(city, 1)])[
        siem[year == 1500],
        on = c("city")][,
            list(`% cities w. large church` = mean(!is.na(V2))),
            by = list(ctr)],
    dynobs_sp[, 
        .(nchurch = uniqueN(osmid)), 
        by = .(ctr, city)][, 
            .(`Large churches/city` = mean(nchurch)), by = ctr],
    dynobs_sp[,
        .N,
        by = list(osmid, ctr)][,
            list(`Building phases / church` = mean(N)),
            by = list(ctr)],
    dynobs_sp[,
        list(`% phases <= 1100` = mean(year <= 1100)),
        by = list(ctr)]
    )
)

tot = Reduce(merge, list(
    siem[year == 1500, .N],
    unique(dynobs_sp[, list(city, 1)])[
        siem[year == 1500 ],
        on = c("city")][,
            list(`% cities w. large church` = mean(!is.na(V2)))],
    dynobs_sp[, 
        .(nchurch = uniqueN(osmid)), 
        by = .(ctr, city)][, 
            .(`Large churches/city` = mean(nchurch))],
    dynobs_sp[,
        .N,
        by = list(osmid, ctr)][,
            list(`Building phases / church` = mean(N))],
    dynobs_sp[, list(`% phases <= 1100` = mean(year <= 1100))]
    )
)

setnames(tot, 'x', "N")
out = rbind(byctr, data.table(tot, ctr = 'all'))
percentvars = grep('\\%', names(out))
out[, 
    (percentvars) := lapply(.SD, function(x) as.integer(round(x * 100))), 
    .SDcols = percentvars]
writeLines(knitr::kable(out, digits = 1, format = 'html'), 
    con = "tab/sumstats_perc.html")

# 700-1100
byctr1100 = Reduce(merge, list(
    siem1100[year == 1100,
        .N,
        by = list(ctr)],
    dynobs_sp1100[, list(city = unique(city), 1)][
        siem1100[year == 1100],
        on = c("city")][,
            list(`% cities w. large church` = mean(!is.na(V2))),
            by = list(ctr)],
    dynobs_sp1100[, 
        .(nchurch = uniqueN(osmid)), 
        by = .(ctr, city)][, 
            .(`Large churches/city` = mean(nchurch)), by = ctr],
    dynobs_sp1100[,
        .N,
        by = list(osmid, ctr)][,
            list(`Building phases / church` = mean(N)),
            by = list(ctr)],
    dynobs_sp1100[,
        list(`% phases <= 900` = mean(year <= 900)),
        by = list(ctr)]))

tot1100 = Reduce(merge, list(
    siem1100[year == 1100, .N],
    unique(dynobs_sp1100[, list(city, 1)])[
        siem1100[year == 1100 ],
        on = c("city")][,
            list(`% cities w. large church` = mean(!is.na(V2)))],
    dynobs_sp1100[, 
        .(nchurch = uniqueN(osmid)), 
        by = .(ctr, city)][, 
            .(`Large churches/city` = mean(nchurch))],
    dynobs_sp1100[,
        .N,
        by = list(osmid, ctr)][,
            list(`Building phases / church` = mean(N))],
    dynobs_sp1100[, list(`% phases <= 900` = mean(year <= 900))]))

setnames(tot1100, 'x', "N")
out = rbind(byctr1100, data.table(tot1100, ctr = 'all'))
percentvars = grep('\\%', names(out))
out[, 
    (percentvars) := lapply(.SD, function(x) as.integer(round(x * 100))), 
    .SDcols = percentvars]
writeLines(knitr::kable(out, digits = 1, format = 'html'), 
    con = "tab/sumstats_perc_7001100.html")

byctr11001500 = Reduce(merge, list(
    siem11001500[year == 1500,
        .N,
        by = list(ctr)],
    dynobs_sp11001500[, list(city = unique(city), 1)][
        siem11001500[year == 1500],
        on = c("city")][,
            list(`% cities w. large church` = mean(!is.na(V2))),
            by = list(ctr)],
    dynobs_sp11001500[, 
        .(nchurch = uniqueN(osmid)), 
        by = .(ctr, city)][, 
            .(`Large churches/city` = mean(nchurch)), 
            by = ctr],
    dynobs_sp11001500[,
        .N,
        by = list(osmid, ctr)][,
            list(`Building phases / church` = mean(N)),
            by = list(ctr)],
    dynobs_sp11001500[,
        list(`% phases <= 1300` = mean(year <= 1300)),
        by = list(ctr)]))

tot11001500 = Reduce(merge, list(
    siem11001500[year == 1500, .N],
    unique(dynobs_sp11001500[, list(city, 1)])[
        siem11001500[year == 1500 ],
        on = c("city")][,
            list(`% cities w. large church` = mean(!is.na(V2)))],
    dynobs_sp11001500[, 
        .(nchurch = uniqueN(osmid)), 
        by = .(ctr, city)][, 
            .(`Large churches/city` = mean(nchurch))],
    dynobs_sp11001500[,
        .N,
        by = list(osmid, ctr)][,
            list(`Building phases / church` = mean(N))],
    dynobs_sp11001500[, list(`% phases <= 1300` = mean(year <= 1300))]))

setnames(tot11001500, 'x', "N")
out = rbind(byctr11001500, data.table(tot11001500, ctr = 'all'))
percentvars = grep('\\%', names(out))
out[, 
    (percentvars) := lapply(.SD, function(x) as.integer(round(x * 100))), 
    .SDcols = percentvars]
writeLines(knitr::kable(out, digits = 1, format = 'html'), 
    con = "tab/sumstats_perc11001500.html")


# old sumstat table
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
