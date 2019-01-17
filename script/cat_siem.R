rm(list=ls())
options(stringsAsFactors=FALSE)

setwd("~/dropbox/cathedrals/")
source("script/cat_functions.r")

library("data.table")

siem = data.table::fread("dat/siem.csv", header = T)
data.table::setnames(siem, names(siem)[!grepl('^V\\d+$', names(siem))], 
    c('inhab', 'coord', 'yr', 'cargoratio', 'rivercanal', 'rivertoll', 
    'sqrtsetdens', 'parliament', 'manuscipt', 'commune', 'bishopric',
    'arcbishopric', 'seatoll', 'coastal', 'riverXcoast', 'landlocked', 
    'atlantic', 'whitesea', 'blacksea', 'northsea', 'caspian', 
    'mediterranean', 'baltic'))
for (i in 5:ncol(siem)){
    if (grepl("^V\\d+$", names(siem)[i])) names(siem)[i] = names(siem)[i - 1]
}
names(siem) = paste0(gsub('^V\\d+$', '', names(siem)), siem[2, ])


siem_fr = data.table::fread("dat/siem_france.csv")
data.table::setnames(siem_fr, 1:ncol(siem), names(siem))
siem_fr[, distance2:=NULL]
siem_ch = data.table::fread("dat/siem_swiss.csv")
data.table::setnames(siem_ch, 1:ncol(siem), names(siem))
siem_be = data.table::fread("dat/siem_belgium.csv")
data.table::setnames(siem_be, 1:ncol(siem), names(siem))
siem_nl = data.table::fread("dat/siem_nl.csv")[, -1, with=F]
data.table::setnames(siem_nl, 1:ncol(siem), names(siem))
siem_uk = data.table::fread("dat/siem_uk.csv")[, -1, with=F]
data.table::setnames(siem_uk, 1:ncol(siem), names(siem))
siem_de = data.table::fread("dat/siem_de.csv")[, -1, with=F]
data.table::setnames(siem_de, 1:ncol(siem), names(siem))
siem_it = data.table::fread("dat/siem_it.csv")[, -1, with=F]
data.table::setnames(siem_it, 1:ncol(siem), names(siem))
siem_lu = data.table::fread('dat/siem_lu.csv')[, -1, with = F]
data.table::setnames(siem_lu, 1:ncol(siem), names(siem))

siem_ad = data.table::fread("dat/siem_add.csv")
setnames(siem_ad, 
    names(siem_ad),
    gsub("^X", "inhab", names(siem_ad)))

siem = data.table::rbindlist(
    list(siem_fr, siem_be, siem_ch, siem_uk, siem_nl, siem_de, siem_it, siem_lu))
siem = siem[, !sapply(siem, function(x) all(is.na(x))), with=F]

measures = unique(gsub('\\d.*', '', names(siem)[grep('\\d{3}', names(siem))]))
measureslist = lapply(measures, function(x) grepr(paste0('^', x), names(siem)))
siem = data.table::melt(siem, measure=measureslist, 
    id.vars=setdiff(names(siem), unlist(measureslist)),
    variable.name="year", variable.factor=F)
siem$year = 600 + as.numeric(siem$year) * 100
setnames(siem, grep('value', names(siem)), measures)
siem[, (measures):=lapply(.SD, as.numeric), .SDcols=measures]

# citobs = data.table::rbindlist(list(citobs_fr, citobs_ch, citobs_be, citobs_gb, citobs_nl, citobs_de))

setnames(siem, 'coord', 'elevation')

# encoding checks
grep('xxx', siem$city) 
# siem[, city:=iconv(city, from="macroman", to="utf8")]

siem[grep("Tue", city), city]
siem[grep("√", city), city]
# siem[grep("uenst", city), city]
# siem[, city:=gsub("√´", "‘", city)]
# siem[, city:=gsub("√∂", "ö", city)]
# siem[, city:=gsub("√º", "ü", city)]
# siem[, city:=gsub("√§", "ä", city)]
# siem[, city:=gsub("√©", "é", city)]

siem[, 
    lapply(.SD, uniqueN), 
    by = .(city)][, 
        lapply(.SD, unique), 
        .SD = -c(1:2)]
# all geo except river is unchanging
# what to do? so have to turn siem_ad into long form
ids = grep("\\d{3,}", names(siem_ad))
siem_ad_long = melt(siem_ad, 
    id.vars = names(siem_ad)[-ids], 
    measure.vars = names(siem_ad)[ids],
    value.name = 'inhab')

siem_ad_long[, year := as.numeric(gsub("\\D", "", variable))]

siem_ad_long[, landlocked := grepl("land", transport.location)]
siem_ad_long[, rivercanal := grepl('river', transport.location)]
siem_ad_long[, coastal := grepl('sea|med|balt', transport.location) & landlocked == FALSE]
siem_ad_long[, northsea := grepl("sea", transport.location) & landlocked == FALSE]
siem_ad_long[, mediterranean  := grepl("med", transport.location) & landlocked == FALSE]
siem_ad_long[, baltic := grepl("balt", transport.location) & landlocked == FALSE]
siem_ad_long[, riverXcoast := coastal == TRUE & rivercanal == TRUE]
siem_ad_long[, atlantic := 0]
siem_ad_long[, whitesea := 0] 
siem_ad_long[, blacksea := 0]
siem_ad_long[, caspian := 0]

siem_long = rbindlist(list(
    siem, 
    siem_ad_long[, names(siem_ad_long) %in% names(siem), with = F]
    ), fill = T)
# tld fill
siem_long[, tld := unique(na.omit(tld)), by = country]
siem_long[, duplicated(city), by = year][V1 == T]
# no dupls

# redefine to purge of mistakes
siem_ad_long[, riverXcoast := coastal == 1 & rivercanal == 1]

# but apparently we need to get the info out of the pop file
# szj
# szj = data.table::fread("dat/churches_vols_pop_def_2018dec5.csv")
# nms = make.unique(unlist(szj[5]), sep = '_')
# setnames(szj, names(szj), nms)
# szj = melt(szj[-c(1:5)], 
#     id.vars = 1:4,
#     measure.vars = list(bishopric = names(szj)[18:26],
#                         commune = names(szj)[28:36],
#                         inhab = names(szj)[48:56]))
# szj[, year := as.numeric(variable) * 100 + 600]
# setdiff(tolower(szj$city), tolower(siem_long$city))
# setdiff(siem_long$city, szj$city)
# but this is hopeless because all the city names are now off
# I give up. We just use the old data.

not_in_siem = readLines("dat/notinsiem.csv")

siem_long = siem_long[!(city %in% not_in_siem)]

siem_long[, ctr:=tolower(siem_long$tld)]
siem_long[ctr=='gb', ctr:="uk"]

data.table::fwrite(siem_long[year <= 1800 & !is.na(year), ], "dat/siem_long.csv")

siem_long[, inhab_before1500 := any(inhab[year <= 1500] > 0), by = city]
siem_long = siem_long[inhab_before1500 == T]
siem_long[, inhab_before1500 := NULL]

data.table::fwrite(siem_long[year <= 1500 & !is.na(year), ], "dat/siem_long_1500.csv")