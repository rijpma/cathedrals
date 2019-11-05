# prep urban population dataset

rm(list=ls())
options(stringsAsFactors=FALSE)

setwd("~/dropbox/cathedrals/")
source("script/cat_functions.r")

library("data.table")

# original city file (additions below)
siem = data.table::fread("dat/siem.csv", header = T)
data.table::setnames(siem, names(siem)[!grepl('^V\\d+$', names(siem))], 
    c('inhab', 'coord', 'yr', 'cargoratio', 'rivercanal', 'rivertoll', 
    'sqrtsetdens', 'parliament', 'manuscipt', 'commune', 'bishopric',
    'arcbishopric', 'seatoll', 'coastal', 'riverXcoast', 'landlocked', 
    'atlantic', 'whitesea', 'blacksea', 'northsea', 'caspian', 
    'mediterranean', 'baltic'))
# name empty columns
for (i in 5:ncol(siem)){
    if (grepl("^V\\d+$", names(siem)[i])) names(siem)[i] = names(siem)[i - 1]
}
names(siem) = paste0(gsub('^V\\d+$', '', names(siem)), siem[2, ])

# re-geocoded versions of dataset
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

siem = data.table::rbindlist(
    list(siem_fr, siem_be, siem_ch, siem_uk, siem_nl, siem_de, siem_it, siem_lu))

# remove completely empty columns
siem = siem[, !sapply(siem, function(x) all(is.na(x))), with=F]

# list-wise melt
measures = unique(gsub('\\d.*', '', names(siem)[grep('\\d{3}', names(siem))]))
measureslist = lapply(measures, function(x) grepr(paste0('^', x), names(siem)))
siem[, (unlist(measureslist)) := lapply(.SD, as.numeric), 
    .SDcols = unlist(measureslist)]
siem = data.table::melt(siem, 
    measure.vars = measureslist, 
    id.vars = setdiff(names(siem), unlist(measureslist)),
    variable.name = "year", 
    variable.factor = F)
siem$year = 600 + as.numeric(siem$year) * 100
setnames(siem, grep('value', names(siem)), measures)

# fix wrong name
setnames(siem, 'coord', 'elevation')

# additions to city dataset
siem_ad = data.table::fread("dat/siem_add.csv")
setnames(siem_ad, 
    old = names(siem_ad),
    new = gsub("^X", "inhab", names(siem_ad)))

# turn siem_ad into long form
ids = grep("\\d{3,}", names(siem_ad))
siem_ad[, (ids) := lapply(.SD, as.numeric),
    .SDcols = ids] # warnings safe to ignore
siem_ad_long = melt(siem_ad, 
    id.vars = names(siem_ad)[-ids], 
    measure.vars = names(siem_ad)[ids],
    value.name = 'inhab')

siem_ad_long[, year := as.numeric(gsub("\\D", "", variable))]

# additions do not have same detailed geo stuff, so add as much as possible manually
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

siem_long = rbindlist(
    list(
        siem, 
        siem_ad_long[, names(siem_ad_long) %in% names(siem), with = F]), 
    fill = TRUE)

# tld fill
siem_long[, tld := unique(na.omit(tld)), by = country]

# some mistakes in river-coast combination so recreate here
siem_ad_long[, riverXcoast := coastal == 1 & rivercanal == 1]

# correction: remove these
not_in_siem = readLines("dat/notinsiem.csv")
siem_long = siem_long[!(city %in% not_in_siem)]

siem_long[, ctr := tolower(siem_long$tld)]
siem_long[ctr == 'gb', ctr := "uk"]

# data checks
if (siem_long[, duplicated(city), by = year][, sum(V1)] > 0){
    warning("Duplicated cities")
}
if (siem_long[, duplicated(year), by = city][, sum(V1)] > 0){
    warning("Duplicated year")
}
# after 1500 additions have different coverage, so beware
if (siem_long[year <= 1500, uniqueN(year), by = city][, length(unique(V1))] != 1){
    warning("Unbalanced dataset")
}

# encoding checks
if (any( !validUTF8(siem_long$city))){
    warning("City names not valid utf8")
}
if (any(grepl("√", siem_long$city))){
    warning("City names contain √, conversion error")
}

data.table::fwrite(siem_long[year <= 1800, ], "dat/siem_long.csv")

# exclude cities with zero inhab before 1500
siem_long[, inhab_before1500 := any(inhab[year <= 1500] > 0), by = city]
siem_long = siem_long[inhab_before1500 == T]
siem_long[, inhab_before1500 := NULL]

data.table::fwrite(siem_long[year <= 1500, ], "dat/siem_long_1500.csv")
