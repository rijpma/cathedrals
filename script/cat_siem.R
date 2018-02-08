rm(list=ls())
options(stringsAsFactors=FALSE)

setwd("~/dropbox/cathedrals/")
source("script/cat_functions.r")

library("data.table")

siem = data.table::fread("dat/siem.csv")
data.table::setnames(siem, names(siem)[!grepl('^V\\d+$', names(siem))], 
    c('inhab', 'coord', 'yr', 'cargoratio', 'rivercanal', 'rivertoll', 
    'sqrtsetdens', 'parliament', 'manuscipt', 'commune', 'bishopric',
    'arcbishopric', 'seatoll', 'coastal', 'riverXcoast', 'landlocked', 
    'atlantic', 'whitesea', 'blacksea', 'northsea', 'caspian', 'mediterranean', 'baltic'))
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

siem = data.table::rbindlist(list(siem_fr, siem_be, siem_ch, siem_uk, siem_nl, siem_de, siem_it))
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



# siem = data.table::fread("dat/siem.csv", header=T)
# data.table::setnames(siem, names(siem)[!grepl('^V\\d+$', names(siem))], 
#     c('inhab', 'coord', 'yr', 'cargoratio', 'rivercanal', 'rivertoll', 
#     'sqrtsetdens', 'parliament', 'manuscript', 'commune', 'bishopric',
#     'arcbishopric', 'seatoll', 'coastal', 'riverXcoast', 'landlocked', 
#     'atlantic', 'whitesea', 'blacksea', 'northsea', 'caspian', 'mediterranean', 'baltic'))
# for (i in 5:ncol(siem)){
#     if (grepl("^V\\d+$", names(siem)[i])) names(siem)[i] = names(siem)[i - 1]
# }
# names(siem) = paste0(gsub('^V\\d+$', '', names(siem)), siem[2, ])
# siem = siem[3:nrow(siem), ]

# measures = unique(gsub('\\d.*', '', names(siem)[grep('\\d{3}', names(siem))]))
# measureslist = lapply(measures, function(x) grepr(paste0('^', x), names(siem)))
# siem = data.table::melt(siem, measure=measureslist, 
#     id.vars=setdiff(names(siem), unlist(measureslist)),
#     variable.name="year", variable.factor=F)
# siem$year = 600 + as.numeric(siem$year) * 100
# setnames(siem, grep('value', names(siem)), measures)
# siem[, (measures):=lapply(.SD, as.numeric), .SDcols=measures]

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

write.csv(siem[year <= 1800 & !is.na(year), ], "dat/siem_long.csv", row.names=F, fileEncoding="UTF-8")