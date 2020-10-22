setwd("~/dropbox/cathedrals")
library("data.table")

x = fread("gunzip -c ~/data/churches/fullobs_sp.csv.gz")
siem = data.table::fread("dat/siem_long_1500.csv", encoding="UTF-8")


out = x[, .SD, .SDcols = patterns("year$|decade|osmid$|lat|lon|city|ctr|category|im3_ann\\d?$")]
fwrite(out, "~/dropbox/cathedrals/dat/churchdata.csv.gz")

out = x[, 
    list(m3 = sum(.SD, na.rm = TRUE)),
    .SDcols = patterns("im3_ann\\d"),
    by = list(ctr, city, decade)]

out = merge(out,
    unique(siem[, list(city, loc_frmtd, lat, lon)]),
    by = "city",
    all.x = TRUE, all.y = FALSE)

fwrite(out, "~/dropbox/cathedrals/dat/citydata.csv.gz")
