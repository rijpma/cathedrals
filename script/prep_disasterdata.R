# prep data on distasters

rm(list = ls())
setwd("~/dropbox/cathedrals")

library("data.table")
library("readxl")
library("sf")

disas = readxl::read_excel(
    path = "excels/saints and exogenous disasters.xlsx",
    sheet = "Blad1",
    skip = 2)
disas_it = readxl::read_excel(
    path = "excels/ital cities def 2.xlsx",
    sheet  = "saints and disasters",
    skip = 2)
setDT(disas_it)
setDT(disas)

# long format
disas = rbindlist(list(
    disas[, .(`X__1`, `destroyed`, `(approximate) date`)],
    disas[, .(`X__1`, `destroyed__1`, `(approximate) date__1`)],
    disas[, .(`X__1`, `destroyed__2`, `(approximate) date__2`)]))

# cleanup
disas[destroyed == "fire 1250", `(approximate) date` := "1250"]
disas[destroyed == "fire 1086", `(approximate) date` := "1086"]
disas[destroyed == "war 978", `(approximate) date` := "978"]
disas[`(approximate) date` == "14th c.", `(approximate) date` := "1350"]
disas[`(approximate) date` == "1113/1115", `(approximate) date` := "1114"]

disas[X__1 == "72680580" & is.na(`(approximate) date`), 
    `(approximate) date` := "1224"]
disas[X__1 == "72680580", destroyed := destroyed[1]]

# all three are years of disas
disas = rbindlist(
    list(disas,
        data.table(X__1 = "80181137", destroyed = "", `(approximate) date` = "848")))
disas[X__1 == "80181137", destroyed := "invasion"] # ditto

disas[destroyed == "xxxxxxx", destroyed := "no"]
disas[destroyed == "xxxxxx", destroyed := "no"]

# saints in cause field
disas = disas[destroyed != "st john baptist" | is.na(destroyed)]
disas = disas[destroyed != "st milburga" | is.na(destroyed)]
disas = disas[destroyed != "mary" | is.na(destroyed)]

# otherwise non-disaster fields
disas = disas[destroyed != "current church 16th c." | is.na(destroyed)]

# italian disasters to long format
disas_it = rbindlist(list(
    disas_it[, .(`X__1`, `destroyed`, `(approximate) date`)],
    disas_it[, .(`X__1`, `destroyed__1`, `(approximate) date__1`)]))

disas_it[destroyed == "destroyed 1156", `(approximate) date` := "1156"]

disasters = rbindlist(list(disas, disas_it))
setnames(disasters, names(disasters), c("osmid", "cause", "year"))
disasters[, year := as.numeric(year)]

disasters = disasters[!is.na(osmid) & !is.na(cause) & !is.na(year)]

# cause dummies
disasters[, earthquake := grepl("earthquake", cause)]
disasters[, war := grepl("vikings|saracens|war|riot|invasion|normans|plun?der|ravaged|raze|danes|rebellion|crusade|mag?yars", cause)]
disasters[, fire := war == FALSE & grepl("fire|confla", cause)]
disasters[, other_natural := fire == FALSE 
                           & war == FALSE 
                           & earthquake == FALSE 
                           & grepl("flood|light|hurri|storm", cause)]
disasters[, other := fire == FALSE 
                   & war == FALSE 
                   & earthquake == FALSE 
                   & other_natural == FALSE]

if (!disasters[, all(earthquake + other + war + fire + other_natural == 1)]){
    warning("categories not mutually exclusive or coverage incomplete")
}
if (disasters[, anyNA(earthquake + other + war + fire + other_natural)]){
    warning("missing values in categories")
}

data.table::fwrite(disasters, "dat/disasters.csv")

# alternative data

# ahead data (actually querying sheec data)
# https://www.emidius.eu/AHEAD/index.php
# ahead = sf::st_read("https://www.emidius.eu/fdsnws/event/1/query?starttime=1000&endtime=1500&limit=5000&format=json")

# # https://emidius.mi.ingv.it/CPTI15-DBMI15/index_en.htm
# # successor of the one the paper uses
# cpti15 = readxl::read_excel("dat/CPTI15_v1.5.xls", sheet = "catalogue")
# setDT(cpti15)
# setDT(ahead)
# cpti15

# both = merge(ahead, cpti15, by.x = "id", by.y = "EqID")
# plot(LatDef ~ lat, data = both)
# # so half in ahead can be found in Italy
