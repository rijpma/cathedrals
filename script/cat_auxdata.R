# prep auxiliary data files

rm(list = ls())
setwd("~/dropbox/cathedrals")

source("script/cat_functions.r")

library("readxl", verbose = FALSE)
library("data.table")
library("countrycode")

# gdp data
gdp_new = readxl::read_excel("excels/mpd2018.xlsx", sheet = "Full data")
setDT(gdp_new)
gdp_new = gdp_new[year <= 1500
    & countrycode %in% c("DEU", "ITA", "GBR", "FRA", "BEL", "CHE", "NLD"), ]
gdp_new[, iso2c := ifelse(countrycode == 'GBR', "UK",
    countrycode::countrycode(countrycode, 'iso3c', 'iso2c'))]

gdp_new = gdp_new
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

data.table::fwrite(gdp_new, "dat/gddnew.csv")

# population data & interpolation
pop_mj = data.table::fread("dat/mcevedyjones.csv", encoding = "UTF-8")
ukgdp = data.table::fread("dat/engdp12001700.csv", skip=1, encoding="UTF-8")
pop_mj[, decade := year]
pop_mj = pop_mj[as.data.table(expand.grid(
        ctr = unique(pop_mj$ctr), 
        decade = seq(from = 20, to = 2000, by = 20))), 
    on = c("ctr", "decade")]
pop_mj[ctr == 'uk', broadberry := ukgdp$population[match(decade, ukgdp$V1)]]
pop_mj[!is.na(broadberry), pop := broadberry / 1e6]
pop_mj[, pop := pop * 1e3]
pop_mj[, bd := ifelse(decade < 1348, "pre", "post")]
pop_mj[
    decade >= 500, 
    pop_spl := exp(zoo::na.spline(log(pop), x = decade, na.rm = F)), 
    by = .(ctr, bd)]
pop_mj = pop_mj[ctr %in% c("uk", "fr", "be", "nl", "de", "ch", "it")]
# spline not very effective on urban populations

data.table::fwrite(pop_mj, "dat/pop_mj.csv")
