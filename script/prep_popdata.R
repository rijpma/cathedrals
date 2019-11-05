# prep population data files

rm(list = ls())
setwd("~/dropbox/cathedrals")

source("script/cat_functions.r")

library("data.table")
library("zoo")

# population data & interpolation
pop_mj = data.table::fread("dat/mcevedyjones.csv", encoding = "UTF-8")
ukgdp = data.table::fread("dat/engdp12001700.csv", skip=1, encoding="UTF-8")
pop_mj[, decade := year]

popgrid = as.data.table(expand.grid(
        ctr = unique(pop_mj$ctr), 
        decade = seq(from = 20, to = 2000, by = 20)))
pop_mj = pop_mj[popgrid, on = c("ctr", "decade")]

pop_mj[ctr == 'uk', broadberry := ukgdp$population[match(decade, ukgdp$V1)]]
pop_mj[!is.na(broadberry), pop := broadberry / 1e6]
pop_mj[, pop := pop * 1e3]

# interpolate population data with spline pre/post 1348
# to avoid unrealistic figures in 1300-1400 period
pop_mj[, bd := ifelse(decade < 1348, "pre", "post")]
pop_mj[decade >= 500, 
    pop_spl := exp(zoo::na.spline(log(pop), x = decade, na.rm = F)), 
    by = .(ctr, bd)]
pop_mj = pop_mj[ctr %in% c("uk", "fr", "be", "nl", "de", "ch", "it")]
# spline not very effective on urban populations

data.table::fwrite(pop_mj, "dat/pop_mj.csv")
