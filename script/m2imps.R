# explore predecessor imputation procedure

rm(list=ls())

setwd("~/dropbox/cathedrals/")

source("script/cat_functions.r")

library("data.table")
library("texreg")
library("brms")
ncores = parallel::detectCores() -1
options(mc.cores = ncores)

statobs = data.table::fread("dat/statobs.csv")
dynobs = data.table::fread("dat/dynobs.csv")
dynobs = dynobs[statobs[, .(osmid, ctr)], on = c("osmid")]
sfc = data.table::fread("dat/backproj.csv")

sfc[, city:=iconv(city, from="macroman", to="utf8")]
sfc[, church:=iconv(church, from="macroman", to="utf8")]
sfc[, osmid := as.character(osmid)]

sfc[, period1:=as.numeric(gsub('\\D', '', period1))]
sfc[, period2:=as.numeric(gsub('\\D', '', period2))]
sfc[, year := period1]

sfc = merge(sfc, statobs[, .(osmid, ctr)], on = "osmid")

sfc_ita = data.table::fread("dat/backproj_ita.csv")
sfc_ita[, ctr := "it"]

sfc_all = rbindlist(list(
    sfc[factor == 1, 
        list(osmid,
             year_predecessor = year, 
             ctr,
             m2predecessor = previous_surface,
             m2successor = end_surface)],
    sfc_ita[, 
        list(year_predecessor = year,
            ctr,
             m2predecessor = m2,
             m2successor = data.table::shift(m2, type = "lead")),
        by = osmid]))

sfc_all = sfc_all[!is.na(m2successor)]
sfc_all[, century := floor(year_predecessor / 100)  * 100] # for compatability w. century
sfc_all = merge(sfc_all, statobs[, .(osmid, category)], by = "osmid")

cat("distribution of predecessors: \n")
ftable(sfc_all$century, sfc_all$ctr)
ftable(sfc_all$century, sfc_all$category)
ftable(sfc_all$ctr, sfc_all$category)

modlist = list(
    `all` = lm(m2predecessor ~ m2successor - 1, data = sfc_all),
    `excl. italy` = lm(m2predecessor ~ m2successor - 1, data = sfc_all[ctr != 'it']),
    `country split` = lm(m2predecessor ~ m2successor:factor(ctr) - 1, data = sfc_all),
    `century split` = lm(m2predecessor ~ m2successor:factor(century) - 1, data = sfc_all),
    `church split` = lm(m2predecessor ~ m2successor:factor(category) - 1, data = sfc_all))
texreg::htmlreg(modlist, "tab/predecessors.html",
    custom.coef.names = unique(unlist(lapply(modlist, function(x) gsub("factor\\(.*\\)", "", names(coef(x)))))))

# end-size of successor chuches
dynobs[, endm2 := sum(m2, na.rm = T), by = .(osmid, bldindex)]
dynobs[, successor_endm2 := data.table::shift(endm2, type = "lead"), by = osmid]
dynobs[, successor_endm2 := tail(successor_endm2, 1), by = list(osmid, bldindex)]

# m2 backprojections were hardcoded in original data
# so to check alternative backprojections we first have 
# to find those points in data derived from fitted relation
# and checking which observations are very close

# first: the original model
m = lm(I(m2predecessor / m2successor) ~ m2successor, data = sfc_all[ctr == 'it'])
# y = bx^2 + ax

dynobs[, ratio_to_successor :=  endm2 / successor_endm2]
dynobs[ctr == "it", ratio_predicted := predict(m, newdata = data.table(m2successor = successor_endm2))]
dynobs[ctr != "it", m2predicted := predict(modlist$`excl. italy`, newdata = data.table(m2successor = successor_endm2))]

bldobs = dynobs[, 
    list(endm2 = unique(endm2), 
         successor_endm2 = unique(successor_endm2), 
         ratio_to_successor = unique(ratio_to_successor), 
         ratio_predicted = unique(ratio_predicted), 
         m2predicted = unique(m2predicted), 
         year = min(year),
         gss = mean(gss_m2, na.rm = T)), 
    by = list(ctr, osmid, bldindex)]

# italy
itmin = -0.025
itmax = 0.058
eumin = -0.00005
eumax = 0.00335
pdf("figs/fittedpoints.pdf")
par(mfrow = c(1, 2))
plot(ecdf(bldobs[ctr == "it", ratio_to_successor - ratio_predicted]), 
    pch = 1, xlim = c(-0.2, 0.2))
abline(v = c(itmin, itmax)) # between these values
# rest
plot(ecdf(bldobs[ctr != "it" & endm2 != 0 & successor_endm2 != 0, ratio_to_successor - 0.53]),
    xlim = c(-0.01, 0.01), pch = NA, lty = 1)
abline(v = c(eumin, eumax)) # between these values
dev.off()

# = predicted in buildings
bldobs[, predicted := "no"]
bldobs[ctr == "it" & data.table::between(ratio_to_successor - ratio_predicted, itmin, itmax), 
     predicted := "italy"]
bldobs[ctr != "it" & data.table::between(ratio_to_successor - 0.53, eumin, eumax), 
     predicted := "rest"]

cat("N measured and imputed: \n")
bldobs[, .N, by = predicted]

# = predicted in dynobs
dynobs[, predicted := "no"]
dynobs[ctr == "it" & data.table::between(ratio_to_successor - ratio_predicted, -0.025, itmax), 
     predicted := "italy"]
dynobs[ctr != "it" & data.table::between(ratio_to_successor - 0.53, eumin, eumax), 
     predicted := "rest"]

# model successors/predecessors relation by country/century
m = brms::brm(log(m2predecessor) ~ (log(m2successor)| ctr + century), 
    data = sfc_all, 
    chains = 3,
    seed = 12904)

# divergent transitions dissappear with adapt_delta = 0.999
# but since there are no noticable differences in predictions
# it is left at default (0.8) for speed

# use this to predict
data_for_predictions = dynobs[, 
    list(m2successor = successor_endm2, 
         ctr = ctr, 
         century = floor(min(year) / 100) * 100),
    by = list(osmid, bldindex)]
# warnings can be ignored

preds = predict(m, 
    newdata = data_for_predictions,
    allow_new_levels = TRUE)
dynobs[, newpreds := exp(preds[, 1])]

# create annual series for original and "no-disaster" predictions
dynobs_alt = data.table::copy(dynobs)

# use * newpreds/endm2 because we want to apply correction
# of imputed end-size to entire series
# predicted is unique per bldindex, so use as is
dynobs_alt[!is.na(newpreds) & predicted != "no", m2 := as.integer(m2 * (newpreds / endm2))]

fullobs = to_annual_obs(dynobs)
fullobs_alt = to_annual_obs(dynobs_alt)
fullobs[, decade := (trunc((year - 1) / 20) + 1) * 20] # so 1500 = 1481-1500
fullobs_alt[, decade := (trunc((year - 1) / 20) + 1) * 20] # so 1500 = 1481-1500

# add countries for plot
fullobs = statobs[, list(osmid, ctr)][fullobs, on = "osmid"]
fullobs_alt = statobs[, list(osmid, ctr)][fullobs_alt, on = "osmid"]

ctrsalt = fullobs_alt[data.table::between(year, 700, 1500), 
    list(
        Italy = sum(im2_ann[ctr == "it"], na.rm = TRUE),
        LowCountries = sum(im2_ann[ctr == "nl" | ctr == "be"], na.rm = TRUE),
        All = sum(im2_ann, na.rm = TRUE),
        Germany = sum(im2_ann[ctr == "de"], na.rm = TRUE),
        France = sum(im2_ann[ctr == "fr"], na.rm = TRUE),
        GreatBritain = sum(im2_ann[ctr == "uk"], na.rm = TRUE)),
    by = decade]
ctrsold = fullobs[data.table::between(year, 700, 1500), 
    list(
        Italy = sum(im2_ann[ctr == "it"], na.rm = TRUE),
        LowCountries = sum(im2_ann[ctr == "nl" | ctr == "be"], na.rm = TRUE),
        All = sum(im2_ann, na.rm = TRUE),
        Germany = sum(im2_ann[ctr == "de" | ctr == "ch"], na.rm = TRUE),
        France = sum(im2_ann[ctr == "fr"], na.rm = TRUE),
        GreatBritain = sum(im2_ann[ctr == "uk"], na.rm = TRUE)),
    by = decade]

pdf("figs/altbackprojs_panel.pdf", width = 9, height= 6)
par(mfrow = c(2, 3), mar = c(4, 4, 2, 0.5), bty = 'l', font.main = 1)
plot(Italy ~ decade, data = ctrsalt,
        type = 'l', lwd = 1.5, col = 1,
        yaxt = "n",
        main = "Italy", ylab = m2y20lbl)
lines(Italy ~ decade, data = ctrsold, col = "gray", lwd = 1.5)
axis1ks(side = 2)
plot(France ~ decade, data = ctrsalt,
        type = 'l', lwd = 1.5, col = 1,
        yaxt = "n",
        main = "France", ylab = "")
lines(France ~ decade, data = ctrsold, col = "gray", lwd = 1.5)
axis1ks(side = 2)
plot(Germany ~ decade, data = ctrsalt,
        type = 'l', lwd = 1.5, col = 1,
        yaxt = "n",
        main = "Germany, incl. Switzerland", ylab = "")
lines(Germany ~ decade, data = ctrsold, col = "gray", lwd = 1.5)
axis1ks(side = 2)
plot(LowCountries ~ decade, data = ctrsalt,
        type = 'l', lwd = 1.5, col = 1,
        yaxt = "n",
        main = "Low Countries", ylab = m2y20lbl)
lines(LowCountries ~ decade, data = ctrsold, col = "gray", lwd = 1.5)
axis1ks(side = 2)
plot(GreatBritain ~ decade, data = ctrsalt,
        type = 'l', lwd = 1.5, col = 1,
        yaxt = "n",
        main = "Great Britain", ylab = "")
lines(GreatBritain ~ decade, data = ctrsold, col = "gray", lwd = 1.5)
axis1ks(side = 2)
plot(All ~ decade, data = ctrsalt,
        type = 'l', lwd = 1.5, col = 1,
        yaxt = "n",
        main = "All", ylab = "")
lines(All ~ decade, data = ctrsold, col = "gray", lwd = 1.5)
axis1ks(side = 2)
dev.off()

# eltjo's estimates
# originally manually set using 1.879^(1:3)
m = lm(end_surface ~ previous_surface - 1, data=sfc)
m1 = lm(end_surface ~ previous_surface - 1, data=sfc[factor==1,])
m2 = lm(end_surface ~ previous_surface - 1, data=sfc[factor==2,])
m3 = lm(end_surface ~ previous_surface - 1, data=sfc[factor==3,])

pdf("figs/surface_fits.pdf", height=3, width=8)
par(mfrow=c(1, 3), font.main=1, mar = c(4.5, 4, 1.5, 0))
plot(end_surface ~ previous_surface, data=sfc, type='n', bty='l', 
    main='1st predecessor', xlab = '', ylab = 'End surface (m2)')
points(end_surface ~ previous_surface, data=sfc[factor==1,])
abline(m1, col = 'gray')
abline(a=0, b=1.879)
# text(2000, 1000, "OLS fit", col=2)
# text(1000, 5000, "Rule of thumb", col=1)
plot(end_surface ~ previous_surface, data=sfc, type='n', bty='l', 
    main='2nd predecessor', xlab = 'Previous surface (m2)', ylab = '')
points(end_surface ~ previous_surface, data=sfc[factor==2,])
abline(m2, col = 'gray')
abline(a=0, b=1.879^2)
plot(end_surface ~ previous_surface, data=sfc, type='n', bty='l', 
    main='3rd predecessor', xlab = '', ylab = '')
points(end_surface ~ previous_surface, data=sfc[factor==3,])
abline(m3, col = 'gray')
abline(a=0, b=1.879^3)
dev.off()
