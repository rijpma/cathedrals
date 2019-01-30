rm(list=ls())

setwd("~/dropbox/cathedrals/")

library("data.table")
library("lme4")
library("texreg")
library("readxl")

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
        by = osmid][!is.na(m2successor)]))

sfc_all[, century := floor(year_predecessor / 100)  * 100] # for compatability w. century
sfc_all = merge(sfc_all, statobs[, .(osmid, category)], on = "osmid")

modlist = list(
    `all` = lm(log(m2predecessor) ~ log(m2successor) - 1, data = sfc_all),
    `excl. italy` = lm(log(m2predecessor) ~ log(m2successor) - 1, data = sfc_all[ctr != 'it']),
    `country split` = lm(log(m2predecessor) ~ log(m2successor):(factor(ctr) - 1) - 1, data = sfc_all),
    `century split` = lm(log(m2predecessor) ~ log(m2successor):(factor(century) - 1) - 1, data = sfc_all),
    `church split` = lm(log(m2predecessor) ~ log(m2successor):(factor(category) - 1) - 1, data = sfc_all))
texreg::screenreg(modlist)
texreg::htmlreg(modlist, "tab/predecessors_utrylogs.html",
    custom.coef.names = unique(unlist(lapply(modlist, function(x) gsub("factor\\(.*\\)", "", names(coef(x)))))))

modlist = list(
    `all` = lm(m2predecessor ~ m2successor - 1, data = sfc_all),
    `excl. italy` = lm(m2predecessor ~ m2successor - 1, data = sfc_all[ctr != 'it']),
    `country split` = lm(m2predecessor ~ m2successor:(factor(ctr) - 1) - 1, data = sfc_all),
    `century split` = lm(m2predecessor ~ m2successor:(factor(century) - 1) - 1, data = sfc_all),
    `church split` = lm(m2predecessor ~ m2successor:(factor(category) - 1) - 1, data = sfc_all))
texreg::screenreg(modlist)
texreg::htmlreg(modlist, "tab/predecessors.html",
    custom.coef.names = unique(unlist(lapply(modlist, function(x) gsub("factor\\(.*\\)", "", names(coef(x)))))))

# sample distribution
ftable(sfc_all$century, sfc_all$ctr)
sfc_all[, .N, by = century][order(N)]
sfc_all[, .N, by = category][order(N)]
sfc_all[, .N, by = ctr][order(N)]

dynobs[, endm2 := sum(m2, na.rm = T), by = .(osmid, bldindex)]
dynobs[, successor_endm2 := data.table::shift(endm2, type = "lead"), by = osmid]
dynobs[, successor_endm2 := tail(successor_endm2, 1), by = list(osmid, bldindex)]
# ugly af, but it works

# eltjo's relation
m = lm(I(m2predecessor / m2successor) ~ m2successor, data = sfc_all[ctr == 'it'])
# so actually y = bx^2 + ax

dynobs[, ratio_to_successor :=  endm2 / successor_endm2]
dynobs[ctr == "it", ratio_predicted := predict(m, newdata = data.table(m2successor = successor_endm2))]
dynobs[ctr != "it", m2predicted := predict(modlist$`excl. italy`, newdata = data.table(m2successor = successor_endm2))]

bldobs = dynobs[, list(endm2 = unique(endm2), 
        successor_endm2 = unique(successor_endm2), 
        ratio_to_successor = unique(ratio_to_successor), 
        ratio_predicted = unique(ratio_predicted), 
        m2predicted = unique(m2predicted), 
        year = min(year),
        gss = mean(gss_m2, na.rm = T)), 
    by = list(ctr, osmid, bldindex)]

# fits and all data
par(mfrow = c(1, 1))
plot(endm2 ~ successor_endm2, data = bldobs, type = 'n', log = 'xy')
points(endm2 ~ successor_endm2, data = bldobs[ctr != 'it'], col = 'black')
abline(modlist$`excl. italy`, col = 1, untf = T)
points(endm2 ~ successor_endm2, data = bldobs[ctr == 'it'], col = 'blue')
curve(coef(m)[1]*x + coef(m)[2]*x^2, add = T, col = 'blue')

pdf("figs/backproj_ita.pdf")
plot(m2predecessor ~ m2successor, data = sfc_all, 
    type = 'n', log = 'xy', bty = "l")
points(m2predecessor ~ m2successor, data = sfc_all[ctr != "it"])
points(m2predecessor ~ m2successor, data = sfc_all[ctr == "it"], col = 'blue')
abline(modlist$`excl. italy`, untf = T)
curve(coef(m)[1]*x + coef(m)[2]*x^2, add = T, col = 'blue')
text(c(4800, 3000), c(5000, 300), labels = c("Italy", "Rest"), col = c("blue", "black"))
dev.off()

# find points on fit
# italy
par(mfrow = c(1, 2))
plot(ecdf(bldobs[, ratio_to_successor - ratio_predicted]), 
    pch = 1, xlim = c(-0.2, 0.2))
abline(v = c(-0.025, 0.058)) # so between these values
# rest
plot(ecdf(bldobs[ctr != "it" & endm2 != 0 & successor_endm2 != 0, ratio_to_successor - 0.53]),
    xlim = c(-0.01, 0.01), pch = NA, lty = 1)
abline(v = c(-0.00005, 0.00335))

# = predicted in buildings
bldobs[, predicted := "no"]
bldobs[ctr == "it" & between(ratio_to_successor - ratio_predicted, -0.025, 0.058), predicted := "italy"]
bldobs[ctr != "it" & between(ratio_to_successor - 0.53, -0.00005, 0.00335), predicted := "rest"]

bldobs[, .N, by = predicted]
bldobs[, mean(predicted != "no"), by = .(ctr == "it", bldindex)][order(ctr, bldindex)]

# = predicted in dynobs
dynobs[, predicted := "no"]
dynobs[ctr == "it" & between(ratio_to_successor - ratio_predicted, -0.025, 0.058), predicted := "italy"]
dynobs[ctr != "it" & between(ratio_to_successor - 0.53, -0.00005, 0.00335), predicted := "rest"]

library("brms")
ncores = parallel::detectCores() -1
options(mc.cores = ncores)

m = brm(log(m2predecessor) ~ (log(m2successor) | ctr + century), data = sfc_all)
preds = predict(m, 
    newdata = dynobs[, 
        list(m2successor = successor_endm2, 
             ctr = ctr, 
             century = floor(min(year) / 100) * 100),
        by = list(osmid, bldindex)], 
             # century = century)], 
    allow_new_levels = TRUE)
dynobs[, newpreds := exp(preds[, 1])]
dynobs[, .(osmid, bldindex, endm2, newpreds, successor_endm2, ctr, endm2 / newpreds)]
dynobs_alt = copy(dynobs)
dynobs_alt[!is.na(newpreds), m2 := m2 * (newpreds / endm2)]

fullobs = to_annual_obs(dynobs)
fullobs_alt = to_annual_obs(dynobs_alt)
fullobs[, decade := (trunc((year - 1) / 20) + 1) * 20] # so 1500 = 1481-1500
fullobs_alt[, decade := (trunc((year - 1) / 20) + 1) * 20] # so 1500 = 1481-1500

fullobs = statobs[, list(osmid, ctr)][fullobs, on = "osmid"]
fullobs_alt = statobs[, list(osmid, ctr)][fullobs_alt, on = "osmid"]

pdf("figs/altbackprojs.pdf")
par(mfrow = c(1, 1), bty = 'l')
plot(fullobs[between(year, 700, 1500), sum(im2_ann, na.rm = T), by = decade], 
    type = 'b', ylab = "m2/20y", xlab = 'decade')
lines(fullobs_alt[between(year, 700, 1500), sum(im2_ann, na.rm = T), by = decade], type = 'b', col = 2)
text(c(900, 1100), c(75e3, 50e3), c("alternative", "current"), col = 2:1)
dev.off()

pdf("figs/altbackprojs_panel.pdf", width = 9, height= 5)
par(mfrow = c(2, 4), mar = c(4, 4, 2, 0.5), bty = 'l', font.main = 1)
for (country in unique(fullobs$ctr)){
    plot(fullobs_alt[between(year, 700, 1500) & ctr == country, 
            sum(im2_ann, na.rm = T), by = decade], 
        type = 'l', ylab = "m2/20y", xlab = 'decade', 
        col = 2, main = country)
    lines(fullobs[between(year, 700, 1500) & ctr == country, 
            sum(im2_ann, na.rm = T), by = decade], type = 'l')

}

# eltjo's estimates
1.879^(1:3)

m = lm(end_surface ~ previous_surface - 1, data=sfc)
m1 = lm(end_surface ~ previous_surface - 1, data=sfc[factor==1,])
m2 = lm(end_surface ~ previous_surface - 1, data=sfc[factor==2,])
m3 = lm(end_surface ~ previous_surface - 1, data=sfc[factor==3,])
m1$coef
m2$coef
m3$coef

par(mfrow=c(1, 1))
plot(c(m1$coef, m2$coef, m3$coef), type='b', log='y', 
    xlab='Generation', ylab='log(coef)', bty='l')
lines(m1$coef^(1:3), col=2)

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
