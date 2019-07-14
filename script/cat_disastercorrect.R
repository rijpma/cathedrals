setwd("~/dropbox/cathedrals")

library("zoo")
library("data.table")
library("texreg")
library("lmtest")
library("sandwich")

source("script/cat_functions.r")

fullobs_sp <- data.table::fread("gunzip -c dat/fullobs_sp.csv.gz")
disasters = data.table::fread("dat/disasters.csv")
dynobs = data.table::fread("dat/dynobs.csv")
statobs = data.table::fread("dat/statobs.csv")

disasters = disasters[osmid != "64181197"]
disasters = disasters[!is.na(year)]

disasters[, year := as.numeric(year)]
disasters[, century:=(trunc((year - 1) / 100) + 1) * 100] # so 1500 = 1481-1500
disasters[, other := other + fire + other_natural]
disasters[war == TRUE, cause := "war"]
disasters[earthquake == TRUE, cause := "earthquake"]
disasters[other == TRUE, cause := "other"]
# disasters[other_natural == TRUE, cause := "other_natural"]
# disasters[fire == TRUE, cause := "fire"]
# too few obs for splitting further

M = 9 # number of imputations
impvrbs = names(fullobs_sp)[grepl('im3_ann\\d', names(fullobs_sp))]

# average building / phase length
fullobs_sp[irestphase != 1, .N, by = .(osmid, iphaselength)][, mean(N)]
fullobs_sp[irestphase != 1, .N, by = .(osmid, ibldindex)][, mean(N)]

disasters_sumstats = statobs[disasters, on = 'osmid']

tot = rowSums(table(disasters_sumstats$ctr, disasters_sumstats$cause))
tot / sum(tot)
statobs[, .N, by = ctr][, .(ctr, N / sum(N))]

table(disasters_sumstats$century, disasters_sumstats$cause)

# length building
hist(dynobs[, diff(range(year)), by = list(osmid, bldindex)][, V1])

fwrite(rbindlist(list(
    disasters_sumstats[
        order(ctr), list(
            earthquake = sum(earthquake), 
            war = sum(war), 
            other = sum(other)),
        by = list(ctr)][order(earthquake + war + other)],
    disasters_sumstats[!is.na(year), list(
            earthquake = sum(earthquake), 
            war = sum(war), 
            other = sum(other)),
        by = list(century)][order(century)])),
    file = "dat/disastercounts.csv")

# number of disasters on churches
disasters[statobs, on = 'osmid'][, .N, by = !is.na(cause)]
disasters[statobs, on = 'osmid'][, .N, by = !is.na(cause)][, N / sum(N)]
# number of churches ever disastered
disasters[statobs, on = 'osmid'][, uniqueN(osmid), by = !is.na(cause)][, V1 / sum(V1)]

disasters150 = disasters[
    !is.na(year), 
    list(cause = cause, 
         war = war,
         other = other,
         earthquake = earthquake,
         year = seq(year, year + 150)), 
    by = .(osmid, groupyear = year)]
# 150s can overlap, so only keep most recent
disasters150[order(osmid, groupyear, year), kill := duplicated(year, fromLast = TRUE), by = osmid]
disasters150 = disasters150[kill == FALSE]

fullobs_sp = disasters150[fullobs_sp, on = c("osmid", "year")]

if (sum(duplicated(fullobs_sp, by = c("osmid", "year"))) > 0){
    warning("Duplicates in dataset")
}

fullobs_sp[, afterdisaster := !is.na(cause)]
fullobs_sp[is.na(war), war := FALSE]
fullobs_sp[is.na(earthquake), earthquake := FALSE]
fullobs_sp[is.na(other), other := 0] # because sum earlier
fullobs_sp[, century := (trunc((year - 1) / 100) + 1) * 100] # so 1500 = 1481-1500

endsizes = fullobs_sp[,
    list(m3_not_imputed = base::sum(im3_ann, na.rm = TRUE), 
        m3_imputed = base::sum(.SD, na.rm = TRUE) / 9), 
    .SDcols = impvrbs,
    by = list(osmid, ibldindex)]

churchobs = fullobs_sp[data.table::between(year, 700, 1500), 
    list(
        share_afterdisaster = mean(afterdisaster, na.rm = TRUE), 
        share_earthquake = mean(earthquake, na.rm = TRUE),
        share_war = mean(war, na.rm = TRUE),
        share_other = mean(other, na.rm = TRUE),
        bldindex = mean(ibldindex),
        category = unique(category),
        im3_dec = base::sum(.SD, na.rm=T) / M),
    by = .(osmid, ctr, decade, century),
    .SDcols = impvrbs]
churchobs[, afterdisaster := share_afterdisaster > 0.5]
churchobs[, earthquake := share_earthquake > 0.5]
churchobs[, war := share_war > 0.5]
churchobs[, other := share_other > 0.5]

churchobs[earthquake + war + other == afterdisaster, list(decade, osmid, earthquake, war, other, afterdisaster)]
churchobs[earthquake + war + other != afterdisaster, list(decade, osmid, earthquake, war, other, afterdisaster)]
# these are all cases where two overlapping disaster-specific periods add up to afterdisaster==TRUE for both, but not for one

churchobs[, bldindex := round(bldindex)] # 0.5 now is old one!
churchobs = endsizes[, list(osmid, 
        bldindex = ibldindex + 1, 
        prevsize = m3_not_imputed) ][ 
    churchobs, on = c("osmid", "bldindex")]
churchobs[bldindex == 1, prevsize := 0]
churchobs[is.na(bldindex), prevsize := 0] # one-phase buildings

m_nocen = lm(im3_dec / 1000 ~ ctr:disaster + ctr, 
    data = churchobs[, .(im3_dec, ctr, century, disaster = as.numeric(afterdisaster))])
m_all = lm(im3_dec / 1000 ~ ctr:disaster + ctr + factor(century), 
    data = churchobs[, .(im3_dec, ctr, century, disaster = as.numeric(afterdisaster))])
m_quake = lm(im3_dec / 1000 ~ ctr:disaster + ctr + factor(century), 
    data = churchobs[, .(im3_dec, ctr, century, disaster = as.numeric(earthquake))])
m_war = lm(im3_dec / 1000 ~ ctr:disaster + ctr + factor(century), 
    data = churchobs[, .(im3_dec, ctr, century, disaster = as.numeric(war))])
m_other = lm(im3_dec / 1000 ~ ctr:disaster + ctr + factor(century), 
    data = churchobs[, .(im3_dec, ctr, century, disaster = as.numeric(other))])
m_all_prev = lm(im3_dec / 1000 ~ ctr:disaster + ctr + factor(century) + prevsize, 
    data = churchobs[, .(im3_dec, ctr, century, disaster = as.numeric(afterdisaster), prevsize)])
m_pois = glm(im3_dec / 1000 ~ ctr:disaster + ctr + factor(century), 
    data = churchobs[im3_dec >= 0, .(im3_dec, ctr, century, disaster = as.numeric(afterdisaster), prevsize)],
    family = quasipoisson(link = "log"))
m_log = lm(log(im3_dec / 1000) ~ ctr:disaster + ctr + factor(century), 
    data = churchobs[im3_dec > 0, .(im3_dec, ctr, century, disaster = as.numeric(afterdisaster), prevsize)])
m_log1p = lm(log1p(im3_dec / 1000) ~ ctr:disaster + ctr + factor(century), 
    data = churchobs[im3_dec >=0, .(im3_dec, ctr, century, disaster = as.numeric(afterdisaster), prevsize)])

mlist = list(m_nocen = m_nocen, m_all = m_all, m_quake = m_quake, 
    m_war = m_war, m_other = m_other, m_all_prev = m_all_prev, 
    m_pois = m_pois, m_log = m_log, m_log1p, m_log1p)

selist = lapply(mlist, coeftest, vcov. = vcovHC)
pvlist = lapply(selist, '[', i =, j = 4)
selist = lapply(selist, '[', i =, j = 2)
texreg::screenreg(mlist[1:4], 
    override.pval = pvlist[1:4],
    override.se = selist[1:4])
texreg::screenreg(mlist[c(2, 7:9)], 
    override.pval = pvlist[c(2, 7:9)],
    override.se = selist[c(2, 7:9)])

cfmap = list(
    # "ctrbe:disaster" = "Belgium × disaster",
    "ctrch:disaster" = "Switzerland × disaster",
    "ctrde:disaster" = "Germany × disaster",
    "ctrfr:disaster" = "France × disaster",
    "ctrit:disaster" = "Italy × disaster",
    "ctrnl:disaster" = "Netherlands × disaster",
    "ctruk:disaster" = "Great Britain × disaster",
    "ctrbe" = "Belgium",
    "ctrch" = "Switzerland",
    "ctrde" = "Germany",
    "ctrfr" = "France",
    "ctrit" = "Italy",
    "ctrnl" = "Netherlands",
    "ctruk" = "Great Britain",
    "factor(century)800" = "800",
    "factor(century)900" = "900",
    "factor(century)1000" = "1000",
    "factor(century)1100" = "1100",
    "factor(century)1200" = "1200",
    "factor(century)1300" = "1300",
    "factor(century)1400" = "1400",
    "factor(century)1500" = "1500",
    "prevsize" = "predecessor size",
    "(Intercept)" = "Intercept"
)
texreg::htmlreg(mlist[1:5], 
    override.se = selist[1:5],
    override.pval = pvlist[1:5],
    custom.coef.map = cfmap,
    file = "tab/disasterregs.html")

cfslist = lapply(mlist, function(m) coef(m)[grep("disaster", names(coef(m)))])
cfsl = data.table(
    var = names(cfslist$m_all), 
    coef_all = cfslist$m_all, 
    coef_quake = cfslist$m_quake, 
    coef_war = cfslist$m_war)
cfsl[, ctr := stringi::stri_match_first_regex(var, "ctr(.*)\\:")[, 2]]
# ok because overall intercept omitted

churchobs = cfsl[churchobs, on = "ctr"]
churchobs[afterdisaster == TRUE, corrected_all := im3_dec - (coef_all * 1000)]
churchobs[is.na(corrected_all), corrected_all := im3_dec]

churchobs[earthquake == TRUE, corrected_quake := im3_dec - (coef_quake * 1000)]
churchobs[is.na(corrected_quake), corrected_quake := im3_dec]

churchobs[afterdisaster == TRUE, corrected_war := im3_dec - (coef_war * 1000)]
churchobs[is.na(corrected_war), corrected_war := im3_dec]

churchobs[ctr == "it", ctr := "Italy"]
churchobs[ctr == "fr", ctr := "France"]
churchobs[ctr == "de" | ctr == "ch", ctr := "Germany (incl. Switzerland)"]
churchobs[ctr == "nl" | ctr == "be", ctr := "Low Countries"]
churchobs[ctr == "uk", ctr := "Great Britain"]

toplot = rbindlist(list(
        churchobs[, list(original = sum(im3_dec / 1e6, na.rm = TRUE), 
                corrected_all =     sum(corrected_all / 1e6, na.rm = TRUE),
                corrected_quake = sum(corrected_quake / 1e6, na.rm = TRUE),
                corrected_war = sum(corrected_war / 1e6, na.rm = TRUE)), 
            by = list(decade, ctr)],
        churchobs[, list(original = sum(im3_dec / 1e6, na.rm = TRUE), 
                corrected_all =     sum(corrected_all / 1e6, na.rm = TRUE),
                corrected_quake = sum(corrected_quake / 1e6, na.rm = TRUE),
                corrected_war = sum(corrected_war / 1e6, na.rm = TRUE)), 
            by = list(decade)]
    ),
    fill = TRUE
)
toplot[is.na(ctr), ctr := "W. Europe"]

pdf("figs/disasterpanel.pdf", width = 10, height = 6)
ctrs = c("Italy", "France", "Germany (incl. Switzerland)",
    "Low Countries", "Great Britain", "W. Europe")
par(mfrow=c(2, 3), mar=c(4, 4, 1.5, 0.5), font.main=1)
for (i in 1:length(ctrs)){
    country = ctrs[i]
    plot(original ~ decade, data = toplot[ctr == country], 
        type = 'l', bty = 'l', main = country, ylab = "", 
        col = "gray", lwd = 1.5)
    lines(corrected_all ~ decade, data = toplot[ctr == country], 
        type = 'l', lwd = 1.5)
    if (i == 1 | i == 4) title(ylab = m3y20lblm)
}
legend("topleft", fill = c("gray", "black"), legend = c("original", "adjusted (all disasters)"))
# text(x = c(900, 1100), y = c(1.5, 1), labels = c("original", "adjusted"), col = c("gray", "black"))
par(mfrow=c(2, 3), mar=c(4, 4, 1.5, 0.5), font.main=1)
for (i in 1:length(ctrs)){
    country = ctrs[i]
    plot(original ~ decade, data = toplot[ctr == country], 
        type = 'l', bty = 'l', main = country, ylab = "", 
        col = "gray", lwd = 1.5)
    lines(corrected_quake ~ decade, data = toplot[ctr == country], 
        type = 'l', lwd = 1.5)
    if (i == 1 | i == 4) title(ylab = m3y20lblm)
}
legend("topleft", fill = c("gray", "black"), legend = c("original", "adjusted (earthquakes)"))
# text(x = c(900, 1100), y = c(1.5, 1), labels = c("original", "adjusted"), col = c("gray", "black"))
dev.off()
hist(toplot[, (original - corrected_all) / original])
hist(toplot[, (original - corrected_quake) / original])


# old test versions
ms = glm(im3_dec ~ ctr*afterdisaster, data = churchobs[im3_dec >= 0], family = poisson())
mp = glm(im3_dec ~ ctr*afterdisaster + factor(century), data = churchobs[im3_dec >= 0], family = poisson())
mp2 = glm(im3_dec ~ ctr*afterdisaster + factor(century) + category + log1p(prevsize), data = churchobs[im3_dec >= 0], family = poisson())
stargazer::stargazer(ms, mp, mp2, type = 'text')

churchobs[prevsize > 0, mean(prevsize), by = afterdisaster]

d = as.data.table(mp$model)[, mean(im3_dec), by = list(ctr, afterdisaster, century = `factor(century)`)]
d$pred = predict(m, newdata = d)
d$nodisaster = predict(m, newdata = data.table(d[, -"afterdisaster"], afterdisaster = FALSE))
d$corrected = d$V1 - (d$pred - d$nodisaster)

d$pred_pois = predict(mp, newdata = d)
d$nodisaster_pois = predict(mp, newdata = data.table(d[, -"afterdisaster"], afterdisaster = FALSE))
d$corrected_pois = d$V1 - (exp(d$pred_pois) - exp(d$nodisaster_pois))
d[order(ctr, century)]

cfs = coef(mp)[grep("afterdisasterTRUE", names(coef(mp)))]
cfs = data.table(cfs, names(cfs))
cfs[, ctr := stringi::stri_match_first_regex(V2, "ctr(.*)\\:")[, 2]]
cfs[is.na(ctr), ctr := "be"]
cfs[, cfs := ifelse(ctr == 'be', cfs, cfs + cfs[ctr=='be'])]

d = cfs[d, on = "ctr"]
d[afterdisaster == TRUE, corrected_mult := V1 / exp(cfs)]
d[is.na(corrected_mult), corrected_mult := V1]

churchobs = cfs[churchobs, on = "ctr"]
churchobs[afterdisaster == TRUE, corrected := im3_dec / exp(cfs)]
churchobs[is.na(corrected), corrected := im3_dec]

churchobs[afterdisaster == TRUE, list(ctr, cfs, im3_dec, corrected)]
churchobs[afterdisaster == TRUE, list(range(im3_dec), range(corrected))]
churchobs[afterdisaster == FALSE, list(ctr, cfs, im3_dec, corrected)]

churchobs[, pred_old := predict(m, newdata = .SD)]
churchobs[, pred_nodisasterworld := predict(m, newdata = 
    data.frame(afterdisaster = FALSE, century, ctr))]
churchobs[, corrected_old := im3_dec - (pred_old - pred_nodisasterworld)]

averages = churchobs[, 
    list(disasterbonus = mean(im3_dec[afterdisaster == TRUE]) - mean(im3_dec[afterdisaster == FALSE])), 
    by = list(ctr)]
churchobs = averages[churchobs, on = "ctr"]
churchobs[, corrected_avg := ifelse(afterdisaster == TRUE, im3_dec - disasterbonus, im3_dec)]

churchobs[afterdisaster == TRUE, list(ctr, cfs, im3_dec, corrected_old, corrected_avg)]
churchobs[afterdisaster == TRUE, list(range(im3_dec), range(corrected_old), range(corrected_avg))]
churchobs[afterdisaster == FALSE, list(ctr, cfs, im3_dec, corrected_old, corrected_avg)]

x = churchobs[, list(original = sum(im3_dec, na.rm = TRUE), 
                corrected =     sum(corrected, na.rm = TRUE),
                corrected_old = sum(corrected_old, na.rm = TRUE),
                corrected_avg = sum(corrected_avg, na.rm = TRUE),
                corrected_l =   sum(corrected_l, na.rm = TRUE)), 
    by = list(decade, ctr)]

library("ggplot2")
pdf("figs/discorrections.pdf")
out = ggplot(x, aes(decade, original)) + 
    geom_line() + 
    geom_line(aes(decade, corrected), col = "red") + 
    facet_wrap(~ ctr, scales = "free_y") + 
    labs(title = paste("poisson", formula(mp), collapse = " "))
# matplot(x$decade, x[, -"decade"], type = 'b')
print(out)
out = ggplot(x, aes(decade, original)) + 
    geom_line() + 
    geom_line(aes(decade, corrected_l), col = "red") + 
    facet_wrap(~ ctr, scales = "free_y") + 
    labs(title = paste("linear (no log)", formula(m)), collapse = " ")
print(out)
out = ggplot(x, aes(decade, original)) + 
    geom_line() + 
    geom_line(aes(decade, corrected_avg), col = "red") + 
    facet_wrap(~ ctr, scales = "free_y") + 
    labs(title = "avg subtraction")
print(out)
out = ggplot(x, aes(decade, original)) + 
    geom_line() + 
    geom_line(aes(decade, corrected_old), col = "red") + 
    facet_wrap(~ ctr, scales = "free_y") + 
    labs(title = "old: fit - fit")
print(out)
dev.off()

shares = churchobs[, mean(afterdisaster), by = list(ctr, century)]
ggplot(shares, aes(century, V1)) + 
    geom_line(aes(col = ctr))
summary(mp)


# some mean of exp logs means I can't do log(im3_dec)
churchobs[, mean(im3_dec, na.rm = T), by = afterdisaster]
churchobs[, mean(im3_dec, na.rm = T), by = list(ctr, afterdisaster)][order(ctr)]
ml = lm(log1p(im3_dec) ~ afterdisaster*ctr + as.factor(century) + category, data = churchobs)

m = lm(im3_dec ~ afterdisaster*ctr + as.factor(century), data = churchobs)
ml = lm(log1p(im3_dec) ~ afterdisaster*ctr + as.factor(century), data = churchobs)
mt = tobit(log1p(im3_dec) ~ afterdisaster*ctr + as.factor(century), 
    data = churchobs, left = 0)
mt2 = tobit(log(im3_dec) ~ afterdisaster*ctr + as.factor(century), 
    data = churchobs, left = 0)
mt3 = survreg(Surv(log1p(im3_dec), log1p(im3_dec) > 0) ~ afterdisaster*ctr + as.factor(century),
    data = churchobs, dist = "gaussian")
mp = glm(im3_dec ~ afterdisaster*ctr + as.factor(century), 
    data = churchobs[im3_dec >= 0], family = poisson())

screenreg(list(m, ml, mp))
screenreg(list(m, mt, mt2, mt3))

topredict = unique(ml$model[, -1])
names(topredict) = c("afterdisaster", "ctr", "century")
topredict$predicted = predict(ml, newdata = topredict)
topredict = as.data.table(topredict)
topredict[order(ctr, afterdisaster), mean(predicted), by = list(ctr, afterdisaster)]

screenreg(list(lm(im3_dec ~ afterdisaster, data = churchobs[ctr == "de"]),
    lm(im3_dec ~ ctr:afterdisaster, data = churchobs)))

churchobs[, pred_asis := predict(m, 
    newdata = data.table(
        century = century, 
        ctr = ctr, 
        afterdisaster = afterdisaster))]
churchobs[, pred_asis_poisson := predict(mp, 
    newdata = data.table(
        century = century, 
        ctr = ctr, 
        afterdisaster = afterdisaster))]
churchobs[, pred_asis_log := predict(ml, 
    newdata = data.table(
        century = century, 
        ctr = ctr, 
        afterdisaster = afterdisaster))]
churchobs[, pred_asis_log_rly := exp(pred_asis_log) * exp(sqrt(mean(resid(ml)^2))^2 / 2)]

churchobs[, .(pred_asis, exp(pred_asis_poisson), exp(pred_asis_log), pred_asis_log_rly)]

predict(mp, newdata = data.table(century = "1200", ctr = "it", afterdisaster = FALSE))


plot(exp(pred_asis_poisson) ~ pred_asis, data = churchobs)
plot(exp(pred_asis_log) ~ pred_asis, data = churchobs)

hist(churchobs$im3_dec)
hist(churchobs$pred_asis)
hist(exp(churchobs$pred_asis_poisson))

churchobs[, pred_nodisasterworld := predict(m, 
    newdata = data.table(
        century = century, 
        ctr = ctr, 
        afterdisaster = FALSE))]
churchobs[, correction := pred_asis - pred_nodisasterworld]
eudec = churchobs[, 
    list(
        original = sum(im3_dec, na.rm = T), 
        corrected = sum(im3_dec - correction, na.rm = T),
        century = century), 
    by = decade]

pdf("figs/eu_disaster_corrected.pdf", height = 6)
plot(original / 1e6 ~ decade, data = eudec, type = 'l', lwd = 1.5, bty = 'l', 
    ylab = m3y20lblm, col = "gray")
lines(corrected / 1e6 ~ decade, data = eudec, type = 'l', lwd = 1.5)
text(x = c(900, 1100), y = c(1.5 ,1), 
    c("unadjusted", "adjusted"), col = c("gray", "black"))
dev.off()

writeLines(
    knitr::kable(
        eudec[, sum(corrected) / sum(original), 
            by = century / 100],
        digits = 2, format = "html"),
    "tab/correct_shares.html")

churchobs[ctr %in% c("de", "ch"), country := "Germany"]
churchobs[ctr %in% c("be", "nl"), country := "Low Countries"]
churchobs[ctr %in% c("fr"), country := "France"]
churchobs[ctr %in% c("it"), country := "Italy"]
churchobs[ctr %in% c("uk"), country := "Britain"]
ctrdec = churchobs[, 
    list(
        original = sum(im3_dec, na.rm = T), 
        corrected = sum(im3_dec - correction, na.rm = T)), 
    by = .(decade, country)]

pdf("figs/disastercorrectionpanel.pdf", width=9, height = 5)
par(mfrow=c(2, 4), mar=c(4, 4, 1.5, 0.5), font.main=1)
for (country in unique(ctrdec$ctr)){
    plot(original ~ decade, data = ctrdec[ctr == country], 
        type = 'l', bty = 'l', main = country, ylab = "m3/dec")
    lines(corrected ~ decade, data = ctrdec[ctr == country], 
        type = 'l', col = 2, lwd = 1.5)
}
plot(original ~ decade, data = eudec, type = 'l', lwd = 1.5, bty = 'l', ylab = 'm3/20y', main = "all")
lines(corrected ~ decade, data = eudec, type = 'l', lwd = 1.5, col = 2)
text(x = c(900, 1150), y = c(1.5e6 ,1e6), c("original", "corrected"), col = c(1, 2))
dev.off()

par(mfrow=c(2, 3), mar=c(4, 4, 1.5, 0.5), font.main=1)
plot(original ~ decade, data = ctrdec[country == "Italy"], 
    type = 'l', col = "gray", bty = 'l', 
    main = "Italy", ylab = m3y20lbl)
lines(corrected ~ decade, data = ctrdec[country == "Italy"], 
    type = 'l', col = 1, lwd = 1.5)
plot(original ~ decade, data = ctrdec[country == "Low Countries"], 
    type = 'l', col = "gray", bty = 'l', 
    main = "Low Countries", ylab = m3y20lbl)
lines(corrected ~ decade, data = ctrdec[country == "Low Countries"], 
    type = 'l', col = 1, lwd = 1.5)
plot(original ~ decade, data = eudec, type = 'l', lwd = 1.5, bty = 'l', 
    ylab = m3y20lbl, col = "gray", main = "All")
lines(corrected ~ decade, data = eudec, type = 'l', lwd = 1.5)
text(x = c(900, 1100), y = c(1.5e6 ,1e6), 
    c("original", "corrected"), col = c("gray", "black"))
# etc.

# do earthquakes only?
