setwd("~/dropbox/cathedrals")

library("zoo")
library("data.table")
library("texreg")

fullobs_sp <- data.table::fread("gunzip -c dat/fullobs_sp.csv.gz")
disasters = data.table::fread("dat/disasters.csv")
dynobs = data.table::fread("dat/dynobs.csv")
statobs = data.table::fread("dat/statobs.csv")

disasters = disasters[osmid != "64181197"]
disasters = disasters[!is.na(year)]

disasters[, year := as.numeric(year)]
disasters[, century:=(trunc((year - 1) / 100) + 1) * 100] # so 1500 = 1481-1500

M = 9 # number of imputations
impvrbs = names(fullobs_sp)[grepl('im3_ann\\d', names(fullobs_sp))]

# average building / phase length
fullobs_sp[irestphase != 1, .N, by = .(osmid, iphaselength)][, mean(N)]
fullobs_sp[irestphase != 1, .N, by = .(osmid, ibldindex)][, mean(N)]

disasters_sumstats = statobs[disasters, on = 'osmid']

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

disasters[statobs, on = 'osmid'][, .N, by = !is.na(cause)]

disasters150 = disasters[
    !is.na(year), 
    list(cause = cause, 
         war = war,
         other = other,
         earthquake = earthquake,
         year = seq(year, year + 150)), 
    by = .(osmid, groupyear = year)]

fullobs_sp = disasters150[fullobs_sp, on = c("osmid", "year")]
fullobs_sp[, afterdisaster := !is.na(cause)]
fullobs_sp[, century := (trunc((year - 1) / 100) + 1) * 100] # so 1500 = 1481-1500

churchobs = fullobs_sp[between(year, 700, 1500), 
    list(share_afterdisaster = mean(afterdisaster), 
        im3_dec = base::sum(.SD, na.rm=T) / M),
    by = .(osmid, ctr, decade, century, category),
    .SDcols = impvrbs]
churchobs[, afterdisaster := share_afterdisaster > 0.5]

# some mean of exp logs means I can't do log(im3_dec)
churchobs[, mean(im3_dec, na.rm = T), by = afterdisaster]
churchobs[, mean(im3_dec, na.rm = T), by = list(ctr, afterdisaster)][order(ctr)]

m = lm(im3_dec ~ afterdisaster*ctr + as.factor(century), data = churchobs)
summary(m)

churchobs[, pred_asis := predict(m, 
    newdata = data.table(
        century = century, 
        ctr = ctr, 
        afterdisaster = afterdisaster))]
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
plot(original ~ decade, data = eudec, type = 'l', lwd = 1.5, bty = 'l', ylab = 'm3/20y')
lines(corrected ~ decade, data = eudec, type = 'l', lwd = 1.5, col = 2)
text(x = c(900, 1100), y = c(1.5e6 ,1e6), c("original", "corrected"), col = c(1, 2))
dev.off()

writeLines(
    knitr::kable(
        eudec[, sum(corrected) / sum(original), 
            by = century / 100],
        digits = 2, format = "html"),
    "tab/correct_shares.html")

ctrdec = churchobs[, 
    list(
        original = sum(im3_dec, na.rm = T), 
        corrected = sum(im3_dec - correction, na.rm = T)), 
    by = .(decade, ctr)]

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

# do earthquakes only?
