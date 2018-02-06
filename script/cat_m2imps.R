rm(list=ls())

setwd("~/dropbox/cathedrals/")

library("data.table")
library("lme4")
library("texreg")

sfc = data.table::fread("dat/backproj.csv")
sfc[, city:=iconv(city, from="macroman", to="utf8")]
sfc[, church:=iconv(church, from="macroman", to="utf8")]

sfc[, period1:=as.numeric(gsub('\\D', '', period1))]
sfc[, period2:=as.numeric(gsub('\\D', '', period2))]

# N and N by 
sfc[, .N, by = factor]

par(mfrow=c(2, 2))
plot(end_surface ~ previous_surface, data=sfc)
plot(end_surface ~ previous_surface, data=sfc[factor==1,])
abline(lm(end_surface ~ previous_surface, data=sfc[factor==1,]))
plot(end_surface ~ previous_surface, data=sfc[factor==2,])
abline(lm(end_surface ~ previous_surface, data=sfc[factor==2,]))
plot(end_surface ~ previous_surface, data=sfc[factor==3,])
abline(lm(end_surface ~ previous_surface, data=sfc[factor==3,]))

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
par(mfrow=c(1, 3), font.main=1)
plot(end_surface ~ previous_surface, data=sfc, type='n', bty='l', main='1st predessor')
points(end_surface ~ previous_surface, data=sfc[factor==1,])
abline(m1, col='gray')
abline(a=0, b=1.879)
# text(2000, 1000, "OLS fit", col=2)
# text(1000, 5000, "Rule of thumb", col=1)
plot(end_surface ~ previous_surface, data=sfc, type='n', bty='l', main='2nd predessor')
points(end_surface ~ previous_surface, data=sfc[factor==2,])
abline(m2, col='gray')
abline(a=0, b=1.879^2)
plot(end_surface ~ previous_surface, data=sfc, type='n', bty='l', main='3rd predessor')
points(end_surface ~ previous_surface, data=sfc[factor==3,])
abline(m3, col='gray')
abline(a=0, b=1.879^3)
dev.off()

