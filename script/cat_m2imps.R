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
plot(c(m1$coef, m2$coef, m3$coef), type='b')
plot(c(m1$coef, m2$coef, m3$coef), type='b', log='y', 
    xlab='log(ratio)', ylab='building _n', bty='l')
lines(m1$coef^(1:3), col=2)

pdf("figs/surface_fits.pdf", height=3, width=8)
par(mfrow=c(1, 3), font.main=1)
plot(end_surface ~ previous_surface, data=sfc, type='n', bty='l', main='1st predessor')
points(end_surface ~ previous_surface, data=sfc[factor==1,])
abline(m1, col=2)
abline(a=0, b=1.879)
# text(2000, 1000, "OLS fit", col=2)
# text(1000, 5000, "Rule of thumb", col=1)
plot(end_surface ~ previous_surface, data=sfc, type='n', bty='l', main='2nd predessor')
points(end_surface ~ previous_surface, data=sfc[factor==2,])
abline(m2, col=2)
abline(a=0, b=1.879^2)
plot(end_surface ~ previous_surface, data=sfc, type='n', bty='l', main='3rd predessor')
points(end_surface ~ previous_surface, data=sfc[factor==3,])
abline(m3, col=2)
abline(a=0, b=1.879^3)
dev.off()


mml = lme4::lmer(end_surface ~ -1 + previous_surface|factor, data=sfc)
ranef(mml)

summary(lm(end_surface ~ previous_surface*factor(factor) - 1, data=sfc))

library("rstan")
options(mc.cores = parallel::detectCores() - 1)
fit = rstan::stan("script/hier2.stan", iter=1000, chains=3,
    data=list(D=1, 
        N=nrow(sfc), 
        L=length(unique(sfc$factor)), 
        prev_surface=sfc$previous_surface, 
        end_surface=sfc$end_surface,
        phases=sfc$factor))

fit
traceplot(fit, inc_warmup=F, "beta_rstr")
get_posterior_mean(fit, 'beta')
get_posterior_mean(fit, 'beta_rstr')
smpls = extract(fit)
hist(smpls$beta[, 1])
hist(smpls$beta[, 2])
hist(smpls$beta[, 3])
# maybe just use a log transform on the pars?
plot(1:8, 1.6^(1:8), type='b', log='y')

betas = apply(smpls$beta_rstr, 2, mean)
betas = apply(smpls$beta, 2, mean)
betas_ci = apply(smpls$beta, 2, quantile, c(0.05, 0.95))

plot(end_surface ~ previous_surface, data=sfc, col=sfc$factor)
abline(a=0, b=betas[1])
abline(a=0, b=betas[2], col=2)
abline(a=0, b=betas[3], col=3)


plot(1:3, betas, type='b')
plot(1:3, betas, type='b', ylim=c(1, 3))
segments(1:3, betas_ci[1, ], 1:3, betas_ci[2, ])
betas[2] / betas[1]
betas[3] / betas[2]

plot(1:3, ranef(mml)$factor[, 1])
ranef(mml)$factor[2, 1] / ranef(mml)$factor[1, 1]
ranef(mml)$factor[3, 1] / ranef(mml)$factor[2, 1]

plot(ifelse(factor==1, end_surface, log(end_surface, factor)) ~ previous_surface, data=sfc)
sfc[, ifelse(factor==1, end_surface, log(end_surface, factor))]

plot(I(end_surface^(1/factor)) ~ previous_surface, data=sfc)
plot(end_surface ~ previous_surface, data=sfc, log='xy')
plot(I(end_surface^(1/factor)) ~ previous_surface, data=sfc, log='xy')

ms_base = lm(end_surface ~ previous_surface, data=sfc)
ms_fc = lm(end_surface ~ I(previous_surface^factor), data=sfc)
ms_yr = lm(end_surface ~ previous_surface + period1, data=sfc)

summary(ms_base)
summary(ms_fc)
