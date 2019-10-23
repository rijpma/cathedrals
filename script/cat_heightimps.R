# explore height imputation procedure

rm(list=ls())

setwd("~/dropbox/cathedrals/")

library("data.table")
library("texreg")

hgt = data.table::fread("dat/heights.csv")
hgt[, city := iconv(city, from="macroman", to="utf8")]
hgt[, church := iconv(church, from="macroman", to="utf8")]
hgt[, build_length := finish - start]
hgt[, century := floor(((start + finish) / 2) / 100) * 100]

hgt_ita = data.table::fread("dat/italyheights.csv")
setnames(hgt_ita, 
    c("started", "surface", "façade height"),
    c("start", "ground_surface_m2", "facade_height"))

m_base = lm(nave_height ~ ground_surface_m2, data=hgt)
m_sq = lm(nave_height ~ sqrt(ground_surface_m2), data=hgt)
m_ll = lm(log(nave_height) ~ log(ground_surface_m2), data=hgt)
m_l = lm(log(nave_height) ~ log(ground_surface_m2) + I(start/100), data=hgt)
m_lld = lm(log(nave_height) ~ log(ground_surface_m2) + I(start/100) + ctr, data=hgt)
# eb's model
m_sq_or = lm(nave_height ~ sqrt(ground_surface_m2) - 1, data=hgt)
m_sq_it = lm(facade_height ~ sqrt(ground_surface_m2) - 1, data=hgt_ita)
# eb's other model
m_nl_it = nls(facade_height ~ a*ground_surface_m2^b, data = hgt_ita,
    start = list(a = 1, b = 0.5))

pdf("figs/height_surface_sq_ci.pdf")
plot(nave_height ~ ground_surface_m2, data=hgt, bty='l', type='n',
    xlab = 'Ground surface (m²)', ylab = 'Nave height (m)')
fit = predict(m_sq_or, 
    newdata = data.frame(ground_surface_m2 = 0:8000), 
    se=TRUE, interval='confidence')
lines(x=0:8000, fit$fit[, 'fit'], lwd=1.5)
points(nave_height ~ ground_surface_m2, data=hgt, bty='l', col = 'gray30')
dev.off()

pdf("figs/height_surface_inclit.pdf")
plot(facade_height ~ ground_surface_m2, data=hgt_ita, bty='l', type='n',
    xlab = 'Ground surface (m²)', ylab = 'Nave height (m)')
points(nave_height ~ ground_surface_m2, data=hgt)
points(facade_height ~ ground_surface_m2, data=hgt_ita, col = "blue")
fit = predict(m_sq_or, newdata=data.frame(ground_surface_m2=0:10000), 
    se=TRUE, interval='prediction', col = 2)
lines(x=0:10000, fit$fit[, 'fit'], col=1, lwd=1.5)
fit = predict(m_nl_it, newdata=data.frame(ground_surface_m2=0:10000), 
    se=TRUE, interval='prediction', col = 2)
lines(x=0:10000, fit, col='blue', lwd=1.5)
text(x = c(7500, 8400), y = c(45, 38), 
    labels = c("Italy", "Rest Europe"), col = c("blue", "black"))
dev.off()

cat("total observations for height imps: ")
nrow(m_sq_or$model) + nrow(hgt_ita[name != ""])

cat("manual R2 because model w/o intercept: ")
sse = sum((m_sq_or$fitted.values - m_sq_or$model$nave_height)^2)
sst = sum((m_sq_or$model$nave_height - mean(m_sq_or$model$nave_height))^2)
1 - sse/sst

# alternative predictions
# pdf("figs/height_v_surface.pdf")
# par(mfrow=c(2, 2))
# plot(nave_height ~ ground_surface_m2, data=hgt, bty='l')
# abline(m_base, col=2)
# plot(nave_height ~ sqrt(ground_surface_m2), data=hgt, bty='l')
# abline(m_sq, col=2, untf=T)
# plot(log(nave_height) ~ log(ground_surface_m2), data=hgt, bty='l')
# abline(m_ll, untf=T, col=2)
# plot(x=m_l$model[, "log(ground_surface_m2)"], 
#     y=m_l$model[, "log(nave_height)"] - coef(m_l)["I(start/100)"] * m_l$model[, "I(start/100)"],
#     xlab="log(ground_surface_m2)", 
#     ylab="log(nave_height), crtd for build year",
#     bty='l')
# abline(reg = m_l, col = 2)
# dev.off()

# m_ml = lme4::lmer(log(nave_height) ~ start + (log(ground_surface_m2) | century), data=hgt)

# library("randomForest")
# x = hgt[, list(nave_height, ground_surface_m2, ctr, start, finish, build_length, century)]
# x = x[complete.cases(x), ]
# x = x[, ctr:=as.factor(ctr)]
# m_rf = randomForest::randomForest(log(nave_height) ~ ., data=x)

# par(mfrow=c(1, 3))
# plot(predict(m_rf, newdata=x), log(x$nave_height))
# curve(1 * x, add = TRUE)
# plot(predict(m_ml, newdata=x), log(x$nave_height))
# curve(1 * x, add = TRUE)
# plot(predict(m_lld, newdata=x), log(x$nave_height))
# curve(1 * x, add = TRUE)
