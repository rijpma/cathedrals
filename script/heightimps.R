# explore height imputation procedure

rm(list=ls())

setwd("~/dropbox/cathedrals/")

library("data.table")

hgt = data.table::fread("dat/heights.csv")

hgt[, city := iconv(city, from="macroman", to="utf8")]
hgt[, church := iconv(church, from="macroman", to="utf8")]
hgt[, build_length := finish - start]
hgt[, century := floor(((start + finish) / 2) / 100) * 100]

hgt_ita = data.table::fread("dat/italyheights.csv")
setnames(hgt_ita, 
    c("started", "surface", "façade height"),
    c("start", "ground_surface_m2", "facade_height"))

# some models
# eb's model
m_sq_or = lm(nave_height ~ sqrt(ground_surface_m2) - 1, data=hgt)
m_sq_it = lm(facade_height ~ sqrt(ground_surface_m2) - 1, data=hgt_ita)
# eb's other model
m_nl_it = nls(facade_height ~ a*ground_surface_m2^b, data = hgt_ita,
    start = list(a = 1, b = 0.5))

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
