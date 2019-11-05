# compare our data with James data for Paris basin

rm(list = ls())

library("data.table")

setwd("~/dropbox/cathedrals")
source("script/cat_functions.r")

statobs = data.table::fread("dat/statobs.csv")
fullobs_sp = data.table::fread("gunzip -c  dat/fullobs_sp.csv.gz")
fullobs = fullobs_sp[, -names(statobs)[-1], with = F]

M = 9
impvrbs = grepr('im3_ann\\d', names(fullobs_sp))

james = data.table::fread("dat/james_exp.csv", header=T)
data.table::setnames(james, "id", "id_james")

yrs = grep('\\d{4}', names(james))

# per church totals
james$JU <- rowSums(james[, yrs, with=F], na.rm=T)
james$JU[rowSums(!is.na(james[, yrs, with=F]))==0] <- NA

# match to our data based on distance
mtchs = distmatch(lonlat1 = as.matrix(james[, list(Longitude, Latitude)]), 
                  lonlat2 = as.matrix(statobs[, list(lon, lat)]))
mtchs = mtchs[order(mtchs$dist), ]
mtchs = mtchs[!duplicated(mtchs$lonlat2), ]

james[mtchs$lonlat1, osmid := statobs$osmid[mtchs$lonlat2]]
james[mtchs$lonlat1, osmname :=statobs$osmname[mtchs$lonlat2]]
james[mtchs$lonlat1, osmcity :=statobs$city[mtchs$lonlat2]]

# Sainte-Geneviève and La Vistiation seem incorrect
james[osmid %in% c("55343672", "147457637"), osmid := NA] 

# comparison 1: our series v. James' series for Paris basin
lonrange = range(james$Longitude)
latrange = range(james$Latitude)
ourparisbasin = fullobs_sp[
    lon %between% lonrange & lat %between% latrange & year %between% c(1051, 1259), 
    list(m3dec = base::sum(.SD, na.rm=T) / M), 
    by = list(decade = floor(year / 10)*10), 
    .SDcols = impvrbs]
jamesparisbasin = melt(
    james[, yrs[-20], with = F], 
    measure.vars = yrs[-20], 
    variable.name = 'decade', 
    value.name = 'JU', 
    variable.factor = F)
jamesparisbasin = jamesparisbasin[, 
    list(JU = sum(JU, na.rm=T)), 
    by = list(decade = as.numeric(decade))]
comp = ourparisbasin[jamesparisbasin, on = 'decade']

comp[, JU_sc := scale(JU) * sd(m3dec) + mean(m3dec)]
comp[, m3_sc := scale(m3dec) * sd(JU) + mean(JU)]

pdf("figs/james_v_osm_series_hc.pdf", height=5, width = 9)
par(mfrow=c(1, 2), font.main=1)
plot(m3dec ~ decade, data = comp,
    type='l', lwd = 1.5, bty='l', xlim=c(1060, 1250),
    yaxt = "n",
    xlab='decade', ylab = m3y10lbl, main='OSM-based data')
lines(JU_sc ~ decade ,data = comp, col = 'gray')
axis1ks(2)
plot(JU ~ decade, data = comp,
    type='l', lwd = 1.5, bty='l', xlim=c(1060, 1250),
    yaxt = "n",
    xlab='decade', ylab="James's cost estimate per 10 years", 
    main = "James's data")
lines(m3_sc ~ decade ,data = comp, col = 'gray')
axis1ks(2)
dev.off()

# comparison 2: building sizes of matched churches in 1250 
james_mtchd = merge(
    james[mtchs$lonlat1, ], 
    fullobs[year == 1250, list(osmid, im2_cml, im3_cml, im3_ann)], 
    by = "osmid")
james_mtchd = james_mtchd[!id_james %in% c("Synopsis.php?id=EVREUX", 
                                           "Synopsis.php?id=ETMPES-B", 
                                           "Synopsis.php?id=ETMPES-G"), ]
# outliers dropped after looking up
# http://creationofgothic.org/V3/PHP/"
    # Synopsis.php?id=EVREUX   # only lower part of nave, rest built later
    # Synopsis.php?id=ETMPES-B # west portal of nave only
    # Synopsis.php?id=ETMPES-G # new, all except nave and top is romanesque
# First two have definite issues; third is largely romanesque, but nave (top windows only) and tower


pdf('figs/james_v_catdat1250_m3.pdf', width = 6, height = 6)
par(mfrow=c(1, 1), font.main=1)
plot(JU ~ im3_cml, # cml because comparison is completed church in 1250
    data=james_mtchd[JU > 0], 
    bty = 'l', log = 'xy',
    xlab = "OSM-based completed church volume (m³ in 1250)",
    ylab= "James's cost estimate")
abline(lm(log(JU) ~ log(im3_cml), 
    data = james_mtchd[JU > 0 & !is.na(im3_cml) & im3_cml > 0]), 
    lwd = 1.5)
dev.off()

# correlation james and osm for same research area
cor(comp[, .(JU, m3dec)])

# share matched
sum(!is.na(james$osmid)) / nrow(james)
