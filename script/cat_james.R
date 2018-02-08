rm(list = ls())

library("data.table")

library(maptools)
library(RColorBrewer)
library(osmar)
library(geosphere)
library(texreg)

M = 9

setwd("~/dropbox/cathedrals")
source("script/cat_functions.r")

statobs = data.table::fread("dat/statobs.csv")
fullobs_sp = data.table::fread("gunzip -c  dat/fullobs_sp.csv.gz")
fullobs = fullobs_sp[, -names(statobs)[-1], with = F]

# add warning for timeouts
# consider just getting all best matches
# then just drop based on min

# james <- read.csv('dat/james.csv')
james = data.table::fread("dat/james_exp.csv", header=T)
james$id_james <- james$id
james$id <- NULL

yrs = grep('\\d{4}', names(james))

james$JU <- rowSums(james[, yrs, with=F], na.rm=T)
james$JU[rowSums(!is.na(james[, yrs, with=F]))==0] <- NA

mtchs = distmatch(ll1=as.matrix(james[, list(Longitude, Latitude)]), 
                  ll2=as.matrix(statobs[, list(lon, lat)]))
mtchs[duplicated(mtchs$ll2), ]
mtchs[duplicated(mtchs$ll2, fromLast=T), ]
mtchs = mtchs[order(mtchs$dist), ]
mtchs = mtchs[!duplicated(mtchs$ll2), ]

james[mtchs$ll1, osmid:= statobs$osmid[mtchs$ll2]]
james[mtchs$ll1, osmname:=statobs$osmname[mtchs$ll2]]
james[mtchs$ll1, osmcity:=statobs$city[mtchs$ll2]]

# Sainte-Geneviève and La Vistiation, probably incorrectly matched
james[osmid %in% c("55343672", "147457637"), osmid := NA] 

pdf('figs/james_mtch_v_unmtchd.pdf', width=10, height=4)
par(mfrow=c(1, 3), font.main=1, mar=c(5, 4, 3.5, 0.5))
plot(names(james)[yrs], colSums(james[, yrs, with=F], na.rm=T), 
    type='l', bty='l', xlab='year', ylab="James' cost est.", col=2, lwd=2, main='All')
plot(names(james)[yrs], colSums(james[!is.na(osmid), yrs, with=F], na.rm=T), 
    type='l', bty='l', xlab='year', ylab="James' cost est.", col=2, lwd=2, main='Matched')
plot(names(james)[yrs], colSums(james[is.na(osmid), yrs, with=F], na.rm=T), 
    type='l', bty='l', xlab='year', ylab="James' cost est.", col=2, lwd=2, main='Unmatched')
dev.off()

lonrange = range(james$Longitude)
latrange = range(james$Latitude)
ourdata = fullobs_sp[lon %between% lonrange & lat %between% latrange & year > 1050 & year < 1260, 
    list(m3dec = base::sum(.SD, na.rm=T) / M), 
    by = list(decade = floor(year / 10)*10), .SDcols = grepr('m3_ann_cmc\\d', names(fullobs_sp))]
jamesdata = melt(james[, yrs[-20], with = F], measure.vars = yrs[-20], 
    variable.name = 'decade', value.name = 'JU', variable.factor = F)
jamesdata = jamesdata[, list(JU = sum(JU, na.rm=T)), by = list(decade = as.numeric(decade))]
comp = ourdata[jamesdata, on = 'decade']

matplot(comp[, list((JU - min(JU)) / (max(JU) - min(JU)), (m3dec - min(m3dec)) / (max(m3dec) - min(m3dec)))])

comp[, JU_sc := scale(JU) * sd(m3dec) + mean(m3dec)]
comp[, m3_sc := scale(m3dec) * sd(JU) + mean(JU)]

pdf("figs/james_v_osm_series_hc.pdf", height=5, width = 9)
par(mfrow=c(1, 2), font.main=1)
plot(m3dec ~ decade, data = comp,
    type='l', lwd = 1.5, bty='l', xlim=c(1060, 1250),
    xlab='decade', ylab='m3/decade', main='OSM-EE')
lines(JU_sc ~ decade ,data = comp, col = 'gray')
plot(JU ~ decade, data = comp,
    type='l', lwd = 1.5, bty='l', xlim=c(1060, 1250),
    xlab='decade', ylab="James cost estimate/decade", main='James')
lines(m3_sc ~ decade ,data = comp, col = 'gray')
dev.off()

james_mtchd = merge(james[mtchs$ll1, ], fullobs[year==1250, list(osmid, im2_cml, im3_cml)], by="osmid")
james_mtchd = james_mtchd[!id_james %in% c("Synopsis.php?id=EVREUX", 
    "Synopsis.php?id=ETMPES-B", "Synopsis.php?id=ETMPES-G"), ]

pdf('figs/james_v_catdat1250_m3.pdf')
par(mfrow=c(1, 1), font.main=1)
plot(log(JU) ~ log(im3_cml), data=james_mtchd[JU > 0], bty = 'l',
    xlab='log(m3)', ylab= "log(James cost estimate)")
abline(lm(log(JU) ~ log(im3_cml), data=james_mtchd[JU > 0]), lwd = 1.5)
dev.off()




osmids_james = unique(na.omit(james$osmid))
slopes = data.frame(JU_py=numeric(length(osmids_james)), m2_py=numeric(length(osmids_james)))
rownames(slopes) = osmids_james
pdf("figs/james_v_catdat.pdf")
# par(mfrow=c(5, 5), mar=c(1, 3.5, 0.5, 0.5))
for (i in osmids_james){
    bld = james[osmid == i, yrs, with=F]
    bld = t(bld)
    if (all(is.na(bld))) next
    bld = bld[!is.na(bld), ]
    catdat = fullobs[osmid==i & year >= 1050 & year <= 1250, ]
    if (all(is.na(catdat$im2_cml))) next
    plot(catdat[, list(year, im2_cml/max(im2_cml, na.rm=T))], type='l', bty='l')
    title(main=paste(paste(statobs[osmid==i, list(osmname, city, osmid)], collapse=', '),
                paste(james[osmid==i, list(Monument, name)], collapse=', '), sep='\n'))
    lines(names(bld), cumsum(bld) / max(cumsum(bld)), type='b', col=2)
    
    bld_years = as.numeric(names(bld))
    bld_years[length(bld_years)] = bld_years[length(bld_years)] + 9
    if (length(bld) > 1){    
        JU_per_year = sum(bld) / diff(range(bld_years))
        m2_per_year = diff(catdat[year %in% range(bld_years), im2_cml]) / diff(range(bld_years))
        slopes[i, "JU_py"] = JU_per_year
        slopes[i, "m2_py"] = m2_per_year
    }
}
dev.off()

plot(JU_py ~ m2_py, data=slopes)

# correlation
cor(colSums(james[, yrs, with=F], na.rm=T),
    colSums(james[!is.na(osmid), yrs, with=F], na.rm=T))

# cor first differences
cor(diff(colSums(james[, yrs, with=F], na.rm=T)),
    diff(colSums(james[!is.na(osmid), yrs, with=F], na.rm=T)))

# share matched
sum(!is.na(james$osmid)) / nrow(james)

# JU matched
sum(james[!is.na(james$osmid), yrs, with=F], na.rm=T) / sum(james[, yrs, with=F], na.rm=T)






plot(im2_cml ~ JU, data=james_mtchd, log='xy')
m1250 = lm(log(im2_cml) ~ log(JU), data=james_mtchd[im2_cml > 0 & JU > 0, ])

cat(paste0("http://creationofgothic.org/V3/PHP/", james_mtchd$id_james, sep='\n')
# Synopsis.php?id=EVREUX
    # Synopsis.php?id=ETMPES-B

    # Synopsis.php?id=DREUX
    # Synopsis.php?id=AMIENS
# Synopsis.php?id=MANS
# /Synopsis.php?id=CHALON-A

# 56301534
# 113037532

par(mfrow=c(1, 1), font.main=1, mar=c(4.5, 4, 1.5, 0.5))
plot(log(JU) ~ log(im2_cml), data=james_mtchd, xlab='log(m2)', bty='l')
m1250m2 = lm(log(JU) ~ log(im2_cml), data=james_mtchd[im2_cml > 0 & JU > 0, ])
abline(m1250m2, col=2)
text(log(james_mtchd$im2_cml), log(james_mtchd$JU), labels=james_mtchd$id_james)
# Synopsis.php?id=EVREUX # only lower part of nave, rest built later
    # Synopsis.php?id=ETMPES-B # west portal of nave only
    # Synopsis.php?id=ETMPES-G # new
# First two have definite issues; third is largely romanesque, but nave (top windows only) and tower

lats <- range(james$Latitude)
lons <- range(james$Longitude)
lats <- seq(from=lats[1], to=lats[2], length.out=4)
lons <- seq(from=lons[1], to=lons[2], length.out=7)

cty = list(lat1 = lats[1], lat2 = lats[2], lon1=lons[1], lon2=lons[2])

block1 = get_osm_data_city(cty, what='way', radius=NULL)

ways = list()
rels = list()
for (i in 2:length(lats)){
    for (j in 2:length(lons)){
        ways[[paste(i, j, sep='_')]] <- get_osm_all_churches_rect(lat1=lats[i - 1], lon1=lons[j - 1], 
                                           lat2=lats[i], lon2=lons[j], what='way')
        rels[[paste(i, j, sep='_')]] <- get_osm_all_churches_rect(lat1=lats[i - 1], lon1=lons[j - 1], 
                                           lat2=lats[i], lon2=lons[j], what='relation')
        Sys.sleep(60)
        cat(paste(i, j, sep='_'), '\n')
    }
}

ways_polys = lapply(ways, osmar::as_sp, what="polygons")
ways_pdf = polylist2df(ways_polys)
ways_pdf$surface = geosphere::areaPolygon(ways_pdf)

allmtchs = distmatch(ll1=as.matrix(james[, list(Longitude, Latitude)]), 
                  ll2=coordinates(ways_pdf))

james$osm_surface[allmtchs$ll1] = ways_pdf$surface[allmtchs$ll2]



pdf("figs/james_v_osm.pdf")
par(mfrow=c(2, 2), font.main=1, mar=c(4.5, 4, 1.5, 0.5))
plot(log(JU) ~ log(im2_cml), data=james_mtchd, xlab='log(m2)', bty='l')
m1250m2 = lm(log(JU) ~ log(im2_cml), data=james_mtchd[im2_cml > 0 & JU > 0, ])
abline(m1250m2, col=2)

plot(log(JU) ~ log(im3_cml), data=james_mtchd, xlab='log(m3)', bty='l')
m1250m3 = lm(log(JU) ~ log(im3_cml), data=james_mtchd[im3_cml > 0 & JU > 0, ])
abline(m1250m3, col=2)

plot(log(JU) ~ log(osm_surface), data=james[osm_surface > 0 & JU > 0, ], xlab="log(m2 present-day church)", bty='l')
m_pd2 = lm(log(JU) ~ log(osm_surface), data=james[osm_surface > 0 & JU > 0, ])
abline(m_pd2, col=2)

plot(log(JU) ~ log(I(osm_surface*sqrt(osm_surface/5))), data=james[osm_surface > 0 & JU > 0, ], xlab="log(appr. m3 present-day church)", bty='l')
m_pd3 = lm(log(JU) ~ log(I(osm_surface*sqrt(osm_surface/5))), data=james[osm_surface > 0 & JU > 0, ])
abline(m_pd3, col=2)

dev.off()
texreg::screenreg(list(m1250m2, m1250m3, m_pd2, m_pd3))

pdf("figs/james_v_osm_series.pdf", height=4)
par(mfrow=c(1, 2), font.main=1)
plot(fullobs[osmid %in% sqr$osmid & year > 1050 & year < 1260, sum(im2, na.rm=T), by=floor(year / 10)*10], 
    type='l', col=2, bty='l', xlab='decade', ylab='m2', main='OSM-EE')
plot(x=names(james)[yrs[-20]], colSums(james[, yrs[-20], with=F], na.rm=T), 
    type='l', col=2, bty='l', xlab='decade', ylab='JU', main='James')
dev.off()


pdf("figs/james_v_osm_nolog.pdf")
par(mfrow=c(2, 2), font.main=1, mar=c(4.5, 4, 1.5, 0.5))

plot(JU ~ im2_cml, data=james_mtchd, xlab='m2', bty='l')
m1250m2 = lm(JU ~ im2_cml, data=james_mtchd[im2_cml > 0 & JU > 0, ])
abline(m1250m2, col=2)

plot(JU ~ im3_cml, data=james_mtchd, xlab='m3', bty='l')
m1250m3 = lm(JU ~ im3_cml, data=james_mtchd[im3_cml > 0 & JU > 0, ])
abline(m1250m3, col=2)

plot(JU ~ osm_surface, data=james[osm_surface > 0 & JU > 0, ], xlab="m2present-day church)", bty='l')
m_pd2 = lm(JU ~ osm_surface, data=james[osm_surface > 0 & JU > 0, ])
abline(m_pd2, col=2)

plot(JU ~ I(osm_surface*sqrt(osm_surface/5)), data=james[osm_surface > 0 & JU > 0, ], xlab="appr.m3 present-day church)", bty='l')
m_pd3 = lm(JU ~ I(osm_surface*sqrt(osm_surface/5)), data=james[osm_surface > 0 & JU > 0, ])
abline(m_pd3, col=2)

dev.off()
texreg::screenreg(list(m1250m2, m1250m3, m_pd2, m_pd3))

# rels_polys = lapply(rels[sapply(rels, nrow) > 1], osmar::as_sp, what="polygons")
# rels_pdf = polylist2df(polylist=rels_polys[-8], what="relation")
# rels_pdf$surface = geosphere::areaPolygon(rels_pdf)


# rels_pdf = polylist2df(rels)

# rels_tags = lapply(rels[sapply(rels, nrow) > 1], get_osm_tags, what='relation')
# dim(do.call(rbind, ways_tags))
# ways_tags = lapply(ways, get_osm_tags, what='way')
# dim(ways_pdf)

# ways_polys = polylist2df(ways)