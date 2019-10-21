rm(list=ls())
setwd("~/dropbox/cathedrals")

library("raster")
library("maptools")
library("RNetCDF")
library("maptools")
library("data.table")

source('script/cat_functions.r')

wgs <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs ")
data("wrld_simpl")

statobs = data.table::fread("dat/statobs.csv")
citobs = data.table::fread("dat/citobs.csv")
fullobs_sp = data.table::fread("gunzip -c dat/fullobs_sp.csv.gz")
siem = data.table::fread("dat/siem_long.csv", encoding="UTF-8")
ukgdp = data.table::fread("dat/engdp12001700.csv", skip=1, encoding="UTF-8")

M = 9 # number of imputations
impvrbs = grepr('im3_ann\\d', names(fullobs_sp))

isor = raster::raster("~/downloads/data/hyde/iso_cr.asc")

nld = raster::getData("GADM", country='NLD', level=0)
fra = raster::getData("GADM", country='FRA', level=0)
che = raster::getData("GADM", country='CHE', level=0)
bel = raster::getData("GADM", country='BEL', level=0)
gbr = raster::getData("GADM", country='GBR', level=0)
deu = raster::getData("GADM", country='DEU', level=0)
ita = raster::getData("GADM", country='ITA', level=0)

rhigh = raster(nrows = 176, ncols = 228, 
    xmn = -11.75, xmx = 44.75, 
    ymn = 27.25, ymx = 70.75)

### churches data ###
#####################
fullobs_sp[!is.na(phase) & firstobs != TRUE, newbuild := m2]

rchm = list()
periods = list(700:1000, 1001:1200, 1201:1347, 1348:1500)
for (i in 1:length(periods)){
    temp = fullobs_sp[year %in% periods[[i]], ]
    spdf = sp::SpatialPointsDataFrame(coords=temp[, list(lon, lat)], 
        data=temp[, list(im2_ann, im3_ann, newbuild)], proj4str=wgs)
    spdf = spdf[!is.na(spdf$im3_ann) & spdf$im3_ann!=0, ]
    r_temp = raster::rasterize(spdf, rhigh, field="im3_ann", fun=sum, na.rm=T)
    rchm[[i]] = raster::focal(r_temp, w=raster::focalWeight(r_temp, 0.7, 'Gauss'), sum, na.rm=T)
    print(sum(getValues(r_temp), na.rm=T))
    print(sum(getValues(rchm[[i]]), na.rm=T))
}
rchm = raster::stack(rchm)
rchm = trim(rchm)

# express values per square km2
rchm = rchm / area(rchm)

sum(geosphere::areaPolygon(wrld_simpl[wrld_simpl$ISO3 %in% c("NLD", "BEL", "CHE", "FRA", "DEU", "GBR"), ])) / (1e3^2)

range(rchm)
cellStats(rchm, 'range')

rchm[is.na(rchm)] = 0
pdf("figs/4m3maps_smoothed.pdf", width=10, height=10)
par(mfrow=c(2,2), font.main=1, mar=c(2, 2, 3, 7))
plot(rchm[[1]], main="700-1000", zlim = c(0, 31), col=magma(256), axes=F,
    axis.args=list(at=seq(0, 31, 10)))
add_borders(border='white')
plot(rchm[[2]], main="1000-1200", zlim = c(0, 72), col=magma(256), axes=F,
    axis.args=list(at=seq(0, 60, 20)))
add_borders(border='white')
plot(rchm[[3]], main="1200-1348", zlim = c(0, 72), col=magma(256), axes=F,
    axis.args=list(at=seq(0, 60, 20)))
add_borders(border='white')
plot(rchm[[4]], main="1348-1500", zlim = c(0, 72), col=magma(256), axes=F,
    axis.args=list(at=seq(0, 60, 20)))
add_borders(border='white')
dev.off()


### population data ###
#######################

p = raster::stack("~/downloads/data/hyde/700AD_pop/popc_700AD.asc",
    "~/downloads/data/hyde/800AD_pop/popc_800AD.asc",
    "~/downloads/data/hyde/900AD_pop/popc_900AD.asc",
    "~/downloads/data/hyde/1000AD_pop/popc_1000AD.asc",
    "~/downloads/data/hyde/1100AD_pop/popc_1100AD.asc",
    "~/downloads/data/hyde/1200AD_pop/popc_1200AD.asc",
    "~/downloads/data/hyde/1300AD_pop/popc_1300AD.asc",
    "~/downloads/data/hyde/1400AD_pop/popc_1400AD.asc",
    "~/downloads/data/hyde/1500AD_pop/popc_1500AD.asc",
    "~/downloads/data/hyde/1600AD_pop/popc_1600AD.asc")
p = raster::crop(p, raster::extent(-10, 20, 40, 60))
isor = raster::crop(isor, raster::extent(-10, 20, 40, 60))

rastpop = cbind(as.data.table(getValues(p)),
    as.data.table(getValues(isor)))[, lapply(.SD, function(x) round(sum(x, na.rm = T))), by = V1]
rastpop[, ctr := countrycode::countrycode(rastpop$V1, "iso3n", "iso3c")]
data.table::fwrite(rastpop[ctr %in% c("NLD", "FRA", "CHE", "BEL", "GBR", "DEU")][order(-popc_700AD)], "dat/totpop_hyde.csv")

# insert NAs for interpolation
pip = raster::setValues(p[[1]], NA)
pip = raster::stack(replicate(4, pip))
pip = raster::addLayer(p, pip)
pip = pip[[c(1, 11:14, 2, 11:14, 3, 11:14, 4, 11:14, 5, 11:14, 
             6, 11:14, 7, 11:14, 8, 11:14, 9, 11:14, 10)]]
names(pip) = as.character(seq(from=700, to=1600, by=20))
pip = exp(raster::approxNA(log(pip)))

# eng pop kees v. broadberry et al.
plot(seq(from=700, to=1600, by=20), colSums(pip[isor==826], na.rm=T))
lines(population ~ V1, data=ukgdp)


### urb pop ###
###############
rsiem = list()
# rgsiem = list()
for (i in seq(from=700, to=1800, by=100)){
    dcd = (i:(i + 9))
    spdf = sp::SpatialPointsDataFrame(coords=siem[year %in% dcd, list(lon, lat)], data=siem[year %in% dcd, list(inhab)]) 
    rst = raster::rasterize(spdf, r, field="inhab", fun=sum, na.rm=T)
    rsiem[[as.character(i)]] = rst
    # rgsiem[[as.character(i)]] = raster::focal(log1p(rst), raster::focalWeight(rst, 0.7, 'Gauss'), sum, na.rm=T)
}
rsiem = raster::brick(rsiem)
rsiem = raster::crop(rsiem, raster::extent(-10, 20, 40, 60))

# interpolate
rsiem_ip = setValues(rsiem[[1]], NA)
rsiem_ip = raster::stack(replicate(4, rsiem_ip))
rsiem_ip = addLayer(rsiem, rsiem_ip)
rsiem_ip = rsiem_ip[[c(1, 13:16, 2, 13:16, 3, 13:16, 4, 13:16, 
                       5, 13:16, 6, 13:16, 7, 13:16, 8, 13:16, 
                       9, 13:16, 10)]]#, 13:16, 11, 13:16, 12)]]
names(rsiem_ip) = as.character(seq(from=700, to=1600, by=20))
rsiem_ip = exp(approxNA(log(rsiem_ip)))

### influence neigbours ###
###########################
# wm = raster::pointDistance(as.matrix(fullobs_sp[year==700, list(lon, lat)][1:10, ]), lonlat=T)
cityear = fullobs_sp[year <= 1500, list(im3_dec=sum(im3, na.rm=T), lat=mean(lat), lon=mean(lon)), by=list(city, decade)]
wm = sp::spDists(as.matrix(cityear[decade==700, list(lon, lat)]))
wm = 1/wm
diag(wm) = 0
cityear[, nbim3:=im3_dec %*% wm, by=decade]
cityear = cityear[order(city, decade), ]
cityear[, paste0("nbim3_lag", 1:10*20) := data.table::shift(nbim3, 1:10), by = city]

m0 = lm(log1p(im3_dec) ~ log1p(nbim3), data = cityear)
m20 = lm(log1p(im3_dec) ~ log1p(nbim3) + log1p(nbim3_lag20), data = cityear)
m40 = lm(log1p(im3_dec) ~ log1p(nbim3) + log1p(nbim3_lag20) 
    + log1p(nbim3_lag40), data = cityear)
m60 = lm(log1p(im3_dec) ~ log1p(nbim3) + log1p(nbim3_lag20) 
    + log1p(nbim3_lag40) + log1p(nbim3_lag60), data = cityear)
m80 = lm(log1p(im3_dec) ~ log1p(nbim3) + log1p(nbim3_lag20) 
    + log1p(nbim3_lag40) + log1p(nbim3_lag60) + log1p(nbim3_lag80), data = cityear)
m100 = lm(log1p(im3_dec) ~ log1p(nbim3) + log1p(nbim3_lag20) 
    + log1p(nbim3_lag40) + log1p(nbim3_lag60) + log1p(nbim3_lag80) 
    + log1p(nbim3_lag100), data = cityear)

texreg::screenreg(list(m0, m20, m40, m60, m80, m100), stars=0) #, file='tab/spatialreg.html')
texreg::texreg(list(m0, m20, m40, m60, m80, m100), stars=0, file='tab/spatialreg.tex')
lapply(list(m0, m20, m40, m60, m80, m100), AIC)





# pdf('figs/pcm2_20y.pdf')
# rch[is.na(rch)] = 0
# animate(rch, n = 1, col = magma(256))
# dev.off()

nldurb = (colSums(raster::extract(rsiem_ip, nld)[[1]], na.rm=T)*1e3)
fraurb = (colSums(raster::extract(rsiem_ip, fra)[[1]], na.rm=T)*1e3)
deuurb = (colSums(raster::extract(rsiem_ip, deu)[[1]], na.rm=T)*1e3)
gbrurb = (colSums(raster::extract(rsiem_ip, gbr)[[1]], na.rm=T)*1e3)
belurb = (colSums(raster::extract(rsiem_ip, bel)[[1]], na.rm=T)*1e3)
cheurb = (colSums(raster::extract(rsiem_ip, che)[[1]], na.rm=T)*1e3)

nldpop = (colSums(raster::extract(pips, nld)[[1]], na.rm=T))
frapop = (colSums(raster::extract(pips, fra)[[1]], na.rm=T))
deupop = (colSums(raster::extract(pips, deu)[[1]], na.rm=T))
gbrpop = (colSums(raster::extract(pips, gbr)[[1]], na.rm=T))
belpop = (colSums(raster::extract(pips, bel)[[1]], na.rm=T))
chepop = (colSums(raster::extract(pips, che)[[1]], na.rm=T))

nldchu = (colSums(raster::extract(rch, nld)[[1]], na.rm=T))
frachu = (colSums(raster::extract(rch, fra)[[1]], na.rm=T))
deuchu = (colSums(raster::extract(rch, deu)[[1]], na.rm=T))
gbrchu = (colSums(raster::extract(rch, gbr)[[1]], na.rm=T))
belchu = (colSums(raster::extract(rch, bel)[[1]], na.rm=T))
chechu = (colSums(raster::extract(rch, che)[[1]], na.rm=T))

d = rbind(data.frame(m2=nldchu[names(nldchu)], pop=nldpop[names(nldchu)], urb=nldurb[names(nldchu)], year=names(nldchu), ctr="nld"), 
    data.frame(m2=frachu[names(frachu)], pop=frapop[names(frachu)], urb=fraurb[names(frachu)], year=names(frachu), ctr="fra"), 
    data.frame(m2=deuchu[names(deuchu)], pop=deupop[names(deuchu)], urb=deuurb[names(deuchu)], year=names(deuchu), ctr="deu"), 
    data.frame(m2=gbrchu[names(gbrchu)], pop=gbrpop[names(gbrchu)], urb=gbrurb[names(gbrchu)], year=names(gbrchu), ctr="gbr"), 
    data.frame(m2=belchu[names(belchu)], pop=belpop[names(belchu)], urb=belurb[names(belchu)], year=names(belchu), ctr="bel"), 
    data.frame(m2=chechu[names(chechu)], pop=chepop[names(chechu)], urb=cheurb[names(chechu)], year=names(chechu), ctr="che"))
d$year = as.numeric(gsub('X', '', d$year))
d$urbrate = ((d$urb*1e3) / d$pop ) * 100

eur = aggregate(cbind(m2, pop, urb) ~ year, sum, data=d)

# per capita series based on rastered population data
pdf('figs/pc_panel_big_hc.pdf', height=6)
par(mfrow=c(2, 2), mar=c(4, 4, 2.5, 0.5), font.main=1)
plot((m2 / pop)*1e3 ~ year, data=aggregate(cbind(m2, pop, urb) ~ year, sum, data=d[d$ctr=='deu' | d$ctr == "che", ]), type='l', 
    bty='l', ylab='m2/1000 cap', main='Germany and Switz.')
lines((m2 / pop)*1e3 ~ year, data=eur, col='gray80')
plot((m2 / pop)*1e3 ~ year, data=d[d$ctr=='fra', ], type='l', 
    bty='l', ylab='m2/1000 cap', main='France')
lines((m2 / pop)*1e3 ~ year, data=eur, col='gray80')
plot((m2 / pop)*1e3 ~ year, data=d[d$ctr=='gbr', ], type='l', 
    bty='l', ylab='m2/1000 cap', main='Britain')
lines((m2 / pop)*1e3 ~ year, data=eur, col='gray80')
plot((m2 / pop)*1e3 ~ year, data=aggregate(cbind(m2, pop, urb) ~ year, sum, data=d[d$ctr=='nld' | d$ctr == "bel", ]), type='l', 
    bty='l', ylab='m2/1000 cap', main='Low Countries')
lines((m2 / pop)*1e3 ~ year, data=eur, col='gray80')
dev.off()


pdf('figs/puc_panel_big.pdf', height=4, width=9)
par(mfrow=c(1, 3), mar=c(4, 4, 2.5, 0.5), font.main=1)
yl = range(d$m2 / d$urb)
plot((m2 / urb) ~ year, data=d[d$ctr=='deu', ], type='l', ylim = yl,
    bty='l', ylab='m2/1000 cap', main='Germany')
lines((m2 / urb) ~ year, data=eur, col='gray80')
plot((m2 / urb) ~ year, data=d[d$ctr=='fra', ], type='l', ylim = yl,
    bty='l', ylab='m2/1000 cap', main='France')
lines((m2 / urb) ~ year, data=eur, col='gray80')
plot((m2 / urb) ~ year, data=d[d$ctr=='gbr', ], type='l', ylim = yl,
    bty='l', ylab='m2/1000 cap', main='Britain')
lines((m2 / urb) ~ year, data=eur, col='gray80')
dev.off()

pdf('figs/pc_panel_small.pdf', height=4, width=9)
par(mfrow=c(1, 3), mar=c(4, 4, 2.5, 0.5), font.main=1)
plot((m2 / pop)*1e3 ~ year, data=d[d$ctr=='che', ], type='l', 
    bty='l', ylab='m2/1000 cap', ylim=c(0, 20), main='Switzerland')
lines((m2 / pop)*1e3 ~ year, data=eur, col='gray80')
plot((m2 / pop)*1e3 ~ year, data=d[d$ctr=='bel', ], type='l', 
    bty='l', ylab='m2/1000 cap', ylim=c(0, 20), main='Belgium')
lines((m2 / pop)*1e3 ~ year, data=eur, col='gray80')
plot((m2 / pop)*1e3 ~ year, data=d[d$ctr=='nld', ], type='l', 
    bty='l', ylab='m2/1000 cap', ylim=c(0, 20), main='Netherlands')
lines((m2 / pop)*1e3 ~ year, data=eur, col='gray80')
dev.off()

pdf('figs/puc_panel_small.pdf', height=4, width=9)
par(mfrow=c(1, 3), mar=c(4, 4, 2.5, 0.5), font.main=1)
plot(m2 / urb ~ year, data=d[d$ctr=='che', ], type='l', 
    bty='l', ylab='m2/1000 urb cap', main='Switzerland')
lines(m2 / urb ~ year, data=eur, col='gray80')
plot(m2 / urb ~ year, data=d[d$ctr=='bel', ], type='l', 
    bty='l', ylab='m2/1000 urb cap', ylim=c(5, 110), main='Belgium')
lines(m2 / urb ~ year, data=eur, col='gray80')
plot(m2 / urb ~ year, data=d[d$ctr=='nld' | d$ctr == "bel", ], type='l', 
    bty='l', ylab='m2/1000 urb cap', ylim=c(5, 110), main='Netherlands')
lines(m2 / urb ~ year, data=eur, col='gray80')
dev.off()

# urb in be/nld simply too high for correction to work

### smoothed maps ###
#####################

# pdf('figs/smoothtest.pdf')
# plot(rch[[31]])
# plot(rch[[31]], interpolate=T)
# plot(disaggregate(rch[[31]], 11, method='bilinear'))
# plot(disaggregate(rch[[31]], 7, method='bilinear'))
# plot(focal(rch[[31]], w=matrix(1, 5, 5), mean, na.rm=T, pad=T))
# plot(focal(rch[[31]], w=focalWeight(rch[[31]], 0.7, "Gauss"), sum, na.rm=T))
# add_borders()
# # dev.off()


# # very similar to gaussian blur
# fullobs_sp[year <= 1500, period := cut(year, c(699, 1000, 1200, 1348, 1500))]

# fullobs_ag = fullobs_sp[, list(im2 = sum(im2_ann, na.rm=T)), by=list(osmid, lon, lat, period)]

# wrld_simpl_eu = sp::spTransform(wrld_simpl[wrld_simpl$REGION == 150, ], sp::CRS("+init=EPSG:3035"))

# par(mfrow=c(2, 2))
# spdf = sp::SpatialPointsDataFrame(coords=fullobs_ag[period=="(699,1e+03]", list(lon, lat)], data=fullobs_ag[period=="(699,1e+03]", list(im2)], proj4str=wgs)
# spdf_eu = sp::spTransform(spdf, sp::CRS("+init=EPSG:3035"))
# x = SpatialPosition::stewart(knownpts = spdf_eu, varname="im2",
#                         typefct = "exponential", span=50e3, beta=2,
#                         resolution = 30e3)
# xr = rasterStewart(x)
# plot(xr, col=magma(256))
# plot(wrld_simpl_eu, add=T, border="white")
# spdf = sp::SpatialPointsDataFrame(coords=fullobs_ag[period=="(1e+03,1.2e+03]", list(lon, lat)], data=fullobs_ag[period=="(1e+03,1.2e+03]", list(im2)], proj4str=wgs)
# spdf_eu = sp::spTransform(spdf, sp::CRS("+init=EPSG:3035"))
# x = SpatialPosition::stewart(knownpts = spdf_eu, varname="im2",
#                         typefct = "exponential", span=50e3, beta=2,
#                         resolution = 30e3)
# xr = rasterStewart(x)
# plot(xr, col=magma(256))
# plot(wrld_simpl_eu, add=T, border='white')
# spdf = sp::SpatialPointsDataFrame(coords=fullobs_ag[period=="(1.2e+03,1.35e+03]", list(lon, lat)], data=fullobs_ag[period=="(1.2e+03,1.35e+03]", list(im2)], proj4str=wgs)
# spdf_eu = sp::spTransform(spdf, sp::CRS("+init=EPSG:3035"))
# x = SpatialPosition::stewart(knownpts = spdf_eu, varname="im2",
#                         typefct = "exponential", span=50e3, beta=2,
#                         resolution = 30e3)
# xr = rasterStewart(x)
# plot(xr, col=magma(256))
# plot(wrld_simpl_eu, add=T, border='white')
# spdf = sp::SpatialPointsDataFrame(coords=fullobs_ag[period=="(1.35e+03,1.5e+03]", list(lon, lat)], data=fullobs_ag[period=="(1.35e+03,1.5e+03]", list(im2)], proj4str=wgs)
# spdf_eu = sp::spTransform(spdf, sp::CRS("+init=EPSG:3035"))
# x = SpatialPosition::stewart(knownpts = spdf_eu, varname="im2",
#                         typefct = "exponential", span=50e3, beta=2,
#                         resolution = 30e3)
# xr = rasterStewart(x)
# plot(xr, col=magma(256))
# plot(wrld_simpl_eu, add=T, border='white')





library("fields")
dcds = seq(from=700, to=1500, by=50)
grds = list()
for (i in 3:length(dcds)){
    dcd = dcds[i - 1]:dcds[i]
    spdf = sp::SpatialPointsDataFrame(coords=fullobs_sp[year %in% dcd, list(lon, lat)], data=fullobs_sp[year %in% dcd, list(im2_ann, im2_ann_l1, im3_ann)], proj4str=wgs)
    spdf = spdf[!is.na(spdf$im2_ann) & spdf$im2_ann!=0, ]
    spdf = sp::spTransform(spdf, sp::CRS("+init=EPSG:3035"))
    fld = fields::Tps(coordinates(spdf), log(abs(spdf$im2_ann_l1)), lambda=0)
    grds[[as.character(dcds[i])]] = fields::predictSurface(fld, nx=200, ny=200)
    # fld = fields::Tps(coordinates(spdf), abs(spdf$im2_ann), Z=abs(spdf$im2_ann_l1), lambda=0)
    # grds[[as.character(dcds[i])]] = fields::predictSurface(fld, nx=200, ny=200, drop.Z=T)
    cat(i)
}


pdf('figs/fieldsmoothed.pdf')
for (i in grds){
    print(quantile(i$z, na.rm=T))
    hist(i$z)
    # image(grds[[1]])
    # i$z[i$z < 0] = 0
    plot(eur, xlim=c(3.5e6, 4.2e6), ylim=c(2e6, 4.5e6))
    image(i, col=RColorBrewer::brewer.pal(9, 'RdPu'), breaks=c(-100,    0, 1,  2,  5,  10,  20,  40, 50,   100), add=T)
    # image(i, col=RColorBrewer::brewer.pal(9, 'RdPu'), breaks=c(-8000, 0, 10, 25, 50, 100, 200, 400, 800, 5000), add=T)
    add_borders()
}
dev.off()


rchpcm = list()
for (i in c('X7|X8', 'X9|X10', 'X11|X12', 'X13')){
    rs = sum(rchpc[[grep(i, names(rchpc))]])
    rchpcm[[i]] = raster::focal(rs, w=raster::focalWeight(rs, 0.7, 'Gauss'), sum, na.rm=T)
}
plot(rchpcm[[1]], main='750-950', col=viridis(256)); add_borders()

pdf('figs/fourmaps_smoothed_pc.pdf', width=10, height=10)
par(mfrow=c(2, 2))
plot(rchpcm[[1]], main='750-950'); add_borders()
plot(rchpcm[[2]], main='950-1150'); add_borders()
plot(rchpcm[[3]], main='1150-1350'); add_borders()
plot(rchpcm[[4]], main='1350-1450'); add_borders()
dev.off()
pdf('figs/percapita.pdf')
plot(8:14*100, colSums(getValues(rchpc), na.rm=T), type='b', col=2, bty='l')
dev.off()
pdf("figs/percapita_all.pdf")
animate(rchpc)

# or use raster::approxNA
plot(gsub('[^0-9]', '', names(p)), colSums(getValues(p), na.rm=T), type='b', col=2, bty='l')


plot(subset(p, 1:9))
plot(subset(p, 1:9))



rch = list()
for (i in seq(from=720, to=1500, by=20)){
    dcd = (i - 19):(i)
    print(dcd)
    spdf = sp::SpatialPointsDataFrame(coords=fullobs_sp[year %in% dcd, list(lon, lat)], data=fullobs_sp[year %in% dcd, list(im2_ann, im3_ann, newbuild)], proj4str=wgs)
    spdf = spdf[!is.na(spdf$im2_ann) & spdf$im2_ann!=0, ]
    rch[[as.character(i)]] = raster::rasterize(spdf, rhigh, field="im2_ann", fun=sum, na.rm=T)
}
rch = raster::brick(rch)
rch = raster::crop(rch, raster::extent(-10, 20, 40, 60))

rchm = list()
# for (i in c('X7|X8|X9|X10', 'X10|X11', 'X12|X13[0-4]', 'X13[5-9]|X14')){
for (i in c(paste0('X', 7:14))){
    print(names(rch)[grep(i, names(rch))])
    rs = sum(rch[[grep(i, names(rch))]])
    rchm[[i]] = raster::focal(rs, w=raster::focalWeight(rs, 0.7, 'Gauss'), sum, na.rm=T)
    print(sum(getValues(rs), na.rm=T))
    print(sum(getValues(rchm[[i]]), na.rm=T))
}
rchm[[1]][is.na(rchm[[1]])] = 0
rchm[[2]][is.na(rchm[[2]])] = 0
rchm[[3]][is.na(rchm[[3]])] = 0
rchm[[4]][is.na(rchm[[4]])] = 0
# # rchm[is.na(rchm)] = 0
# par(mfrow=c(2, 2))

pdf('figs/smthest.pdf')
for (i in rchm){
    plot(i)
    add_borders()
    print(sum(getValues(i), na.rm=T))
}
dev.off()

plot(sum(rch[[1:14]]), main='700-1000'); add_borders(border='black')


pdf('figs/fourmaps.pdf', width=10, height=10)
# pdf('figs/fourmaps.pdf', paper='a4')
par(mfrow=c(2, 2))
plot(rchm[[1]], main='700-1000'); add_borders(border='black')
plot(rchm[[2]], main='1000-1200'); add_borders(border='black')
plot(rchm[[3]], main='1200-1340'); add_borders(border='black')
plot(rchm[[4]], main='1350-1500'); add_borders(border='black')
# plot(rchm[[1]], main='700-1000', col=magma(256)); add_borders(border='white')
# plot(rchm[[2]], main='1000-1200', col=magma(256)); add_borders(border='white')
# plot(rchm[[3]], main='1200-1340', col=magma(256)); add_borders(border='white')
# plot(rchm[[4]], main='1350-1500', col=magma(256)); add_borders(border='white')
# plot(rchm[[1]], main='700-1000', col=viridis(256)); add_borders(border='white')
# plot(rchm[[2]], main='1000-1200', col=viridis(256)); add_borders(border='white')
# plot(rchm[[3]], main='1200-1340', col=viridis(256)); add_borders(border='white')
# plot(rchm[[4]], main='1350-1500', col=viridis(256)); add_borders(border='white')
dev.off()

plot(rchm[[1]], main='700-1000', col=plasma(256)); add_borders(border='white')
plot(rchm[[2]], main='700-1000', col=plasma(256)); add_borders(border='white')
plot(rchm[[3]], main='700-1000', col=plasma(256)); add_borders(border='white')
plot(rchm[[4]], main='700-1000', col=plasma(256)); add_borders(border='white')

pdf('figs/fourmaps.pdf', width=10, height=10)
par(mfrow=c(2, 2))
plot(rchm[[1]], main='700-1000'); add_borders()
plot(rchm[[2]], main='1000-1200'); add_borders()
plot(rchm[[3]], main='1200-1340'); add_borders()
plot(rchm[[4]], main='1350-1500'); add_borders()
dev.off()

# rchm = list()
# for (i in 1:nlayers(rch)){
#     rchm[[i]] = raster::focal(rch[[i]], w=raster::focalWeight(rch[[i]], 0.7, 'Gauss'), sum, na.rm=T)
# }
# rchm = raster::brick(rchm)
# rchm = raster::crop(rchm, raster::extent(-10, 20, 40, 60))


rch = list()
for (i in seq(from=700, to=1480, by=20)){
    dcd = i:(i + 19)
    spdf = sp::SpatialPointsDataFrame(coords=fullobs_sp[year %in% dcd, list(lon, lat)], data=fullobs_sp[year %in% dcd, list(im2_ann, im3_ann, newbuild)], proj4str=wgs)
    spdf = spdf[!is.na(spdf$im2_ann) & spdf$im2_ann!=0, ]
    rch[[as.character(i)]] = raster::rasterize(spdf, r, field="im2_ann", fun=sum, na.rm=T)
}
rch = raster::brick(rch)
rch = raster::crop(rch, raster::extent(-10, 20, 40, 60))

r2cs = (abs(subset(r, 2:nlayers(r))) > 4) + (abs(subset(r, 1:(nlayers(r) - 1))) > 4)
names(r2cs) = names(r)[-1]
r3cs = (abs(subset(r, 3:nlayers(r))) > 4) + (abs(subset(r, 2:(nlayers(r) - 1))) > 4) + (abs(subset(r, 1:(nlayers(r) - 2))) > 4)
names(r3cs) = names(r)[-1]

drought_cons = drought_vars = drought_mns = list()
for (i in seq(from=720, to=1500, by=20)){
    dcd = (i - 19):(i)
    drought_vars[[as.character(i)]] = calc(r[[dcd]], var)
    drought_mns[[as.character(i)]] = mean(r[[dcd]], na.rm=T)
    # drought_2cons[[as.character(i)]] = sum(r2cs[[dcd]]==2, na.rm=T)
    # drought_3cons[[as.character(i)]] = sum(r3cs[[dcd]]==3, na.rm=T)
}
rdv = raster::brick(drought_vars)
rdv = raster::crop(rdv, raster::extent(-10, 20, 40, 60))
rdm = raster::brick(drought_mns)
rdm = raster::crop(rdm, raster::extent(-10, 20, 40, 60))
rd2c = raster::brick(drought_2cons)
rd2c = raster::crop(rd2c, raster::extent(-10, 20, 40, 60))
rd3c = raster::brick(drought_2cons)
rd3c = raster::crop(rd3c, raster::extent(-10, 20, 40, 60))

dd = data.table(m2=c(values(rch)), 
    droughtvar=c(values(rdv)), 
    droughtmean=c(values(rdm)), 
    # drought2cons=c(values(rd2c)),
    # drought3cons=c(values(rd3c)),
    yr=gsub('X', '', rep(names(rch), each=ncell(rch))))


par(mfrow=c(1, 3))
summary(lm(m2 ~ droughtvar, data=dd))
plot(m2 ~ droughtvar, data=dd)
abline(lm(m2 ~ droughtvar, data=dd), col=2)

summary(lm(m2 ~ droughtmean, data=dd))
plot(m2 ~ droughtmean, data=dd)
abline(lm(m2 ~ droughtmean, data=dd), col=2)

summary(lm(m2 ~ droughtcons, data=dd))
plot(m2 ~ droughtcons, data=dd)
abline(lm(m2 ~ droughtcons, data=dd), col=2)

fit = loess(m2 ~ droughtvar, data=dd)
yhat = predict(fit, newdata=seq(from=0, to=20, by=0.1), se=T)
plot(yhat$fit, ylim=c(0, 2000))
lines(yhat$fit + 2*yhat$se.fit)
lines(yhat$fit - 2*yhat$se.fit)

plot(dd[, mean(m2, na.rm=T), by=droughtcons])

plot(m2 ~ abs(droughtmean), data=dd)
abline(lm(m2 ~ abs(droughtmean), data=dd), col=2)

plot(mean ~ cut, data=dd[, list(mean=mean(m2, na.rm=T), sd=sd(m2, na.rm=T)), by=cut(droughtvar, 20)][order(cut)])




lines(predict(fit, newdata=seq(from=0, to=20, by=0.1), se=T)$se.fit)
aggregate(c(values(rch)), by=list(cut(c(values(rdv)), 20)), mean, na.rm=T)
aggregate(c(values(rch)), by=list(cut(c(values(rdv)), 20)), sd, na.rm=T)