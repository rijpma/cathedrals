# create raster maps of building activity

rm(list=ls())
setwd("~/dropbox/cathedrals")

library("raster")
library("RNetCDF")
library("maptools")
library("data.table")
library("viridisLite")

source('script/cat_functions.r')

wgs <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs ")
data("wrld_simpl", package = "maptools")

statobs = data.table::fread("dat/statobs.csv")
citobs = data.table::fread("dat/citobs.csv")
fullobs_sp = data.table::fread("gunzip -c dat/fullobs_sp.csv.gz")
siem = data.table::fread("dat/siem_long.csv", encoding="UTF-8")
ukgdp = data.table::fread("dat/engdp12001700.csv", skip=1, encoding="UTF-8")

M = 9 # number of imputations
impvrbs = grepr('im3_ann\\d', names(fullobs_sp))

rhigh = raster(
    nrows = 176,  ncols = 228, 
    xmn = -11.75, xmx = 44.75, 
    ymn = 27.25,  ymx = 70.75)

### churches data ###
#####################
fullobs_sp[!is.na(phase) & firstobs != TRUE, newbuild := m2]

rchm = list()
periods = list(700:1000, 1001:1200, 1201:1347, 1348:1500)
for (i in 1:length(periods)){
    temp = fullobs_sp[year %in% periods[[i]], ]
    spdf = sp::SpatialPointsDataFrame(
        coords = temp[, list(lon, lat)], 
        data = temp[, list(im2_ann, im3_ann, newbuild)], 
        proj4str = wgs)
    spdf = spdf[!is.na(spdf$im3_ann) & spdf$im3_ann != 0, ]
    r_temp = raster::rasterize(spdf, rhigh, 
        field = "im3_ann", fun = sum, na.rm = TRUE)
    rchm[[i]] = raster::focal(r_temp, 
        w = raster::focalWeight(r_temp, 0.7, 'Gauss'), 
        fun = sum, 
        na.rm = TRUE)

    cat("Sum in raw: ")
    print(sum(getValues(r_temp), na.rm = TRUE))
    cat("Sum in smooth: ")
    print(sum(getValues(rchm[[i]]), na.rm = TRUE))
}
rchm = raster::stack(rchm)
rchm = trim(rchm)

# express values per square km2
rchm = rchm / area(rchm)

rchm[is.na(rchm)] = 0
pdf("figs/4m3maps_smoothed.pdf", width=10, height=10)
par(mfrow=c(2,2), font.main=1, mar=c(2, 2, 3, 7))
plot(rchm[[1]], main="700-1000", zlim = c(0, 31), col=viridisLite::magma(256), axes=F,
    axis.args=list(at=seq(0, 31, 10)))
add_borders(border='white')
plot(rchm[[2]], main="1000-1200", zlim = c(0, 72), col=viridisLite::magma(256), axes=F,
    axis.args=list(at=seq(0, 60, 20)))
add_borders(border='white')
plot(rchm[[3]], main="1200-1348", zlim = c(0, 72), col=viridisLite::magma(256), axes=F,
    axis.args=list(at=seq(0, 60, 20)))
add_borders(border='white')
plot(rchm[[4]], main="1348-1500", zlim = c(0, 72), col=viridisLite::magma(256), axes=F,
    axis.args=list(at=seq(0, 60, 20)))
add_borders(border='white')
dev.off()
