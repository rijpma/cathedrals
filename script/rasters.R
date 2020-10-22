# create raster maps of building activity

rm(list=ls())
setwd("~/dropbox/cathedrals")

library("raster")
library("maptools")
library("data.table")
library("viridisLite")

source('script/cat_functions.r')

wgs <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs ")
data("wrld_simpl", package = "maptools")

fullobs_sp = data.table::fread("gunzip -c dat/fullobs_sp.csv.gz")
eur = wrld_simpl[wrld_simpl$ISO3 %in% c("NLD", "BEL", "CHE", "FRA", "DEU", "GBR", "ITA", "LUX"), ]

M = 9 # number of imputations
impvrbs = names(fullobs_sp)[grep('im3_ann\\d', names(fullobs_sp))]

eu_extent = raster::extent(eur)
eu_extent@xmin = -7
baseraster = raster::raster(
    x = eu_extent, 
    nrows = 150,
    ncol = 180,
    crs = raster::crs(eur))

fullobs_sp[!is.na(phase) & firstobs != TRUE, newbuild := m2]

# smoothed raster of period totals
rba = list()
periods = list(700:1000, 1001:1200, 1201:1347, 1348:1500)
for (i in 1:length(periods)){
    temp = fullobs_sp[year %in% periods[[i]], ]
    spdf = sp::SpatialPointsDataFrame(
        coords = temp[, list(lon, lat)], 
        data = temp[, list(im2_ann, im3_ann, newbuild)], 
        proj4str = wgs)
    spdf = spdf[!is.na(spdf$im3_ann) & spdf$im3_ann != 0, ]
    r_temp = raster::rasterize(spdf, baseraster, 
        field = "im3_ann", fun = sum, na.rm = TRUE)
    rba[[i]] = raster::focal(r_temp, 
        w = raster::focalWeight(r_temp, 0.7, 'Gauss'), 
        fun = sum, 
        na.rm = TRUE)

    cat("Sum in raw: ", sum(getValues(r_temp), na.rm = TRUE))
    cat("Sum in smooth: ", sum(getValues(rba[[i]]), na.rm = TRUE), "\n")
}
rba = raster::stack(rba)
rba = raster::trim(rba)

# express values per square km2
rba = rba / raster::area(rba)

# NA to zero to ensure black like bg
rba[is.na(rba)] = 0 
rba = raster::mask(rba, eur)

pdf("figs/4m3maps_smoothed.pdf", width = 10, height = 10)
par(mfrow = c(2,2), font.main = 1, mar = c(2, 2, 3, 7))
plot(rba[[1]], main = "700-1000", zlim = c(0, 31), 
    col = viridisLite::plasma(256), axes = FALSE,
    axis.args = list(at = seq(0, 31, 10)))
add_borders(border='white')
plot(rba[[2]], main = "1000-1200", zlim = c(0, 72), 
    col = viridisLite::plasma(256), axes = FALSE,
    axis.args = list(at = seq(0, 60, 20)))
add_borders(border='white')
plot(rba[[3]], main = "1200-1348", zlim = c(0, 72), 
    col = viridisLite::plasma(256), axes = FALSE,
    axis.args = list(at = seq(0, 60, 20)))
add_borders(border='white')
plot(rba[[4]], main = "1348-1500", zlim = c(0, 72), 
    col = viridisLite::plasma(256), axes = FALSE,
    axis.args = list(at = seq(0, 60, 20)))
add_borders(border='white')
dev.off()
