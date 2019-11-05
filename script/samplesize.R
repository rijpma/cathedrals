# calculate share >1000 in all churches

rm(list = ls())

library("sf")
library("data.table")

setwd("~/dropbox/cathedrals/")

belgium = sf::read_sf("dat/gis/belgium.shp")
france_extra_more = sf::read_sf("dat/gis/france_extra_more.shp")
france_extra = sf::read_sf("dat/gis/france_extra.shp")
france = sf::read_sf("dat/gis/france.shp")
germany = sf::read_sf("dat/gis/germany.shp")
netherlands = sf::read_sf("dat/gis/netherlands.shp")
swiss = sf::read_sf("dat/gis/swiss.shp")
unitedkingdom = sf::read_sf("dat/gis/unitedkingdom.shp")
# italy shapefiles were saved after dropping < 1000 m2, so no point adding

nweur = rbind(belgium, 
    france_extra_more, 
    france_extra, 
    france, 
    germany, 
    netherlands,
    swiss,
    unitedkingdom)

nweur$surface = geosphere::areaPolygon(as_Spatial(st_geometry(nweur)))

cat("Total churches from OSM: ")
length(unique(nweur$osmid))

surfaces = data.table::as.data.table(nweur)
surfaces = surfaces[, list(surface = sum(surface)), by = osmid]

cat("Churches in OSM > 1000 m2")
sum(surfaces$surface > 1000)
prop.table(table(larger1000m2 = surfaces$surface > 1000))
