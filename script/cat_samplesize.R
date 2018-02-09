rm(list = ls())
library("sf")

setwd("~/dropbox/cathedrals/")

belgium = sf::read_sf("dat/gis/belgium.shp")
france_extra_more = sf::read_sf("dat/gis/france_extra_more.shp")
france_extra = sf::read_sf("dat/gis/france_extra.shp")
france = sf::read_sf("dat/gis/france.shp")
germany = sf::read_sf("dat/gis/germany.shp")
netherlands = sf::read_sf("dat/gis/netherlands.shp")
swiss = sf::read_sf("dat/gis/swiss.shp")
unitedkingdom = sf::read_sf("dat/gis/unitedkingdom.shp")

nweur = rbind(belgium, 
    france_extra_more, 
    france_extra, 
    france, 
    germany, 
    netherlands,
    swiss,
    unitedkingdom)

length(unique(nweur$osmid))

nweur$surface = geosphere::areaPolygon(as_Spatial(st_geometry(nweur)))

surfaces = as.data.table(nweur)[, list(surface = sum(surface)), by = osmid]

hist(surfaces$surface)
hist(log(surfaces$surface))
abline(v = log(1e3), col = 2)

table(surfaces$surface > 1000)
prop.table(table(surfaces$surface > 1000))

# not currently in db
# bs = sf::read_sf("dat/gis/bs.shp")
# it = sf::read_sf("dat/gis/it.shp")
# james = sf::read_sf("dat/gis/james.shp")
# pt_rels = sf::read_sf("dat/gis/pt_rels.shp")
# pt_ways = sf::read_sf("dat/gis/pt_ways.shp")
# pt = sf::read_sf("dat/gis/pt.shp")
# rur = sf::read_sf("dat/gis/rur.shp")
# ruralbritain = sf::read_sf("dat/gis/ruralbritain.shp")
# ruralfrance = sf::read_sf("dat/gis/ruralfrance.shp")

