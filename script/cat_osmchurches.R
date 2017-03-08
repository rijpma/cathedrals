rm(list=ls())
setwd("~/dropbox/cathedrals/")
options(stringsAsFactors=FALSE)

library(data.table)
library(jsonlite)
library(XML)
library(osmar)
library(sp)
library(geosphere)

os = sp::CRS("+init=EPSG:27700")
wgs = sp::CRS("+proj=longlat +datum=WGS84 +no_defs ")
rdriehoek = CRS("+init=epsg:28992")

source("script/cat_functions.r")

siem_pt = data.table::fread("dat/siem_pt.csv")
siem_uk = data.table::fread("dat/siem_uk.csv")
siem_fr = data.table::fread("dat/siem_france.csv")
siem_bs = data.table::fread('dat/extra_bishoprics.csv')
siem_be = data.table::fread("dat/siem_belgium.csv")
siem_ch = data.table::fread("dat/siem_swiss.csv")
siem_de = data.table::fread("dat/siem_de.csv")
siem_nl = data.table::fread("dat/siem_nl.csv")
siem_lu = data.table::fread("dat/siem_lu.csv")


# rural squares
#--------------
pet_way = get_osm_data(siem_uk[city=="Peterborough (Medeshamstede)", ], 
    what='way', radius=50, ruins=TRUE)
pet_rel = get_osm_data(siem_uk[city=="Peterborough (Medeshamstede)", ], 
    what='relation', radius=50, ruins=TRUE)
che_way = get_osm_data(siem_uk[city=="Chester", ], 
    what='way', radius=50, ruins=TRUE)
che_rel = get_osm_data(siem_uk[city=="Chester", ], 
    what='relation', radius=50, ruins=TRUE)
ami_way = get_osm_data(siem_fr[city=="Amiens", ], 
    what='way', radius=50, ruins=TRUE)
ami_rel = get_osm_data(siem_fr[city=="Amiens", ], 
    what='relation', radius=50, ruins=TRUE)
tou_way = get_osm_data(siem_fr[city=="Toulouse", ], 
    what='way', radius=50, ruins=TRUE)
tou_rel = get_osm_data(siem_fr[city=="Toulouse", ], 
    what='relation', radius=50, ruins=TRUE)
dij_way = get_osm_data(siem_fr[city=="Dijon", ], 
    what='way', radius=50, ruins=TRUE)
dij_rel = get_osm_data(siem_fr[city=="Dijon", ], 
    what='relation', radius=50, ruins=TRUE)
maa_way = get_osm_data(siem_nl[city=="Maastricht", ], 
    what='way', radius=50, ruins=TRUE)
maa_rel = get_osm_data(siem_nl[city=="Maastricht", ], 
    what='relation', radius=50, ruins=TRUE)
osn_way = get_osm_data(siem_de[city=="Osnabrueck (Osnabrück)", ], 
    what='way', radius=50, ruins=TRUE)
osn_rel = get_osm_data(siem_de[city=="Osnabrueck (Osnabrück)", ], 
    what='relation', radius=50, ruins=TRUE)
nue_way = get_osm_data(siem_de[city=="Nuernberg (Nürnberg)", ], 
    what='way', radius=50, ruins=TRUE)
nue_rel = get_osm_data(siem_de[city=="Nuernberg (Nürnberg)", ], 
    what='relation', radius=50, ruins=TRUE)

rur_ways = list(pet_way, che_way, ami_way, tou_way, dij_way, maa_way, osn_way, nue_way)
rur_rels = list(pet_rel, che_rel, ami_rel, tou_rel, dij_rel, maa_rel, osn_rel, nue_rel)

polys_way_rur = polylist2df(rur_ways)
polys_way_rur@data$surface = geosphere::areaPolygon(polys_way_rur)
polys_way_rur@data$lon = sp::coordinates(polys_way_rur)[, 1]
polys_way_rur@data$lat = sp::coordinates(polys_way_rur)[, 2]

polys_rel_rur = polylist2df(rur_rels, what='relation')
polys_rel_rur@data$surface = geosphere::areaPolygon(polys_rel_rur)
polys_rel_rur@data$surface[polys_rel_rur@data$role=="inner"] = -1 * polys_rel_rur@data$surface[polys_rel_rur@data$role=="inner"]
polys_rel_rur@data$lon = sp::coordinates(polys_rel_rur)[, 1]
polys_rel_rur@data$lat = sp::coordinates(polys_rel_rur)[, 2]
polys_rel_rur = aggregate_multipolys(polys=polys_rel_rur, by='osmid')

add_borders()

out = sp_rbind(polys4merge=polys_way_rur, polys=polys_rel_rur)

dim(out)
out = out[out$surface >= 1e3, ]
dim(out)

# previously checked osmids
osmids = NULL
fls = grepr('_eb[^g]', list.files("dat"))
for (fl in fls){
    osmids = c(osmids, data.table::fread(paste0("dat/", fl), skip=2, colClasses="character")[, 1])
}
osmids = unique(unlist(osmids))
osmids = gsub('_.*', '', osmids)
osmids = gsub('\\D', '', osmids)

out = out[!gsub('_.*', '', out$osmid) %in% unique(unlist(osmids)), ]

max(nchar(unlist(out@data))) < 254
out@data$timestamp = as.character(out@data$timestamp)
maptools::writeSpatialShape(out, "dat/gis/rur")
maptools::readSpatialShape(out, "dat/gis/rur")




out = out[order(out$city, out$surface), ]
write_filltable(out, outfile="dat/rurcities.csv")

# extra bishoprics
#---------
polylist_rel_bs = list()
for (i in 37:nrow(siem_bs)){
    cty = siem_bs[i, ]
    polylist_rel_bs[[cty$city]] = get_osm_data(cty, what='relation', block=FALSE)
    Sys.sleep(30)
}
polylist_way_bs = list()
for (i in 1:nrow(siem_bs)){
    cty = siem_bs[i, ]
    polylist_way_bs[[cty$city]] = get_osm_data(cty, what='way', block=FALSE)
    Sys.sleep(30)
}
polys_way_bs = polylist2df(polylist_way_bs)
polys_way_bs@data$surface = geosphere::areaPolygon(polys_way_bs)
polys_way_bs@data$lon = sp::coordinates(polys_way_bs)[, 1]
polys_way_bs@data$lat = sp::coordinates(polys_way_bs)[, 2]

polys_rel_bs = polylist2df(polylist_rel_bs, what='relation')
polys_rel_bs@data$surface = geosphere::areaPolygon(polys_rel_bs)
polys_rel_bs@data$surface[polys_rel_bs@data$role=="inner"] = -1 * polys_rel_bs@data$surface[polys_rel_bs@data$role=="inner"]
polys_rel_bs@data$lon = sp::coordinates(polys_rel_bs)[, 1]
polys_rel_bs@data$lat = sp::coordinates(polys_rel_bs)[, 2]
polys_rel_bs = aggregate_multipolys(polys=polys_rel_bs, by='osmid')

plot(polys_way_bs)
plot(polys_rel_bs, add=T, col=2)
add_borders()

out = sp_rbind(polys4merge=polys_way_bs, polys=polys_rel_bs)

pdf('figs/bs_churches.pdf')
plot_churches_by_city(c(polylist_way_bs, polylist_rel_bs), siem_bs)
# plot_churches_by_city(polylist_rel_bs, polylist_rel_bs), siem_bs)
dev.off()

dim(out)
out = out[out$surface >= 1e3, ]
dim(out)

max(nchar(unlist(out@data))) < 254
out@data$timestamp = as.character(out@data$timestamp)
maptools::writeSpatialShape(out, "dat/gis/bs")

out = out[order(out$city, out$surface), ]

write_filltable(out, outfile="dat/bscities.csv")



# portugal
#---------

polylist_rel_pt = list()
for (i in 1:nrow(siem_pt)){
    cty = siem_pt[i, ]
    polylist_rel_pt[[cty$city]] = get_osm_data(cty, what='relation', block=FALSE)
    Sys.sleep(30)
}
polylist_way_pt = list()
for (i in 1:nrow(siem_pt)){
    cty = siem_pt[i, ]
    polylist_way_pt[[cty$city]] = get_osm_data(cty, what='way', block=FALSE)
    Sys.sleep(30)
}

# combine two lists 
# or later as dataframe?
polys_way_pt = polylist2df(polylist_way_pt)
polys_way_pt@data$surface = geosphere::areaPolygon(polys_way_pt)
polys_way_pt@data$lon = sp::coordinates(polys_way_pt)[, 1]
polys_way_pt@data$lat = sp::coordinates(polys_way_pt)[, 2]

polys_rel_pt = polylist2df(polylist_rel_pt, what='relation')
polys_rel_pt@data$surface = geosphere::areaPolygon(polys_rel_pt)
polys_rel_pt@data$surface[polys_rel_pt@data$role=="inner"] = -1 * polys_rel_pt@data$surface[polys_rel_pt@data$role=="inner"]
polys_rel_pt@data$lon = sp::coordinates(polys_rel_pt)[, 1]
polys_rel_pt@data$lat = sp::coordinates(polys_rel_pt)[, 2]
polys_rel_pt = aggregate_multipolys(polys=polys_rel_pt, by='osmid')

plot(polys_way_pt)
plot(polys_rel_pt, add=T, col=2)


pdf('figs/pt_way_churches.pdf')
plot_churches_by_city(polylist_way_pt, siem_pt)
dev.off()

out = sp_rbind(polys4merge=polys_way_pt, polys=polys_rel_pt)
# out = rbindlist(list(polys_way_pt@data, polys_rel_pt@data), use.names=TRUE, fill=TRUE)

pdf('figs/pt_churches.pdf')
plot_churches_by_city(c(polylist_way_pt, polylist_rel_pt), siem_pt)
# plot_churches_by_city(polylist_rel_pt, polylist_rel_pt), siem_pt)
dev.off()

dim(out)
out = out[out$surface >= 1e3, ]
dim(out)

max(nchar(unlist(out@data))) < 254
out@data$timestamp = as.character(out@data$timestamp)
maptools::writeSpatialShape(out, "dat/gis/pt")

out = out[order(out$city, out$surface), ]

write_filltable(out, outfile="dat/ptcities.csv")

# belgium relations
#------------------
polylist_rel_be = list()
for (i in 1:nrow(siem_be)){
    cty = siem_be[i, ]
    polylist_rel_be[[cty$city]] = get_osm_data(cty, what='relation', block=FALSE)
    Sys.sleep(30)
}
polys_rel_be = polylist2df(polylist_rel_be, what="relation")
polys_rel_be@data$surface = geosphere::areaPolygon(polys_rel_be)
polys_rel_be@data$surface[polys_rel_be@data$role=="inner"] = -1 * polys_rel_be@data$surface[polys_rel_be@data$role=="inner"]
polys_rel_be@data$lon = sp::coordinates(polys_rel_be)[, 1]
polys_rel_be@data$lat = sp::coordinates(polys_rel_be)[, 2]

dim(polys_rel_be)
polys_rel_be = polys_rel_be[polys_rel_be$Group.1 %in% polys_rel_be$Group.1[polys_rel_be$surface >= 1e3], ]
dim(polys_rel_be)

pdf('figs/bel_rel_churches.pdf')
plot_churches_by_city(polylist_rel_be, siem_be)
dev.off()

out = aggregate_multipolys(polys_rel_be)
write_filltable(out, outfile="dat/belgiancities_rels.csv")


# switzerland relations
#----------------------

# additional churches entered as relation
polylist_rel_ch = list()
for (i in 13:nrow(siem_ch)){
    polylist_rel_ch[[siem_ch$city[i]]] = get_osm_data(cty=siem_ch[i, ], what='relation', block=FALSE)
    Sys.sleep(30)
}
polys_rel_ch = polylist2df(polylist_rel_ch)
polys_rel_ch@data$surface = geosphere::areaPolygon(polys_rel_ch)
polys_rel_ch@data$surface[polys_rel_ch@data$role=="inner"] = -1 * polys_rel_ch@data$surface[polys_rel_ch@data$role=="inner"]
polys_rel_ch@data[polys_rel_ch@data$role!='inner', c('osmid', 'role', 'surface')]
polys_rel_ch@data$lon = sp::coordinates(polys_rel_ch)[, 1]
polys_rel_ch@data$lat = sp::coordinates(polys_rel_ch)[, 2]

dim(polys_rel_ch)
polys_rel_ch = polys_rel_ch[polys_rel_ch$Group.1 %in% polys_rel_ch$Group.1[polys_rel_ch$surface >= 1e3], ]
dim(polys_rel_ch)

pdf('figs/swiss_rel_churches.pdf')
plot_churches_by_city(polylist_rel_ch, siem_ch)
dev.off()

out = aggregate_multipolys(polys_rel_ch)
write_filltable(out, outfile="dat/swisscities_rels.csv")

# french relations
#-----------------
# additional churches entered as relation
polylist_rel_fr = list()
for (i in 53:nrow(siem_fr)){
    polylist_rel_fr[[siem_fr$city[i]]] = get_osm_data(cty=siem_fr[i, ], what='relation', block=FALSE)
    Sys.sleep(30)
}
polys_rel_fr = polylist2df(polylist_rel_fr, what="relation")
polys_rel_fr@data$surface = geosphere::areaPolygon(polys_rel_fr)
polys_rel_fr@data$surface[polys_rel_fr@data$role=="inner"] = -1 * polys_rel_fr@data$surface[polys_rel_fr@data$role=="inner"]
polys_rel_fr@data[polys_rel_fr@data$role!='inner', c('osmid', 'role', 'surface')]
polys_rel_fr@data$lon = sp::coordinates(polys_rel_fr)[, 1]
polys_rel_fr@data$lat = sp::coordinates(polys_rel_fr)[, 2]

dim(polys_rel_fr)
polys_rel_fr = polys_rel_fr[polys_rel_fr$Group.1 %in% polys_rel_fr$Group.1[polys_rel_fr$surface >= 1e3], ]
dim(polys_rel_fr)

pdf('figs/france_rel_churches.pdf')
plot_churches_by_city(polylist_rel_fr, siem_fr)
dev.off()

out = aggregate_multipolys(polys_rel_fr)
write_filltable(out, outfile="dat/frenchcities_rels.csv")

# germany
#--------

polylist_rel_de = list()
for (i in 48:nrow(siem_de)){
    cty = siem_de[i, ]
    polylist_rel_de[[cty$city]] = get_osm_data(cty, what='relation', block=FALSE)
    Sys.sleep(30)
}

polys_rel_de = polylist2df(polylist_rel_de)
polys_rel_de@data$surface = geosphere::areaPolygon(polys_rel_de)
polys_rel_de@data$surface[polys_rel_de@data$role=="inner"] = -1 * polys_rel_de@data$surface[polys_rel_de@data$role=="inner"]
polys_rel_de@data$lon = sp::coordinates(polys_rel_de)[, 1]
polys_rel_de@data$lat = sp::coordinates(polys_rel_de)[, 2]

dim(polys_rel_de)
polys_rel_de = polys_rel_de[polys_rel_de$Group.1 %in% polys_rel_de$Group.1[polys_rel_de$surface >= 1e3], ]
dim(polys_rel_de)

pdf('figs/deu_rel_churches.pdf')
plot_churches_by_city(polylist_rel_de, siem_de)
dev.off()

out = aggregate_multipolys(polys_rel_de)
write_filltable(out, outfile="dat/deucities_rels.csv")

# ways

polylist_de = list()
for (i in 1:nrow(siem_de)){
    cty = siem_de[i, ]
    polylist_de[[cty$city]] = get_osm_data(cty, what='way', block=FALSE)
    Sys.sleep(30)
}
# polylist_de1 = get_osm_data(siem_de[1:74, ])
# polylist_de2 = get_osm_data(siem_de[75:149, ])
# polylist_de3 = get_osm_data(siem_de[150:nrow(siem_de), ])
# polylist_de = c(polylist_de1, polylist_de2, polylist_de3)

polys_de = polylist2df(polylist_de)

plot(polys_de)
polys_de@data$surface = geosphere::areaPolygon(polys_de)
polys_de@data$lon = sp::coordinates(polys_de)[, 1]
polys_de@data$lat = sp::coordinates(polys_de)[, 2]

max(nchar(unlist(polys_de@data))) < 254
polys_de@data$timestamp = as.character(polys_de@data$timestamp)
maptools::writeSpatialShape(polys_de, "dat/gis/germany")

dim(polys_de)
polys_de = polys_de[polys_de$surface >= 1e3, ]
dim(polys_de)

pdf("figs/de_churches.pdf")
plot_churches_by_city(polylist_de, siem_de)
dev.off()

write_filltable(polys_de, 'dat/deucities.csv')

# united kingdom
#---------------

# additional churches entered as relation
polylist_rel_uk = list()
for (i in 129:nrow(siem_uk)){
    polylist_rel_uk[[siem_uk$city[i]]] = get_osm_data(cty=siem_uk[i, ], what='relation', block=FALSE)
    Sys.sleep(30)
}
# manchester cathedral has broken polys, could be fixed on osm
# newark not in siem
# stamford not in siem
# stirling not in siem
# windsor, louth, melford, lavenham all not in siem
# rochester (), salisbury (149498084) was accidentally omitted in bruce's file
# wells (1277182) not in siem (bath is)
# ripon, southwell, binham not in siem
# bury st edmunds, 
# St Augustine's Abbey (ruin) not in osm
# crowland, dunster, glastonbury, great malvern, hexham, lindisfarm, malmesbury, pershore not in siem
# cambridge SMG is only 700 m2
# altham niet in siem

unique(siem$city[grep('ranth|avenha|elfo|outh', siem$city)])
# Dorchester could be added: get_osm_data_church(35311931, what='way')
# Norwich could be added: get_osm_data_church(103327553, what='way')
# Greyfriars church (77468736), reading is 900m2, could be added as this is in part because it is ruined
# southwark 26183417 zou er in moeten zitten; AR controleren

# Manchester
topo = osmar::as_sp(osmar::get_osm(osmar::relation("4423340"), full=TRUE))
coordinates(rgeos::gCentroid(topo$lines))

# Norwich
topo = osmar::as_sp(osmar::get_osm(osmar::way("103327553"), full=TRUE))
coordinates(rgeos::gCentroid(topo$lines))

# Reading
topo = osmar::as_sp(osmar::get_osm(osmar::way("77468736"), full=TRUE))
coordinates(rgeos::gCentroid(topo$lines))

# canterbury: aug abbey (in osm, ruin)
# bury st edmunds: niet in osm (ruine)
# whitby abbey: ruine in osm
# st mary's in york: ruine in osm
# reading abbey: ruine in osm
# rest: ruine | niet in siem | te klein

polys_rel_uk = polylist2df(polylist_rel_uk)
polys_rel_uk@data$surface = geosphere::areaPolygon(polys_rel_uk)
polys_rel_uk@data$surface[polys_rel_uk@data$role=="inner"] = -1 * polys_rel_uk@data$surface[polys_rel_uk@data$role=="inner"]
polys_rel_uk@data[polys_rel_uk@data$role!='inner', c('osmid', 'role', 'surface')]
polys_rel_uk@data$lon = sp::coordinates(polys_rel_uk)[, 1]
polys_rel_uk@data$lat = sp::coordinates(polys_rel_uk)[, 2]

dim(polys_rel_uk)
polys_rel_uk = polys_rel_uk[polys_rel_uk$Group.1 %in% polys_rel_uk$Group.1[polys_rel_uk$surface >= 1e3], ]
dim(polys_rel_uk)

pdf('figs/gbr_rel_churches.pdf')
plot_churches_by_city(polylist_rel_uk, siem_uk)
dev.off()

out = aggregate_multipolys(polys_rel_uk)
write_filltable(out, outfile="dat/gbrcities_rels.csv")

# ways
polylist_uk = list()
for (i in 1:nrow(siem_uk)){
    cty = siem_uk[i, ]
    polylist_uk[[cty$city]] = get_osm_data(cty, what='way', block=FALSE)
    Sys.sleep(30)
}

# polylist_uk1 = get_osm_data(siem_uk[1:74, ])
# polylist_uk2 = get_osm_data(siem_uk[75:nrow(siem_uk), ])
# polylist_uk = c(polylist_uk1, polylist_uk2)
polys_uk = polylist2df(polylist_uk)

polys_uk@data$surface = geosphere::areaPolygon(polys_uk)
polys_uk@data$lon = coordinates(polys_uk)[, 1]
polys_uk@data$lat = coordinates(polys_uk)[, 2]

max(nchar(unlist(polys_uk@data))) < 254
polys_uk@data$timestamp = as.character(polys_uk@data$timestamp)
maptools::writeSpatialShape(polys_uk, "dat/gis/unitedkingdom")
polys_uk = maptools::readShapeSpatial("dat/gis/unitedkingdom.shp", proj4str=wgs)

dim(polys_uk)
polys_uk = polys_uk[polys_uk$surface >= 1e3, ]
dim(polys_uk)

pdf("figs/uk_churches.pdf")
plot_churches_by_city(polylist_uk, siem_uk)
dev.off()

lbe = maptools::readShapeSpatial("~/downloads/data/listed buildings/7April2016_ListedBuildingPoints.shp", proj4str=os)
lbe = sp::spTransform(lbe, wgs)
lbe@data$lbelink = paste0("https://historicengland.org.uk/listing/the-list/list-entry/", lbe@data$ListEntry)

lbs = maptools::readShapeSpatial("/Volumes/Transcend/dat/lb_scotland/listed_buildings.shp", proj4str=os)
lbs = sp::spTransform(lbs, wgs)

dm = distmatch(coordinates(polys_uk), coordinates(lbe))
polys_uk@data = data.frame(polys_uk@data, 
    lbe@data[dm$ll2, c("lbelink", "ListEntry", "Name")][match(1:nrow(polys_uk), dm$ll1), ])

dm = distmatch(coordinates(polys_uk), coordinates(lbs))
polys_uk@data$lbslink = as.character(lbs@data$WEBLINK[dm$ll2][match(1:nrow(polys_uk), dm$ll1)])

polys_uk@data$wales = polys_uk@data$city %in% c("Cardiff", "Swansea")
polys_uk@data[, c("lon_os", "lat_os")] = coordinates(spTransform(polys_uk, os))
polys_uk@data$lbwlink = paste0("http://historicwales.gov.uk/#zoom=7&lat=",
    polys_uk@data$lat_os,
    "&lon=",
    polys_uk@data$lon_os,
    "&layers=BFFTFFTFFTTT")
polys_uk@data$lblink = NA
polys_uk@data$lblink[is.na(polys_uk@data$lblink)] = polys_uk@data$lbelink[is.na(polys_uk@data$lblink)]
polys_uk@data$lblink[is.na(polys_uk@data$lblink)] = polys_uk@data$lbslink[is.na(polys_uk@data$lblink)]
polys_uk@data$lblink[polys_uk@data$wales] = polys_uk@data$lbwlink[polys_uk@data$wales]

names(polys_uk)[names(polys_uk)=="osmwikiped"] = "osmwikipedia"
write_filltable(polys_uk, outfile="dat/gbrcities.csv",
    baseinfo=c("osmid", "city", "osmname", "surface", "osmwikipedia", "lblink", "osmlink", "lat", "lon"))

# netherlands
#------------

# additional churches entered as relation
polylist_rel_nl = list()
for (i in 1:nrow(siem_nl)){
    polylist_rel_nl[[siem_nl$city[i]]] = get_osm_data(cty=siem_nl[i, ], what='relation', block=FALSE)
    Sys.sleep(30)
}
polys_rel_nl = polylist2df(polylist_rel_nl)
polys_rel_nl@data$surface = geosphere::areaPolygon(polys_rel_nl)
polys_rel_nl@data$surface[polys_rel_nl@data$role=="inner"] = -1 * polys_rel_nl@data$surface[polys_rel_nl@data$role=="inner"]
polys_rel_nl@data$lon = sp::coordinates(polys_rel_nl)[, 1]
polys_rel_nl@data$lat = sp::coordinates(polys_rel_nl)[, 2]

dim(polys_rel_nl)
polys_rel_nl = polys_rel_nl[polys_rel_nl$Group.1 %in% polys_rel_nl$Group.1[polys_rel_nl$surface >= 1e3], ]
dim(polys_rel_nl)

pdf('figs/nld_rel_churches.pdf')
plot_churches_by_city(polylist_rel_nl, siem_nl)
dev.off()

out = aggregate_multipolys(polys_rel_nl)
write_filltable(out, outfile="dat/nldcities_rels.csv")

# ways
polylist_nl = list()
for (i in 1:nrow(siem_nl)){
    cty = siem_nl[i, ]
    polylist_nl[[cty$city]] = get_osm_data(cty, what='way', block=FALSE)
    Sys.sleep(30)
}

# polylist_nl = get_osm_data(siem_nl)
# grote of jacobijnenkerk, leeuwarden: niet als "way" aanwezig
# kathedraal st cristoffel roermond; niet als "way" aanwezig
# sint laurenskerk rotterdam; niet juist beschreven in osm: 54085821
# grote of sint-jacobskerk den haag: niet juist beschreven in osm: 58617235
# laurenskerk = osmar::get_osm(way(54085821), full=T)
# jacobskerk = osmar::get_osm(way(58617235))
# osmar::as_sp(laurenskerk["ways"])
# osmar::as_osmar(XML::xmlParse(laurenskerk))

polys_nl = polylist2df(polylist_nl)

polys_nl@data$surface = geosphere::areaPolygon(polys_nl)
polys_nl@data$lon = coordinates(polys_nl)[, 1]
polys_nl@data$lat = coordinates(polys_nl)[, 2]

max(nchar(unlist(polys_nl@data))) < 254
polys_nl@data$timestamp = as.character(polys_nl@data$timestamp)
# maptools::writeSpatialShape(polys_nl, "dat/gis/netherlands")
# polys_nl = maptools::readShapeSpatial("dat/gis/netherlands")

pdf('figs/utrechtchurches.pdf', width=10, height=10)
plot(polys_nl[polys_nl$city=="Utrecht", ], xlim=c(5.1, 5.14), ylim=c(52.07, 52.11), border=uublu)
plot(polys_nl[polys_nl$city=="Utrecht" & polys_nl$surface > 1000, ], border=uured, add=T)
text(x=coordinates(polys_nl[polys_nl$city=="Utrecht" & polys_nl$surface > 1000, ])[, 1],
             y=coordinates(polys_nl[polys_nl$city=="Utrecht" & polys_nl$surface > 1000, ])[, 2] + 0.0006,
             labels=polys_nl@data[polys_nl$city=="Utrecht" & polys_nl$surface > 1000, "osmname"], cex=0.4, col=uured)
axis(1); axis(2)
dev.off()

dim(polys_nl)
polys_nl = polys_nl[polys_nl$surface >= 1e3, ]
dim(polys_nl)

pdf('figs/nld_churches.pdf')
plot_churches_by_city(polylist_nl, siem_nl)
dev.off()

mr = data.table::fread("/Volumes/Transcend/dat/monumentenregister/object.csv")
mr = mr[!(is.na(OBJ_X_COORD) | is.na(OBJ_Y_COORD)),]
mr[OBJ_RIJKSNUMMER%in% c("519047", "7258"),]
mr[, rmlink:=paste0("http://monumentenregister.cultureelerfgoed.nl/php/main.php?cAction=show&cOffset=0&cLimit=25&cOBJnr=",
    OBJ_NUMMER, "&sCompMonNr=", OBJ_RIJKSNUMMER)]

mrs = sp::SpatialPointsDataFrame(coords=as.matrix(mr[, list(OBJ_X_COORD, OBJ_Y_COORD)]), 
    proj4str=rdriehoek, data=mr)
mrs = sp::spTransform(mrs, wgs)
dm = distmatch(coordinates(polys_nl), coordinates(mrs))
polys_nl@data = data.frame(polys_nl@data, 
    mrs@data[dm$ll2, c("rmlink", "OBJ_NUMMER", "OBJ_RIJKSNUMMER", "OBJ_NAAM")][match(1:nrow(polys_nl), dm$ll1), ])

names(polys_nl)[names(polys_nl)=="osmwikiped"] = "osmwikipedia"

write_filltable(polys_nl, outfile="dat/nldcities.csv",
    baseinfo=c("osmid", "city", "osmname", "surface", "osmwikipedia", "rmlink", "osmlink", "lat", "lon"))

# luxembourg
#-----------
polylist_lu = list()
for (i in 1:nrow(siem_lu)){
    polylist_lu[[siem_lu[i, city]]] = get_osm_data(siem_lu[i, ], block=FALSE)
    # has no relations
}

polys_lu = polylist2df(polylist_lu)

polys_lu@data$surface = geosphere::areaPolygon(polys_lu)
polys_lu@data$lon = coordinates(polys_lu)[, 1]
polys_lu@data$lat = coordinates(polys_lu)[, 2]

# prep for spatial db write
max(nchar(unlist(polys_lu@data))) < 254
polys_lu@data$timestamp = as.character(polys_lu@data$timestamp)
maptools::writeSpatialShape(polys_lu, "dat/gis/luxembourg")

dim(polys_lu)
polys_lu = polys_lu[polys_lu$surface >= 1e3, ]
dim(polys_lu)

pdf("figs/lux_churches.pdf")
plot_churches_by_city(polylist_lu, siem_lu)
dev.off()

write_filltable(polys_lu, '~/desktop/test.csv')
write_filltable(polys_lu, 'dat/luxcities.csv')