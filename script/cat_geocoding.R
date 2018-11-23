rm(list=ls())
options(stringsAsFactors=FALSE)

setwd('~/dropbox/cathedrals/')

library("data.table")
library(countrycode)
library(sp)

source('script/cat_functions.r')

apikey = readLines("dat/googleapikey.txt")

hdr <- unlist(read.csv("dat/siem.csv", nrows=1))

siem <- read.csv('dat/siem.csv', skip=2)
siemloc <- c('city', 'north', 'east')
siem$city <- iconv(siem$city, from='macroman', to='utf8')

tld <- structure(countrycode(unique(siem$country), 'country.name', 'iso2c'), names=unique(siem$country))
siem$tld <- tld[siem$country]
mss = read.csv("dat/missing_bishoprics.csv", skip=1)

# additions to siem dataset
# -------------------------
siem_new = readxl::read_excel("excels/urb pop 700 to 2000.xlsx",
    skip = 2, sheet = "Sheet2")
siem_old = data.table::fread("dat/siem_long.csv")

setDT(siem_new)

siem_new = siem_new[country %in% unique(siem_old$country)]

new_not_in_old = setdiff(siem_new$city, siem_old$city)

new_gc = lapply(new_not_in_old, geocode, reg = 'eu', apikey = apikey)
new_gc[sapply(new_gc, nrow) > 1]

# replace wrong reichen
new_gc[[grep("Reichen", sapply(new_gc, `[`, 'loc'))]] = geocode("Reichenbach im Vogtland", apikey = apikey)

new_gc_2nd = filter_prox(new_gc, siem_new[city %in% new_not_in_old],
    ycoords = c("longtitude", "latitude"))
new_gc_2nd[sapply(new_gc_2nd, nrow) > 1]

# first quess always best here
new_gc_3rd = lapply(new_gc_2nd, function(x) x[1, ])

# stay in europe
new_gc_3rd[sapply(new_gc_3rd, `[`, 'lon') < -40]
new_gc_3rd[[grep("Le Blanc", sapply(new_gc_3rd, `[`, 'loc'))]] = geocode("Le Blanc", reg = 'fr', apikey = apikey)
new_gc_3rd[[grep("Newark", sapply(new_gc_3rd, `[`, 'loc'))]] = geocode("Newark", reg = 'uk', apikey = apikey)

# no matches, repair by fixing country
new_gc_3rd[is.na(sapply(new_gc_3rd, `[`, 'lon'))]
new_gc_3rd[[grep("Dour", sapply(new_gc_3rd, `[`, 'loc'))]] = geocode("Dour", reg = 'fr', apikey = apikey)
new_gc_3rd[[grep("Lens", sapply(new_gc_3rd, `[`, 'loc'))]] = geocode("Lens", reg = 'fr', apikey = apikey)
new_gc_3rd[[grep("Pirna", sapply(new_gc_3rd, `[`, 'loc'))]] = geocode("Pirna", reg = 'de', apikey = apikey)
new_gc_3rd[[grep("Alba", sapply(new_gc_3rd, `[`, 'loc'))]] = geocode("Alba", reg = 'it', apikey = apikey)
new_gc_3rd[[grep("Cavour", sapply(new_gc_3rd, `[`, 'loc'))]] = geocode("Cavour, Piedmont", reg = 'it', apikey = apikey)
new_gc_3rd[[grep("Este", sapply(new_gc_3rd, `[`, 'loc'))]] = geocode("Este", reg = 'it', apikey = apikey)
new_gc_3rd[[grep("Latina", sapply(new_gc_3rd, `[`, 'loc'))]] = geocode("Latina", reg = 'it', apikey = apikey)
new_gc_3rd[[grep("Oria", sapply(new_gc_3rd, `[`, 'loc'))]] = geocode("Oria", reg = 'it', apikey = apikey)[1, ]
new_gc_3rd[[grep("Trino", sapply(new_gc_3rd, `[`, 'loc'))]] = geocode("Trino", reg = 'it', apikey = apikey)
new_gc_3rd[[grep("Goes", sapply(new_gc_3rd, `[`, 'loc'))]] = geocode("Goes", reg = 'nl', apikey = apikey)
new_gc_3rd[[grep("Leek", sapply(new_gc_3rd, `[`, 'loc'))]] = geocode("Leek", reg = 'uk', apikey = apikey)
new_gc_3rd[[grep("Middlesborough", sapply(new_gc_3rd, `[`, 'loc'))]] = geocode("Middlesborough", reg = 'uk', apikey = apikey)[1, ]
new_gc_3rd[[grep("Rugby", sapply(new_gc_3rd, `[`, 'loc'))]] = geocode("Rugby", reg = 'uk', apikey = apikey)
new_gc_3rd[[grep("Sandwich", sapply(new_gc_3rd, `[`, 'loc'))]] = geocode("Sandwich, Kent", reg = 'uk', apikey = apikey)
new_gc_3rd[[grep("Sefton", sapply(new_gc_3rd, `[`, 'loc'))]] = geocode("Sefton", reg = 'uk', apikey = apikey)
new_gc_3rd[[grep("Windsor", sapply(new_gc_3rd, `[`, 'loc'))]] = geocode("Windsor", reg = 'uk', apikey = apikey)
new_gc_3rd[[grep("Schwytz", sapply(new_gc_3rd, `[`, 'loc'))]] = geocode("Schwyz, Schwyz", reg = 'ch', apikey = apikey)
new_gc_3rd[[grep("Ebersbach", sapply(new_gc_3rd, `[`, 'loc'))]] = geocode("Ebersbach, Görlitz", reg = 'de', apikey = apikey)
new_gc_3rd[is.na(sapply(new_gc_3rd, `[`, 'lon'))]

plot(do.call(rbind, new_gc_3rd)[, c("lon", "lat")])
points(siem_new[city %in% new_not_in_old, .(longtitude, latitude)], col = 2, pch = 19, cex = 0.5)

siem_new_gcd = check_geocodes(siem_ctr = siem_new[city %in% new_not_in_old], 
    ctr_gcd = new_gc_3rd, ycoords = c("longtitude", "latitude"))

tail(siem_new_gcd[order(siem_new_gcd$distance), c("city", "distance", "lon", "lat", "longtitude", "latitude")])

data.table::fwrite(siem_new_gcd, "dat/siem_add.csv")

# missing bishoprics
# ------------------
mss$ctr = substring(mss$ctr, 1, 2)
mss$ctr[mss$ctr=="sw"] = "ch"
ms_gcd = apply(mss, 1, function(x) geocode(x['loc'], reg=x['ctr']))
nms = sapply(ms_gcd, function(x) unique(x$loc))
# replace with names
ms_gcd[sapply(ms_gcd, nrow) > 1]
ms_gcd[[grep('bangor', nms)]] = ms_gcd[[grep('bangor', nms)]][ms_gcd[[grep('bangor', nms)]]$lon > -5, ]
ms_gcd[[grep('magelone', nms)]] = geocode("Villeneuve-lès-Maguelone", reg='fr')
ms_gcd[[grep('dol', nms)]] = geocode("Dol-de-Bretagne", reg='fr')


ms_gcd[[grep('david', nms)]] = geocode("St Davids, Pembrokeshire")
ms_gcd[[grep('lizier', nms)]] = geocode("Saint-Lizier, arriege", reg='fr')
ms_gcd[[grep('brugny', nms)]][c('lat', 'lon')] = c(48.71, 2.49)
ms_gcd[[grep('galndive', nms)]][c('lat', 'lon')] = c(43.95, 6.81)
# brungy zijn: 48.71 N, 2.49  E
# glandive zijn: 43.95 N en 6.81 E
out = do.call(rbind, ms_gcd)
names(out)[1] = 'city'
out$tld = mss$ctr
write.csv(out, 'dat/extra_bishoprics.csv', row.names=F)


# Italy
#------
siem_it = siem[grep('tal', siem$country), ]
it_gcd = lapply(gsub('\\(.*', '', siem_it$city), 
    function(x) geocode(x, reg='it'))
it_gcd[sapply(it_gcd, nrow) > 1]

it_2nd = filter_prox(it_gcd, siem_it)
it_2nd[sapply(it_2nd, nrow) > 1]
it_2nd[sapply(it_2nd, nrow) > 1] = it_gcd[sapply(it_2nd, nrow) > 1]

it_3rd = lapply(it_2nd, function(x) x[1, ])

# it_3rd[[grep('ldenburg', siem_it$city)]] = geocode("Oldenburg-Land", reg="de")
it_3rd[[grep('illafra', siem_it$city)]] = geocode("Villafrance Piemonte", reg='it')[1, ]
# varallo = slightly better code?
it_3rd[[grep('inigag', siem_it$city)]] = geocode("Senigalia", reg='it')
it_3rd[[grep('artolome', siem_it$city)]] = geocode("San Bartolomeo in Galdo", reg="it")
it_3rd[[grep('Piana', siem_it$city)]] = geocode("Piana degli Albanesi", reg="it")
it_3rd[[grep('Novi', siem_it$city)]] = geocode("Novi Ligure", reg="it")
it_3rd[[grep('Montemaggiore', siem_it$city)]] = geocode("Montemaggiore Belsito", reg="it")
it_3rd[[grep('Monte di San Giuliano', siem_it$city)]]= geocode("Erice", reg="it")
it_3rd[[grep('Mola', siem_it$city)]] = geocode("Mola di Bari", reg="it")
it_3rd[[grep("Acquaviva", siem_it$city)]] = geocode("Acquaviva", reg="sm")
it_3rd[[grep("Aderno", siem_it$city)]] = geocode("Adrano", reg="it")
it_3rd[[grep("Aquila", siem_it$city)]] = geocode("L'Aquila", reg="it")
it_3rd[[grep("Augusta", siem_it$city)]] = geocode("Augusta, Sicily", reg="it")
it_3rd[[grep("Bella", siem_it$city)]] = geocode("Bella, Basilicata", reg="it")
it_3rd[[grep("Casale", siem_it$city)]] = geocode("Casale Monferrato", reg="it")
it_3rd[[grep("Francavilla", siem_it$city)]] = geocode("Francavilla Fontana", reg="it")
it_3rd[[grep("Gravina", siem_it$city)]] = geocode("Gravina in Puglia", reg="it")
it_3rd[[grep("Mazzara", siem_it$city)]] = geocode("Mazara del Vallo", reg="it")
# both of these are correct, yes?
siem[grep("Gravina", siem_it$city), c("north", "east")]
siem[grep("Mazzara", siem_it$city), c("north", "east")]

siem_it_gcd = check_geocodes(siem_ctr=siem_it, ctr_gcd=it_3rd)
siem_it_gcd[siem_it_gcd$distance > 25, c('city', 'north', 'east', 
    'lat', 'lon', 'loc_frmtd', 'distance')]

write.csv(siem_it_gcd, 'dat/siem_it.csv')

# Portugal
#--------
siem_pt = siem[grep('ortuga', siem$country), ]
pt_gcd = lapply(gsub('\\(.*', '', siem_pt$city), 
    function(x) geocode(x, reg='pt'))
all(sapply(pt_gcd, nrow) == 1)
siem_pt_gcd = check_geocodes(siem_ctr=siem_pt, ctr_gcd=pt_gcd)
siem_pt_gcd[siem_pt_gcd$distance > 25, c('city', 'north', 'east', 
    'lat', 'lon', 'loc_frmtd', 'distance')]
write.csv(siem_pt_gcd, "dat/siem_pt.csv")

# Germany
#--------
siem_de = siem[grep('erma', siem$country), ]
de_gcd = lapply(gsub('\\(.*', '', siem_de$city), 
    function(x) geocode(x, reg='de'))
de_2nd = filter_prox(de_gcd, siem_de)
de_2nd[sapply(de_2nd, nrow) > 1] = de_gcd[sapply(de_2nd, nrow) > 1]
de_3rd = lapply(de_2nd, function(x) x[1, ])
de_3rd[[grep('ldenburg', siem_de$city)]] = geocode("Oldenburg-Land", reg="de")
de_3rd[[grep('nnaberg', siem_de$city)]] = geocode("Annaberg-Buchholz", reg="de")
de_3rd[[grep('Oder', siem_de$city)]] = geocode("Frankfurt an der Oder", reg="de")
de_3rd[[grep('angensal', siem_de$city)]] = geocode("Bad Langensalza", reg="de")
de_3rd[[grep('ronenb', siem_de$city)]] = geocode("Cronenberg, Wuppertal", reg="de")
siem_de_gcd = check_geocodes(siem_ctr=siem_de, ctr_gcd=de_3rd)
siem_de_gcd[siem_de_gcd$distance > 25, c('city', 'north', 'east', 
    'lat', 'lon', 'loc_frmtd', 'distance')]

write.csv(siem_de_gcd, 'dat/siem_de.csv')

# England
#--------

siem_uk = siem[grep('UK', siem$country), ]
uk_gcd = lapply(gsub('\\(.*', '', siem_uk$city), 
    function(x) geocode(x, reg='uk'))
uk_2nd = filter_prox(uk_gcd, siem_uk)
uk_2nd[[grep("amw", siem_uk$city)]] = geocode(grepr("amw", siem_uk$city))[1, ]
uk_2nd[[grep("West Ham", siem_uk$city)]] = geocode("West Ham, London")
siem_uk_gcd = check_geocodes(siem_ctr=siem_uk, ctr_gcd=uk_2nd)
siem_uk_gcd[siem_uk_gcd$distance > 10, c('city', 'north', 'east', 
    'lat', 'lon', 'loc_frmtd', 'distance')]
write.csv(siem_uk_gcd, 'dat/siem_uk.csv')

# Netherlands
#------------
siem_nl = siem[grep('ether', siem$country), ]
nld_gcd = lapply(gsub('\\(.*', '', siem_nl$city), function(x) geocode(x, reg='lu'))
nld_gcd[sapply(nld_gcd, nrow) > 1]
nld_2nd = filter_prox(nld_gcd, siem_nl)
nld_2nd[sapply(nld_2nd, nrow) > 1]
nld_2nd[sapply(nld_gcd, nrow) > 1]
siem_nl_gcd = check_geocodes(siem_ctr=siem_nl, ctr_gcd=nld_2nd)
write.csv(siem_nl_gcd, 'dat/siem_nl.csv')

# luxemburg
#----------
siem_lu = siem[grep('ux', siem$country), ]
lxb_gcd = lapply(gsub('\\(.*', '', siem_lu$city), function(x) geocode(x, reg='lu'))
lxb_2nd = filter_prox(lxb_gcd, siem_lu)
siem_lu_gcd = check_geocodes(siem_ctr=siem_lu, ctr_gcd=lxb_2nd)
write.csv(siem_lu_gcd, 'dat/siem_lu.csv')

# switzerland
#------------
siem_ch = siem[grep('wi', siem$country), ]
swiss_gcd = lapply(gsub('\\(.*', '', siem_ch$city), function(x) geocode(x, reg='ch'))
sapply(swiss_gcd, nrow) > 1
wiss_2nd, siem_ctr=siem_ch)
siem_ch_gcd = check_geocodes(siem_ctr=siem_ch, ctr_gcd=swiss_gcd)
write.csv(siem_ch_gcd, "dat/siem_swiss.csv", row.names=F)

# belgium
#--------
siem_be = siem[grep('elg', siem$country), ]
belgium_gcd = lapply(gsub('\\(.*', '', siem_be$city), function(x) geocode(x, reg='be'))
sapply(belgium_gcd, nrow) > 1
belgium_2nd = lapply(belgium_gcd, function(x) x[!grepl("\\d|,.*,", x$loc_frmtd),])
belgium_2nd[sapply(belgium_2nd, nrow) < 1] <- belgium_gcd[sapply(belgium_2nd, nrow) < 1]

belgium_3rd = filter_prox(ctr_gcd=belgium_2nd, siem_ctr=siem_be)

siem_be_gcd = check_geocodes(siem_ctr=siem_be, ctr_gcd=belgium_3rd)

write.csv(siem_be_gcd, "dat/siem_belgium.csv", row.names=F)

# france
#-------
siem_fr <- siem[grep('rance', siem$country), ]

# first pass
france_gcd <- lapply(gsub('\\(.*', '', siem_fr$city), function(x) geocode(x, reg='fr'))
failed <- sapply(france_gcd, nrow) > 1

# 2nd pass
# take option closest to original
france_2nd <- france_gcd
for (i in 1:sum(failed)){
    dsts <- sp::spDistsN1(
        as.matrix(france_2nd[failed][[i]][, c('lon', 'lat')]),
        as.matrix(siem_fr[failed, ][i, c('east', 'north')]), longlat=T)
    if (min(dsts) < 2){
        france_2nd[failed][[i]] <- france_2nd[failed][[i]][which.min(dsts), ]
    } else {
        cat(siem_fr$city[failed][[i]], 'failed\n\n')
    }
}
failed_2nd <- sapply(france_2nd, nrow) > 1
france_2nd[failed_2nd] <- lapply(gsub('.*\\(|)', '', siem_fr$city[failed_2nd]), function(x) geocode(x, reg='fr'))

siem_fr_gcd <- data.frame(siem_fr, do.call(rbind, france_2nd))
dstsmat <- sp::spDists(
    as.matrix(siem_fr_gcd[, c('lon', 'lat')]),
    as.matrix(siem_fr_gcd[, c('east', 'north')]), longlat=T)
siem_fr_gcd$distance <- diag(dstsmat)
siem_fr_gcd[siem_fr_gcd$distance > 10, c('city', 'north', 'east', 'loc', 'loc_frmtd', 'lat', 'lon', 'distance')]

# 3rd pass
failed_3rd <- siem_fr_gcd$distance > 100
france_3rd <- france_2nd
france_3rd[failed_3rd] <- lapply(gsub('.*\\(|)', '', siem_fr$city[failed_3rd]), function(x) geocode(x, reg='fr'))
france_3rd[failed_3rd]
siem_fr_gcd[failed_3rd, names(france_3rd[[1]])] <- do.call(rbind, france_3rd[failed_3rd])
dstsmat2 <- sp::spDists(
    as.matrix(siem_fr_gcd[, c('lon', 'lat')]),
    as.matrix(siem_fr_gcd[, c('east', 'north')]), longlat=T)
siem_fr_gcd$distance2 <- diag(dstsmat2)
siem_fr_gcd[siem_fr_gcd$distance2 > 10, c('city', 'north', 'east', 'loc', 'loc_frmtd', 'lat', 'lon', 'distance')]

siem_fr_gcd[siem_fr_gcd$city=="Bagnols (Bagnols (Puy-de-Dôme))", names(france_3rd[[1]])] <- geocode("Bagnols, Puy-de-Dôme, France", reg='fr')
siem_fr_gcd[siem_fr_gcd$city=="Dreux", c('city', 'north', 'east', 'loc', 'loc_frmtd', 'lat', 'lon', 'distance')]
siem_fr_gcd[siem_fr_gcd$city=="Ploermel (Ploërmel)", c('city', 'north', 'east', 'loc', 'loc_frmtd', 'lat', 'lon', 'distance')]
# both of these are correct

write.csv(siem_fr_gcd, 'dat/siem_france.csv', row.names=F)