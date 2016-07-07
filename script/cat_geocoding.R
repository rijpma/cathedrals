rm(list=ls())
options(stringsAsFactors=FALSE)

setwd('~/dropbox/cathedrals/')

library(countrycode)
library(sp)

source('script/cat_functions.r')

hdr <- unlist(read.csv("dat/siem.csv", nrows=1))

siem <- read.csv('dat/siem.csv', skip=2)
siemloc <- c('city', 'north', 'east')
siem$city <- iconv(siem$city, from='macroman', to='utf8')

tld <- structure(countrycode(unique(siem$country), 'country.name', 'iso2c'), names=unique(siem$country))
siem$tld <- tld[siem$country]


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