# prepare datasets

rm(list=ls())
options(stringsAsFactors=FALSE)

setwd("~/dropbox/cathedrals/")
source("script/cat_functions.r")

library("raster") # for spatial splitting of data
library("sf")
library("data.table")
library("stringi")

# rasters for country ids where absent
nld = raster::getData("GADM", country='NLD', level=0)
fra = raster::getData("GADM", country='FRA', level=0)
che = raster::getData("GADM", country='CHE', level=0)
bel = raster::getData("GADM", country='BEL', level=0)
gbr = raster::getData("GADM", country='GBR', level=0)
deu = raster::getData("GADM", country='DEU', level=0)
ita = raster::getData("GADM", country='ITA', level=0)
nweu = rbind(sf::st_as_sf(nld),
    sf::st_as_sf(fra),
    sf::st_as_sf(che),
    sf::st_as_sf(bel),
    sf::st_as_sf(gbr),
    sf::st_as_sf(deu),
    sf::st_as_sf(ita))

# pop data
siem = data.table::fread("dat/siem_long_1500.csv", encoding="UTF-8")

# church construction histories
chr = data.table::fread("dat/checkedchurches_eb_8_2018sep4.csv", 
    header = TRUE, encoding = "UTF-8", colClasses = "character")
chr = chr[, 1:29, with=F]
firstm2col_chr = 14
setnames(chr, 
    old = firstm2col_chr:ncol(chr), 
    new = paste0("y", 1:(ncol(chr) - firstm2col_chr + 1)))

# mixed encoding at some point, fix mistakes
chr[, osmname := gsub("√©", "é", osmname)]
chr[, osmwikipedia := gsub("√©", "é", osmwikipedia)]
# checked below whether there are others

# italian churches
chr_it = data.table::fread("dat/churches_italy_2_2018oct2.csv",
    header = TRUE, encoding = "UTF-8", colClasses = "character")
chr_it = chr_it[, 1:23]
firstm2col_chr_it = 11
setnames(chr_it, 
    old = firstm2col_chr_it:ncol(chr_it), 
    new = paste0("y", 1:(ncol(chr_it) - firstm2col_chr_it + 1)))

chr_it[, ctr := "it"]

# later additions to db
chr_ad = data.table::fread("dat/churches_add_2018nov30.csv",
    header = TRUE, encoding = "UTF-8")
chr_ad = chr_ad[, 1:20]
firstm2col_chr_ad = 11
setnames(chr_ad, 
    old = firstm2col_chr_ad:ncol(chr_ad), 
    new = paste0("y", 1:(ncol(chr_ad) - firstm2col_chr_ad + 1)))

# country codes from nwEU map for additions
ad_sf = st_as_sf(chr_ad[!is.na(lat) & osmlink != ""], 
    coords = c("lon", "lat"), crs = 4326)
chr_ad[!is.na(lat) & osmlink != "",
    iso3 := sf::st_join(ad_sf, nweu, join = sf::st_intersects)$ISO]
chr_ad[, ctr := tolower(substr(iso3, 1, 2))]
chr_ad[, ctr := ifelse(ctr == "gb", "uk", ctr)]

# all church building histories together
chr = rbindlist(list(chr, chr_it, chr_ad), fill = TRUE)

# check encoding
if (!all(sapply(chr, validUTF8))){
    warning("Encoding issue in ", deparse(substitute(chr)))
}
if (any(sapply(chr, function(x) any(grep(x, pattern = "√"))))){
    warning("Encoding issue in ", deparse(substitute(chr)))
}

# manual fixes

# middelburg should have one id
chr[city=="Middelburg", osmid := osmid[1]]

# halle should be two churches with unique osmids
chr[osmid == "217546683", osmid := paste0(osmid, rep(c("a", "b"), each = 6))]

# duplicate
chr = chr[!(osmid == "1832902" & city == "Carrara")] # is in massa

 # not enough inhab before 1500
chr = chr[osmid != "91391781" | is.na(osmid)]

# Konstanz Dreifaltichkeit should not have Vlaardingen Grote Kerk osmid
chr[city == "Konstanz" & osmid == "273129012", osmid := "66771121"]

# ¯\_(ツ)_/¯
chr[osmid == '\r', osmid := ""]

# extend lblink and rmlink to duplicates
chr[osmid != "", 
    lblink := lblink[1][lblink != "" & !is.na(lblink)], 
    by = osmid]
chr[osmid != "", 
    rmlink := rmlink[1][rmlink != "" & !is.na(rmlink)], 
    by = osmid]

# duplicates: entered twice identically (both correct)
dpls = chr[osmid != "", .N, by = osmid][N != 6, osmid]
# second one always better, so keep those
chr[, tokeep := TRUE]
chr[osmid %in% dpls, tokeep := rep(c(FALSE, TRUE), each = 6), by = osmid]
chr = chr[tokeep == TRUE]
chr[, tokeep := NULL]

# check: no duplicates and all 6 rows?
if (! all(chr[osmid != "", table(osmid)] == 6)){
    warning("non-6 row entries in file")
}

# various typos
chr[osmid == "2322752", "lat"][1] = "46.4349" # cluny
chr[osmid == "66636479", "osmlink"][1] <- "http://www.openstreetmap.org/way/66636479"
chr[osmid == "66636479", "osmwikipedia"][1] <- "https://fr.wikipedia.org/wiki/Cathédrale_Saint-Pierre_de_Condom"

chr[city == "reading", city := "Reading"]
chr[city == "norwich", city := "Norwich"]
chr[city == "bernburg", city:= "Bernburg"]
chr[city == "bethune", city:= "Bethune"]
chr[city == "biberach", city:= "Biberach"]
chr[city == "colchester", city:= "Colchester"]
chr[city == "diest", city:= "Diest"]
chr[city == "dole", city:= "Dole"]
chr[city == "dueren", city:= "Dueren"]
chr[city == "dundee", city:= "Dundee"]
chr[city == "emden", city:= "Emden"]
chr[city == "gap", city:= "Gap"]
chr[city == "geraardsbergen", city:= "Geraardsbergen"]
chr[city == "grasse", city:= "Grasse"]
chr[city == "husum", city:= "Husum"]
chr[city == "joigny", city:= "Joigny"]
chr[city == "lauingen", city:= "Lauingen"]
chr[city == "leeuwarden", city:= "Leeuwarden"]
chr[city == "mettmann", city:= "Mettmann"]
chr[city == "montargis", city:= "Montargis"]
chr[city == "morlaix", city:= "Morlaix"]
chr[city == "northampton", city:= "Northampton"]
chr[city == "roanne", city:= "Roanne"]
chr[city == "rotherham", city:= "Rotherham"]
chr[city == "siegen", city:= "Siegen"]
chr[city == "st etienne", city:= "St Etienne"]
chr[city == "temse", city:= "Temse"]
chr[city == "zierikzee", city:= "Zierikzee"]
chr[city == "zwickau", city:= "Zwickau"]

# border city, choose to prevent join duplicates later
chr[city == "Konstanz (Drusomagus)" & ctr == "ch", ctr := "de"]

# more accurate coordinates
chr[osmid == "293268847", `:=` (lon = "4.213874", lat = "51.12405")] # temse
chr[osmid == "2322752",   `:=` (lon = "4.659411", lat = "46.43467")] # cluny (substituting with way/51548194)
chr[osmid == "136677965", `:=` (lon = "2.638217", lat = "50.53216")] # bethune
chr[osmid == "37128921",  `:=` (lon = "5.49472",   lat =  "47.0925")] # Dole
chr[osmid == "90885376",  `:=` (lon = "6.078113", lat =  "44.5582")] # Gap
chr[osmid == "63038103",  `:=` (lon = "3.395135", lat =  "47.9831")] # Joingy
chr[osmid == "124614369", `:=` (lon = "4.071535", lat =  "46.0397")] # Roanne
chr[osmid == "147917758", `:=` (lon = "10.53739", lat =  "52.16192")] # Wolfenbuettel
chr[osmid == "34667153",  `:=` (lon = "12.49508", lat =  "50.7179")] # Zwickau 
# remainder are good enough

# fix height and year typos
chr[osmid == "32530870" & surface == "height", y6 := "15.8"]
chr[osmid == "69972010" & surface == "year", y4 := "1000"]
chr[osmid == "136200148" & surface == "year", y10 := "1450"]
chr[osmid == "136677965" & surface == "year", y1 := "700"]
chr[osmid == "94434589" & surface == "year", y6 := "1050"]

# more checks
yearvrbs = grep("y\\d", names(chr))

if (any(unlist(
    chr[surface %in% c("height", "surface", "year") ][, 
        lapply(.SD, function(x) anyNA(as.numeric(x[!(is.na(x) | x == "")]))), 
        .SDcols = yearvrbs]))){
    warning("Values for height not convertable to numeric")
}
if (! any(unlist(
    chr[surface == "year", 
        lapply(.SD, function(x) all(grepl("^\\d{3,4}$", x[!(is.na(x) | x =="")]))), 
        .SDcols = yearvrbs]))){
    warning("Values for year not 3-4 digits")
}
if (! any(unlist(
    chr[surface == "year", 
        lapply(.SD, function(x) all(grepl("^1\\d{3}$|^[2-9]\\d{2}$|100", x[!(is.na(x) | x =="")]))), 
        .SDcols = yearvrbs]))){
    warning("Values for year not of form 1XXX or 2+XX or 100")
}
if (any(
    (chr[surface == "year", lapply(.SD, as.numeric), .SDcols = yearvrbs][, -1] - 
        chr[surface == "year", 
            lapply(.SD, as.numeric), 
            .SDcols = yearvrbs][, 
                -length(yearvrbs), with = F]) < 0, 
        na.rm = TRUE)){
    warning("years not chronological: year[n + 1] < year[n]")
}
if (any(unlist(
    chr[surface == "surface", 
        lapply(.SD, function(x) any(as.numeric(x[!(is.na(x) | x == "")]) > 1e4)), 
        .SDcols = yearvrbs]))){
    warning("Values for surface too high")
}
if (any(unlist(
    chr[surface == "height", 
        lapply(.SD, function(x) any(as.numeric(x[!(is.na(x) | x == "")]) > 1e3)), 
        .SDcols = yearvrbs]))){
    warning("Values for surface too high")
}
if (chr[!is.na(ctr) & ctr != "", list(nctr = uniqueN(ctr)), by = city ][ 
        , all(nctr != 1)]){
    warning("City coded in different countries")
}

chrlist = recombine_churches(churches = chr, guesses = NULL, firstm2col = 14)

# static church data
statobs = do.call(rbind, lapply(chrlist, `[[`, 'static')) 
statobs = data.table::as.data.table(statobs)

### fix statobs city names to match those in siem
# seemingly no problems with Italian cities
fixes = lapply(
    gsub('-', ' ', setdiff(statobs$city, siem$city)), 
    function(x) unique(siem$city)[grep(x, gsub('-', ' ', unique(siem$city)))])
fixes[sapply(fixes, length) == 0] = NA
fixes = unlist(fixes)
names(fixes) = setdiff(statobs$city, siem$city)

statobs[, city2 := fixes[match(city, names(fixes))]]
statobs[!is.na(city2), city := city2]
statobs[!is.na(city2), city := city2][, city2 := NULL]

if (!all(unique(statobs$city) %in% siem$city)){
    warning("names mismatch between statobs and siem")
}
if (nrow(statobs[!city %in% siem$city, ]) > 0 
    | nrow(siem[, .SD[1], by = city ][ 
        city %in% unique(statobs$city) ][ duplicated(city)]) > 0){
    warning("different city names in siem compared to statobs")
}
# check for cities spelt in multiple ways
doubles = c("Strasbourg", "Strasbourg (Strassburg)", 
            "St Omer", "St Omer (Saint-Omer (Pas-de-Calais))", 
            "Chalons-sur-Marne", "Chalons-sur-Marne (Châlons-en-Champagne)")
if (!all.equal(target = statobs[city %in% doubles, unique(city)],
               current = siem[city %in% doubles, unique(city)])){
    warning("Spelling mismatch ", deparse(substitute(chr)))
}

statobs[, lat := as.numeric(lat)]
statobs[, lon := as.numeric(lon)]

# country splits
# --------------
# north/south
statobs[, ctr2 := ctr]
statobs[lat >  46.0 & ctr == "fr", ctr2 := "fr_north"]
statobs[lat <= 46.0 & ctr == "fr", ctr2 := "fr_south"]
statobs[lat >  53.0 & ctr == "uk", ctr2 := "uk_north"]
statobs[lat <= 53.0 & ctr == "uk", ctr2 := "uk_south"]
statobs[lat >  50.5 & ctr == "de", ctr2 := "de_north"]
statobs[lat <= 50.5 & ctr == "de", ctr2 := "de_south"]

# select southern Italy based on today's provinces
ita = raster::getData("GADM", country='ITA', level=1)
# Only Chiesa di San Vitale, Como and St Peter in Rome are missed, 
# both actually not in Italy and can be circumvented by assigning
# south first, and then assiging "rest" rest north
ita_south = sf::st_as_sf(ita[ita$NAME_1 %in% c("Abruzzo", "Molise",
    "Campania", "Apulia", "Basilicata", "Calabria", "Sicily", "Sardegna"), ])

statob_sf = sf::st_as_sf(statobs, coords = c("lon", "lat"), crs = 4326)
statobs_in_ita_south = sf::st_join(statob_sf, ita_south, join = sf::st_intersects, left = F)
# planar warning can be ignored

statobs[osmid %in% statobs_in_ita_south$osmid, ctr2 := "it_south"]
statobs[ctr2 == "it", ctr2 := "it_north"]

# country splits natural
deu_lander = raster::getData("GADM", country = 'DEU', level = 1)
deu_lander$region[grep("Bay|Thü|Sach|Bra|Ber|Meck", deu_lander$NAME_1)] = "de_ne"
deu_lander$region[is.na(deu_lander$region)] = "de_sw"

statobs_in_de = sf::st_intersects(statob_sf, st_as_sf(deu_lander))
statobs_in_de[sapply(statobs_in_de, length) == 0] = NA
statobs$ctr3 = deu_lander$region[unlist(statobs_in_de)]

statobs[ctr=="ch", ctr3 := "de_sw"]
statobs[osmid == "180797850", ctr3 := "de_sw"] # on border

lat_york = siem[city == "York", lat[1]] + km2lat(5)
lon_york = siem[city == "York", lon[1]] - km2lon(5, siem[city == "York", lat[1]])
lat_bris = siem[city == "Bristol", lat[1]] + km2lat(5)
lon_bris = siem[city == "Bristol", lon[1]] - km2lon(5, siem[city=="Bristol", lat[1]])
york2bristol = lm(lat ~ lon, 
    data = data.frame(lat = c(lat_bris, lat_york), lon = c(lon_bris, lon_york)))
north = predict(york2bristol, newdata = statobs) < statobs[, lat]
statobs[ctr == 'uk' & north == TRUE, ctr3 := "uk_nw"]
statobs[ctr == 'uk' & north == FALSE, ctr3 := "uk_se"]

statobs[ctr=="nl" | ctr == "be" | ctr == "lu", ctr3 := "lc"]
statobs[lat >  46.0 & ctr == "fr", ctr3 := "fr_north"]
statobs[lat <= 46.0 & ctr == "fr", ctr3 := "fr_south"]

statobs[ctr == "it", ctr3 := ctr2]

# dynamic church data (building histories)
dynobs = to_dynobs(churchlist = chrlist)

# correct date heaping
# --------------------
dynobs[, year_lead := data.table::shift(year, type='lead', fill=Inf), by=osmid]
dynobs[, year_lag := data.table::shift(year, type='lag', fill=-Inf), by=osmid]
dynobs[, dyear := data.table::shift(year, type='lead') - year, by=osmid]

dynobs[, heap100 := (year %% 100 == 0) | (year %% 100 == 1)] # 1 because end=1200 start=1201
dynobs[, heap20 := ((year - 20) %% 100 == 0) | ((year + 20) %% 100 == 0) ]
dynobs[, heap25 := ((year - 25) %% 100 == 0) | ((year + 25) %% 100 == 0) | ((year - 50) %% 100 == 0) ]
dynobs[, heap10 := (year %% 10 == 0) & ((heap100 + heap20 + heap25) == 0)]

dynobs[heap100 == TRUE, sdev := 30]
dynobs[heap20 == TRUE, sdev := 12]
dynobs[heap25 == TRUE, sdev := 15]

dynobs[heap100 == TRUE, delta := 50]
dynobs[heap20 == TRUE, delta := 10]
dynobs[heap25 == TRUE, delta := 12]
dynobs[heap10 == TRUE, delta := 5]

dynobs[, heap := FALSE]
dynobs[heap100 == TRUE | heap10 == TRUE | heap25 == TRUE | heap20 == TRUE, heap := TRUE]

dynobs[, year := as.numeric(year)] # prevent assign complaints

set.seed(121314)
M = 9
for (j in 1:M){
    dynobs[, year_crc := year]

    # heaping on 20, 25, and 100 using truncated normal
    rsmpl = rtnorm(n = nrow(dynobs[!is.na(sdev)]), 
                   mean = dynobs[!is.na(sdev), year],
                   sd = dynobs[!is.na(sdev), sdev],
                   min = dynobs[!is.na(sdev), year_lag] + 1,
                   max = dynobs[!is.na(sdev), year_lead] - 1)
    dynobs[!is.na(sdev), year_crc := round(rsmpl)]

    # uniform double resampling to prevent new heaping on 5
    # still causes slight underheaping at 5 because 2/3 
    # of times neighbour is twenty years away
    n = nrow(dynobs[heap10 == TRUE])
    dynobs[heap10 == TRUE, splt10 := rbinom(n, size=1, prob=0.5)]
    rsmpl4 = runif(n = sum(dynobs$splt10 == 1, na.rm=TRUE),
                   min = dynobs[heap10 == TRUE & splt10 == 1, year - 4],
                   max = dynobs[heap10 == TRUE & splt10 == 1, year + 4])
    rsmpl5 = runif(n = sum(dynobs$splt10 == 0, na.rm=TRUE),
                   min = dynobs[heap10 == TRUE & splt10 == 0, year - 5],
                   max = dynobs[heap10 == TRUE & splt10 == 0, year + 5])

    dynobs[heap10 == TRUE & splt10 == 1, year_crc := round(rsmpl4)]
    dynobs[heap10 == TRUE & splt10 == 0, year_crc := round(rsmpl5)]

    # de-heaping can swap building order, check and correct:
    dynobs[, year_lead_crc := data.table::shift(year_crc, type='lead'), by=osmid]
    dynobs[, year_lag_crc := data.table::shift(year_crc, type='lag'), by=osmid]

    cat("Original: N swapped: ", sum(dynobs[, list(delta = diff(year_crc) < 0), by=osmid][, delta]), " - ")
    cat("N same: ",    sum(dynobs[, list(delta = diff(year_crc) == 0), by=osmid][, delta]), " - ")

    dynobs[year_lead_crc - year_crc <= 0, year_crc := year_lead_crc - 1, by=osmid]
    dynobs[year_crc - year_lag_crc <= 0, year_crc := year_lag_crc + 1, by=osmid]
    # because that is the closest value to the original imputed value that is a non-swap

    cat("After fix: N swapped: ", sum(dynobs[, list(delta = diff(year_crc) < 0), by=osmid][, delta]), " - ")
    cat("N same: ",    sum(dynobs[, list(delta = diff(year_crc) == 0), by=osmid][, delta]), "\n")

    setnames(dynobs, 'year_crc', paste0("year_crc", j))
}

# annual series from building histories
fullobs = to_annual_obs(dyn = dynobs)

if ((length(fullobs[, .N, by = osmid][, unique(N)]) != 1) | 
    (length(fullobs[, .N, by = year][, unique(N)]) != 1)){
    warning("unbalanced dataset")
}

# annual series for each imputation
for (j in 1:M){
    dynobs_rs = data.table::copy(dynobs)

    dynobs_rs[, year := dynobs[, paste0("year_crc", j), with = FALSE]]

    # these do nothing anymore because de-ordering is now fixed in dynobs deheaping
    dynobs_rs[, prb := duplicated(year, fromLast = TRUE), by = osmid]
    dynobs_rs[prb == TRUE & data.table::shift(year) != year, year := year - 1]

    # fix the denominator to annualise in interpolations to the imputed one
    # probably better addressed in to_annual_obs()
    dynobs_rs[, phaselength := c(diff(year)[1], diff(year)), by = osmid]
    
    tomerge = to_annual_obs(
        dyn = dynobs_rs[, .SD, .SDcols = !grepl("year_crc", names(dynobs_rs))])

    fullobs = merge(
        fullobs, 
        tomerge[, list(osmid, year, im2_ann, im3_ann, im2_cml, im3_cml)], 
        all.x = TRUE, all.y = FALSE, 
        by = c("osmid", "year"), 
        suffixes = c("", j))

    cat("Imp", j, ' - ', "dataset dim: ", dim(fullobs), '\n')
    rm("tomerge")
    rm("dynobs_rs")
}

sumcheck = fullobs[, 
    lapply(.SD, sum, na.rm = TRUE), 
    .SDcols = grep("im3_ann\\d|im3_ann$", names(fullobs)),
    by = osmid]
imps = as.list(sumcheck[, -"osmid"])
if(!all(sapply(imps, all.equal, current = sumcheck$im3_ann))){
    warning("differences in original and imputed by-church totals")
}

# add maintenance cost (unused)
im3crc = fullobs[, .SD, .SDcols = grep('im3_ann\\d', names(fullobs))] +
    fullobs[, .SD * 0.005, .SDcols = grep('im3_cml\\d', names(fullobs))]
im2crc = fullobs[, .SD, .SDcols = grep('im2_ann\\d', names(fullobs))] +
    fullobs[, .SD * 0.005, .SDcols = grep('im2_cml\\d', names(fullobs))]
fullobs[, paste0("im3_ann_cmc", 1:M) := im3crc]
fullobs[, paste0("im2_ann_cmc", 1:M) := im2crc]
fullobs[, im3_ann_cmc := im3_ann + im3_cml * 0.005]
fullobs[, im2_ann_cmc := im2_ann + im2_cml * 0.005]

fullobs[, decade := (trunc((year - 1) / 20) + 1) * 20] # so 1500 = 1481-1500

# city-level aggregates
citobs = to_city_obs(statobs=statobs, fullobs=fullobs)

# annual series with static + spatial attributes
fullobs_sp = merge(fullobs, statobs, by="osmid", all=TRUE)

if (any(is.na(fullobs_sp$year))){
    warning("missing years")
}

if ((length(fullobs_sp[, .N, by = osmid][, unique(N)]) != 1) | 
    (length(fullobs_sp[, .N, by = year][, unique(N)]) != 1)){
    warning("unbalanced dataset")
}

data.table::fwrite(dynobs, "dat/dynobs.csv")
data.table::fwrite(statobs, "dat/statobs.csv")
data.table::fwrite(citobs, "dat/citobs.csv")

# todo: replace with fwrite once binaries on cran
# this takes a few minutes
outfile = gzfile("dat/fullobs_sp.csv.gz", 'w')
write.csv(fullobs_sp, outfile)
close(outfile)

# this can give issues:
unique(stringi::stri_enc_mark(siem$city))
unique(stringi::stri_enc_mark(citobs$city))

# m3 per region per 20y
out = fullobs_sp[data.table::between(year, 700, 1500), list(im2 = sum(im2_ann, na.rm=TRUE), 
                                    im3 = sum(im3_ann, na.rm=T), 
                                    im2_tot = max(im2_cml, na.rm=T),
                                    im3_tot = max(im3_cml, na.rm=T)), 
    by=list(ctr, city, decade, category)][order(ctr, city, category, decade), ]
# data.table::fwrite(out, "dat/fullobs_sp_20y.csv")
