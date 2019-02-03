rm(list=ls())
options(stringsAsFactors=FALSE)

setwd("~/dropbox/cathedrals/")
source("script/cat_functions.r")

library("data.table")
library("stringi")
library("raster") # for spatial splitting of data
library("sf")

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

siem = data.table::fread("dat/siem_long_1500.csv", encoding="UTF-8")

chr = data.table::fread("dat/checkedchurches_eb_8_2018sep4.csv", 
    header = T, encoding = "UTF-8", colClasses = "character")
chr = chr[, 1:29, with=F]
firstm2col_chr = 14
setnames(chr, firstm2col_chr:ncol(chr), paste0("y", 1:(ncol(chr) - firstm2col_chr + 1)))

# chr[, osmwikipedia := iconv(osmwikipedia, from='macroman', to='utf8')]
# chr[, osmname := iconv(osmname, from='macroman', to='utf8')]
# chr[, osmlink := iconv(osmlink, from='macroman', to='utf8')]
# chr[, city := iconv(city, from='macroman', to='utf8')]

# because there was mixed encoding at some point, fix mistakes
chr[, osmname := gsub("√©", "é", osmname)]
chr[, osmwikipedia := gsub("√©", "é", osmwikipedia)]
# checked below whether there are others

chr_it = data.table::fread("dat/churches_italy_2_2018oct2.csv",
    header = T, encoding = "UTF-8", colClasses = "character")
chr_it = chr_it[, 1:23]
firstm2col_chr_it = 11
setnames(chr_it, firstm2col_chr_it:ncol(chr_it), paste0("y", 1:(ncol(chr_it) - firstm2col_chr_it + 1)))

chr_it[, ctr := "it"]

chr_ad = data.table::fread("dat/churches_add_2018nov30.csv",
    header = T, encoding = "UTF-8")
chr_ad = chr_ad[, 1:20]
firstm2col_chr_ad = 11
setnames(chr_ad, firstm2col_chr_ad:ncol(chr_ad), paste0("y", 1:(ncol(chr_ad) - firstm2col_chr_ad + 1)))

ad_sf = st_as_sf(chr_ad[osmlink != "" & !is.na(lat)], coords = c("lon", "lat"), crs = 4326)
chr_ad[osmlink != "" & !is.na(lat),
    iso3 := sf::st_join(ad_sf, nweu, join = sf::st_intersects)$ISO]
unique(chr_ad[osmlink != "", .(city, iso3)][order(iso3)])
chr_ad[, ctr := tolower(substr(iso3, 1, 2))]
chr_ad[, ctr := ifelse(ctr == "gb", "uk", ctr)]

chr_ad[osmlink != "" & is.na(lat), .(city, osmid, osmlink)]

chr = rbindlist(list(chr, chr_it, chr_ad), fill = T)

# maybe the fact that the V\\d+ don't match up doesn't matter, 
# let's find out
# otherwise make some sort of similar name and make sure recombine is fixed

# check 
if (!all(sapply(chr, validUTF8))){
    warning("Encoding issue in ", deparse(substitute(chr)))
}
if (any(sapply(chr, function(x) any(grep(x, pattern = "√"))))){
    warning("Encoding issue in ", deparse(substitute(chr)))
}
# gsub("√∂", "ö"
# gsub("√º", "ü"
# gsub("√§", "ä"
# gsub("√©", "é"
stopifnot(any(grepl("ö", chr$osmname)),
    any(grepl("ü", chr$osmname)),
    any(grepl("ä", chr$osmname)),
    any(grepl("è", chr$osmname)),
    any(grepl("é", chr$osmname)))

# manual fixes
# middelburg should have one id
chr[city=="Middelburg", osmid := osmid[1]]
# halle should be two churches with unique osmids
chr[osmid == "217546683", osmid := paste0(osmid, rep(c("a", "b"), each = 6))]

# drop duplicated churches in nearby italian towns
# now redundant
chr = chr[!(osmid == "166506824" & city == "Calascibetta") & 
    !(osmid == "201681493" & city == "Maddaloni") & 
    !(osmid == "275036771" & city == "Cava de' Tirreni") & 
    !(osmid == "93662475" & city == "Cava de' Tirreni") & 
    !(osmid == "330535041" & city == "Torre del Greco") &  # check with eb
    !(osmid == "315026102" & city == "Portici")]

# still necessary
chr = chr[!(osmid == "1832902" & city == "Carrara")] # is in massa
chr = chr[osmid != "91391781" | is.na(osmid)] # not enough inhab before 1500
                                              # keep NA just to be safe

# Konstanz Dreifaltichkeitskirche should not have Vlaardingen Grote Kerk osmid
chr[city == "Konstanz" & osmid == "273129012", osmid := "66771121"]

# ¯\_(ツ)_/¯
chr[osmid == '\r', osmid := ""]

# fix more duplicate osmids
# extend lblink and rmlink to duplicates
chr[osmid != "", lblink := lblink[1][lblink != "" & !is.na(lblink)], by = osmid]
chr[osmid != "", rmlink := rmlink[1][rmlink != "" & !is.na(rmlink)], by = osmid]

# duplicate list
dpls = chr[osmid != "", .N, by = osmid][N != 6, osmid]
# second one always better, so keep those
chr[, tokeep := TRUE]
chr[osmid %in% dpls, tokeep := rep(c(FALSE, TRUE), each = 6), by = osmid]
chr = chr[tokeep == TRUE]
chr[, tokeep := NULL]

stopifnot(
    all(chr[osmid != "", table(osmid)] == 6)
)

duplids = names(table(chr$osmid[chr$osmid!='']))[table(chr$osmid[chr$osmid!='']) < 6]
match(duplids, chr$osmid)
chr[osmid %in% duplids, ]

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

# much more to be done here now...
# chr[grep("^[a-z]", city), unique(city)]

# choose one, I would suggest the first
unique(chr[city %in% c("Chalons-sur-Marne", "Chalons-sur-Marne (Châlons-en-Champagne)"), .(city, ctr, osmid, osmname, lat, lon)])
unique(chr[city %in% c("St Omer", "St Omer (Saint-Omer (Pas-de-Calais))"), .(city, ctr, osmid, osmname, lat, lon)])
unique(chr[city %in% c("Strasbourg", "Strasbourg (Strassburg)"), .(city, ctr, osmid, osmname, lat, lon)])

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
# remainder are broken, but coords good enough
# chr[!is.na(lat) & lat != "" & nchar(lat) < 7, .(city, osmid, osmname, lat, lon)]

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
        chr[surface == "year", lapply(.SD, as.numeric), .SDcols = yearvrbs][, -length(yearvrbs), with = F]) < 0, 
        na.rm = T)){
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

# Italian churches look ok

chrlist = recombine_churches(churches = chr, guesses = NULL, firstm2col = 14)

statobs = do.call(rbind, lapply(chrlist, `[[`, 'static')) 
statobs = data.table::as.data.table(statobs)

### fix statobs city names to match siem
# seemingly no problems with Italian cities
fixes = lapply(gsub('-', ' ', setdiff(statobs$city, siem$city)), function(x) unique(siem$city)[grep(x, gsub('-', ' ', unique(siem$city)))])
fixes[sapply(fixes, length) == 0] = NA
fixes = unlist(fixes)
names(fixes) = setdiff(statobs$city, siem$city)

unique(fixes[match(statobs$city, names(fixes))])
statobs[, city2:=fixes[match(city, names(fixes))]]
statobs[!is.na(city2), city:=city2]
unique(statobs[!is.na(city2), list(city, city2)])
statobs[!is.na(city2), city:=city2][, city2:=NULL]

setdiff(statobs$city, siem$city)

if (!all(unique(statobs$city) %in% siem$city)){
    warning("names mismatch between statobs and siem")
}

if (nrow(statobs[!city %in% siem$city, ]) > 0 |
    nrow(siem[, .SD[1], by = city ][ city %in% unique(statobs$city) ][ duplicated(city)]) > 0){
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
# ---------------
statobs[, ctr2 := ctr]
statobs[lat >  46.0 & ctr == "fr", ctr2 := "fr_north"]
statobs[lat <= 46.0 & ctr == "fr", ctr2 := "fr_south"]
statobs[lat >  53.0 & ctr == "uk", ctr2 := "uk_north"]
statobs[lat <= 53.0 & ctr == "uk", ctr2 := "uk_south"]
statobs[lat >  50.5 & ctr == "de", ctr2 := "de_north"]
statobs[lat <= 50.5 & ctr == "de", ctr2 := "de_south"]

# select southern Italy based on today's provinces
ita = raster::getData("GADM", country='ITA', level=1)
# locates perfectly: only Chiesa di San Vitale, Como and St Peter in Rome are
# exluded, both actually not in Italy and can be circumvented by  selecting
# south first, calling rest north
ita_south = sf::st_as_sf(ita[ita$NAME_1 %in% c("Abruzzo", "Molise",
    "Campania", "Apulia", "Basilicata", "Calabria", "Sicily", "Sardegna"), ])

statob_sf = sf::st_as_sf(statobs, coords = c("lon", "lat"), crs = 4326)
statobs_in_ita_south = sf::st_join(statob_sf, ita_south, join = sf::st_intersects, left = F)

# planar warning can be ignored it seems
statobs[osmid %in% statobs_in_ita_south$osmid, ctr2 := "it_south"]
statobs[ctr2 == "it", ctr2 := "it_north"]
table(statobs$ctr2)

# country splits natural
deu = raster::getData("GADM", country = 'DEU', level = 1)
deu$region[grep("Bay|Thü|Sach|Bra|Ber|Meck", deu$NAME_1)] = "de_ne"
deu$region[is.na(deu$region)] = "de_sw"

statobs_in_de = sf::st_intersects(statob_sf, st_as_sf(deu))
statobs_in_de[sapply(statobs_in_de, length) == 0] = NA
statobs$ctr3 = deu$region[unlist(statobs_in_de)]

statobs[ctr=="ch", ctr3 := "de_sw"]

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
table(statobs$ctr3)

out = merge(statobs, unique(siem[, .(city, country)]), by = 'city', all.y = T)
writexl::write_xlsx(
    out[, sum(!is.na(osmid)), by = .(city, country)][order(country, city)],
    "excels/cityoverview.xlsx")

dynobs = to_dynobs(churchlist=chrlist)

# correct date heaping
# --------------------
dynobs[, year_lead := data.table::shift(year, type='lead', fill=Inf), by=osmid]
dynobs[, year_lag := data.table::shift(year, type='lag', fill=-Inf), by=osmid]
dynobs[, dyear := data.table::shift(year, type='lead') - year, by=osmid]

dynobs[, heap100 := (year %% 100 == 0) | (year %% 100 == 1)]
dynobs[, heap20 := ((year - 20) %% 100 == 0) | ((year + 20) %% 100 == 0) ]
dynobs[, heap25 := ((year - 25) %% 100 == 0) | ((year + 25) %% 100 == 0) | ((year - 50) %% 100 == 0) ]
dynobs[, heap10 := (year %% 10 == 0) & (heap100 + heap20 + heap25) == 0]

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

    rsmpl = rtnorm(n = nrow(dynobs[!is.na(sdev)]), 
               mean = dynobs[!is.na(sdev), year],
               sd = dynobs[!is.na(sdev), sdev],
               min = dynobs[!is.na(sdev), year_lag] + 1,
               max = dynobs[!is.na(sdev), year_lead] - 1)
    dynobs[!is.na(sdev), year_crc := round(rsmpl)]

    # uniform double resampling to prevent new heaping on 5
    # note that this currently causes underheaping at 5
    # because 2/3 of times neighbour is twenty years away
    # maybe simple approach preferable because smaller pbolem?
    n = nrow(dynobs[heap10 == TRUE])
    dynobs[heap10 == TRUE, splt10 := rbinom(n, size=1, prob=0.5)]
    # rsmpl = runif(n = n,
    #                min = dynobs[heap10 == TRUE, year - 4.75],
    #                max = dynobs[heap10 == TRUE, year + 4.75])
    rsmpl4 = runif(n = sum(dynobs$splt10 == 1, na.rm=T),
                   min = dynobs[heap10 == TRUE & splt10 == 1, year - 4],
                   max = dynobs[heap10 == TRUE & splt10 == 1, year + 4])
    rsmpl5 = runif(n = sum(dynobs$splt10 == 0, na.rm=T),
                   min = dynobs[heap10 == TRUE, ][splt10 == 0, year - 5],
                   max = dynobs[heap10 == TRUE, ][splt10 == 0, year + 5])

    # dynobs[heap10 == TRUE, year_crc := round(rsmpl)]
    dynobs[heap10 == TRUE & splt10 == 1, year_crc := round(rsmpl4)]
    dynobs[heap10 == TRUE & splt10 == 0, year_crc := round(rsmpl5)]

    # swap if diff(year) < 0 ?
    # or mean to original observation?
    dynobs[, year_lead_crc := data.table::shift(year_crc, type='lead'), by=osmid]
    dynobs[, year_lag_crc := data.table::shift(year_crc, type='lag'), by=osmid]

    cat("Original: N swapped: ", sum(dynobs[, list(delta = diff(year_crc) < 0), by=osmid][, delta]), " - ")
    cat("N same: ",    sum(dynobs[, list(delta = diff(year_crc) == 0), by=osmid][, delta]), " - ")

    dynobs[(((year_lead_crc - year_crc) < 0) | ((year_crc - year_lag_crc) < 0)), year_crc := round((year + year_crc) / 2), by=osmid]
    dynobs[duplicated(paste0(osmid, year_crc)), year_crc := year_crc + 1]
    dynobs[duplicated(paste0(osmid, year_crc), fromLast=TRUE), year_crc := year_crc - 1]

    cat("After fix: N swapped: ", sum(dynobs[, list(delta = diff(year_crc) < 0), by=osmid][, delta]), " - ")
    cat("N same: ",    sum(dynobs[, list(delta = diff(year_crc) == 0), by=osmid][, delta]), "\n")

    setnames(dynobs, 'year_crc', paste0("year_crc", j))
}

fullobs = to_annual_obs(dyn = dynobs)

if ((length(fullobs[, .N, by = osmid][, unique(N)]) != 1) | 
    (length(fullobs[, .N, by = year][, unique(N)]) != 1)){
    warning("unbalanced dataset")
}

impvarmat = matrix(NA, length(unique(fullobs$year)), M)
for (j in 1:M){
    dynobs_rs = data.table::copy(dynobs)
    dynobs_rs[, year := dynobs[, paste0("year_crc", j), with=F]]
    # fullobslist[[j]] = to_annual_obs(dynobs_rs, chrlist)    
    dynobs_rs[, prb := duplicated(year, fromLast=T), by=osmid]
    dynobs_rs[prb == TRUE & data.table::shift(year) != year, year := year - 1]

    
    tomerge = to_annual_obs(
        dyn = dynobs_rs[, .SD, .SDcols = ! like(names(dynobs_rs), "year_crc")])

    # for imputations var plot do
    # impvarmat[, j] = tomerge[, .(im3_ann = sum(im3_ann, na.rm = T)), by = year]$im3_ann

    # else do
    fullobs = merge(fullobs, tomerge[, list(osmid, year, im2_ann, im3_ann, im2_cml, im3_cml)], all.x=T, all.y=F, by=c("osmid", "year"), suffixes=c("", j))

    cat(j, ' - ', dim(fullobs), '\n')
    rm("tomerge")
    rm("dynobs_rs")
}

# pdf("figs/imputations_var.pdf")
# yrdex = between(unique(fullobs$year), 700, 1500)
# matplot(x = 700:1500, y = impvarmat[yrdex, ], 
#     type = 'l', lty = 1, col = gray(0.5, alpha = 0.1))
# lines(x = 700:1500, y = apply(impvarmat[yrdex, ], 1, median), col = 1)

# matplot(
#     x = 700:1500, 
#     y = t(apply(impvarmat[yrdex, ], 1, quantile, c(0.1, 0.5, 0.9))), 
#     type = 'l', lty = c(2, 1, 2), col = c("pink", "red", "pink"))
# dev.off()

im3crc = fullobs[, .SD, .SDcols = grep('im3_ann\\d', names(fullobs))] +
    fullobs[, .SD * 0.005, .SDcols = grep('im3_cml\\d', names(fullobs))]
im2crc = fullobs[, .SD, .SDcols = grep('im2_ann\\d', names(fullobs))] +
    fullobs[, .SD * 0.005, .SDcols = grep('im2_cml\\d', names(fullobs))]
fullobs[, paste0("im3_ann_cmc", 1:M) := im3crc]
fullobs[, paste0("im2_ann_cmc", 1:M) := im2crc]
fullobs[, im3_ann_cmc := im3_ann + im3_cml * 0.005]
fullobs[, im2_ann_cmc := im2_ann + im2_cml * 0.005]

# do the imputations on dynobs on e.g. duration itself?
# still no city level dataset or panel dataset...
# do here, get standard errors, plug back in ?

fullobs[, decade := (trunc((year - 1) / 20) + 1) * 20] # so 1500 = 1481-1500

citobs = to_city_obs(statobs=statobs, fullobs=fullobs)

dim(fullobs)
fullobs_sp = merge(fullobs, statobs, by="osmid", all=T)
dim(fullobs_sp)
dynobs[fullobs_sp[is.na(year), osmid], ] # are one obs churches in 700
fullobs_sp = fullobs_sp[!is.na(year), ]

if ((length(fullobs_sp[, .N, by = osmid][, unique(N)]) != 1) | 
    (length(fullobs_sp[, .N, by = year][, unique(N)]) != 1)){
    warning("unbalanced dataset")
}

write.csv(dynobs, "dat/dynobs.csv", row.names=F)
write.csv(statobs, "dat/statobs.csv", row.names=F)
write.csv(citobs, "dat/citobs.csv", row.names=F)

outfile = gzfile("dat/fullobs_sp.csv.gz", 'w')
write.csv(fullobs_sp, outfile)
close(outfile)

# beware:
unique(stringi::stri_enc_mark(siem$city))
unique(stringi::stri_enc_mark(citobs$city))

# m3 per region

out = fullobs_sp[data.table::between(year, 700, 1500), list(im2 = sum(im2_ann, na.rm=T), 
                                    im3 = sum(im3_ann, na.rm=T), 
                                    im2_tot = max(im2_cml, na.rm=T),
                                    im3_tot = max(im3_cml, na.rm=T)), 
    by=list(ctr, city, decade, category)][order(ctr, city, category, decade), ]
write.csv(out, "dat/fullobs_sp_20y.csv", row.names=F)
