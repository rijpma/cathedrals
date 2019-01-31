rm(list = ls())
setwd("~/dropbox/cathedrals")

library("data.table")
library("sf")
library("png")
library("maptools")

# could add population rasters here, but are all assumed equally distributed within country
# puc correction should be possible anyway

data(wrld_simpl, package = "maptools")
wrld_simpl = st_as_sf(wrld_simpl)

siem = data.table::fread("dat/siem_long_1500.csv")
statobs = data.table::fread("dat/statobs.csv")
fullobs_sp <- data.table::fread("gunzip -c dat/fullobs_sp.csv.gz")

fullobs_sp = fullobs_sp[between(year, 700, 1500)]
fullobs_sp[, century := round(year / 100)  * 100] # for compatability w. century in siem

M = 9 # number of imputations
impvrbs = names(fullobs_sp)[grepl('im3_ann\\d', names(fullobs_sp))]

fullobs_sp_dec = fullobs_sp[data.table::between(decade, 700, 1500), list(m3dec = base::sum(.SD, na.rm=T) / M), by=list(century, decade, osmid, lat, lon), .SDcols = impvrbs]
# midcent = midcent[data.table::between(year, 650, 1550), list(im3_cnt = base::sum(abs(.SD), na.rm=T) / M), by=list(city, century = year, lat, lon), .SDcols=impvrbs]

statobs_sp = st_as_sf(statobs, coords = c("lon", "lat"))
siem_sp = st_as_sf(siem, coords = c("lon", "lat"))

eu = wrld_simpl[tolower(wrld_simpl$ISO2) %in% c(unique(statobs$ctr), "gb"), ]
st_crs(siem_sp) = st_crs(statobs_sp) = st_crs(eu)
# set to planar eu crs
eu = sf::st_transform(eu, crs = 3035)
siem_sp = sf::st_transform(siem_sp, crs = 3035)
statobs_sp = sf::st_transform(statobs_sp, crs = 3035)


possible_grids = expand.grid(3:6, 3:6)
# grd = as.data.frame(t(possible_grids))[, 11]
# grd = as.data.frame(t(possible_grids))[, 5]
for (grd in as.data.frame(t(possible_grids))){
    griddim = c(cols = grd[1], rows = grd[2])
    eugr = st_make_grid(eu, n = griddim)
    # plot(eu[, "geometry"], reset = F, border = 'gray')
    # plot(eugr, add = T)

    # number of each grid cell, yay!
    cells = st_intersects(statobs_sp, eugr)

    # plot(eu[, "geometry"], reset = F)
    # plot(eugr, add = T)
    # plot(statobs_sp[, 'geometry'], col = 'pink', pch = 19, cex = 0.4, add = T)

    statobs[, cell := unlist(cells)]
    fullobs_grid = statobs[, list(osmid, cell)][fullobs_sp_dec, on = 'osmid']
    cellsums = fullobs_grid[, sum(m3dec, na.rm = T), by = .(cell, decade)]
    # number of each grid cell, yay!

    # plot(eu[, "geometry"], reset = F)
    # plot(eugr, add = T)
    # plot(statobs_sp[, 'geometry'], col = 'pink', pch = 19, cex = 0.4, add = T)

    ncells = prod(griddim)
    grsize = c(x = mean(sapply(eugr, function(x) st_bbox(x)$xmax - st_bbox(x)$xmin)),
        y = mean(sapply(eugr, function(x) st_bbox(x)$ymax - st_bbox(x)$ymin)))
    grsize = grsize / grsize['x']

    tmplist = list()
    for (i in ncells:1){
        tmp = tempfile()
        png(tmp, width = grsize["x"] * 300, 
                 height = grsize["y"] * 300)
        par(mar = rep(0, 4))
        plot(eugr[[i]], border = 0, reset = F)
        try(plot(st_crop(eu, eugr[[i]])[, 'geometry'], 
            add = T, col = 'gray90', border = NA))
        dev.off()
        tmplist[[i]] = tmp
    }

    lims = list(y = range(cellsums$V1), x = range(cellsums$decade))

    print(grsize)
    print(griddim)


    pdf(paste0("figs/gridded_", griddim["cols"], "x", griddim["rows"], ".pdf"),
        width = 1.1*(grsize["x"]* griddim[1]) + 1, 
        height = 1.1*(grsize["y"] * griddim[2]) + 1)
        # some end up a bit squoshed because
    # plotmat = rbind(16:20, 11:15, 6:10, 1:5)
    plotmat = matrix(1:ncells, nrow = griddim[2], byrow = T)
    plotmat = plotmat[nrow(plotmat):1, ]

    yaxslocs = plotmat[, 1]
    xaxslocs = plotmat[nrow(plotmat), ]
    # yaxslocs = plotmat[seq(from = nrow(plotmat) - 1, to = 1, by = - 2), 1]
    # xaxslocs = plotmat[nrow(plotmat), seq(from = 1, to = ncol(plotmat), by = 2)]

    plotmat = cbind(0, plotmat)
    plotmat = rbind(plotmat, 0)
    layout(plotmat, 
        widths = c(ncol(plotmat) * 0.1, rep.int(1, ncol(plotmat) - 1)),
        height = c(rep.int(1, nrow(plotmat) - 1), nrow(plotmat) * 0.1))
    # middlerow = max(
    #     median(plotmat[-nrow(plotmat), 2]), 
    #     median(plotmat[-c(1, nrow(plotmat)), 2]))
    # middlecol = max(
    #     median(plotmat[nrow(plotmat) - 1, -1], ), 
    #     median(plotmat[nrow(plotmat) - 1, -1]))
    par(mar = c(0, 0, 0, 0))
    for (i in 1:ncells){
        toplot = cellsums[cell == i]
        if (nrow(toplot) > 0){
            plot(toplot[, .(decade, V1)], type = 'n', 
                ylim = lims$y, xlim = lims$x, 
                axes = F, xlab = "", ylab = "")
            rasterImage(as.raster(png::readPNG(tmplist[[i]])), 
                lims$x[1], lims$y[1], lims$x[2], lims$y[2])
            lines(toplot[, .(decade, V1)], col = 2, lwd = 1.5)
        } else {
            plot(cellsums[, .(decade, V1)], type = 'n', 
                axes = F, xaxs = 'i', yaxs = 'i')
            rasterImage(as.raster(png::readPNG(tmplist[[i]])), 
                lims$x[1], lims$y[1], lims$x[2], lims$y[2])
        }
        if (i %in% yaxslocs){
            if (i %in% yaxslocs[c(FALSE, TRUE)]){
                axis(2, at = round(lims$y, digits = -floor(log10(lims$y[2]) - 1)))
            } else {
                axis(2, at = round(lims$y, digits = -floor(log10(lims$y[2]) - 1)),
                    labels = FALSE)
            }
        }
        if (i %in% xaxslocs){
            if (i %in% xaxslocs[c(TRUE, FALSE)]){
                axis(1, at = lims$x)
            } else {
                axis(1, at = lims$x, labels = FALSE)
            }
        }
    }
    dev.off()

    # repeat with siem for puc
    cells = st_intersects(siem_sp, eugr)

    siem[, cell := unlist(cells)]

    siem_cellsums = siem[, list(urbpop = sum(inhab, na.rm = T)), by = .(cell, century = year)]

    cellsums_cent = fullobs_grid[, list(m3cen = sum(m3dec, na.rm = T)), by = .(cell, century)]

    cellsums_cent = cellsums_cent[siem_cellsums, on = c('century', 'cell')]
    cellsums_cent[, m3cen_puc := m3cen / urbpop]

    lims = list(ylim = range(cellsums_cent[urbpop > 0, m3cen_puc], na.rm = T),
        xlim = c(700, 1500))

    pdf(paste0("figs/gridded_puc_", griddim["cols"], "x", griddim["rows"], ".pdf"),
        width = grsize["x"] * griddim[1] + griddim[1], 
        height = grsize["y"] * griddim[2] + griddim[1])
        # some end up a bit squoshed because
    # plotmat = rbind(16:20, 11:15, 6:10, 1:5)
    plotmat = matrix(1:ncells, nrow = griddim[2], byrow = T)
    plotmat = plotmat[nrow(plotmat):1, ]
    layout(plotmat)
    par(mar = rep(0, 4))
    for (i in 1:ncells){
        toplot = cellsums_cent[cell == i]
        if (nrow(toplot) > 0){
            plot(toplot[, .(century, m3cen / urbpop)], type = 'n', 
                ylim = lims$y, xlim = lims$x)
            rasterImage(as.raster(png::readPNG(tmplist[[i]])), 
                lims$x[1], lims$y[1], lims$x[2], lims$y[2])
            lines(toplot[, .(century, m3cen / urbpop)], col = 2, lwd = 1.5)

        } else {
            plot(c(0, 1), c(0, 1), type = 'n', axes = F, xaxs = 'i', yaxs = 'i')
            rasterImage(as.raster(png::readPNG(tmplist[[i]])), 0, 0, 1, 1)
        }

        if (i %in% plotmat[, 2]){
            if (i %% 2 == 0){
                axis(2, at = round(lims$y, digits = -floor(log10(lims$y[2]) - 1)))
            } else {
                axis(2, at = round(lims$y, digits = -floor(log10(lims$y[2]) - 1)), 
                    labels = FALSE)
            }
        }
        if (i %in% plotmat[nrow(plotmat) - 1, ]){
            if (i %% 2 == 1){
                axis(1, at = lims$x)
            } else {
                axis(1, at = lims$x, labels = FALSE)
            }
        }
    }
    dev.off()
}


    # ditto puc?
# would need: take siem, also intersect with grid
# than add pop totals to grid cell and divide
# obviously this could also be done with total pop









# trash

x = as.raster(readPNG(tmp))
toplot = cellsums[cell == 5]
plot(toplot[, .(year, V1)], type = 'n')
lim = par()
rasterImage(x, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
lines(toplot[, .(year, V1)], col = 2)


dev.off()
st_intersects(eugr, eu)

x[, sum(im3, na.rm = T), by = .(cell, year)]
plot(x[, sum(im3, na.rm = T), by = .(cell, year)][cell == 3, .(year, V1)])
plot(x[, sum(im3, na.rm = T), by = .(cell, year)][cell == 4, .(year, V1)], type = 'l')

cellsums = x[, sum(im3, na.rm = T), by = .(cell, year)]
par(mfrow = c(5, 4), mar = rep(0, 4))
for (i in 20:1){
    toplot = cellsums[cell == i]
    if (nrow(toplot) > 0){
        plot(toplot[, .(year, V1)])
    } else {
        plot.new()
    }
}

st_crs(statobs_sp)
st_crs(eugr)

inherits(statobs_sp, 'sf')
inherits(eugr, 'sf')