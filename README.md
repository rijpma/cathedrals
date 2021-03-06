# Church building repro

This repository contains reproduction files for the paper "Church building and the economy during Europe’s ‘Age of the Cathedrals’, 700–1500 CE".

There are scripts to create the data as well as the figures. The scripts prep_citydata.R prep_popdata.R, and prep_churchdata.R create the basic data files. The remaining scripts create the figures in the main paper and the appendices.

All tables and figures except figure 1 can be reproduced by running the script `reproduce.sh`. Figure 1 is included as a separate excel file: `excels/England Ch building GDP & earnings.xlsx`. The table below matches the files to the tables and figures in the paper.

|paper    |file              |
|---------|------------------|
|Figure 1 | excels/England Ch building GDP & earnings.xlsx|
|Table 1  | tab/sumstats_perc_7001100.html|
|         | tab/sumstats_perc11001500.html|
|         | tab/sumstats.html|
|Figure 2 | figs/distrs_prepost_heap.pdf|
|Figure 3 | figs/europetotal_hc.pdf|
|Figure 3 | figs/europetotal_hc.pdf|
|Figure 4 | figs/4m3maps_smoothed.pdf|
|Table 2  |  tab/regiontable.html|
|         | tab/regionindices.html|
|Figure 5 | figs/pcpanel.pdf|
|Figure 6 | figs/pucpanel.pdf|
|Figure 7 | figs/eucompare.pdf|
|Figure 8 | figs/bytype_hc.pdf|
|Figure 9 | figs/bygeography_split.pdf|
|Figure 10| figs/completedstock_activity.pdf|
|Table A1 | tab/predecessors.html|
|Figure A1| figs/altbackprojs_panel.pdf|
|Figure A2| figs/height_surface_inclit.pdf|
|Figure A3| figs/researcharea.png|
|Figure A4| figs/ruralcorrections_eu.pdf|
|Figure A5| figs/ruralcorrections_smt.pdf|
|Figure A6| figs/james_v_catdat1250_m3.pdf|
|Figure A7| figs/james_v_osm_series_hc.pdf|
|Table A2 | tab/disastercounts.html|
|Table A3 | tab/disasterregs.html|
|Figure A8| figs/disasterpanel.pdf|
|Figure A9| figs/disasterpanel.pdf|
|Figure A9| figs/disasterpanel.pdf|

The scripts cat_osmchurches.R and cat_geocoding.R are excluded from the overall reproduction script because they make a lot of requests at the OpenStreetMap/overpass and google geocoding servers. Moreover, the data on the servers can have changed since the original requests. They are included here only to show how this part of the data was created.

The scripts have a GPL-3 license. The data based on OpenStreetMap has an [Open Database License](http://opendatacommons.org/licenses/odbl/1.0/). This holds for the following files:

* dat/checkedchurches_eb_8_2018sep4.csv
* dat/churches_italy_2_2018oct2.csv
* dat/churches_add_2018nov30.csv
* dat/fullobs_sp.csv.gz
* dat/dynobs.csv
* dat/heights.csv
* dat/statobs.csv
* dat/citobs.csv
* dat/gis/rur.shp
* dat/rurchurches_eb.csv
* dat/backproj.csv
* dat/backproj_ita.csv
* excels/saints and exogenous disasters.xlsx
* excels/ital cities def 2.xlsx
* dat/backproj.csv
* dat/backproj_ita.csv


Environment:

    R version 3.4.1 (2017-06-30)
    Platform: x86_64-apple-darwin15.6.0 (64-bit)
    Running under: macOS  10.14.2

    Matrix products: default
    BLAS: /Library/Frameworks/R.framework/Versions/3.4/Resources/lib/libRblas.0.dylib
    LAPACK: /Library/Frameworks/R.framework/Versions/3.4/Resources/lib/libRlapack.dylib

    locale:
    [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

    attached base packages:
    [1] stats     graphics  grDevices utils     datasets  methods   base     

    other attached packages:
     [1] brms_1.10.2       ggplot2_2.2.1     Rcpp_1.0.0        sandwich_2.4-0    lmtest_0.9-35    
     [6] texreg_1.36.23    zoo_1.8-0         viridisLite_0.3.0 maptools_0.9-2    RNetCDF_1.9-1    
    [11] knitr_1.20        writexl_0.2       stringi_1.3.1     sf_0.6-3          raster_2.5-8     
    [16] sp_1.2-5          countrycode_1.1.0 readxl_1.3.1      data.table_1.12.0
