#!/bin/bash

# data prep
Rscript --verbose script/prep_citydata.R > prep_citydata.Rout 2>&1
Rscript --verbose script/prep_popdata.R > prep_popdata.Rout 2>&1
Rscript --verbose script/prep_churchdata.R > prep_churchdata.Rout 2>&1 # take a few minutes
Rscript --verbose script/prep_disasterdata.R > prep_disasterdata.Rout 2>&1

# main paper tables and figurs
Rscript --verbose script/sumstats.R > sumstats.Rout 2>&1
Rscript --verbose script/explorations.R > explorations.Rout 2>&1
Rscript --verbose script/stockact.R > stockact.Rout 2>&1
Rscript --verbose script/rasters.R > rasters.Rout 2>&1
Rscript --verbose script/samplesize.R > samplesize.Rout 2>&1

# appendices tables and figures
Rscript --verbose script/heightimps.R > heightimps.Rout 2>&1
Rscript --verbose script/m2imps.R > m2imps.Rout 2>&1 # takes a few minutes
Rscript --verbose script/jamescompare.R > jamescompare.Rout 2>&1
Rscript --verbose script/ruralsample.R > ruralsample.Rout 2>&1
Rscript --verbose script/disastercorrect.R > disastercorrect.Rout 2>&1
