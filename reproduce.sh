#!/bin/bash

# data creation
Rscript --verbose script/cat_siem.R > siem.Rout 2>&1
Rscript --verbose script/cat_auxdata.R > auxdata.Rout 2>&1
Rscript --verbose script/cat_dataprep.R > dataprep.Rout 2>&1 # take a few minutes

# main paper tables and figurs
Rscript --verbose script/cat_sumstats.R > sumstats.Rout 2>&1
Rscript --verbose script/cat_explorations.R > explorations.Rout 2>&1
Rscript --verbose script/cat_stockact.R > stockact.Rout 2>&1
Rscript --verbose script/cat_rasters.R > rasters.Rout 2>&1
Rscript --verbose script/cat_samplesize.R > samplesize.Rout 2>&1

# appendices tables and figures
Rscript --verbose script/cat_heightimps.R > heightimps.Rout 2>&1
Rscript --verbose script/cat_m2imps.R > m2imps.Rout 2>&1 # takes a few minutes
Rscript --verbose script/cat_james.R > james.Rout 2>&1
Rscript --verbose script/cat_ruralsample.R > ruralsample.Rout 2>&1
Rscript --verbose script/cat_disasters.R > disasters.Rout 2>&1
Rscript --verbose script/cat_disastercorrect.R > disastercorrect.Rout 2>&1
