## Preprocess data, write TAF data tables

## Before: raw input data
## After: data preprocessed, combined and parsed to appropriate formats for further processing (stored in /data)

library(icesTAF)

mkdir("data")

sourceTAF("data_01_Lowestoft2SAM.R")
sourceTAF("data_02_prep_official_landings.R")
