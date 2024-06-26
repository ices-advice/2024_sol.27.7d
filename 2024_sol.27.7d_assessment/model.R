## Run analysis, write model results

## Before: take preprocessed input data
## After: fitted SAM model with diagnostics

library(icesTAF)

mkdir("model")

sourceTAF("model_01_SAM_configuration.R")
sourceTAF("model_02_SAM_fit_diagnostics.R")
