

## Before: read SAM input data
## After: define SAM configuration and parameter object

library(stockassessment)

load(file = "data/sol7d_SAM_data_object.RData", verbose = T)

# Create a default parameter configuration object
conf <- defcon(dat.sol7d)

# //1// MinAge, maxAge, maxAgePlusGroup

conf$minAge          <- 1                  # minimum age class in assessment
conf$maxAge          <- 11                 # maximum age class in assessment
conf$maxAgePlusGroup <- c(1,0,0,0,0,0,0)   
conf$keyLogFsta[1,]  <- c(0,1,2,3,4,5,5,6,6,7,7)   # c(0, 1, 2, 3, 3, 3, 4, 4, 5, 5) 
conf$corFlag         <- 0                     

conf$keyVarObs[1,]      <- c(0,1,rep(2,9))
conf$keyVarObs[2,1]     <- 3
conf$keyVarObs[3,1]     <- 4
conf$keyVarObs[4,1]     <- 5
conf$keyVarObs[5,1:6]   <- c(6,7,8,8,8,8)
conf$keyVarObs[6,1]     <- 9
conf$keyVarObs[7,1]     <- 10

conf$obsCorStruct <- factor(c("ID","ID","ID","ID","AR","ID","ID"), levels = c("ID","AR","US")) 
conf$keyCorObs[5,1:5]  <- c(0,1,1,1,1)

conf$fbarRange       <- c(3,7)
conf$keyBiomassTreat  <- c(-1,2,2,2,-1,-1,-1)

## Fit the model
par                        <- defpar(dat.sol7d, conf)

save(dat.sol7d, conf, par, file = "model/SAM_dat_conf_par.RData")