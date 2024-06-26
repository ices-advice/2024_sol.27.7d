## Extract results of interest, write TAF output tables

## Before: model/data outputs
## After: produce tables 


# time series of input data
# fitted model object
# official landings
# FLStock objects

library(icesTAF)
library(stockassessment)
library(flextable)
library(FLfse)
library(FLCore)
library(officer)


set_flextable_defaults(
  border.width = 1,
  big.mark = "",
  digits = 2
)

mkdir("output")

# sourceTAF("output_01_official_landings.R")
# sourceTAF("output_02_assessment_tables.R")


load('model/SAM_fit.RData')
# load('data/assessment_inputs.RData')

FLQuant2flextable <- function(x) {
            # CONVERT to year~age data.frame
            if(length(dimnames(x)[[1]])>1) df <- as.data.frame(t(x[drop=TRUE]))
            if(length(dimnames(x)[[1]])==1) df <- as.data.frame(x[drop=TRUE])
            rownames(df) <- NULL
            colnames(df) <- dimnames(x)[[1]]
            colnames(df)[colnames(df)=="-1"] <- "biomass"
            # ADD year column
            ft <- cbind(year=dimnames(x)$year, df)
            # CREATE flextable
            autofit(flextable(ft))
}

SAM_index_table <- function(fit, fleetNames = NULL, fleetType = NULL){
  
  years       <- fit$data$years
  fleet_idx   <- which(fit$data$fleetTypes %in% c(1,2,3,6))
  fleet_names <- attributes(fit$data)$fleetNames[fleet_idx]
  no_fleets   <- length(fleet_idx)
  data_type   <- setNames(ifelse(fit$data$fleetTypes[fleet_idx] == 2, "numbers", ifelse(fit$data$fleetTypes[fleet_idx] == 3,
                                                                                               ifelse(fit$conf$keyBiomassTreat[fleet_idx] == 0, "SSB index",
                                                                                                      ifelse(fit$conf$keyBiomassTreat[fleet_idx] == 1, "catch index", "FSB index")), 
                                                                                               "other")),fleet_names)
  fleet_type  <- setNames(ifelse(data_type == "numbers", "survey", "commercial LPUE"),fleet_names)
  sample_time <- setNames(fit$data$sampleTimes[fleet_idx],fleet_names)
  min_age     <- setNames(fit$data$minAgePerFleet, attributes(fit$data)$fleetNames)
  max_age     <- setNames(fit$data$maxAgePerFleet, attributes(fit$data)$fleetNames)
  
  
  obs.tab <- as.data.frame(append(list("year"= as.numeric(years)),
                       lapply(fleet_names, function(x){
                         as.data.frame(lapply(min_age[x]:max_age[x], function(x)return(NA))) 
                         }))) 
  colnames(obs.tab) <- c("Year",unlist(lapply(fleet_names, function(x) paste0(x, ".Age ", min_age[x]:max_age[x])))) 
  desc.tab <- setNames(as.data.frame(append(list("spec"= c("type","data","timing")),
                                                 lapply(fleet_names,function(x)NA))), c("spec",fleet_names)) 
  
  
  
  obs         <- exp(fit$data$logobs)
  attr.desc   <- c(1)
  
  for(i in fleet_idx){
    for(j in min_age[i]:max_age[i]){
      tmp <- data.frame("year" = fit$data$aux[fit$data$aux[,"fleet"] == i & fit$data$aux[,"age"] == j,"year"],
                        "index" = obs[fit$data$aux[,"fleet"] == i & fit$data$aux[,"age"] == j])
      tmp <- tmp[order(tmp$year),]
      obs.tab[obs.tab$Year %in% tmp$year ,paste0(attributes(fit$data)$fleetNames[i],".Age ",as.character(j))] <- tmp$index
    }
    desc.tab[desc.tab$spec == "type",attributes(fit$data)$fleetNames[i]] <- fleet_type[attributes(fit$data)$fleetNames[i]]
    desc.tab[desc.tab$spec == "data",attributes(fit$data)$fleetNames[i]] <- data_type[attributes(fit$data)$fleetNames[i]]
    desc.tab[desc.tab$spec == "timing",attributes(fit$data)$fleetNames[i]] <- sample_time[attributes(fit$data)$fleetNames[i]]
    attr.desc <- c(attr.desc,length(min_age[i]:max_age[i])) 
  }
  attr.desc <- setNames(attr.desc, c("spec",fleet_names))
  attr(desc.tab,"ncol") <- attr.desc

  return(list("data" = obs.tab, "specs" = desc.tab))
}


# convert SAM to FLStock objects, without and with estimated catches
flstock           <- SAM2FLStock(SAM_fit_sol_7d)
landings(flstock) <- computeLandings(flstock)
discards(flstock) <- computeDiscards(flstock)
catch(flstock)    <- computeCatch(flstock)

flstock.est.catch           <- SAM2FLStock(SAM_fit_sol_7d, catch_estimate = T)
landings(flstock.est.catch) <- computeLandings(flstock.est.catch)
discards(flstock.est.catch) <- computeDiscards(flstock.est.catch)
catch(flstock.est.catch)    <- computeCatch(flstock.est.catch)

# tables

tables <- qtables <- list()

# - Model parameters (modparams)

# TODO Intercatch

# - Time-series landings at age (in thousands) 
qtables$landings.n <- landings.n(flstock)

# - Time-series discards at age (in thousands) 
qtables$discards.n <- discards.n(flstock)

# - Time-series of the mean weights-at-age in the landings 
qtables$landings.wt <- landings.wt(flstock)

# - Time-series of the mean weights-at-age in the discards 
qtables$discards.wt <- discards.wt(flstock)

# - Time-series of mean stock weights at age 
qtables$stock.wt <- stock.wt(flstock)


qtables$stock.n <- stock.n(flstock)

# - Fishing mortality-at-age

qtables$harvest <- harvest(flstock)

# ftables
ftables <- lapply(qtables, FLQuant2flextable)

indices <- SAM_index_table(SAM_fit_sol_7d)
obs.tab <- qtables$indices <- indices$data
desc.tab <- indices$specs

colnames(obs.tab) <- sub("-LPUE","",colnames(obs.tab))
colnames(obs.tab) <- sub("-model","",colnames(obs.tab))
colnames(obs.tab) <- sub("Age -1","biomass",colnames(obs.tab))


ft_1 <- flextable(obs.tab)
ft_1 <- colformat_double(ft_1, digits = 2, big.mark = "", na_str = "N/A")
ft_1 <- colformat_double(ft_1,j=1, big.mark = "",  digits = 0, na_str = "N/A")
for(i in 1:nrow(desc.tab)){
  ft_1 <- add_footer_row(ft_1, values = desc.tab[i,], colwidths = attributes(desc.tab)$ncol)
}
col_idx <- seq_len(sum(attributes(desc.tab)$ncol))[unlist(lapply(attributes(desc.tab)$ncol, function(x){
  rep(ifelse(x==1,T,F),x)
}))]

ft_1 <- align(ft_1, align = "center", part = "footer")
ft_1 <- align(ft_1, j = col_idx, align = "right", part = "footer")
ft_1 <- align(ft_1, j = 1, align = "left", part = "footer")

ft_1 <- italic(ft_1, part = "footer")
ft_1 <- hline_bottom(ft_1, part = "footer", border = fp_border(width = 2))
ft_1 <- hline_top(ft_1, part = "header", border = fp_border(width = 2))
# vline(ft_1,j = seq_len(length(ft_1$col_keys)-1), part = "footer")
ft_1 <- separate_header(
  x = ft_1,
  opts = c("span-top", "bottom-vspan")
)
ftables$indices <- autofit(ft_1)



# SSB and F w/error

tssb <-   as.data.frame(cbind("Year" = as.numeric(rownames(ssbtable(SAM_fit_sol_7d))),ssbtable(SAM_fit_sol_7d)))
colnames(tssb) <- c("Year", "SSB", "SSB lower", "SSB upper")
tfbar <- as.data.frame(fbartable(SAM_fit_sol_7d))
colnames(tfbar) <- c("F", "F lower", "F upper")
tables$ssbf <- cbind(tssb, tfbar)
ftables$ssbf <- colformat_double(flextable(tables$ssbf), digits = 2) 

# - Time-series of the official landings by country

# setnames(stats, c("year", "other", "official", "bms", "ices", "tac"),
#          c("Year", "Other", "Official", "BMS", "ICES", "TAC"))
# 
# tables$catches <- stats
# ftables$catches <- flextable(stats)

# MAT & M

matm <- model.frame(FLQuants(Maturity=mat(flstock)[,'2022'],
                                        M=m(flstock)[,'2022']), drop=TRUE)
colnames(matm)[colnames(matm)=="age"] <-  "Age"

tables$matm <- matm
ftables$matm <- autofit(flextable(matm))

# Mohn's rho table
load("model/SAM_diagnostics.RData")
mrho <- mohn(diagnostics$retrospective_analysis)



# SAVE tables

save(tables, qtables, ftables, mrho, file="output/tables.Rdata")


cp('model/SAM_fit.RData', 'output')
save(flstock, file = "output/flstock_obs_catch.RData")
save(flstock.est.catch, file = "output/flstock_est_catch.RData")

