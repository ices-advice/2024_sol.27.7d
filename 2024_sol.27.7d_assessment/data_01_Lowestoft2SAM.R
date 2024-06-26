
# Lowestoft input file to SAM data object

library(stockassessment)

## Read original data
datadir           <- "bootstrap/data/assessment_input"

file_names        <- list.files(datadir)
stock_data        <- lapply(file_names,function(x)read.ices(file.path(datadir,x))) # note dn and ln are sopcorrected in propdiscarded.R script when reconstructing discards
names(stock_data) <- unlist(strsplit(file_names,".txt"))

## Preprocess data and create SAM object
# Restrict info to plus group
min_age    <- 1
plus_group <- 11

sam_data <- list()

sam_data$dn              <- stock_data$dn[,min_age:plus_group]
sam_data$dn[,plus_group] <- rowSums(stock_data$dn[,plus_group:ncol(stock_data$dn)])
sam_data$dw              <- stock_data$dw[,min_age:plus_group]
sam_data$dw[,plus_group] <- rowSums(stock_data$dw[,plus_group:ncol(stock_data$dw)]  * sweep(stock_data$dn[,plus_group:ncol(stock_data$dn)],1, FUN = "/", rowSums(stock_data$dn[,plus_group:ncol(stock_data$dn)],na.rm = T)),na.rm = T)

sam_data$ln              <- stock_data$ln[,min_age:plus_group]
sam_data$ln[,plus_group] <- rowSums(stock_data$ln[,plus_group:ncol(stock_data$ln)])
sam_data$lw              <- stock_data$lw[,min_age:plus_group]
sam_data$lw[,plus_group] <- rowSums(stock_data$lw[,plus_group:ncol(stock_data$lw)]  * sweep(stock_data$ln[,plus_group:ncol(stock_data$ln)],1, FUN = "/", rowSums(stock_data$ln[,plus_group:ncol(stock_data$ln)],na.rm = T)),na.rm = T)

stock_data$cn            <- stock_data$ln + stock_data$dn
stock_data$cw            <- (stock_data$dw * stock_data$dn + stock_data$lw * stock_data$ln) / (stock_data$dn + stock_data$ln)
stock_data$cw[is.na(stock_data$cw)] <- 0       
sam_data$cn              <- stock_data$cn[,min_age:plus_group]
sam_data$cn[,plus_group] <- rowSums(stock_data$cn[,plus_group:ncol(stock_data$cn)])
sam_data$cw              <- stock_data$cw[,min_age:plus_group]
sam_data$cw[,plus_group] <- rowSums(stock_data$cw[,plus_group:ncol(stock_data$cw)]  * sweep(stock_data$cn[,plus_group:ncol(stock_data$cn)],1, FUN = "/", rowSums(stock_data$cn[,plus_group:ncol(stock_data$cn)],na.rm = T)),na.rm = T)

sam_data$sw              <- stock_data$sw[,min_age:plus_group]  
sam_data$sw[,plus_group] <- rowSums(stock_data$sw[,plus_group:ncol(stock_data$sw)]  * sweep(stock_data$cn[,plus_group:ncol(stock_data$cn)],1, FUN = "/", rowSums(stock_data$cn[,plus_group:ncol(stock_data$cn)],na.rm = T)),na.rm = T)


sam_data$mo              <- stock_data$mo[,min_age:plus_group]
sam_data$nm              <- stock_data$nm[,min_age:plus_group]
sam_data$pf              <- stock_data$pf[,min_age:plus_group]
sam_data$pm              <- stock_data$pm[,min_age:plus_group]

sam_data$lf              <- sam_data$ln / sam_data$cn
sam_data$lf[is.na(sam_data$lf)] <- 0


# tuning fleets
sam_data$tun <- stock_data$tun
uk_bts <- sam_data$tun[[which(grepl("BTS-Q3", names(sam_data$tun)))]]
save(uk_bts, file = "data/UK_BTS_full.RData")

# delete last Belgian BE_CBT point
# sam_data$tun$`BE-CBT-LPUE													`["2022",1] <- NA
# remove age 1:3 from UK BTS since 2010
final_year <- max(as.numeric(rownames(sam_data$tun[[which(grepl("BTS-Q3", names(sam_data$tun)))]])))
sam_data$tun[[which(grepl("BTS-Q3", names(sam_data$tun)))]][as.character(2010:final_year),c(1:3)] <- NA

save(sam_data, file = "data/assessment_inputs.RData")


dat.sol7d     <- setup.sam.data(
  surveys = sam_data$tun,                # tuning fleets
  residual.fleet = sam_data$cn,          # catch numbers-at-age
  prop.mature = sam_data$mo,             # maturity ogive
  stock.mean.weight = sam_data$sw,       # stock weights
  catch.mean.weight = sam_data$cw,       # catch weights
  dis.mean.weight = sam_data$dw,         # discard weights
  land.mean.weight = sam_data$lw,        # landing weights
  prop.f = sam_data$pf,                  # proportion fished before spawning
  prop.m = sam_data$pm,                  # proportion natural mortality before spawning
  natural.mortality = sam_data$nm,       # natural mortality
  land.frac = sam_data$lf )              # landing proportion


#Save the SAM data object 
save(dat.sol7d, file = "data/sol7d_SAM_data_object.RData")

