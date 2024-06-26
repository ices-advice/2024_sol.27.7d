
# read the official landings and put them in an easy format

library(readr)
library(readxl)
library(data.table)

official_landings <- as.data.frame(read_xlsx("bootstrap/data/official_landings/sol7d_official_landings.xlsx", sheet = 1))

# clean up
official_landings <- official_landings[!apply(official_landings, 1, function(x)all(is.na(x))),]

official_landings[is.na(official_landings$Year),]

# reshape this a bit
years <- official_landings$Year[!is.na(official_landings$Year)]

colnumn_names <- cbind(colnames(official_landings),c(official_landings[1,]))

country <- c(); data_type <- c(); ref_col <- ""
for(i in colnames(official_landings)){
  if(i %in% c("Year","Official Landings","TAC (initial)","ICES InterCatch Landings","ICES InterCatch Discards (Raised)","ICES InterCatch Catch","Official BMS")){
    ref_col <- i
  }
  data_type <- c(data_type,ref_col)
}
for(i in official_landings[1,]){
  country <- c(country, i)
}
country[is.na(country) & data_type != "Year"] <- "Total"

ctr <- 1; ls <- list()
for(i in 1:ncol(official_landings)){
  if(data_type[i] != "Year"){
    ls[[ctr]] <- data.frame("year" = years,
                            "variable" = data_type[i],
                            "country" = country[i],
                            "value" = official_landings[-1,i])
  }
  ctr <- ctr + 1
}

data <- do.call("rbind",ls)
data <- data[!is.na(data$value),]

data <- data[!(data$year >= 2022 & data$variable %in% c("Official Landings","Official BMS")) ,]


# read new data
prelim_catch <- read.csv("bootstrap/data/official_landings/preliminary_catches.csv")
prelim_catch <- prelim_catch[prelim_catch$Area == "27_7_D",]
colnames(prelim_catch)[6:7] <- c("Catch","Official BMS")

prelim_catch2 <- read.csv("bootstrap/data/official_landings/PreliminaryCatchesFor2022.csv", sep = ";")
prelim_catch2 <- prelim_catch2[prelim_catch2$Species.Latin.Name == "Solea solea" & prelim_catch2$Area == "27_7_D",]
colnames(prelim_catch2)[6:7] <- c("Catch","Official BMS")

TAC           <- read.csv("bootstrap/data/official_landings/TACs.csv")


# add to data

prelim_catch <- rbind(prelim_catch2, prelim_catch)
prelim_catch <- prelim_catch[,-c(2,3,4)]
setDT(prelim_catch)

prelim_catch <- melt(prelim_catch, id.vars = c("Year","Country"))
colnames(prelim_catch)[1:2] <- c("year","country")

prelim_catch$country[prelim_catch$country == "BE"] <- "Belgium"
prelim_catch$country[prelim_catch$country == "FR"] <- "France"
prelim_catch$country[prelim_catch$country == "GB"] <- "UK (E&W)"
prelim_catch$country[prelim_catch$country %in% c("IE","NL")] <- "Other"
prelim_catch$value[is.na(prelim_catch$value)] <- 0

prelim_catch$country[prelim_catch$variable == "Official BMS"] <- "Total"

catch <- aggregate(value ~ year + country + variable, FUN = "sum", data = prelim_catch[prelim_catch$variable == "Catch",], na.rm = T)
bms   <- aggregate(value ~ year + country + variable, FUN = "sum", data = prelim_catch[prelim_catch$variable == "Official BMS",], na.rm = T)

catch$variable <- as.character(catch$variable)
catch$variable <- "Official Landings"

bms$variable <- as.character(bms$variable)
bms$variable <- "Official BMS"


data <- rbind(data, catch, bms)

data$country[grepl("UK",data$country)] <- "UK"

# add TAC
TAC <- TAC[,-c(1,2)]
colnames(TAC)[1:3] <- c("year","country","value")
TAC$variable <- "TAC (initial)"
TAC$country[TAC$country == "United Kingdom"] <- "UK"


# remove last year
data <- data[!(data$variable == "TAC (initial)" & data$year == 2023),]
data <- rbind(data, TAC)

# add intercatch

# StockOverviewFile <- paste(datapath,"StockOverview_2021.txt",sep="")
# NumbersAtAgeLengthFile <- paste(datapath,"NumbersAtAgeLength.txt",sep="")
# 
# WtData <- readStockOverview(StockOverviewFile,NumbersAtAgeLengthFile) 
# Ndata <- readNumbersAtAgeLength(NumbersAtAgeLengthFile) 
# 
# WtData$Area <- factor(WtData$Area)
# 
# WtData %>% 
#   group_by(Country, CatchCat, Year,Area) %>% 
#   summarise(weight = sum(CatchWt))


# StockOverviewFile <- paste(datapath,"StockOverview.txt",sep="")
# NumbersAtAgeLengthFile <- paste(datapath,"NumbersAtAgeLength.txt",sep="")
# 
# WtData <- readStockOverview(StockOverviewFile,NumbersAtAgeLengthFile)
# Ndata <- readNumbersAtAgeLength(NumbersAtAgeLengthFile)
# 
# WtData$Area <- factor(WtData$Area)
# 
# WtData %>%
#   group_by( CatchCat, Year,Area) %>%
#   summarise(weight = sum(CatchWt))
# 
# 
# WtData$fleet_group <- "Others"
# WtData$fleet_group[grepl("OTB",WtData$Fleet)] <- "Bottom_trawls"
# WtData$fleet_group[grepl("TBB",WtData$Fleet)] <- "Beam trawls"
# WtData$fleet_group[grepl("GNS|GTR",WtData$Fleet)] <- "Netters"
# 
# dat <- WtData %>%
#   group_by(fleet_group, CatchCat, Year,Area) %>%
#   summarise(weight = sum(CatchWt)) %>%
#   as.data.table() %>%
#   dcast(fleet_group + Area + Year ~ CatchCat) %>%
#   as.data.frame()
# dat$D.prop <- round(dat$D/sum(dat$D, na.rm = T)*100,2)
# dat$L.prop <- round(dat$L/sum(dat$L, na.rm = T)*100,2)
# dat


IC_landings_2023 <- data.frame("year" = 2023,
                          "country" = c("Belgium","France","UK","Other","Total"),
                          "variable" = "ICES InterCatch Landings",
                          "value" = c(534350,569139,183667,361,1287517))

IC_discards_2023 <- data.frame("year" = 2023,
                          "country" = c("Belgium","France","UK","Other","Total"),
                          "variable" = "ICES InterCatch Discards (Raised)",
                          "value" = c(104423,47463,0,0,151886))

IC_landings_2022 <- data.frame("year" = 2022,
                               "country" = c("Belgium","France","UK","Other","Total"),
                               "variable" = "ICES InterCatch Landings",
                               "value" = c(531924,892360,262813,1336,1688433))

IC_discards_2022 <- data.frame("year" = 2022,
                               "country" = c("Belgium","France","UK","Other","Total"),
                               "variable" = "ICES InterCatch Discards (Raised)",
                               "value" = c(108303,143834,898,0,253035))

IC_landings_2021 <- data.frame("year" = 2021,
                               "country" = c("Belgium","France","UK","Other","Total"),
                               "variable" = "ICES InterCatch Landings",
                               "value" = c(477932,854962,233232,941,1567067))

IC_discards_2021 <- data.frame("year" = 2021,
                               "country" = c("Belgium","France","UK","Other","Total"),
                               "variable" = "ICES InterCatch Discards (Raised)",
                               "value" = c(84208,221468,676,0,306352))


IC_discards <- rbind(IC_discards_2021, IC_discards_2022, IC_discards_2023)
IC_landings <- rbind(IC_landings_2021, IC_landings_2022, IC_landings_2023)
IC_catches  <- IC_landings
IC_catches$value <- IC_landings$value + IC_discards$value 
IC_catches$variable <- "ICES InterCatch Catch"

IC_update <- rbind(IC_landings, IC_discards, IC_catches)
IC_update$value <- IC_update$value/1000

data[data$variable == "Official BMS",]
data$value <- as.numeric(as.character(data$value))

save(data, IC_update, file = "data/landings_discards.RData")

