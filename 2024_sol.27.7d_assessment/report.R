## Prepare plots and tables for report

## Before: take inputs
## After: prepare figures

library(icesTAF)
library(ggplot2)
library(gridExtra)
library(rmarkdown)
library(dplyr)
library(tidyr)
library(FLCore)
library(viridis)
library(flextable);set_flextable_defaults(background.color = "white")


set_flextable_defaults(
  border.width = 1,
  big.mark = "",
  digits = 2,
  background.color = "white"
)


outdir     <- "report"
assyear    <- 2024
plus_group <- max(SAM_fit_sol_7d$data$maxAgePerFleet[SAM_fit_sol_7d$conf$maxAgePlusGroup])
my_colours <- viridis(5)
SavePlot0<-function(plotname,width=11,height=11){
  file <- file.path(outdir,paste0(plotname,'.png'))
  dev.print(png,file,width=width,height=height,units='in',res=300, bg = "white")
  dev.off()
}

mkdir("report")


load("data/landings_discards.RData", verbose = T)
yrs <- data[data$variable == "TAC (initial)" & data$country == "Total","year"]
width = .65

data2 <- data
data2 <- data2[!(grepl("InterCatch",data2$variable) & data$year >= 2021),]
data2 <- rbind(data2, IC_update)
data2$variable[data2$variable == "ICES InterCatch Catch"] <- "IC Catches"
data2$variable[data2$variable == "ICES InterCatch Landings"] <- "IC Landings"
data2$variable[data2$variable == "ICES InterCatch Discards (Raised)"] <- "IC Discards"

data2 <- data2[data2$year >= 2022 & grepl("IC ", data2$variable) & data2$country != "Other",]
data2$country <- factor(data2$country, levels = c("Belgium","France","UK","Total"))
data3 <- data[data$country != "Other",]

data3$country <- factor(data3$country, levels = c("Belgium","France","UK","Total"))

p <- ggplot(data2, aes(variable, value, width = width)) +
  geom_bar(stat = "identity") +
  geom_hline(data = data[data$variable %in% c("TAC (initial)","Official Landings") & data$year %in% c(2022,2023) & data$country != "Other",], aes(yintercept = value, col = variable, group = variable))+
  facet_grid(year ~ country) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  ylab("tonnes") +
  xlab("") +
  ggtitle("InterCatch, Official landings and TAC by country and year") 
ggsave("InterCatch_landings_TAC.png", path = outdir, plot = p, width = 16, height = 8, units = "in", dpi=300)  

segments <- data.frame(x = yrs - width/2,
                       xend = yrs + width/2,
                       y = data[data$variable == "TAC (initial)" & data$country == "Total","value"],
                       yend = data[data$variable == "TAC (initial)" & data$country == "Total","value"],
                       country = "Total")

bms_table <- data[data$variable == "Official BMS",c("year","variable","value")]
bms_table$value <- round(bms_table$value, 2)

p <- ggplot(data[data$variable == "Official Landings" & data$country != "Total",], 
       aes(year, value, group = country, col = country, fill = country,width = width)) +
  geom_bar(stat = "identity") + 
  geom_segment(data = segments, aes(x = x, y = y, xend = xend, yend = yend), size = 1)+
  theme_bw() +
  theme(legend.title = element_blank()) +
  ylab("tonnes") +
  xlab("year") +
  ggtitle("Official landings and BMS by country and TAC") + 
  annotation_custom(tableGrob(bms_table, rows=NULL, theme = ttheme_minimal()), 
                    xmin= min(yrs)-7, xmax=min(yrs), ymin=5500, ymax=max(data$value[data$variable %in% c("Official Landings","TAC (initial)")]))

ggsave("Official_landings.png", path = outdir, plot = p, width = 14, height = 12, units = "in", dpi=300)  

IC_update$variable <- paste0(IC_update$variable," update")

p1 <- ggplot(data[grepl("InterCatch", data$variable) & data$country == "Total" ,], 
       aes(year, value, group = variable, col = variable, fill = variable,width = width)) +
  geom_line() + 
  geom_line(data = IC_update[IC_update$country == "Total",], aes(year, value)) +
  # facet_grid(variable ~ country, scales = "free_y") +
  theme_bw() +
  ylab("tonnes") +
  xlab("year") +
  ggtitle("InterCatch landings and discards (no BMS)") 

max_val <- max(data[grepl("InterCatch", data$variable) & data$country == "Total"  ,"value"])
p2 <- ggplot(data[grepl("InterCatch", data$variable) & data$country == "Total" & data$year >= 2021 ,], 
             aes(year, value, group = variable, col = variable, fill = variable,width = width)) +
  geom_line() + 
  geom_line(data = IC_update[IC_update$country == "Total",], aes(year, value)) +
  # facet_grid(variable ~ country, scales = "free_y") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_x_continuous(n.breaks = 3) +
  ylab("tonnes") +xlab("")

p <- p1 + annotation_custom(ggplotGrob(p2), xmin = min(yrs) - 15, xmax = min(yrs) + 10, ymin = max_val-0.25*max_val, ymax = max_val) 
ggsave("InterCatch.png", path = outdir, plot = p, width = 14, height = 12, units = "in", dpi=300)  
# cp("bootstrap/initial/report/*", "report/")  #intercatch figures stored here


IC_old <- data[grepl("InterCatch", data$variable) & data$country == "Total" ,]
IC_new <- IC_update[IC_update$country == "Total",]

IC_new$variable <- unlist(strsplit(IC_new$variable," update"))
IC_old <- IC_old[IC_old$year < min(IC_new$year),]
IC <- rbind(IC_old,IC_new)
OL <- aggregate(value ~ year + variable, FUN = "sum", data = data[data$variable == "Official Landings" & data$country != "Total",])


segments$variable = "TAC"
p<-ggplot(rbind(IC[,-3],OL), 
       aes(year, value, group = variable, col = variable, fill = variable,width = width)) +
  geom_line() + 
  geom_segment(data = segments, aes(x = x, y = y, xend = xend, yend = yend), size = 1)+
  # facet_grid(variable ~ country, scales = "free_y") +
  theme_bw() +
  theme(legend.position = "top",
        legend.title = element_blank()) +
  scale_x_continuous(n.breaks = 3) +
  ylab("tonnes") +xlab("")
ggsave("InterCatch_Official_TAC.png", path = outdir, plot = p, width = 14, height = 12, units = "in", dpi=300) 

dat <- data[data$variable == "Official Landings" & data$country != "Total",]
setDT(dat)
dat[, prop := value /sum(value), by = year]

p<-ggplot(dat,
       aes(x = year, y = prop, group = country, col = country, fill = country)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(legend.position = "top",
        legend.title = element_blank()) +
  scale_x_continuous(n.breaks = round(length(unique(dat$year))/3)) +
  ylab("relative contribution by country") +xlab("")
ggsave("Official_landings_share_by_country.png", path = outdir, plot = p, width = 14, height = 12, units = "in", dpi=300) 

coverage <- data.table(
  "Year" = 2004:2023,
  "Landings with age" = c(22,20,33,54,66,69,73,77,78,80,81,85,85,72,82,74,82.47,87.98,85,91),
  "Landings with discards" = c(22,27,30,42,30,59,66,57,73,79,73,76,72,79,81,71,53.5,82.01,74,67),
  "Discards with age" = c(74,58,59,53,99.6,52,61,37,93,56,80,60,94,74,73,39,51.96,75.07,63,84)
)

coverage <- melt(coverage,id.vars = "Year")

p<-ggplot(coverage, aes(Year,value, col = variable, group = variable)) +
  geom_line() +
  theme_bw() +
  ylim(c(0,100)) +
  scale_x_continuous(n.breaks = round(length(unique(coverage$Year))/3)) +
  ylab("percentage")+
  xlab("")+
  theme(legend.position = "top",
        legend.title = element_blank())
ggsave("IC_sampled_overview.png", path = outdir, plot = p, width = 14, height = 12, units = "in", dpi=300)

load('model/SAM_diagnostics.RData')
load("data/assessment_inputs.RData", verbose = T)

SAM_fit_sol_7d_last_yr <- get(load("bootstrap/data/previous_assessment/SAM_fit_sol7d_last_year.RData"))

load('model/SAM_fit.RData', verbose = T)


## Numbers-at-age
# landings numbers

ln <- as.data.frame(sam_data$ln)
ln$Year <- 1982:(assyear-1)

ln <- ln %>% 
  gather("Age", "landingN", 1:11)
ln$Age <- as.factor(as.numeric(ln$Age))

p <- ggplot(ln, aes(Year, Age, size = landingN)) +
  theme_bw() + geom_point(colour = "blue4", fill = "blue3", alpha = 0.5) +
  geom_abline(slope = 1, intercept = -c((min(ln$Year)-plus_group):assyear-1), col = "grey10", lty = 3) +
  scale_size(range = c(1, 15)) +
  ylab("Age") + labs(size="LAN numbers-at-age")+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"), legend.text = element_text(size=15), legend.title = element_text(size=15))
ggsave("LNaA.png", path = outdir, plot = p, width = 9, height = 8, units = "in", dpi=300)  



# discard numbers

dn <- as.data.frame(sam_data$dn)
dn$Year <- 1982:(assyear-1)

dn <- dn %>% 
  gather("Age", "discardN", 1:11)
dn$Age <- as.factor(as.numeric(dn$Age))

p <- ggplot(dn, aes(Year, Age, size = discardN)) +
  theme_bw() + geom_point(colour = "yellow4", fill = "yellow3", alpha = 0.5) +
  geom_abline(slope = 1, intercept = -c((min(ln$Year)-plus_group):assyear-1), col = "grey10", lty = 3) +
  scale_size(range = c(1, 15)) +
  ylab("Age") + labs(size="DIS numbers-at-age")+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"), legend.text = element_text(size=15), legend.title = element_text(size=15))
ggsave("DNaA.png", path = outdir, plot = p, width = 9, height = 8, units = "in", dpi=300)  


# catch numbers

cn <- as.data.frame(sam_data$cn)
cn$Year <- 1982:(assyear-1)

cn <- cn %>% 
  gather("Age", "catchN", 1:11)
cn$Age <- as.factor(as.numeric(cn$Age))

p <- ggplot(cn, aes(Year, Age, size = catchN)) +
  theme_bw() + geom_point(colour = "red4", fill = "red3", alpha = 0.5) +
  geom_abline(slope = 1, intercept = -c((min(ln$Year)-plus_group):assyear-1), col = "grey10", lty = 3) +
  scale_size(range = c(1, 15)) +
  ylab("Age") + labs(size = "CATCH numbers-at-age")+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"), legend.text = element_text(size=15), legend.title = element_text(size=15))
ggsave("CNaA.png", path = outdir, plot = p, width = 9, height = 8, units = "in", dpi=300)



# barplot dn + ln

ln <- select(ln, Year, Age, Numbers = landingN)
ln$fraction <- "LAN"


dn <- select(dn, Year, Age, Numbers = discardN)
dn$fraction <- "DIS"

ca <- rbind(dn, ln)

p <- ggplot(ca, aes(Age, Numbers))+geom_bar(stat = "identity", position = "stack", colour = "black", aes(fill = fraction))+facet_wrap(~Year) + theme_bw()+
  scale_fill_manual(values = c(my_colours[5], my_colours[2]))+ylab("Numbers-at-age")+ labs(fill = "Catch fraction") +
  theme(axis.text=element_text(size=15), axis.title=element_text(size=20,face="bold"), legend.text = element_text(size=15), legend.title = element_text(size=15), strip.text = element_text(size=20))
ggsave("NaAbarplot.png", path = outdir, plot = p, width = 14, height = 14, units = "in", dpi=300)


## weight-at-age
# Discards weight

dw <- as.data.frame(sam_data$dw)

dw$Year <- 1982:(assyear-1)

dwplot <- dw %>% 
  gather("Age", "mWaA", 1:11)
head(dwplot)

dwplot$Age <- as.factor(as.numeric(dwplot$Age))


p <- ggplot(dwplot, aes(Year, mWaA))+geom_line(aes(colour = Age),linewidth = 1)+theme_bw()+ggtitle("Discard weight-at-age")+ylab("weight-at-age (kg)")+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"), plot.title = element_text(size=20),legend.text = element_text(size=15), legend.title = element_text(size=15))
ggsave("dwplot.png", path = outdir, plot = p, width = 14, height = 14, units = "in", dpi=300)

dwplot2 <- dwplot %>% 
  filter(Age %in% c(1:5))
p <- ggplot(dwplot2, aes(Year, mWaA))+geom_line(aes(colour = Age),size = 1)+theme_bw()+ylab("weight-at-age (kg)")+ggtitle("Discard weight-at-age")+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"), plot.title = element_text(size=20),legend.text = element_text(size=15), legend.title = element_text(size=15))
ggsave("dwplotage1_5.png", path = outdir, plot = p, width = 14, height = 14, units = "in", dpi=300)


# Landings weight
lw <- as.data.frame(sam_data$lw)

lw$Year <- 1982:(assyear-1)

lwplot <- lw %>% 
  gather("Age", "mWaA", 1:11)


lwplot$Age <- as.factor(as.numeric(lwplot$Age))

p <- ggplot(lwplot, aes(Year, mWaA))+geom_line(aes(colour = Age),linewidth = 1)+theme_bw()+ggtitle("Landings weight-at-age")+ylab("weight-at-age (kg)")+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"), plot.title = element_text(size=20),legend.text = element_text(size=15), legend.title = element_text(size=15))
ggsave("lwplot.png", path = outdir, plot = p, width = 14, height = 14, units = "in", dpi=300)


# Catch weight

cw <- as.data.frame(sam_data$cw)
cw$Year <- 1982:(assyear-1)

cwplot <- cw %>% 
  gather("Age", "mWaA", 1:11)


cwplot$Age <- as.factor(as.numeric(cwplot$Age))


p <- ggplot(cwplot, aes(Year, mWaA))+geom_line(aes(colour = Age),size = 1)+theme_bw()+ggtitle("Catch weight-at-age")+ylab("weight-at-age (kg)")+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"), plot.title = element_text(size=20),legend.text = element_text(size=15), legend.title = element_text(size=15))
ggsave("cwplot.png", path = outdir, plot = p, width = 14, height = 14, units = "in", dpi=300)


## Total weight
# weight * numbers

dwplot$fraction <- "DIS"
lwplot$fraction <- "LAN"

caw <- ca %>% 
  left_join(dwplot, by=c("Year", "Age", "fraction")) %>% 
  left_join(lwplot, by=c("Year", "Age", "fraction")) 

caw$mWaA.x <- ifelse(is.na(caw$mWaA.x), 0, caw$mWaA.x)
caw$mWaA.y <- ifelse(is.na(caw$mWaA.y), 0, caw$mWaA.y)

caw2 <- caw %>% 
  mutate(mWaA = mWaA.x + mWaA.y)

caw3 <- caw2 %>% 
  mutate(weight = Numbers*mWaA)

# discards total weight
daw <- caw3 %>% 
  filter(fraction == "DIS")


p <- ggplot(daw, aes(Year, Age, size = weight)) +
  theme_bw() + geom_point(colour = "yellow4", fill = "yellow3", alpha = 0.5) +
  geom_abline(slope = 1, intercept = -c((min(ln$Year)-plus_group):assyear-1), col = "grey10", lty = 3) +
  scale_size(range = c(1, 11)) +
  ylab("Age") + labs(size="DIS weight-at-age")+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"), legend.text = element_text(size=15), legend.title = element_text(size=15))
ggsave("DWaA.png", path = outdir, plot = p, width = 9, height = 8, units = "in", dpi=300)

# landings total weight
law <- caw3 %>% 
  filter(fraction == "LAN")


p <- ggplot(law, aes(Year, Age, size = weight)) +
  theme_bw() + geom_point(colour = "blue4", fill = "blue3", alpha = 0.5) +
  geom_abline(slope = 1, intercept = -c((min(ln$Year)-plus_group):assyear-1), col = "grey10", lty = 3) +
  scale_size(range = c(1, 11)) +
  ylab("Age") + labs(size="LAN weight-at-age")+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"), legend.text = element_text(size=15), legend.title = element_text(size=15))
ggsave("LWaA.png", path = outdir, plot = p, width = 9, height = 8, units = "in", dpi=300)


# catch total weight
caw4 <- caw3 %>% 
  group_by(Year, Age) %>% 
  summarise(TotW = sum(weight))

p <- ggplot(caw3, aes(Age, weight))+geom_bar(stat = "identity", position = "stack", colour = "black", aes(fill = fraction))+facet_wrap(~Year) + theme_bw()+
  scale_fill_manual(values = c(my_colours[5], my_colours[2]))+ylab("Weight-at-age (kg)")+ labs(fill = "Catch fraction")+
  theme(axis.text=element_text(size=15), axis.title=element_text(size=20,face="bold"), legend.text = element_text(size=15), legend.title = element_text(size=15), strip.text = element_text(size=20))
ggsave("WaAbarplot.png", path = outdir, plot = p, width = 14, height = 14, units = "in", dpi=300)



## Proportion discarded


pdis <- cn %>% 
  left_join(dn, by=c("Year", "Age")) %>% 
  mutate(propdis = Numbers/catchN)
# pdis$propdis <- ifelse(is.na(pdis$propdis),0,pdis$propdis)

pdis$Age <- as.factor(as.numeric(pdis$Age))

p <- ggplot(pdis, aes(Year, propdis))+geom_line(aes(colour = Age),size = 1)+theme_bw()+xlab("Year")+ylab("Proportion discarded (discard numbers/catch numbers)")+
  geom_vline(xintercept = 2004, linetype = "dashed")+ geom_vline(xintercept = 2008, linetype = "dashed")+geom_text(label=pdis$Age, aes(colour=Age))+
  theme(legend.position = "none")+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))
ggsave("Propdis.png", path = outdir, plot = p, width = 14, height = 14, units = "in", dpi=300)


p <- ggplot(pdis, aes(Age, propdis))+geom_boxplot()+theme_bw()+ylab("Proportion discarded")+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))
ggsave("PropdisatAge.png", path = outdir, plot = p, width = 14, height = 14, units = "in", dpi=300)





## Stock weight

sw <- as.data.frame(sam_data$sw)
sw$Year <- 1982:(assyear-1)

swplot <- sw %>% 
  gather("Age", "mWaA", 1:11)


swplot$Age <- as.factor(as.numeric(swplot$Age))

p <- ggplot(swplot, aes(Year, mWaA))+geom_line(aes(colour = Age),size = 1)+theme_bw()+ggtitle("Stock weight-at-age")+ylab("weight-at-age (kg)")+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"), plot.title = element_text(size=20),legend.text = element_text(size=15), legend.title = element_text(size=15))
ggsave("swplot.png", path = outdir, plot= p, width = 14, height = 14, units = "in", dpi=300)

sw <- as.data.frame(sam_data$sw)
sw <- as.data.frame(apply(sw, 2, function(x)x/x[1]))
setDT(sw, keep.rownames = T)
colnames(sw)[1] <- "Year"
sw <- melt(sw)
colnames(sw)[2] <- "Age"

sw$Year <- as.numeric(as.character(sw$Year))
p<-ggplot(sw, aes(Year, value, group = Age, col = Age)) +
  geom_line() +
  geom_smooth() +
  facet_wrap(Age~., ncol = 4) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab("") +
  ylab("scaled Weigth")
ggsave("stock_weights_scaled.png", path = outdir, plot= p, width = 14, height = 14, units = "in", dpi=300)

## Tuning fleets 


BELtun <- as.data.frame(sam_data$tun$`BE-CBT-LPUE													`)
ENGtun <- as.data.frame(sam_data$tun$`UK(E&W)-CBT																	`)
FRAtun <- as.data.frame(sam_data$tun$`FR-COTB-model																	`)
BTStun <- as.data.frame(sam_data$tun$`UK(E&W)-BTS-Q3																	`)
FYtun <- as.data.frame(sam_data$tun$`FR-YFS																		`)
EYtun <- as.data.frame(sam_data$tun$`UK(E&W)-YFS																	`)



# BTS survey
BTStun$Year <- as.numeric(rownames(BTStun))

BTStunplot <- BTStun %>% 
  gather("Age", "index", 1:6)


BTStunplot$Age <- as.factor(as.numeric(BTStunplot$Age))


# ggplot(BTStunplot, aes(Year, index, color=Age))+geom_line(size=1)+theme_bw()+
#   ggtitle("BTS")

# surfeb1 <- feb1 + geom_line(data = BTStunplot, aes(Year, index), size=1, color = "red")+facet_wrap(~Age)+theme_bw()

BTStun2 <- as.data.frame(cbind(do.call("cbind",lapply(BTStun[,1:6],scale)), BTStun$Year))
colnames(BTStun2) <- colnames(BTStun)

BTStunplot2 <- BTStun2 %>% 
  gather("Age", "index", 1:6)

BTStunplot2$Age <- as.factor(as.numeric(BTStunplot2$Age))


# sur <- ggplot(BTStunplot2, aes(Year, index))+geom_line(size=1, color = "red")+facet_wrap(~Age)+theme_bw()



# FRA YFS survey
FYtun$Year <- as.numeric(rownames(FYtun))
FYtunplot <- FYtun %>% 
  gather("Age", "index", 1)
FYtunplot$Age <- as.factor(as.numeric(FYtunplot$Age))

# 
# ggplot(FYtunplot, aes(Year, index, color=Age))+geom_line(size=1)+theme_bw()+
#   ggtitle("FRA YFS")

# fysurfeb1 <- surfeb1 + geom_line(data = FYtunplot, aes(Year, index), size=1, color = "green")+facet_wrap(~Age)+theme_bw()

FYtun2 <- as.data.frame(cbind(scale(FYtun[,1]), FYtun$Year))
colnames(FYtun2) <- colnames(FYtun)

FYtunplot2 <- FYtun2 %>% 
  gather("Age", "index", 1)


FYtunplot2$Age <- as.factor(as.numeric(FYtunplot2$Age))

# fysur <- sur + geom_line(data = FYtunplot2, aes(Year, index), size=1, color = "green")+facet_wrap(~Age)+theme_bw()



# ENG YFS survey

EYtun$Year <- as.numeric(rownames(EYtun))

EYtunplot <- EYtun %>% 
  gather("Age", "index", 1)


EYtunplot$Age <- as.factor(as.numeric(EYtunplot$Age))

# ggplot(EYtunplot, aes(Year, index, color=Age))+geom_line(size=1)+theme_bw()+
#   ggtitle("ENG YFS")

# eyfysurfeb1 <- fysurfeb1 + geom_line(data = EYtunplot, aes(Year, index), size=1, color = "yellow")+facet_wrap(~Age)+theme_bw()

EYtun2 <- as.data.frame(cbind(scale(EYtun[,1]), EYtun$Year))
colnames(EYtun2) <- colnames(EYtun)

EYtunplot2 <- EYtun2 %>% 
  gather("Age", "index", 1)

EYtunplot2$Age <- as.factor(as.numeric(EYtunplot2$Age))
# eyfysur <- fysur + geom_line(data = EYtunplot2, aes(Year, index), size=1, color = "yellow")+facet_wrap(~Age)+theme_bw()

BTStunplot2$index[BTStunplot2$Year>2010 & BTStunplot2$Age %in% c(1:3)] <- NA

colors <- c("ENG BTS" = "red", "FRA YFS" = "green", "ENG YFS"="yellow")
p <- ggplot()+geom_line(data = BTStunplot2, aes(Year, index, color = "ENG BTS"), size=1)+facet_wrap(~Age)+theme_bw()+
  geom_line(data = FYtunplot2, aes(Year, index, color="FRA YFS"), size=1)+
  geom_line(data = EYtunplot2, aes(Year, index, color="ENG YFS"), size=1)+
  labs(y = "scaled index", color = "Legend")+scale_color_manual(values = colors)+
  theme(axis.text=element_text(size=10), axis.title=element_text(size=20,face="bold"), 
        legend.text = element_text(size=15), legend.title = element_text(size=15), strip.text = element_text(size=20))

ggsave("similarityplot_sur.png", path = outdir, plot = p, width = 14, height = 14, units = "in", dpi=300)






# Belgian commercial beam trawl fleet
BELtun$Year <- as.numeric(rownames(BELtun))
BELtun<- BELtun %>% 
  select(index = "-1", Year)

# ggplot(BELtun, aes(Year, index))+geom_line(size=1)+theme_bw()+
#   ggtitle("BEL_CBT")

BELtun2 <- as.data.frame(cbind(scale(BELtun[,1]), BELtun$Year))
colnames(BELtun2) <- colnames(BELtun)

# ENG commercial beam trawl fleet
ENGtun$Year <- as.numeric(rownames(ENGtun))

ENGtun<- ENGtun %>% 
  select(index = "-1", Year)


# ggplot(ENGtun, aes(Year, index))+geom_line(size=1)+theme_bw()+
#   ggtitle("ENG_CBT")

ENGtun2 <- as.data.frame(cbind(scale(ENGtun[,1]), ENGtun$Year))
colnames(ENGtun2) <- colnames(ENGtun)

# FRA commercial otter trawl fleet
FRAtun$Year <- as.numeric(rownames(FRAtun))

FRAtun<- FRAtun %>% 
  select(index = "-1", Year)


# ggplot(FRAtun, aes(Year, index))+geom_line(size=1)+theme_bw()+
#   ggtitle("FRA_COTB")

FRAtun2 <- as.data.frame(cbind(scale(FRAtun[,1]), FRAtun$Year))
colnames(FRAtun2) <- colnames(FRAtun)

# alle data aan elkaar hangen

colors2 <- c("BEL CBT"="blue", "ENG CBT"="red", "FRA COTB" = "darkgreen")
p <- ggplot()+
  geom_line(data = BELtun2, aes(Year, index, color="BEL CBT"), size=1)+
  geom_line(data = ENGtun2, aes(Year, index, color="ENG CBT"), size=1)+
  geom_line(data = FRAtun2, aes(Year, index, color="FRA COTB"), size=1)+theme_bw()+
  labs(y = "scaled index", color = "Legend")+scale_color_manual(values = colors2)+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"), legend.text = element_text(size=15), legend.title = element_text(size=15))
ggsave("similarityplot_biomass.png", path = outdir, plot = p, width = 14, height = 14, units = "in", dpi=300)



# Internal consistency plots--------------------------------

tuns <- read.ices("bootstrap/data/assessment_input/tun.txt")
bts_all <- as.data.frame(tuns$`UK(E&W)-BTS-Q3																	`)

# full series
FLIndex_ENGBTS_all <- FLIndex(index = FLQuant(c(t(bts_all)), dim=c(6,nrow(BTStun)),
                                          quant="age", units="numbers",dimnames=list(age=1:6, year=as.numeric(rownames(bts_all)))))
plot(FLIndex_ENGBTS_all, type="internal", log.scales = T)
SavePlot0("ENGBTSinternal_full",11,11)

FLIndex_ENGBTS_2009 <- FLIndex(index = FLQuant(c(t(bts_all[as.numeric(rownames(bts_all))<2010,])), 
                                              dim=c(6,nrow(BTStun[as.numeric(rownames(bts_all))<2010,])),
                                              quant="age", units="numbers",
                                              dimnames=list(age=1:6, 
                                                            year=as.numeric(rownames(bts_all))[as.numeric(rownames(bts_all))<2010])))
plot(FLIndex_ENGBTS_2009, type="internal", log.scales = T)
SavePlot0("ENGBTSinternal_to_2009",6.5,6.5)



FLIndex_ENGBTS_2010 <- FLIndex(index = FLQuant(c(t(bts_all[as.numeric(rownames(bts_all))>=2010,])), 
                                               dim=c(6,nrow(BTStun[as.numeric(rownames(bts_all))>=2010,])),
                                               quant="age", units="numbers",
                                               dimnames=list(age=1:6, 
                                                             year=as.numeric(rownames(bts_all))[as.numeric(rownames(bts_all))>=2010])))
plot(FLIndex_ENGBTS_2010, type="internal", log.scales = T)
SavePlot0("ENGBTSinternal_from_2010",6.5,6.5)

bts_all_sc <- as.data.frame(apply(bts_all,2, function(x)scale(log(x))))
rownames(bts_all_sc) <- rownames(bts_all)
setDT(bts_all_sc, keep.rownames = T)
colnames(bts_all_sc)[1]<-"Year"
bts_all_sc <- melt(bts_all_sc)
colnames(bts_all_sc)[2] <- "Age"

bts_all_sc$cohort <- as.numeric(as.character(bts_all_sc$Year))-as.numeric(as.character(bts_all_sc$Age))

p<-ggplot(bts_all_sc, aes(cohort, value, group = as.factor(Age), col= as.factor(Age))) +
  geom_line() +
  theme_bw() +
  theme(legend.title = element_blank()) +
  ylab("scaled index") +
  xlab("yearclass")
ggsave("cohort_tracking_scaled.png", path = outdir, plot = p, width = 14, height = 8, units = "in", dpi=300)
# ENG BTS
FLIndex_ENGBTS <- FLIndex(index = FLQuant(c(t(BTStun[,-7])), dim=c(6,nrow(BTStun)),
                                          quant="age", units="numbers",dimnames=list(age=1:6, year=as.numeric(rownames(BTStun)))))

plot(FLIndex_ENGBTS, type="internal", log.scales = T)
SavePlot0("ENGBTSinternal",11,11)

FLIndex_ENGBTS1 <- FLIndex(index = FLQuant(c(t(BTStun[as.numeric(rownames(BTStun))<=2010,-7])), dim=c(6,nrow(BTStun[as.numeric(rownames(BTStun))<=2010,])),
                                           quant="age", units="numbers",dimnames=list(age=1:6, year=as.numeric(rownames(BTStun))[as.numeric(rownames(BTStun))<=2010])))

FLIndex_ENGBTS2 <- FLIndex(index = FLQuant(c(t(BTStun[as.numeric(rownames(BTStun))>2010,-c(1,2,3,7)])), dim=c(3,nrow(BTStun[as.numeric(rownames(BTStun))>2010,])),
                                           quant="age", units="numbers",dimnames=list(age=4:6, year=as.numeric(rownames(BTStun))[as.numeric(rownames(BTStun))>2010])))

plot(FLIndex_ENGBTS1, type="internal", log.scales = T)
SavePlot0("ENGBTSinternal_up_to_2010",11,11)

plot(FLIndex_ENGBTS2, type="internal", log.scales = T)
SavePlot0("ENGBTSinternal_from_2011",11,11)


# discard rate
png(file.path(outdir,"discard_rate.png"),
    width=11,height=11,units='in',res=300, bg = "white")
plot(x = as.numeric(rownames(sam_data$cn)),
     y = rowSums(sam_data$dn * sam_data$dw)/rowSums(sam_data$cn * sam_data$cw), 
     pch = 16, type = "b", yaxt = "n", ylab = "discard rate",xlab="");axis(2,las=2)
grid()
dev.off()



#------ assessment

modeltable(SAM_fit_sol_7d)
# log(L) #par      AIC
# M1 -476.9038   26 1005.808

# snelle blik - verkennende plotjes
png(file.path(outdir,"SAMsum.png"),
    width=11,height=11,units='in',res=300, bg = "white")
plot(SAM_fit_sol_7d)                           # eerste blik op SSB, F en R
dev.off()#SavePlot0("SAMsum",11,11)

png(file.path(outdir,"fitplot_catch.png"),
    width=11,height=11,units='in',res=300, bg = "white")
fitplot(SAM_fit_sol_7d, fleets = 1)                        # hoe goed fitten de observaties het model? 
dev.off()#SavePlot0("fitplot_catch",11,11)

png(file.path(outdir,"fitplot_commercial.png"),
    width=11,height=11,units='in',res=300, bg = "white")
fitplot(SAM_fit_sol_7d, fleets = which(SAM_fit_sol_7d$conf$keyBiomassTreat == 2))                        # hoe goed fitten de observaties het model? 
dev.off()#SavePlot0("fitplot_commercial",11,5.5)

png(file.path(outdir,"fitplot_survey.png"),
    width=11,height=11,units='in',res=300, bg = "white")
fitplot(SAM_fit_sol_7d, fleets = which(SAM_fit_sol_7d$conf$keyBiomassTreat == -1)[-1])                        # hoe goed fitten de observaties het model? 
dev.off()#SavePlot0("fitplot_survey",11,11)

                     # recruits
png(file.path(outdir,"srplot.png"),
    width=11,height=11,units='in',res=300, bg = "white")
srplot(SAM_fit_sol_7d)                         # stock recruitment plot
dev.off()#SavePlot0("srplot",11,11)

# cor(faytable(SAM_fit_sol_7d))                  # hier kijken welke F at age gecorreleerd zijn -> conf$keyLogFsta
# qtable(SAM_fit_sol_7d)
png(file.path(outdir,"fsel.png"),
    width=11,height=11,units='in',res=300, bg = "white")
fselectivityplot(SAM_fit_sol_7d)               # ingeschatte selectivity at age (F) ??? 
dev.off()#SavePlot0("fsel",11,11)

png(file.path(outdir,"dataplot.png"),
    width=11,height=11,units='in',res=300, bg = "white")
dataplot(SAM_fit_sol_7d)
dev.off()#SavePlot0("dataplot",11,11)

png(file.path(outdir,"parplot.png"),
    width=11,height=11,units='in',res=300, bg = "white")
parplot(SAM_fit_sol_7d)
dev.off()#SavePlot0("parplot",11,11)

# F at age plotten met ggplot
fatage <- as.data.frame(faytable(SAM_fit_sol_7d))
fatage$Year <- 1982:(assyear-1)
fatage <- fatage %>% 
  gather("Age", "FatAge", 1:11)


fatage$Age <- as.factor(as.numeric(fatage$Age))
ggplot(fatage, aes(Year, FatAge))+
  geom_line(aes(color = Age))+
  theme_bw()+
  geom_text(label=fatage$Age, aes(colour = Age))+
  theme(legend.position = "none")
SavePlot0("FatAge",11,11)



png(file.path(outdir,"SummarySAM_run.png"),
    width=11,height=11,units='in',res=300, bg = "white")
# make summary plot of this run
par(mfrow = c(2,2),
    mar = c(4,4,2,2))

y_max <- max(catchtable(SAM_fit_sol_7d)[,"Estimate"])
# plot(catxsa2$year,catxsa2$caton, col ="red", lwd = 2, type = "l", ylim = c(0, y_max), ylab = "Catch (tonnes)",yaxt="n",xlab="")
# axis(2, las = 2)
catchplot(SAM_fit_sol_7d)
grid(col = "grey60", lty = 2)

y_max <- max(ssbtable(SAM_fit_sol_7d)[,"Estimate"])
# plot(ssbxsa2$year,ssbxsa2$biomass, col ="red", lwd = 2, type = "l", ylim = c(0, y_max), ylab = "SSB (tonnes)",yaxt="n",xlab="")
axis(2, las = 2)
ssbplot(SAM_fit_sol_7d)
grid(col = "grey60", lty = 2)

y_max <- max(fbartable(SAM_fit_sol_7d)[,"Estimate"])
# plot(fxsa2$year,fxsa2$fbr, col ="red", lwd = 2, type = "l", ylim = c(0, y_max), ylab = "Fbar (age 3-7)",yaxt="n",xlab="")
axis(2, las = 2)
fbarplot(SAM_fit_sol_7d)
grid(col = "grey60", lty = 2)

y_max <- max(rectable(SAM_fit_sol_7d)[,"Estimate"])
# plot(recxsa2$year,recxsa2$NumRec, col ="red", lwd = 2, type = "l", ylim = c(0, y_max), ylab = "Recruits",yaxt="n",xlab="")
axis(2, las = 2)
recplot(SAM_fit_sol_7d)
grid(col = "grey60", lty = 2)
dev.off()
# par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
# plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
# legend('top',c('SAM_XSAsetup','XSAbase'),lty=c(1,1),lwd=2,col=c(1,10),bty='n',xpd = TRUE, horiz = TRUE)

# SavePlot0("SummarySAM_run",11,11)

png(file.path(outdir,"qplot.png"),
    width=11,height=11,units='in',res=300, bg = "white")
qtableplot(qtable(SAM_fit_sol_7d))
dev.off()
# SavePlot0("qplot",11,11)




# model diagnostics

res   <- diagnostics$OSA_residuals
resp  <- diagnostics$process_residuals
retro <- diagnostics$retrospective_analysis
lo    <- diagnostics$leave_one_out_fits


# residuals
png(file.path(outdir,"Residuals.png"),
    width=11,height=11,units='in',res=300, bg = "white")
plot(res)
dev.off()#SavePlot0("Residuals",11,11)

png(file.path(outdir,"summaryResiduals.png"),
    width=11,height=11,units='in',res=300, bg = "white")
plot(res, type="summary")
dev.off()#SavePlot0("summaryResiduals",11,11)

res$sign      <- ifelse(res$residual>0,"positive","negative")
res$fleetname <- attributes(res)$fleetNames[res$fleet]
class(res) <- "list"
res <- as.data.frame(res)

ggplot(res[!is.na(res$observation),], aes(year, residual, col = sign)) +
  geom_point(size = .5) +
  geom_hline(yintercept = 0, linewidth = .2) +
  theme_bw() +
  facet_grid(fleetname ~ age) +
  theme(legend.position = "none",
        strip.text = element_text(size = 6))
SavePlot0("OSA_residuals_yr",11,11)


# joint sample residuals or process residuals 
png(file.path(outdir,"ProcResiduals.png"),
    width=11,height=11,units='in',res=300, bg = "white")
plot(resp)
dev.off()#SavePlot0("ProcResiduals",11,11)

png(file.path(outdir,"summaryProcResiduals.png"),
    width=11,height=11,units='in',res=300, bg = "white")
plot(resp, type="summary")
dev.off()#SavePlot0("summaryProcResiduals",11,11)

# retro
png(file.path(outdir,"Retro.png"),
    width=11,height=11,units='in',res=300, bg = "white")
plot(retro)
dev.off()#SavePlot0("Retro",11,11)



# leave-one-out
png(file.path(outdir,"leaveoneout.png"),
    width=11,height=11,units='in',res=300, bg = "white")
plot(lo)
dev.off()#SavePlot0("leaveoneout",11,11)




## plot observational variance - observation error
sdplot<-function(fit){
  opar <- par()
  cf <- fit$conf$keyVarObs
  fn <- attr(fit$data, "fleetNames")
  ages <- fit$conf$minAge:fit$conf$maxAge
  pt <- partable(fit)
  sd <- unname(exp(pt[grep("logSdLogObs",rownames(pt)),1]))
  v<-cf
  v[] <- c(NA,sd)[cf+2]
  res<-data.frame(fleet=fn[as.vector(row(v))],name=paste0(fn[as.vector(row(v))]," age ",ages[as.vector(col(v))]), sd=as.vector(v))
  res<-res[complete.cases(res),]
  o<-order(res$sd)
  res<-res[o,]
  par(mar=c(13,6,2,1))
  barplot(res$sd, names.arg=res$name,las=2, col=colors()[as.integer(as.factor(res$fleet))*10], ylab="SD"); box()
  suppressWarnings(par(opar)) 
}
png(file.path(outdir,"sdplot_by_datastream.png"))
sdplot(SAM_fit_sol_7d)
dev.off()

png(file.path(outdir,"corplot.png"),
    width=11,height=11,units='in',res=300, bg = "white")
corplot(SAM_fit_sol_7d)
dev.off()#SavePlot0("corplot",11,11)



# ## Simulation analysis
# sim_fit <- simstudy(SAM_fit_sol_7d, nsim = 35, ncores = 4) # moet lang lopen ---- 2022: lukte niet met >35 simulations
# save(sim_fit,file=file.path(outdir,"sol7d_sim_fit.Rdata"))
# plot(sim_fit)
# a <- SavePlot0("Simfit",11,11)
# 
# ## Jitter analysis
# jit_fit <- jit(SAM_fit_sol_7d, nojit = 50, ncores = 4) # moet lang lopen
# save(jit_fit,file=file.path(outdir,"sol7d_jit_fit.Rdata"))
# plot(jit_fit)
# a <- SavePlot0("Jitfit",11,11)


write.csv(rectable(SAM_fit_sol_7d), file = file.path(outdir,"rectable.csv"))
write.csv(ssbtable(SAM_fit_sol_7d), file = file.path(outdir,"ssbtable.csv"))
write.csv(fbartable(SAM_fit_sol_7d), file = file.path(outdir,"ftable.csv"))


add_legend <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
              mar=c(0, 2.5, 2, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}

png(file.path(outdir,"biomass_proportion_by_age.png"),
    width=14,height=8,units='in',res=300, bg = "white")
barplot(t(ntable(SAM_fit_sol_7d)*sam_data$sw/rowSums(ntable(SAM_fit_sol_7d)*sam_data$sw)), col = terrain.colors(11), 
        yaxt = "n"); axis(2, las = 2, at = seq(0,1,0.1))
# abline(h = seq(0.1,0.9,0.1), col = "black", lty = "dashed")
for(i in seq(0.1,0.9,0.1))segments(x0 = 0.2, y0 = i, x1 = 49, y1 = i, col = "grey40", lty = 3, lwd = 2)
add_legend("top", legend = paste0("age ",1:11), col = terrain.colors(11), pch = 15, horiz = T, cex = .828, bty = "n")
dev.off()
# SavePlot0("biomass_proportion_by_age",14,8)

png(file.path(outdir,"biomass_by_age.png"),
    width=14,height=8,units='in',res=300, bg = "white")
barplot(t(ntable(SAM_fit_sol_7d)*sam_data$sw), col = terrain.colors(11), yaxt = "n"); axis(2, las = 2)
add_legend("top", legend = paste0("age ",1:11), col = terrain.colors(11), pch = 15, horiz = T, cex = .828, bty = "n")
dev.off()# SavePlot0("biomass_by_age",14,8)


flt_idx <- which(attributes(SAM_fit_sol_7d$data)$fleetNames %in% c("Residual catch"))

estimated_catches <- matrix(exp(SAM_fit_sol_7d$rep$predObs[SAM_fit_sol_7d$data$aux[,"fleet"] == flt_idx]),nrow = 11)

png(file.path(outdir,"catch_by_age.png"),
    width=14,height=8,units='in',res=300, bg = "white")
barplot(t(t(estimated_catches)*sam_data$cw), col = terrain.colors(11), yaxt = "n"); axis(2, las = 2)
add_legend("top", legend = paste0("age ",1:11), col = terrain.colors(11), pch = 15, horiz = T, cex = .828, bty = "n")
dev.off()#SavePlot0("catch_by_age",14,8)

png(file.path(outdir,"relative_catch_by_age.png"),
    width=14,height=11,units='in',res=300, bg = "white")
barplot(t(t(estimated_catches)*sam_data$cw/rowSums(t(estimated_catches)*sam_data$cw)), col = terrain.colors(11), yaxt = "n"); axis(2, las = 2)
add_legend("top", legend = paste0("age ",1:11), col = terrain.colors(11), pch = 15, horiz = T, cex = .828, bty = "n")
dev.off()# SavePlot0("relative_catch_by_age",14,8)


# compare with previous assessment model

add_leg <- function(fit.names,...){
  colSet  <- c("#332288"  , "#88CCEE"  , "#44AA99"  , "#117733"  , "#999933"  , "#DDCC77"  , "#661100"  , "#CC6677"  , "#882255"  , "#AA4499")  
  idxfrom <- 1
  legend(legend = fit.names, 
         col=colSet[seq_along(fit.names)], ...)
}


fit.set <- list(SAM_fit_sol_7d,SAM_fit_sol_7d_last_yr)
class(fit.set) <- "samset"
fit.names <- c("2023_assessment","2023_assessment")

png(file.path(outdir,"Summaryruns.png"),
    width=11,height=11,units='in',res=300, bg = "white")

par(mfrow = c(2,2),
    mar = c(4,4,2,2))
catchplot(fit.set,obs.show = F)

add_leg(fit.names,"bottomleft",lwd = 3, ncol=2, bty="n",cex = 0.8)
ssbplot(fit.set)
add_leg(fit.names,"bottomleft",lwd = 3, ncol=2, bty="n",cex = 0.8)
fbarplot(fit.set)
add_leg(fit.names,"bottomleft",lwd = 3, ncol=2, bty="n",cex = 0.8)
recplot(fit.set)
add_leg(fit.names,"bottomleft",lwd = 3, ncol=2, bty="n",cex = 0.8)
dev.off()#SavePlot0("Summaryruns",11,11)



# --- presentation

cp("bootstrap/software/functions/sol27.7d data and assessment presentation/*", ".") 

# render('presentation.md',
#        output_file = 'report/sol.27.d_assessment-WGNSSK2024.pdf')

# --- report

# render("report.Rmd", output_dir="report", clean=TRUE,
#        output_file="WGNSSK 2023_Section xx_Sole in Subdivision 7d.docx")
# 
