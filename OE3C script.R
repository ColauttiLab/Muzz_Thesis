# Simplified/clean theme for plotting
theme_simple <- function (base_size = 12, base_family = "") {
  theme_classic(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text = element_text(colour = "black"),
      axis.title.x = element_text(size=18),
      axis.text.x = element_text(size=12),
      axis.title.y = element_text(size=18,angle=90),
      axis.text.y = element_text(size=12),
      axis.ticks = element_blank(),
      panel.background = element_rect(fill="white"),
      panel.border = element_blank(),
      plot.title=element_text(face="bold", size=24),
      legend.position="none"
    )  
}


##have library tidyverse
library(tidyverse)
library(dplyr)
library(ggplot2)
library(zoo)
library(lubridate)
##read in data
ldata<-read.csv("oe3c_dat.csv", header=T)

Visitdata<-ldata
Visitdata$date<-as.factor(Visitdata$date)

date<-c(1:16)
visitdate<-c("2016/06/10","2016/06/14","2016/06/18","2016/06/22","2016/06/26","2016/06/30","2016/07/04","2016/07/12","2016/07/16","2016/07/20","2016/07/24", "2016/08/01", "2016/08/05", "2016/08/09", "2016/08/13", "2016/08/17")

Vdat<-as.data.frame(cbind(date,visitdate))
Vdat
Visitdata<- left_join(Visitdata, Vdat)
Visitdata$yday <- yday(Visitdata$visitdate)


## calculate time to flowering, flowering date (relative to start), flowering duration 
fdata<-as.data.frame(unique(Visitdata$indiv))
names(fdata)<-"indiv"
fdata$ftime<-NA ## time to flowering
fdata$fday<-NA ## day of flowering
fdata$fdur<-NA ## duration of flowering

for (ind in unique(Visitdata$indiv)) {
  # subset all rows for this individual
  individualdata<-Visitdata[Visitdata$indiv==ind,]
  # subset out rows with no measurements
  individualdata<-individualdata[!is.na(individualdata$node),]
  # make indicator for days with a flower measurement
  individualdata$indicatorfstart<-ifelse(individualdata$flower>0, TRUE, FALSE)
  # find the first time that flowers is positive 
  floweringday<-min(which(individualdata$indicatorfstart == TRUE))
  # ftime is time to flowering, the day of first flower growth minus the start day of measurement
  fdata[fdata$indiv==ind,]$ftime <- individualdata$yday[floweringday]- individualdata$yday[1]
  # fday is the day that flower growth first appears
  fday<- individualdata$yday[floweringday]
  fdata[fdata$indiv==ind,]$fday<- fday
  # subset data for only the visits with flowering, fruiting, or buds
  individualdata<-individualdata[individualdata$fruit>0 | individualdata$flower >0 | individualdata$bud>0,]
  # new indicator where bud and flowers both equal zero, there will only be fruit measurements
  individualdata$indicatorfend<-ifelse(individualdata$bud==0 & individualdata$flower==0, TRUE, FALSE)
  # find the first day with only fruit measurements, no flowers or buds, taken as maximum from last visit
  durend<- max(which(individualdata$indicatorfend == FALSE))
  # duration of flowering is the last day of flowering minus the first day of flower growth (as calculated above)
  fdata[fdata$indiv==ind,]$fdur<-individualdata$yday[durend] - fday
}


fdata<-na.omit(fdata)
fdata$trt<-gsub("[0-9]","",fdata$indiv)

ggplot(fdata, aes(trt, ftime, colour = trt)) +
  geom_boxplot()

#ggplot(fdata, aes(trt, fdur, colour = trt)) +
# geom_boxplot()

test<-fdata[order(fdata$trt, fdata$ftime),]
test$cumulative<-c(1:109, 1:129)
test<-test[order(test$trt, decreasing=TRUE),]

test2 <- test %>% 
  filter(fday <230)

#### FLOWERING TIME

ggplot(test2, aes(x=fday, y=cumulative, color=trt)) +
  geom_smooth() +
  theme_simple() +
  xlab("\nDay") +
  ylab("# of flowering plants\n")

mod1 <- lm(fday ~ cumulative + trt, data = test2)
anova(mod1)


#ggplot(test2, aes(fday, colour = trt)) +
# geom_density()

#ggplot() +
# geom_density(data=subset(test, trt=="P"), aes(x=fday), alpha=0.9, size=5, fill= "#08db21", color="#08db21") +
#geom_density(data=subset(test, trt=="C"), aes(x=fday), alpha=0.5, size=1, fill = "#4286f4", color = "#4286f4") +
#theme(text=element_text(size=25))

#ggsave(densityplot, filename="Histogram.png", width=8, height=9, dpi=300)



ldata$sec_ap_un<-ldata$sec_branch-ldata$sec_ap1
ldata$sec_ap_pct<-ldata$sec_ap1
ldata$visitdate<-NA
for(i in 1:nrow(Vdat)){
  ldata$visitdate[ldata$date==Vdat$date[i]]<-paste0(Vdat$visitdate[i])
}
ldata$visitdate<-as.factor(ldata$visitdate)

apsum<-aggregate(cbind(sec_branch,sec_ap_pct) ~ trt + visitdate,data=ldata,FUN=mean,na.action=na.omit)
# Calculate SE for 2o branches
sec_branch_sd<-aggregate(sec_branch ~ trt + visitdate,data=ldata,FUN=sd,na.action=na.omit)
names(sec_branch_sd)[3]<-"sec_branch_sd"
sec_branch_N<-aggregate(sec_branch ~ trt + visitdate,data=ldata,FUN=length,na.action=na.omit)
names(sec_branch_N)[3]<-"sec_branch_N"
# Calculate SE for proportion damaged 2o branches
sec_ap_pct_sd<-aggregate(sec_ap_pct ~ trt + visitdate,data=ldata,FUN=sd,na.action=na.omit)
names(sec_ap_pct_sd)[3]<-"sec_ap_pct_sd"
sec_ap_pct_N<-aggregate(sec_ap_pct ~ trt + visitdate,data=ldata,FUN=length,na.action=na.omit)
names(sec_ap_pct_N)[3]<-"sec_ap_pct_N"

# Merge everything
apsum<-merge(apsum,sec_branch_sd,by=c("trt","visitdate"))
apsum<-merge(apsum,sec_branch_N,by=c("trt","visitdate"))
apsum$sec_branch_se<-apsum$sec_branch_sd/sqrt(apsum$sec_branch_N - 1)

apsum<-merge(apsum,sec_ap_pct_sd,by=c("trt","visitdate"))
apsum<-merge(apsum,sec_ap_pct_N,by=c("trt","visitdate"))
apsum$sec_ap_pct_se<-apsum$sec_ap_pct_sd/sqrt(apsum$sec_ap_pct_N - 1)

apsum$visitdate<-as.Date(apsum$visitdate)


pd <- position_dodge(width = 2)

### SECONDARY BRANCHES
ggplot(aes(x=visitdate,y=sec_branch,colour=trt),data=apsum)+
  geom_errorbar(aes(ymin=sec_branch-1.96*sec_branch_se, ymax=sec_branch+1.96*sec_branch_se),position=pd, width=.1)+geom_point(position=pd) +
  theme_simple() +
  xlab("\nDay") +
  ylab("# of secondary branches\n")


#### CUMULATIVE REP BRANCHES

repsum<-aggregate(cumulative ~ trt + visitdate,data=Visitdata,FUN=mean,na.action=na.omit)

sec_rep_sd<-aggregate(cumulative ~ trt + visitdate,data=Visitdata,FUN=sd,na.action=na.omit)
names(sec_rep_sd)[3]<-"sec_rep_sd"
sec_rep_N<-aggregate(cumulative ~ trt + visitdate,data=Visitdata,FUN=length,na.action=na.omit)
names(sec_rep_N)[3]<-"sec_rep_N"
# Merge everything
repsum<-merge(repsum,sec_rep_sd,by=c("trt","visitdate"))
repsum<-merge(repsum,sec_rep_N,by=c("trt","visitdate"))
repsum$sec_rep_se<-repsum$sec_rep_sd/sqrt(repsum$sec_rep_N - 1)

#### PLOT SECONDARY REP BRANCHES OVER DATE
ggplot(aes(x=visitdate,y=cumulative,colour=trt),data=repsum)+
  geom_errorbar(aes(ymin=cumulative-1.96*sec_rep_se, ymax=cumulative+1.96*sec_rep_se),position=pd, width=.1)+geom_point(position=pd) +
  theme_simple()+
  #theme(text=element_text(size=30))+
  xlab("\nDay") +
  ylab("# of secondary rep. branches\n")

repsum$visitdate<-as.Date(repsum$visitdate)
mod1<-lm(cumulative~trt+visitdate, data=repsum)
anova(mod1)
str(repsum)

### APICAL DAMAGE OVER DAYS
ggplot(aes(x=visitdate,y=sec_ap_pct,colour=trt),data=apsum)+
  geom_errorbar(aes(ymin=sec_ap_pct-1.96*sec_ap_pct_se, ymax=sec_ap_pct+1.96*sec_ap_pct_se),position=pd, width=.1)+geom_point(position=pd) +
  theme_simple()+
  xlab("\nDay") +
  ylab("Proportion of damaged apical meristems\n")



#### DAMAGE
View(ldata)
# Calculate SE for dmg
dmg_sd<-aggregate(dmg_index ~ trt + visitdate,data=ldata,FUN=sd,na.action=na.omit)
names(dmg_sd)[3]<-"dmg_sd"
dmg_N<-aggregate(dmg_index ~ trt + visitdate,data=ldata,FUN=length,na.action=na.omit)
names(dmg_N)[3]<-"dmg_N"

dmgsum<-aggregate(dmg_index ~ trt + visitdate,data=ldata,FUN=mean,na.action=na.omit)
dmgsum<-merge(dmgsum,dmg_sd,by=c("trt","visitdate"))
dmgsum<-merge(dmgsum,dmg_N,by=c("trt","visitdate"))
dmgsum$dmg_se<-dmgsum$dmg_sd/sqrt(dmgsum$dmg_N - 1)

### PLOT for dmg
dmgsum$visitdate<-as.Date(dmgsum$visitdate)
pd <- position_dodge(width = 2)

ggplot(aes(x=visitdate,y=dmg_index,colour=trt),data=dmgsum)+
  geom_errorbar(aes(ymin=dmg_index-1.96*dmg_se, ymax=dmg_index+1.96*dmg_se),position=pd, width=.1)+geom_point(position=pd) +
  theme_simple() +
  xlab("\nDay") +
  ylab("Proportion of leaf damage\n") 





seed_dat <- read.csv("seeds_per_plant.csv", header=T)

##### SEEDS PER PLANT

ggplot(seed_dat, aes(trt, prim_seeds_per_plant, fill = trt)) +
  geom_boxplot() +
  theme_simple() +
  xlab("") +
  ylab("Average seeds per plant\n")

mod1 <-lm(log(prim_seeds_per_plant +1) ~ trt, data=seed_dat)
anova(mod1)

##### SEEDS PER FRUIT #####

ggplot(seed_dat, aes(trt, prim_avgseed, fill = trt)) +
  geom_boxplot() +
  theme_simple() +
  xlab("\nTreatment") +
  ylab("Average seeds per fruit\n")


##### FRUIT PER PLANT #####

ggplot(seed_dat, aes(trt, prim_nfrts, fill = trt)) +
  geom_boxplot() +
  theme_simple() +
  xlab("") +
  ylab("Average fruits per plant\n")


##### AVERAGE SEEDS PER FRUIT IN SEED POL
dat<-read.csv("SeedPolData_oe3c.csv",header=T)

qplot(plant_trt:trt,seedv,geom="boxplot",fill = plant_trt, data=dat) +
  theme_simple() +
  xlab("") +
  ylab("Average seeds per fruit\n")

mod1 <-lm(seedv~plant_trt*trt,data=dat)
anova(mod1)





##### REGRESSION

dat2 <- ldata %>% 
  filter(date==16)


dat3 <- 
