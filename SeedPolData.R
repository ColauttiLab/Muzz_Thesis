# Load Packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)

# Load and view data
dat<-read.csv("SeedPolData.csv",header=T)
View(dat)

# Inspect variables
summary(dat)
str(dat)

# Fix coding for trt variable
levels(dat$trt)
dat$trt[dat$trt==""]<-"UNP"
dat$trt[dat$trt=="OP "]<-"OP"
dat$trt<-factor(dat$trt) # Remove erroneous factors

# Need to make factor: (pop)ulation, bulk, stem, (obs)erver, and (fr)ui(t) set
dat$pop <- as.factor(dat$pop)
dat$bulk <- as.factor(dat$bulk)
dat$stem <- as.factor(dat$stem)
dat$obs <- as.factor(dat$obs)
dat$frt_set <- as.factor(dat$frt_set)

  
# A quick look at treatment means
qplot(plant_trt:trt,seedv,geom="boxplot",data=dat) +
  geom_signif(comparisons = list(c("C:OP", "P:HP")), map_signif_level=TRUE) 
  

# Full model
trtmixmod<-lmer(seedv~plant_trt*trt*pop+(1|plantid:pop),data=dat)
summary(trtmixmod)
anova(trtmixmod)

# Test 3-way effect
trtmixmod2<-lmer(seedv~plant_trt*trt*pop-plant_trt:trt:pop+(1|plantid:pop),data=dat)
anova(trtmixmod2,trtmixmod)

# Test 2-way effects of population
trtmixmod3<-lmer(seedv~plant_trt*trt*pop-plant_trt:trt:pop-trt:pop+(1|plantid:pop),data=dat)
anova(trtmixmod3,trtmixmod2)
trtmixmod4<-lmer(seedv~plant_trt*trt*pop-plant_trt:trt:pop-plant_trt:pop+(1|plantid:pop),data=dat)
anova(trtmixmod4,trtmixmod2)

# Test 2-way effects of trt:plant_trt
trtmixmod5<-lmer(seedv~plant_trt*trt+(1|plantid:pop),data=dat)
trtmixmod6<-lmer(seedv~plant_trt+trt+(1|plantid:pop),data=dat)
anova(trtmixmod6,trtmixmod5)

## ANALYSIS SUMMARY
# No significant interactions with population
# Significant interaction between pesticide treatment and pollination treatment

## Test residuals of best model:
# Test for normality of residuals
shapiro.test(residuals(trtmixmod5))

# Plot residuals from model against prediction
qplot(predict(trtmixmod5),residuals(trtmixmod5))
# plot residuals against observed values
qplot(predict(trtmixmod5),predict(trtmixmod5)+residuals(trtmixmod5))
# histogram plot of residuals
qplot(residuals(trtmixmod5))

# histogram plot of observed values
qplot(predict(trtmixmod5)+residuals(trtmixmod5))


# we want to find the mean for all (seed) (v)iable and (seed) (inv)iable of each plant. We will look at (H)and (P)ollinated flowers first

datHP <- dat %>%
  unite(id, plantid, pop, sep=".", remove = FALSE) %>% # unite plantid and pop into a new var "id", seperated by "."
  arrange(id) %>% # sort from lowest to highest id
  filter(bulk == 0, trt == "HP") # bulk == 0 are individual fruits 

H_meanv <- aggregate(seedv ~ id + trt, FUN = mean, data=datHP) # (H)and pollinated (mean) (v)iable
H_meaninv <- aggregate(seedinv ~ id, FUN = mean, data =datHP) # (H)and pollinated (mean) (inv)iable
HP_mean <- full_join(H_meanv, H_meaninv, by = "id")

HP_mean$seedratio <- HP_mean$seedv/(HP_mean$seedv + HP_mean$seedinv) # get ratio between viable and inviable per plant

# repeat top code for (O)pen (P)ollinated treatement
datOP <- dat %>%
  unite(id, plantid, pop, sep = ".", remove = FALSE) %>%
  arrange(id) %>%
  filter(bulk == 0, trt == "OP")

O_meanv <- aggregate(seedv ~ id + trt, FUN = mean, data=datOP) 
O_meaninv <- aggregate(seedinv ~ id, FUN = mean, data =datOP)
OP_mean <- full_join(O_meanv, O_meaninv, by = "id")

OP_mean$seedratio <- OP_mean$seedv/(OP_mean$seedv + OP_mean$seedinv)

# we will join these two into one df
temp <- na.omit(full_join(HP_mean, OP_mean, by = "id")) # removes NA values that result from having one treatment but not the other on a plant. NOTE -- frt_set information will tell you some more information here. But how to incorporate that? We would have to aggregate like above, to get a frt_set per plant. But what is FUN = to in that case? I don't know enough about how FUN works. What other things can you put instead of = sum and = mean?

temp <- rename(temp, HP_seedv = seedv.x)
temp <- rename(temp, HP_seedinv = seedinv.x)
temp <- rename(temp, HP_seedratio = seedratio.x)
temp <- rename(temp, OP_seedv = seedv.y)
temp <- rename(temp, OP_seedinv = seedinv.y)
temp <- rename(temp, OP_seedratio = seedratio.y)

final <- temp %>% gather(HP_seedv, OP_seedv, key = "pol_trt", value = "viable_seeds")
final <- final[c(1,3,4,6:9)]


# Once we model build, we have to test assumptions normal residuals -- "hist(residuals(mod))" & "shapiro.test(residuals(mod))", then check for no patterns -- "plot(abs(residuals(mod))~fitted(mod))" 