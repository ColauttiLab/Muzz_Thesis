dat<-read.csv("SeedPolData.csv",header=T)
View(dat)

summary(dat)
# Need to make factor: (pop)ulation, bulk, stem, (obs)erver, and (fr)ui(t) set

dat$pop <- as.factor(dat$pop)
dat$bulk <- as.factor(dat$bulk)
dat$stem <- as.factor(dat$stem)
dat$obs <- as.factor(dat$obs)
dat$frt_set <- as.factor(dat$frt_set)

library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)


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