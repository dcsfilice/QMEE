### Final QMEE project
## Question 1.  
# HYPOTHESIS: Males previously housed with rival males will reduce the lifespan 
# of their female mates

library(tidyverse)
library(lme4)
library(lmPerm)
library(car)
library(ggplot2); theme_set(theme_bw())


# Read in our main data frame of female fly lifespan
fly_dat1 <- read.csv("lifespan.csv")


# Check that data was read in properly
summary(fly_dat1)
str(fly_dat1)
# No cleaning needed


## Compare lifespan between females 
# TREATMENT mated with males housed either SINGLE or with RIVALS
# RESPONSE lifespan of individual female by treatment

fly_dat1.lm <- lm(lifespan~treatment, data = fly_dat1)
summary(fly_dat1.lm)


# Check diagnostic plots
par(mfrow=c(2,2))  # show four graphs in one panel
plot(lm(fly_dat1.lm), las = 1, col = "purple")  # no issues in these plots?


# Look at the the data 
print(ggplot(fly_dat1.lm, aes(x=treatment, y=lifespan))
      + geom_boxplot()
)


# Plot an effects plot 
library(effects)
plot(allEffects(fly_dat1.lm))


## Design the mixed model to include these parameters:
# FIXED effect: female line (factor with 2 levels)   NOT ADDED YET!!
# RANDOM effect: male clonal backgrounds (factor with 6 levels)
fly_dat2.lm <-  lmer(lifespan~treatment + (1|line), data=fly_dat1)


# Check summary stats for the model
summary(fly_dat2.lm)  
# Var and Std Dev is low/high for Random Effects?
# Therefore random effect of ? has ? impact
# on overall model?


# How does effects plot changes after running mixed model
plot(allEffects(fly_dat2.lm))








