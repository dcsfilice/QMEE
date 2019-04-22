### Question 1.  
## HYPOTHESIS: Males previously housed with rival males will reduce the lifespan 
# of their female mates

library(tidyverse)
library(lme4)
library(lmPerm)
library(car)
library(ggplot2); theme_set(theme_bw())
setwd("~/R/STATS CLASS/QMEE_repo/projects")

#First, we want to use a linear model to look at lifespan as a continuous, gaussian response
#Before we do that, we should compare how well some transformations fit a linear model
## THIS SECTION DISCUSSES DIAGNOSTICS AND COMPARES FEMALE LIFESPAN:
# RAW DATA VS LOG_TRANSFORMED DATA  VS BOX.COX TRANSFORMED DATA

# Read in our main data frame of female fly lifespan
fly_dat1 <- read.csv("lifespan.csv")

# Check that data was read in properly
summary(fly_dat1)   
str(fly_dat1)
# No cleaning needed :-)

## TRANSFORMATIONS

## Log transform female lifespan
fly_dat1$log_lifespan = log(fly_dat1$lifespan)

## Box.Cox transform female lifespan
library(MASS)

Box = boxcox(lifespan ~ treatment*pop,
             data = fly_dat1,
             lambda = seq(-1,1,0.025)
)
Cox = data.frame(Box$x, Box$y)
Cox2 = Cox[with(Cox, order(-Cox$Box.y)),]
Cox2[1,]
lambda = Cox2[1, "Box.x"]

#Since lambda is very close to 0.5, we decided to square root transform the data (y^0.5)

fly_dat1$sqrt_lifespan = sqrt(fly_dat1$lifespan)

## HISTOGRAMS OF FEMALE LIFESPAN (3 THREE VERSIONS)

## Plot histograms of female lifespan data

# Non transformed
h1 <- ggplot(data=fly_dat1, aes(lifespan)) +
  ggtitle("Female Lifespan, non-transformed") +
  geom_histogram()
ggsave("histogram_female-lifespan.png", plot = h1, width = 8, height = 4, dpi = "print")

# Log tranformed
h2 <- ggplot(data=fly_dat1, aes(log_lifespan)) + 
  ggtitle("Female Lifespan, log transformed") + 
  geom_histogram()
ggsave("histogram_female-log_lifespan.png", plot = h2, width = 8, height = 4, dpi = "print")

# Square Root transformed    
h3 <- ggplot(data=fly_dat1, aes(sqrt_lifespan)) + 
  ggtitle("Female Lifespan, square root transformed")+ 
  geom_histogram()
ggsave("histogram_female-sqrt_lifespan.png", plot = h3, width = 8, height = 4, dpi = "print")

## NORMALITY TEST

# Use Shapiro-Wilk test to test normality of 
# raw lifespan vs log_lifespan vs sqrt_lifespan
# If p>0.05 then data is normally distributed
shapiro.test(fly_dat1$lifespan)
shapiro.test(fly_dat1$log_lifespan)
shapiro.test(fly_dat1$sqrt_lifespan)

## DIAGNOSTIC PLOTS

## Check diagnostic plots
par(mfrow=c(2,2))  # show four graphs in one panel

#maximal model, singular fit
model1a<-lmer(lifespan~treatment*pop+(treatment*pop|line),data=fly_dat1)
#simplified model
model1b<-lmer(lifespan~treatment*pop+(1|line),data=fly_dat1)

# Non transformed female lifespan
fly_dat1.lm <- lmer(lifespan~treatment*pop+(1|line), data = fly_dat1)
diagnos1 <- plot(lm(fly_dat1.lm), las = 1, col = "purple")  # no issues in these plots?x
ggsave("diagnostic_female-lifespan.png", plot = diag1, width = 8, height = 4, dpi = "print")

# Log transformed female lifespan
fly_dat2.lm <- lmer(log_lifespan~treatment*pop+(1|line), data = fly_dat1)
diagnos2 <- plot(lm(fly_dat2.lm), las = 1, col = "red")  # no issues in these plots?
ggsave("diagnostic_female-log_lifespan.png", plot = diag2, width = 8, height = 4, dpi = "print")

# Square Root transformed female lifespan
fly_dat3.lm <- lmer(sqrt_lifespan~treatment*pop+(1|line), data = fly_dat1)
diagnos3 <- plot(lm(fly_dat3.lm), las = 1, col = "green")  # no issues in these plots?
ggsave("diagnostic_female-square_root_lifespan.png", plot = diag3, width = 8, height = 4, dpi = "print")

# Based on our various diagnostics, it appears lifespan square-root transformed is the best fit
# Looking at residuals:
#1.) Linearity: The slope is the most horizontal in our sqrt transformed data, meaning it is most linear
#2.) Normality: The residuals all look relatively normal, but most of the residuals fit best in sqrt transformation
#3.) Heteroscadicity: Slope isn't perfectly zero suggesting some heteroscadicity, but it is most horizontal in our sqrt transformation
#4.) Outliers: No problematic outliers in any of our transformations

#Looking at raw data:
#Shapiro-wilk test and histograms
#Looking at the raw data, the distribtion looks most normal (visually, and highest shapiro-wilk test value) when sqrt transformed

#Therefore, let's go ahead with using the sqrt transformed model to statistically test our prediction
sqrt_model<-lmer(sqrt_lifespan~treatment*pop+(1|line), data = fly_dat1)

#test statistics of fixed and random effects
#summary(sqrt_model)
#confint(sqrt_model)

library(dotwhisker)
#dwplot(sqrt_model)

#p-value for random effect, using a permutation appraoch
nreps<-1000
per.line<-numeric(nreps)
per.line[1]<-as.data.frame(VarCorr(sqrt_model))[1,"vcov"]
for (i in 2:nreps) {
  perm.data <- fly_dat1[sample(nrow(fly_dat1),size=nrow(fly_dat1),replace=FALSE),]
  lmer.perm <- lmer(perm.data$sqrt_lifespan~treatment*pop+(1|line), data=fly_dat1)
  per.line[i] <- as.data.frame(VarCorr(lmer.perm))[1,4]
}
length(per.line[per.line >= per.line[1]])/nreps

#Finally, let's compare this outcome to the cox hazard appraoch
library(coxme)
survmodel<-coxme(Surv(lifespan) ~ treatment*pop + (1|line) , data=fly_dat1) 
summary(survmodel)

# End script