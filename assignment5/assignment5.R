library(lme4)
library(lmPerm)
library(tidyverse)
## BMB: might want to specify that working directory is *not*  the
## same as the source directory

dat <- read_csv("remating.csv")

## BMB: this is NOT advised. (See ?attach, "Good practice")
## instead, use 'data=' arguments wherever possible
##attach(dat)

#First, let's test our first hypothesis by running a classic t-test to compare mating duration between treatments
t.test(first.d~treatment, data=dat)
#As hypothesized males housed with rivals appear to have significantly longer mating durations
#Next, let's try to test this hypothesis using a permutation approach
summary(lmp(first.d~treatment,data=dat))
#similar to our simple t-test, our p value is less than 0.05
## BMB: and in fact quoted as "2e-16", which here means that the
## p-value is actually zero! should be listed as 1/5001 instead.
coef(summary(lmp(first.d~treatment,data=dat)))

#Now let's try something a bit more fancy by using permutations and mixed models to estimate genetic variation
#First, let's take a look at the classic approach by running a simple ANOVA
anova(aov(first.d~treatment*line, data=dat))
#As hypothesized, different genotypes appear to significantly vary in their mating duration
#Now, let's construct a linear mixed-effect model
model<-lmer(first.d~treatment+(treatment|line),data=dat)
#Now let's run a permutation test by resampling the data without replacement 1000 times
nreps<-1000
#create variable for number of permutations
per.line<-numeric(nreps)
##create dataframe with calculated variance component associated with genetic line
##  BMB: try to use labels rather than indices
per.line[1]<-as.data.frame(VarCorr(model))[1,"vcov"]
## or
vv <- VarCorr(model)
vvdf <- as.data.frame(vv)
vvdf[vvdf$grp=="line" & vvdf$var1=="(Intercept)" & is.na(vvdf$var2), "vcov"]
## or
vv$line["(Intercept)","(Intercept)"]

#run permutation test
#this randomly resamples the duration data to the different lines/treatments without replacement
#then, uses the same model to re-estimate the variance component associated with genetic line and places each component into a data frame
for (i in 2:nreps) {
    perm.data <- dat[sample(nrow(dat),size=nrow(dat),replace=FALSE),]
    ## use data= argument!  (can actually speed this up a bit with refit())
    lmer.perm <- lmer(dat$first.d~treatment+(treatment|line), data=perm.data)
    ## BMB more elegant to this by permuting the *response* variable only
    ## (equivalent to permuting all response variables together)
    per.line[i] <- as.data.frame(VarCorr(lmer.perm))[1,4]
}
#This formula then estimates our "p-value" by seeing how often our permuted components are greater than or equal to our actual component
length(per.line[per.line >= per.line[1]])/nreps
#Our permutation test appears to be a bit more conservative, but p-value is still less than 0.05
#This further suggests the presence of genetic variation in mating duration
## BMB: try to avoid discussing the "presence of genetic variation". We KNOW it exists!

## score: 2.5
