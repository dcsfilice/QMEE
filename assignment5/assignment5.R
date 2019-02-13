library(lme4)
library(lmPerm)
library(tidyverse)
dat<-read_csv("remating.csv")
attach(dat)

#First, let's test our first hypothesis by running a classic t-test to compare mating duration between treatments
t.test(first.d~treatment)
#As hypothesized males housed with rivals appear to have significantly longer mating durations
#Next, let's try to test this hypothesis using a permutation approach
summary(lmp(first.d~treatment,data=dat))
#similar to our simple t-test, our p value is less than 0.05

#Now let's try something a bit more fancy by using permutations and mixed models to estimate genetic variation
#First, let's take a look at the classic approach by running a simple ANOVA
anova(aov(first.d~treatment*line))
#As hypothesized, different genotypes appear to signifcantly vary in their mating duration
#Now, let's construct a linear mixed-effect model
model<-lmer(first.d~treatment+(treatment|line),data=dat)
#Now let's run a permuation test by resampling the data without replacement 1000 times
nreps<-1000
#create variable for number of permutations
per.line<-numeric(nreps)
#create dataframe with calculated variance compontent associated with genetic line
per.line[1]<-as.data.frame(VarCorr(model))[1,4]
#run permutation test
#this randomly resamples the duration data to the different lines/treatments without replacement
#then, uses the same model to re-estimate the variance component associated with genetic line and places each component into a data frame
for (i in 2:nreps) {
  perm.data<-dat[sample(nrow(dat),size=nrow(dat),replace=FALSE),]
  lmer.perm<-lmer(dat$first.d~perm.data$treatment+(perm.data$treatment|perm.data$line))
  per.line[i]<-as.data.frame(VarCorr(lmer.perm))[1,4]
}
#This formula then estimates our "p-value" by seeing how often our permutated components are greater than or equal to our actual component
length(per.line[per.line >= per.line[1]])/nreps
#Our permutation test appears to be a bit more conservative, but p-value is still less than 0.05
#This further suggests the presence of genetic variation in mating duration
