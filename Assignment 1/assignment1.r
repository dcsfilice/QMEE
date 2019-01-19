## BMB: best to *avoid* file names an ddirectories with spaces in them
## (e.g. "Assignment 1")

library(tidyverse)
library(coxme)  ## BMB: fancy!
## BMB: I prefer spaces around "<-" (see style guide; but OK
##  whatever you prefer as long as you're consistent)
data<-read_csv("lifespan.csv")
## BMB: best not to call this 'data' (masks built-in function,
##  occasionally causes trouble)
geno1<-filter(data,line=="M239")
model1<-coxme(Surv(lifespan, alive) ~ treatment + (1|female) , data=geno1)
summary(model1)
geno2<-filter(data,line=="M315")
model2<-coxme(Surv(lifespan, alive) ~ treatment + (1|female) , data=geno2)
summary(model2)
geno3<-filter(data,line=="M395")
model3<-coxme(Surv(lifespan, alive) ~ treatment + (1|female) , data=geno3)
summary(model3)
geno4<-filter(data,line=="M596")
model4<-coxme(Surv(lifespan, alive) ~ treatment + (1|female) , data=geno4)
summary(model4)
geno5<-filter(data,line=="M703")
model5<-coxme(Surv(lifespan, alive) ~ treatment + (1|female) , data=geno5)
summary(model5)
geno6<-filter(data,line=="M900")
model6<-coxme(Surv(lifespan, alive) ~ treatment + (1|female) , data=geno6)
summary(model6)

## BMB: cool, but: why do 5 separate analyses? line should probably be
## an additional random effect:
## are lines nested within treatment?
with(data,table(line,treatment))

## ? not quite sure why this doesn't work:
## should be able to estimate among-line variance in treatment effects?
coxme(Surv(lifespan, alive) ~ treatment + (treatment|line)+
          (1|female) , data=data)

## 
coxme(Surv(lifespan, alive) ~ treatment + (1|line)+
          (1|female) , data=data)

## BMB: score 2. (2=fine, 1=poor, 3=excellent)
