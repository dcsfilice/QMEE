library(tidyverse)
library(coxme)
data<-read_csv("lifespan.csv")
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
