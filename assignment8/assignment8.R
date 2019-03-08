dat<-read.csv("remating.csv")
#Let's look if the previous competitive experience of a male influences the remating propensity of his mate
#binary response (0,1), lets use binomial family
model<-glm(mating~treatment,family=binomial,data=dat)
library(car)
Anova(model)
#Not quite <0.05, but there does appear to be a trend, whereby
#males exposed to rivals decrease their mates propensity to remate
