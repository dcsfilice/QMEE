dat<-read.csv("remating.csv")

#Make a linear model with first mating duration as response variable, and experience treatment and genetic line as interacting predictor variables
m1<-lm(first.d~treatment*line,data=dat)
#Add all four diagnostic plots to single panel
par(mfrow=c(2,2),mar=c(2,3,1.5,1),mgp=c(2,1,0))
#Print diagnostic plots
plot(m1,id.n=4)

library(effects)
#Plot inferential plot showing the estimated means for each line (could not figure out how to remove unnecessary lines connecting points)
plot(effect("line", m1))