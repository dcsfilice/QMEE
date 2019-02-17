dat<-read.csv("remating.csv")
model<-lm(first.d~treatment,data=dat)
plot(model)
