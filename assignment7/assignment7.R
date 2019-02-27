library(R2jags)
dat<-read.csv("remating.csv")
model<-lm(duration~treatment*line,data=dat)
summary(model)
N <- nrow(dat)
bayesmod <- with(dat, jags(model.file='bayes.bug'
                          , parameters=c("ma", "mb", "mab","tau")
                          , data = list('first.d' = first.d, 'treatment' = treatment, 'line' = line, 'N'=N)
                          , n.chains = 4
                          , inits=NULL
))

print(bayesmod)
plot(bayesmod)



