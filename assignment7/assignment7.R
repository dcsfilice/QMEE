library(R2jags)
dat<-read.csv("remating.csv")
#Create linear model of data: Mating duration as response, with experience treatment and genetic line as predictors
model<-lm(first.d~treatment*line,data=dat)
#run JAGS
#ma=experience treatment, mb=genetic line, mab=interaction between the two
N <- nrow(dat)
jdat <- with(droplevels(dat[1:N,]),
             list('first.d' = first.d, 'treatment' = as.numeric(treatment),
                  'line' = as.numeric(line), 'N'=N,
                  'nline'=length(levels(line))))
bayesmod <- jags(model.file='bayes.bug'
               , parameters=c("ma", "mb", "mab","tau")
               , data=jdat,
               , n.chains = 4
               , inits=NULL
                 )

print(bayesmod)
##Based on the 95% confidence intervals, the variances for line and treatment alone are significant as they do not overlap zero. The interaction term does.

library(broom.mixed)
library(dotwhisker)
library(dplyr)
(tidy(bayesmod)
    ## ignore intercept and mb[1], mab[1]
    %>% filter(term!="int", !grepl("\\[1\\]",term))
    %>% dwplot)
dwplot(bayesmod)
## ignore mb[1]
#plot(bayesmod)
#Display SD of each predictor
#traceplot(bayesmod)
#View traceplots - everything looks okay.
#Now, let's take a frequenist approach to analyzing our model
library(car)
Anova(model)
##Interesting, similar to our bayes model, we observe p-values < 0.05 for treatment and line alone, but not for their interaction
dwplot(model)

## score: 2
