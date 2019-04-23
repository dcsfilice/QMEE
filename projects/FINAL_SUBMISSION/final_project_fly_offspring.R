### Question 2.  
## HYPOTHESIS: Males previously housed with rival males will reduce the offspring production
# of their female mates
library(lme4)
library(car)
dat1 <- read.csv("offspring.num.csv")

#maximal model, count data, let's try a poisson distribution
model2a<-glmer(offspring~treatment*day*pop+(treatment*pop|line)+((poly(day,2))-1|line)+(poly(day,2)|female),data=dat1,family=quasipoisson)
#singular fit

#We reduced our random effect interactions until no singular fits
model2b<-glmer(offspring~treatment*day*pop+(1|line)+(1|female),data=dat1,family=poisson)
#singular fit, but we will bear with it since it is from female identity random factor. We need this here to account for repeated measures.

#given our poisson distrubtion, we should check for overdispersion 
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
overdisp_fun(model2b)
#The model is very overdispersed, likely because of many zero counts in dataset.
#Let's use a negative binomial family instead
library(glmmTMB)
model2c<- model2c<-glmmTMB(offspring~treatment*day*pop+(1|line)+(1|female),data=dat1,family=nbinom2)
#Singular fit, but let's bear with it considering it is of course from the OBS random factor (and female factor)
overdisp_fun(model2c)
#Model no longer looks overdispersed! 
summary(model2c)

confint(model2c,method="uniroot")

#End script.