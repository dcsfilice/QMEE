library(lme4)
library(coxme)
library(car)
dat1 <- read.csv("offspring.num.csv")
dat2 <- read.csv("lifespan.csv")

#maximal model, singular fit
model1a<-lmer(lifespan~treatment*pop+(treatment*pop|line),data=dat2)
#simplified model
model1b<-lmer(lifespan~treatment*pop+(1|line),data=dat2)
summary(model1b)
model1c<-coxme(Surv(lifespan) ~ treatment*pop + (1|line) , data=dat2)  
Anova(model1c)
summary(model1c)  

#maximal model, singular fit
model2a<-glmer(offspring~treatment*day*pop+(treatment*pop|line)+((poly(day,2))-1|line)+(poly(day,2)|female),data=dat1,family=quasipoisson)

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
model2c<-glmer.nb(offspring~treatment*day*pop+(1|line)+(1|female),data=dat1,family=nbinom)
#Singular fit, but let's bear with it considering it is of course from the OBS random factor (and female factor)
overdisp_fun(model2c)
#Model no longer looks overdispersed! 
summary(model2c)

confint(model2c,method="wald")

library(glmmTMB)
library(glmmTMB)
> model2c<-glmmTMB(offspring~treatment*day*pop+(1|line)+(1|female),data=dat1,family=nbinom2)

confint(model2c,method="uniroot")
