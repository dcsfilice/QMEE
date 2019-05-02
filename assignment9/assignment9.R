library(lme4)
library(car)
dat1 <- read.csv("remating.csv")
#First, let's try to play with a relatively "simple" model..
#Mating duration continuous, so we'll use a lmer
#Treatment is a fixed effect, line and the week of testing are random... Treatment was tested in every line and every week. 
#Therefore we want to cross their effects with treatment on left side of pipe, and random effects on the right
model1max <- lmer(first.d~treatment+(treatment|line)+(treatment|week)+(treatment|line:week),data=dat1)
#Personally, I don't care much about the interactions of the day effect with other variables
##Furthermore, it just makes the model messy. I only care about the effect of week on its own
## BMB: "don't care about" is not necessarily sufficient reason to exclude
## a term from the model. Things you don't care about could affect inference
## on the things you do care about.
## (max model is not _exactly_ singular, but in effect it is:
##  corr is -0.9999765 ...)
attr(VarCorr(model1max)$line,"correlation")

model1 <- lmer(first.d~treatment+(treatment|line)+(1|week),data=dat1)

#This model addresses everything I care about
summary(model1)
Anova(model1)
#Effect of male experience (single, or housed with rivals) significantly influences mating duration
## BMB: should always try to say something about magnitude of effect
## (singly housed males mate for 120 (seconds?) less.
## might want to switch factor order so that 'single' is the baseline
## level, thus you can talk about parameter measuring "effect of competition"

#Let's analyze random effects by using a function to estimate the upper and lower 95% CI
#This is estimated by running 100 bootstraps (with replacement) to re-estimate the varience components of our model
##I understand 100 reps is on the low side, but for the sake of computing time I picked 100 ## BMB: makes sense
#Function for line x treatment interaction
FUN.int <- function(fit) {
    return(as.data.frame(VarCorr(fit))[1,4])
    ## BMB: try to index by name rather than position
    ## could use VarCorr(fit)$line[c("(Intercept)","(Intercept)")]
}
result.int <- bootMer(model1, FUN.int, nsim = 100)
#These equations then returns the upper and lower 95% CI
quantile(result.int$t,0.975)
quantile(result.int$t,0.025)
## BMB: can use c(0.025,0.975) to get both at once
#Function for line effect
FUN.line<- function(fit) {
  return(as.data.frame(VarCorr(fit))[2,4])
}
## BMB: write one function with an additional parameter to
## specify which piece you want, rather than three separate functions?
result.line <- bootMer(model1, FUN.line, nsim = 100)
quantile(result.line$t,0.975)
quantile(result.line$t,0.025)
#function for week effect
FUN.week<- function(fit) {
    return(as.data.frame(VarCorr(fit))[3,4])
}

## BMB: maybe a little too fancy but ...
FUN.var <- function(level) {
    f <- function(fit) {
        vv <- VarCorr(fit)[[level]]
        return(vv["(Intercept)","(Intercept)"])
    }
    return(f)
}
result.week <- bootMer(model1, FUN.week, nsim = 100)
result.week <- bootMer(model1, FUN.var("week"), nsim = 100)
quantile(result.week$t,0.975)
quantile(result.week$t,0.025)
#Variance components of genetic line, and day also seem to be relatively high. 
#Interaction between treatment and line, not so much, meaning all lines seem to exhibit similar plasticity
#95% CI for line do not overlap zero
#95% CI for interaction and week effect do overlap zero.. however there is something wonky with my values
##for week... My actual variance component doesn't fall inbetween the CIs - not sure why?
## BMB: At least in the version of lme4 I'm using, row 3 is the *correlation*,
##  not the variance.  See fixed version above (again, this is why selecting by index number can be dangerous)


## 95% CI for variances will **RARELY** overlap zero.
## this is not actually the way to test significance of variances
## (but you should think about whether you want to test significance
## of variances).  Why not bootstrap something about fractions of variance
## and stop there?  (Also, see GLMM FAQ about tests of random effect
## significance)

#Now I want to play with something I'm a bit unsure about... bear with me
dat2 <- read.csv("offspring.csv")

#count data, use poisson model
#Treatment and day are fixed effects. Every line was tested between 2 treatments and across 10 days, so we should cross their effects (treatment|line)+(day|line)
#The thing is, we may also look at three-way interaction - this is where things get messy and I don't know if what I'm doing makes sense 
#does (treatment*day|line) make any sense at all? 
#We also need to include a random effect for individual female, since they were measured over mulitple days
#Typically, we'd also want to look at (day|female), however here we can't because not all females were tested on all days, due to mortality
model2max <- glmer(offspring~treatment*day+(treatment*day|line)+(1|female),family=poisson,data=dat2)

## BMB: (day|female) should still be OK, I think, even when not balanced ...
## but this will be a hard model to fit -- 20x20 covariance matrix!
## maybe make day numeric to allow fitting a slope instead of separate values
## for each day???

#We aren't really interested in how lines vary across time, so let's take out that mess of a (treatment*day|line) term
## BMB: again see comment above about "not interested in X so leave it out"

model2 <- glmer(offspring~treatment*day+(treatment|line)+(1|female),family=poisson,data=dat2)
summary(model2)
Anova(model2)
#As expected, offspring production varies significantly across days.
#Interaction with treatment is very interesting, meaning offspring production varies temporally depending on previous mate's experience!
#Based on variance components, individual female seems to have a sizable influence on offspring production (not very suprising)
#However, genetic line, and the interaction between line and day, seem to be quite small in their effect
#We could also use the same approach as above to calculate the upper/lower 95% CI
