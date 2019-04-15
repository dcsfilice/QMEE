### Final QMEE project with David :-)

## Question 1.  
# HYPOTHESIS: Males previously housed with rival males will reduce the lifespan 
# of their female mates

library(tidyverse)
library(lme4)
library(lmPerm)
library(car)
library(ggplot2); theme_set(theme_bw())


# Read in our main data frame of female fly lifespan
fly_dat1 <- read.csv("lifespan.csv")


# Check that data was read in properly
summary(fly_dat1)
str(fly_dat1)
# No cleaning needed :-)


## Compare lifespan between females 
# TREATMENT mated with males housed either SINGLE or with RIVALS
# RESPONSE lifespan of individual female by treatment

fly_dat1.lm <- lm(lifespan~treatment, data = fly_dat1)
summary(fly_dat1.lm)


# Check diagnostic plots
par(mfrow=c(2,2))  # show four graphs in one panel
plot(lm(fly_dat1.lm), las = 1, col = "purple")  # no issues in these plots?


# Look at the the data 
print(ggplot(fly_dat1.lm, aes(x=treatment, y=lifespan))
      + geom_boxplot()
)


# Plot an effects plot 
library(effects)
plot(allEffects(fly_dat1.lm))


## Design the mixed model to include these parameters:
# FIXED effect: female line (factor with 2 levels) 
# RANDOM effect: male clonal backgrounds (factor with 6 levels)
fly_dat1.lmer <-  lmer(lifespan~treatment + (1|line), data=fly_dat1)
model1b <- lmer(lifespan ~ treatment * pop + (1|line), data=fly_dat1)


# Check summary stats for the model
summary(fly_dat1.lmer)  
summary(model1b)
# Var and Std Dev is low/high for Random Effects?
# Therefore random effect of ? has ? impact
# on overall model?


# How does effects plot changes after running mixed model
plot(allEffects(fly_dat1.lmer))
plot(allEffects(model1b))


####### NOW start working with log transformed data (female lifespan)

fly_dat2 <- fly_dat1 %>%  
  mutate(log_lifespan = log(lifespan))

## Compare lifespan between females 
# TREATMENT mated with males housed either SINGLE or with RIVALS
# RESPONSE lifespan of individual female by treatment

fly_dat2.lm <- lm(log_lifespan~treatment, data = fly_dat2)
summary(fly_dat2.lm)


# Check diagnostic plots
par(mfrow=c(2,2))  # show four graphs in one panel
plot(lm(fly_dat2.lm), las = 1, col = "red")  # no issues in these plots?


# Look at the the data 
print(ggplot(fly_dat2.lm, aes(x=treatment, y=log_lifespan))
      + geom_boxplot()
)


# Plot an effects plot 
library(effects)
plot(allEffects(fly_dat2.lm))


## Design the mixed model to include these parameters:
# FIXED effect: female line (factor with 2 levels)
# RANDOM effect: male clonal backgrounds (factor with 6 levels)

# these are both the same but one for DF name conventions and one for me :-)
fly_dat2.lmer <-  lmer(log_lifespan~treatment + (1|line), data=fly_dat2)
model2b <- lmer(log_lifespan ~ treatment * pop + (1|line), data=fly_dat2)


# Check summary stats for the model (Again these are the same)
summary(fly_dat2.lmer)  
summary(model2b)

# Var and Std Dev is low/high for Random Effects?
# Therefore random effect of ? has ? impact
# on overall model?


# How does effects plot changes after running mixed model
plot(allEffects(fly_dat2.lmer))
plot(allEffects(model2b))

# Use a histogram to look at distribution of lifespan vs log_lifespan
ggplot(data=fly_dat1, aes(lifespan)) + 
  geom_histogram()

ggplot(data=fly_dat2, aes(log_lifespan)) + 
  geom_histogram()

# Use Shapiro-Wilk test to test normality of lifespan vs log_lifespan
# If p>0.5 then data is normally distributed
shapiro.test(fly_dat1$lifespan)
shapiro.test(fly_dat2$log_lifespan)



## *********************************
# Here goes for the Box-Cox transformation  :-O
# http://rcompanion.org/handbook/I_12.html

library(MASS)

Box = boxcox(lifespan ~ pop,
             data = fly_dat1,
             lambda = seq(-6,6,0.1)
)

Cox = data.frame(Box$x, Box$y)

Cox2 = Cox[with(Cox, order(-Cox$Box.y)),]

Cox2[1,]

lambda = Cox2[1, "Box.x"]

fly_dat1$lifespan_box = (fly_dat1$lifespan ^ lambda - 1)/lambda   

boxplot(lifespan_box ~ pop,
        data = fly_dat1,
        ylab="Box–Cox-transformed lifespan",
        xlab="Population")



##Perform ANOVA and check residuals

model = lm(lifespan_box ~ pop, 
           data=fly_dat1)

library(car)

Anova(model, type="II")
                
x = residuals(model)

library(rcompanion)
plotNormalHistogram(x)


###### This is useful for printing ggplots to our folder

# EXAMPLES
#p1 <- ggplot( data = df1, aes(x = Date,y = Avg.Pace, color = Distance)) + 
  geom_point() +
  scale_y_datetime(date_labels = "%M:%S") +
  geom_smooth(color = "orange") +
  labs(x = "Date", y = "Average Pace (min/km)")

  # save all plots  EXAMPLES
#  ggsave("allPace.png", plot = p1, width = 8, height = 4, dpi = "print")

######  sum-to-zero contrasts :-)
#  http://atyre2.github.io/2016/09/03/sum-to-zero-contrasts.html  
  
  
## STEPS  
# plot the data to find the intercept
# center the continuous variable
# replot the data  
# use sum to zero contrast for female population

  # plot the data to find the intercept  
  m0 <- lm(lifespan ~ treatment * pop, data = fly_dat1)
  (summary_m0 <- summary(m0)) 

#####  LEFT OFF HERE!!!  Still working on this 
  # center the continuous variable
  base_lifespan <- ggplot(fly_dat1, aes(x = lifespan, y = treatment)) + geom_point(aes(shape = Species)) + 
    xlab("Sepal Length [mm]") + ylab("Sepal Width [mm]")
  library(broom)
  nd <- expand.grid(Sepal.Length = seq(-1, 8, 0.1), Species = factor(levels(iris$Species)))
  pred.0 <- augment(m0, newdata = nd)
  base_iris + geom_line(aes(y = .fitted, linetype = Species), data = pred.0)    
  
  library(dplyr)  #Stay in the tidyverse! 
  iris <- iris %>% mutate(cSepal.Length = Sepal.Length - mean(Sepal.Length))
  m1 <- lm(Sepal.Width ~ cSepal.Length * Species, data = iris)
  (summary_m1 <- summary(m1))
