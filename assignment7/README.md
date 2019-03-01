# Refresher of hypothesis
Males exposed to rivals before a mating oppritunity will mate for longer compared to males housed in isolation. There is also genetic variation in male mating duration.

# Prior assumptions
In fruit flies, males typically mate for 900s-1100s. Therefore, a fair estimate of the SD associated with this response is 100s, making 10^-6 a conservative prior assumption for the variance between males in different treatments, different lines, and the interaction between the two predictors.

# Bayesian vs. frequentist fit
To analyze my bayesian model, I used the print function to display the 95% confidence intervals of each predictor variable. As the 95% CIs for experience treatment and genetic line do not overlap with zero (569.823, 633.119 and 60.015, 69.588, respectively) we can conclude the variances associated with these predictors are signifcantly different from zero. 

To take a frequentist approach to analyzing my model, I used the Anova function from the car package. Similar to our Bayes approach, it appears that the treatment and genetic line predictors have a signifcant influence on the response variable, as their p-values are <0.05 (1.072e-10 and 1.030e-06, respectively). Similarly, the interaction term is >0.05 and we can thus make no strong conclusions on its effect.

#PS sorry if some of my language describing my analysis is off, please continue to correct me where needed!
