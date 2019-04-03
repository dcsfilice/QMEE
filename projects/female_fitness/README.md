# Data
Our project consists of two data sets. 
In the first data set (lifespan.csv), we simply scored the lifespan of 240 females by checking for mortality every day.
The second data set (offspring.csv) contains the number of progeny each female produced every two days in the first 20 days of her life 
(or less, if she 
perished before the 20th day). Offspring were counted every day, meaning each individual female has a repeated measure of up to 10 
offspring counts.

In both data sets, females were exposed to a male every other day over 20 days (unless they died before the 20th day). We manipulated the 
experience of these males by either housing them alone in a vial or in a vial with two rivals for 3 days prior to the female exposure. 
Additionally, we used males from 6 randomly selected clonal backgrounds. Females were randomly assigned to be paired with one of these 
experience x clonal combinations. Each exposure, females were exposed to a new male from the same experience x clone combo. Finally, we 
collected both data sets using two different female populations. One of these populations is 
descended from recently-captured wild flies, while the other is descended from a lab population maintained in standardized lab conditions 
for over 30 years.

# Scientific questions
For background, please refer to the main repo README.
Broadly, we want to determine how these different conditions (experience and genotype of previous mate, population history) influence
lifetime female fitness by measuring her lifespan and offspring production. 
Specifically, we predict:

1.) Males previously housed with rival males will reduce the lifespan of their mates

2.) Males previously housed with rivals or housed alone will have differential effects on the lifetime offspring production of their mates

3.) There will be significant genetic variation associated with both these effects (i.e. in the plasticity)

## JD This is not really a prediction unless you can tell us what you mean by "significant". Remember, reality is not "significant" (or not), only your results.

4.) Ultimately, females mated to males housed alone will have higher fitness

# Analysis plans
The main goal is to indepedently analyze the lifespan and offspring data by taking a mixed-modelling approach.

1.) Lifespan can be analyzed with a linear mixed model.

JD What effects are you thinking of? What link function? I guess there are two appropriate ones to consider: cloglog, which is about hazard of dying, and log, which is more about thinking about daily probability as a probability

2.) Lifetime offspring production with a generalized linear mixed model (poisson distribution).

JD: Poisson doesn't really sound like a good guess, and I'm not really sure what a good guess would be. I'm sort of tempted to say normal (or lognormal unless you have zeroes, but you probably do).

To analyze our models, we will use the Anova function in the car package to test the significance of our fixed effects (and their 
interactions).
Furthermore, we will estimate the upper and lower 95% CI of the variance components of our random effects using a bootstrapping approach, 
and estimate p-values using a permutation test approach (i.e. how frequently does a permutated data return a variance component greater 
than our calculated one?)

JD: I'm not sure why you are particularly interested in variance components of random effects? This may relate to the question about whether "significant" genetic variance is a thing. It's not.

Finally, we want to do some sort of combined analysis in order to estimate total female fitness. After further thought, I realized it 
probably wouldn't be possible to calculate total fitness using my offspring production and lifespan data because the two aren't fully 
independent of one another - in other words, females that died within the 20 day testing period for offspring production will naturally 
have less offspring due to fewer testing oppritunites. Therefore, we will calculate selection coefficents for each female population using 
their lifetime offspring production.

JD: What's wrong with estimating total fitness as total ofspring production? Less offspring because you died is roughly the same as less offspring for other reasons, as far as the next generation is concerned. Or maybe you're talking about the offspring that non-dead flies would produce after the 20 days -- we could likely find ways to project that, if there is time. How long do flies live?

Bonus (if there is time): I want to figure out how to somehow analyze the relationship between the TREND in offspring production (i.e. the 
magnitude of offspring investment depending on temporal variation) and lifespan. No idea how I would go about doing this, though.

JD: This seems like the kind of thing that you could do with random effects (a random slope underlying daily egg production and a random effect on survival probability).

JD: I feel like a hard problem here is a good distribution for daily and for total egg production. Total egg production may be more likely that you can get away with normal.
