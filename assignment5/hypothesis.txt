#Test 1
I hypothesize that males housed with rivals will mate for signifcantly longer compared to males housed alone. The classic way to approach this would be to use a t-test. I will test this by performing a permutation test to generate , 

#Test 2
I also hypothesize that there will be significant genetic variation in this effect (i.e. different genetic lines will display 
variation in this phenotype). The classic way to approach this would be to use an ANOVA. I will test this by using a permutation test to
estimate 1000 variance compontents by resampling my data without replacement and running this resampled data through my mixed model. Then,
see how often these these permutated variance compontents are greater than or equal to my calculated varience component associated with 
the random effect of genetic line in order to estimate a p-value. 
