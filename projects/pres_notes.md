## presentation notes

### bmb

- you should definitely consider log-transforming lifespan. Maybe try a Box-Cox transformation (use a regular linear model and apply `MASS::boxcox()` or one of the corresponding functions in the `car` package) to see what power transformations are reasonable?
- the relationships between Cox PH and parametric lifespan analysis are interesting/deep. Frank Harrell's *Regression Modeling Strategies* talks some about parametric survival analysis, although the math is not at all sugarcoated ...
- you should almost definitely use the p-values from `lmerTest` summaries rather than Wald $\chi^2$.
- bootMer does not do nonparametric bootstrapping (which is what you described, it does *parametric* bootstrapping (i.e. it simulates the model rather than resampling the data).  It's probably much cheaper to get profile CIs (i.e. use `confint()` from `lme4`, and they will be nearly as good as parametric bootstrap CIs (which you can also get via `confint(..., method="boot")`) for this size data set
- it would be great to overlay/compare the predictions of the CoxME and log-Normal survival models on the same plot with the (non-parametric, Kaplan-Meier) survival curves.
- consider using sum-to-zero contrasts for female pop (at least).
