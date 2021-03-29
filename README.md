# Bayesian Inference Simulation

In this project, we aimed to understand which Bayesian Inference method
is able to provide the most stable results in terms of:

- first type error
- second type error
- type S error
- type M error

The Inference methods we will try are:

1. Bayes Factors ($BF_{10}$) by means of `bayesfactor` R package
2. $BF_{10}$ by means of Savage-Dickey Density Ratio with `brms` package
3. $BF_{10}$ by means of Savage-Dickey Density Ratio with model in `JAGS`
   and `logspline` package
4. Comparison by means of  `brms` and `bridgesampling` R package
5. Comparison by means of Indicator Variable Selection by Kuo and Mallick
   implemented in `JAGS`
5. Comparison by means of Carlin and Chib Product-Space Method implemented
   in `JAGS`
6. WAIC and loo by means of `brms` and `loo` R packages
7. Highest Density Interval

We simulated data for:

- unpaired t-test
- paired t-test
- 3 x 3 mixed design

The prior distributions for the regressors will vary among:

- normal distributions with $\sigma =$ 10 for the intercept, 1 for the $\beta$s
- cauchy distributions with scale = 10 for the intercept, $\sqrt{2}/2$ for
  the $\beta$s
- normal distributions with $\sigma =$ 10 for the intercept, 0.4 for the $\beta$s
- cauchy distributions with scale = 10 for the intercept, 0.4 for
  the $\beta$s
- normal distributions with $\sigma =$ 10 for the intercept, Effect Size/2 for
  the $\beta$s
- cauchy distributions with scale = 10 for the intercept, Effect Size/7 for
  the $\beta$s
