### Code for Session 1 of Stat4Onc short course

### Prior choices figure
curve(dnorm(x, mean=0, sd=1), from=-10, to=10, xlab='x', ylab='Density', lwd=2, main='Possible of Priors', cex.axis=1.5, cex.main=1.5, cex.lab=1.5)
curve(dnorm(x, mean=0, sd=3), add=T, lwd=2, lty=2, col='orangered2')
curve(dnorm(x, mean=0, sd=10), add=T, lwd=2, lty=3, col='blue')
curve(dunif(x, -10, 10), add=T, lwd=2, lty=4, col='green')
curve(dnorm(x, mean=5, sd=2), add=T, lwd=2, lty=5, col='purple')

legend('topright', bty='n', lwd=c(2,2,2,2,2), lty=1:5, col=c('black','orangered2','blue','green','purple'), legend=c('N(0,1)','N(0,3)','N(0,10)','Unif(-10,10)','N(5,2)'), cex=1.5)

### Simulate logistic regression data
library(brms)
library(dplyr)

set.seed(20)
dat <- data.frame( trt=c(rep(1,32), rep(0,32)), out=c(rbinom(n=32,size=1,prob=0.4), rbinom(n=32,size=1,prob=0.1)) )
doBy::summaryBy(out~trt,data=dat)

# mod1 with Weakly Informative Priors:
mod1 <- brm(out ~ trt,
           data=dat,
           family='bernoulli',
           prior = c(set_prior("normal(0,sqrt(10))", class = "b"),
                     set_prior("normal(0,sqrt(10))", class="Intercept")),
           seed= 123,
           warmup = 1000, 
           iter = 10000)

summary(mod1) # review summary of results
bayestestR::hdi(mod1, ci=0.95) # get 95% HDP Credible Intervals since not given in output
plot(mod1) # plot density and trace plots
mcmc_plot(mod1, type="acf") # plot autocorrelation
prior_summary(mod1) # check priors used

# Extract posterior chains to estimate OR
post_samp <- as_draws_df(mod1)

# Create an indicator for exp(trt) > 1, because null is 1 on OR scale
post_samp <- post_samp %>%
  mutate(exp_trt=exp(b_trt),
         indicator=ifelse(exp_trt>1, 1, 0))

# Get estimates and 95% HDP Credible Intervals for exponentiated (i.e., on Odds Ratio scale) intercept and group posterior values
summary(post_samp)
quantile(post_samp$exp_trt,c(0.025,0.975)) # 95% equal-tailed CI
bayestestR::hdi(post_samp$exp_trt, ci=0.95) # 95% HPDI


# mod2 with Informative trt Prior:
mod2 <- brm(out ~ trt,
           data=dat,
           family='bernoulli',
           prior = c(set_prior("normal(log(10),sqrt(1))", class = "b"),
                     set_prior("normal(0,sqrt(10))", class="Intercept")),
           seed= 123,
           warmup = 1000, 
           iter = 10000)

summary(mod2) # compare to mod1

# Frequentist results
mod_freq <- glm(out ~ trt, data=dat, family='binomial')
summary(mod_freq)

