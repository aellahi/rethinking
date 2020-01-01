# Ch 6: Overfitting, regularization, and information criteria 

# How to deal with two problems on the opposite ends of the spectrum:
# 1. Overfitting: poor predictions because the model learns "too much" from the training data
# 2. Underfitting: poor predictions because the model is learning "too little" from the data; poor predictions both within and out of sample/training data

# Two common families of approaches: 
# Using a REGULARIZING PRIOR 
# INFORMATION CRITERIA: model the prediction task and estimate predictive accuracy

# 6.1 The problem with parameters
# why not just keep adding parameters? 
# Because adding parameters almost always improves model fit. Particularly
# true for linear models computing R2--adding parameters will ALWAYS increase R2!

# example data exemplifying over-fitting
library(rethinking)
sppnames <- c("afarensis", "africanus", "habilis", "boisei",
              "rudolfensis", "ergaster", "sapiens")
brainvolcc <- c(438, 452, 612, 521, 752, 871, 1350)
masskg <- c(37.0, 35.5, 34.5, 41.5, 55.5, 61.0, 53.5)
d <- data.frame(species=sppnames, brain=brainvolcc, mass=masskg)
head(d)
plot(brain ~ mass, data=d)

# fit linear model of brain vol as function of mass
m6.1 <- lm(brain ~ mass, data=d)
precis(m6.1)
# compute R^2
1-var(resid(m6.1))/var(d$brain)
summary(m6.1)

# now let's fit more complex polynomial models
m6.2 <- lm(brain ~ mass + I(mass^2), data=d)
summary(m6.2)

m6.3 <- lm(brain ~ mass + I(mass^2) + I(mass^3), data=d)
summary(m6.3)

m6.4 <- lm(brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4), data=d)
summary(m6.4)

m6.5 <- lm(brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) + I(mass^5), 
           data=d)
summary(m6.5)

m6.6 <- lm(brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) + I(mass^5) + I(mass^6), data=d)
summary(m6.6)

# As the model gets more complex, R2 always increases, until it gets to 1!

# Try a model with too few parameters:
m6.7 <- lm(brain ~ 1, data=d)
summary(m6.7)

# Underfit models are fairly insensitive to the data used to generate them
# Overfit models are very senstiive
# try using LOOCV to see this in action! 

# BIAS-VARIANCE TRADE-OFF: another way to refer to the same problem
# Mathematical formula for quantifying uncertainty (aka entropy):
# H(p) = -Elog(pi) 
# example: weather; suppose Prain = 0.3 and Psun = 0.7
p <- c(0.3, 0.7)
-sum(p*log(p))

# Entropy matters more in the context of MEASURING ACCURACY
# What we want is a way to quantitatively say how far a model is from hitting the target

# DIVERGENCE: additional uncertainty induced by using probabilities 
# from one distribution to describe another distribution

# What is ACCURACY? 

# DEVIANCE: a measure of *relative* model fit (i.e., between 2 models, which fits target better)
# DEVIANCE = D(q) = -2(Sigmai)log(qi)
# example to calculate using MAP:

m6.1 <- lm(brain ~ mass, data=d)
# logLik function will return sum of log likelihoods of each event
logLik(m6.1)
# compute deviance
(-2)*logLik(m6.1)
# there is also a distribution of deviances! 


# WIDELY-ACCEPTABLE INFORMATION CRITERIA (WAIC)
# No assumptions about priors 
# example to calculate it
library(rethinking)
data(cars)
head(cars)
m <- map(
  alist(
    dist ~ dnorm(mu, sigma),
    mu <- a + b*speed,
    a ~ dnorm(0,100),
    b ~ dnorm(0,10), 
    sigma ~ dunif(0,30)
    ), 
  data=cars)

post <- extract.samples(m, n=1000)
dim(post)

# get the log-likelihood of each observation i at each sample
# s from posterior
n_samples <- 1000
ll <- sapply(1:n_samples, 
             function(s) {
               mu <- post$a[s] + post$b[s]*cars$speed
               dnorm(cars$dist, mu, post$sigma[s], log=TRUE)
             })

# ll = 50 x 1000, each row = observation, each column a sample
# compute lppd by avging samples in each row, take log, and add logs
n_cases <- nrow(cars)
lppd <- sapply(1:n_cases, function(i) log_sum_exp(ll[i,]) - log(n_samples))
sum(lppd)
lppd
# pwaic = compute variance across samples for each observation
# then add:
pWAIC <- sapply(1:n_cases, function(i) var(ll[i,]))
sum(pWAIC)
# calculate WAIC
-2*(sum(lppd) - sum(pWAIC))
# compute standard error of WAIC
waic_vec <- -2*(lppd - pWAIC)
sqrt(n_cases*var(waic_vec))

?WAIC

waic_fxn = WAIC(m)

# Use DIC & WAIC to carry out 
# MODEL COMPARISON and 
# MODEL AVERAGING 
data(milk)
d <- milk[complete.cases(milk),]
d$neocortex <- d$neocortex.perc/100
dim(d)

# try fitting 4 different models to predict kcal.per.g
# with 2 predictor variables: neocortex & mass
# constrain sigma to be positive
a.start <- mean(d$kcal.per.g)
sigma.start <- log(sd(d$kcal.per.g))

# first fit model using just intercept, no predictors
m6.11 <- map(
  alist(
    kcal.per.g ~ dnorm(a, exp(log.sigma))
  ),
  data=d, start=list(a=a.start, log.sigma=sigma.start)
  )

# second model: neocortex as predictor 
m6.12 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, exp(log.sigma)),
    mu <- a + bn*neocortex
  ),
  data=d, start=list(a=a.start,
                     bn=0,
                     log.sigma=sigma.start)
)

# third: log mass as predictor
m6.13 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, exp(log.sigma)),
    mu <- a + bm*log(mass)
  ),
  data=d, start=list(a=a.start, bm=0, log.sigma=sigma.start)
)

# fourth: both neocortex and log.mass as predictors
m6.14 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, exp(log.sigma)),
    mu <- a + bn*neocortex + bm*log(mass)
  ),
  data=d, start=list(a=a.start, bn=0, bm=0, log.sigma=sigma.start)
)

# Comparing WAIC values
WAIC(m6.14)
WAIC(m6.11)
WAIC(m6.12)
WAIC(m6.13)

# can directly compare WAIC using "compare" fxn:
compare(m6.11, m6.12, m6.13, m6.14)
milk.models <- compare(m6.11, m6.12, m6.13, m6.14)
milk.models

# can plot these comparisons to get more intuitive view:
plot(milk.models, SE=TRUE, dSE=TRUE)

# compare parameter estimates
coeftab_plot(coeftab(m6.11, m6.12, m6.13, m6.14))