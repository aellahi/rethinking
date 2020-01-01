# Chapter 4 notes and problems

library(rethinking)
pos <- replicate(1000, sum(runif(16, -1, 1)))

# runif: picks a random number (n) of observations
# from a uniform distribution spanning min and max

# plot
plot(density(pos))

# normal by multiplication
prod(1 + runif(12, 0, 0.1))
growth <- replicate(1000, prod(1 + runif(12, 0, 0.1)))
dens(growth, norm.comp=TRUE)

big <- replicate(1000, prod(1 + runif(12, 0, 0.5)))
dens(big, norm.comp=TRUE)

log.big <- replicate(1000, log(prod(1 + runif(12, 0, 0.5))))
dens(log.big)

# probability mass functions, which model continuous outcomes, can be > 1!
dnorm(0, 0, 0.1)

# can redefine Bayes theorem in terms of probability distributions:
# and remodel globe-tossing example

w <- 6; n <- 9
p_grid <- seq(from=0, to=1, length.out=100)
posterior <- dbinom(w, n, p_grid)*dunif(p_grid, 0, 1)
posterior <- posterior/sum(posterior)
dens(posterior)
posterior

# Gaussian model of height
data(Howell1)
d <- Howell1

# Filter for only heights of adults
d2 <- d[d$age >= 18,]

# plot the distribution of heights
dens(d2$height)

curve(dnorm(x, 178, 20), from=100, to=250)

# sigma is a flat prior (uniform distribution)
?dunif

curve(dunif(x, 0, 50), from=-10, to=60)

# can simulate a prior distribution of heights
# based on sampling from prior distributions of sigma and mu
sample_mu <- rnorm(1e4, 178, 20)
sample_sigma <- runif(1e4, 0, 50)
prior_h <- rnorm(1e4, sample_mu, sample_sigma)
dens(prior_h)

# prior_h defines the "expected" distribution of heights
# given the priors 

# play around with bounds to understand effect on prior dist of heights  
sample_mu <- rnorm(1e4, 100, 50)
sample_sigma <- runif(1e4, 0, 50)
prior_h <- rnorm(1e4, sample_mu, sample_sigma)
dens(prior_h)

# generating a posterior distribution for a linear model 
# with two parameters mu and sigma:

mu.list <- seq(from=140, to=160, length.out=200)
sigma.list <- seq(from=4, to=9, length.out=200)
post <- expand.grid(mu=mu.list, sigma=sigma.list)

# expand.grid = creates data from all combinations of supplied vectors 
post$LL <- sapply(1:nrow(post), function(i) sum(dnorm(d2$height,  mean=post$mu[i],  sd=post$sigma[i],  log=TRUE)))  
post$prod <- post$LL + dnorm( post$mu, 178, 20, TRUE) +  dunif( post$sigma, 0, 50, TRUE)  
post$prob <- exp( post$prod - max(post$prod)) 

contour_xyz(post$mu, post$sigma, post$prob)
image_xyz(post$mu, post$sigma, post$prob)

# sample from posterior; have to sample both parameters
sample.rows <- sample(1:nrow(post), size=1e4, replace=TRUE, prob=post$prob)
length(sample.rows)
sample.mu <- post$mu[sample.rows]
sample.sigma <- post$sigma[sample.rows]

# plot these samples of mu and sigma to see which combos 
# show up with the highest density 
plot(sample.mu, sample.sigma, cex=0.75, pch=16, col=col.alpha(rangi2, 0.1))

# plot dens of samples
dens(sample.mu)
HPDI(sample.mu)
dens(sample.sigma)
HPDI(sample.sigma)

# the posterior may not always be Gaussian because it is very 
# sensitive to sigma. To illustrate, take a sample of the original data:
d3 <- sample(d2$height, size=20)

# repeat code above to generate a posterior distribution
mu.list <- seq(from=150, to=170, length.out=200)
sigma.list <- seq(from=4, to=20, length.out=200)
post2 <- expand.grid(mu=mu.list, sigma=sigma.list)

# expand.grid = creates data from all combinations of supplied vectors 
post2$LL <- sapply(1:nrow(post2), function(i) sum(dnorm(d3,  mean=post2$mu[i],  sd=post2$sigma[i],  log=TRUE)))  
post2$prod <- post2$LL + dnorm( post2$mu, 178, 20, TRUE) +  dunif( post2$sigma, 0, 50, TRUE)  
post2$prob <- exp( post2$prod - max(post2$prod)) 

# sample from posterior
sample2.rows <- sample(1:nrow(post2), size=1e4, replace=TRUE, 
                       prob=post2$prob)
sample2.mu <- post2$mu[sample2.rows]
sample2.sigma <- post2$sigma[sample2.rows]
plot(sample2.mu, sample2.sigma, cex=0.5, pch=16, col=col.alpha(rangi2, 0.1),
     xlab="mu", ylab="sigma")

# inspect marginal density of sigma
dens(sample2.sigma, norm.comp=TRUE)

# using map ("maximum a-posteriori estimate") to find combo
# of sigma and mu to maximize posterior probability
# map uses quadratic approximation to estimate the shape
# of the posterior distribution
library(rethinking)
data(Howell1)
d<- Howell1
d2 <- d[d$age >=18, ]

?alist

flist <- alist(
  height ~ dnorm(mu, sigma),
  mu ~ dnorm(178, 20),
  sigma ~ dunif(0, 50)
)
# fit the model to the data
m4.1 <- map(flist, data=d2)
# exmine the fit maximum a posteriori model
precis(m4.1)

# map climbs posterior like a hill; starts at a random sample
# of mu and sigma. can also specify a starting value:
start <- list(mu=mean(d2$height), sigma=sd(d2$height))

# repeating the model with a stronger prior (i.e., one defined
# by a smaller standard deviation; i.e., it's narrower)
m4.2 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu ~ dnorm(178, 0.1),
    sigma ~ dunif(0, 50)
  ),
  data=d2
)
precis(m4.2)

# 4.3.6 How to sample from a map fit (i.e., a quadratic approx
# posterior distribution?)
# think of it like a multi-dimensional Gaussian distribution 

# to describe a quadratic approx, need co-variances of all 
# pairs of parameters, or the "Variance-Covariance" matrix:
vcov(m4.1)

# to sample vectors of mu and sigma,
# use extract:
post <- extract.samples(m4.1, n=1e4)
head(post)

# each value is a sample from the posterior distribution
# generated by map 
precis(post)

# Adding a predictor to the model: Weight

# plot raw data
plot(d2$height ~ d2$weight)

# Define components of the model
height ~ dnorm(mu, sigma)
mu <- a + b*weight
a ~ dnorm(178, 100)
b ~ dnorm(0, 10)
sigma ~ dunif(0, 50)

# fit model
m4.3 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*weight,
    a ~ dnorm(178, 100),
    b ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ),
  data=d2
)

# examine model fit using table of estimates:
precis(m4.3, corr=TRUE)

# use CENTERING to avoid the strong negative correlation
# seen between a & b; strong correlations among parameters make models
# "difficult" to fit

# center the weight values
d2$weight.c <- d2$weight - mean(d2$weight)
d2$weight.c
# centered values have a mean of 0:
mean(d2$weight.c)

# remodel
m4.4 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*weight.c,
    a ~ dnorm(178, 100),
    b ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ),
  data=d2
)

# examine & compare
precis(m4.3, corr=TRUE)
precis(m4.4, corr=TRUE)

# plot MAP values (from posterior) over actual data
# "coef" function returns vector of map values
plot(height ~ weight, data=d2)
abline(a=coef(m4.3)["a"], b=coef(m4.3)["b"], col="red")

# to convey uncertainty in the MAP values, extract samples
# from the posterior
post <- extract.samples(m4.3)
# inspect
post[1:5,]

# to understand how the amount of data affects scatter of the posterior
# sample lines, let's extract some data and re-fit the model
N <- 352
dN <- d2[1:N,]
mN <- map(alist(
  height ~ dnorm(mu, sigma),
  mu <- a + b*weight,
  a ~ dnorm(178, 100),
  b ~ dnorm(0, 10),
  sigma ~ dunif(0, 50)
),
data=dN)

# extract 20 samples from the posterior
post <- extract.samples(mN, n=20)
# display raw data and sample size
plot(dN$weight, dN$height,
     xlim=range(d2$weight), ylim=range(d2$height),
     col=rangi2, xlab="weight", ylab="height")
mtext(concat("N = ", N))

# plot lines, with transparency
for (i in 1:20)
  abline(a=post$a[i], b=post$b[i], col=col.alpha("black", 0.3))

# change values of N from 10, 50, 150, 352, and see how it affects
# the confidence/scatter of the posterior sampled ab lines

post <- extract.samples(mN)
# how to plot an interval around a regression line
mu_at_50 <- post$a + post$b*50
mu_at_50
# the above calculates the possible means for an individual at 
# 50 kg of weight, using samples of a and b from the posterior

# plot this density of values:
dens(mu_at_50, col=rangi2, lwd=2, xlab="mu|weight=50")
# to get 89% highest posterior density interval:
HPDI(mu_at_50, prob=.89)

# now we want to compute this HPDI interval for every possible weight
# value with samples from the posterior
# use the "link" function, which will sample from the posterior
# and compute mu for each case in the data
mu <- link(m4.3)
str(mu)
# returns a matrix of mu values, each row being a sample from the 
# posterior distribution, and each column being for each case 
# of the data (i.e., 352 columns)

# define sequence of weights to compute predictions for
weight.seq <- seq(from=25, to=70, by=1)

# use link to compute mu for each sample from posterior
# and each weight in weight.seq
mu <- link(m4.3, data=data.frame(weight=weight.seq))
str(mu)
?str

# plot distribution of mu values at each height
# type="n" hides raw data
plot(height ~ weight, d2, type="n")
# loop over samples and plot each mu
for (i in 1:100)
  points(weight.seq, mu[i,], pch=16, col=col.alpha(rangi2, 0.1))

# summarize the distribution for each weight value
mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob=0.89)
# examine
str(mu.mean)
str(mu.HPDI)

# plot raw data
plot(height ~ weight, data=d2, col=col.alpha(rangi2, 0.5))
# plot MAP line, aka mean mu for each weight
lines(weight.seq, mu.mean)
# plot shaded region for 89% HPDI
shade(mu.HPDI, weight.seq)

# Prediction intervals - obtain by sampling from the distribution of 
# mu and sigma and thereby simulating a distribution of heights
sim.height <- sim(m4.3, data=list(weight=weight.seq))
str(sim.height)
?sim

# can summarize these simulated heights in the same way we summarize
# distributions of mu:
height.PI <- apply(sim.height, 2, PI, prob=0.89)
str(height.PI)

# plot it all!
# raw data
plot(height ~ weight, d2, col=col.alpha(rangi2, 0.5))
# MAP line
lines(weight.seq, mu.mean)
# draw HPDI region for line
shade(mu.HPDI, weight.seq)
# draw PI region for simulated heights
shade(height.PI, weight.seq)

# jagged edges in plot due to simulation variance; can ameliorate 
# by drawing more samples:
sim.height <- sim(m4.3, data=list(weight=weight.seq), n=1e4)
height.PI <- apply(sim.height, 2, PI, prob=0.89)

# POLYNOMIAL REGRESSION - modeling a non-linear (aka curved) relationship
# between predictor and response variable

# plot entire Howell dataset to see that it is curved
plot(height ~ weight, d, col=col.alpha(rangi2, 0.65))

# first standardize weight:
d$weight.s <- (d$weight - mean(d$weight))/sd(d$weight)
# sanity check
mean(d$weight.s)
sd(d$weight.s)

# modify the model and redefine function for mu as polynomial
# square of weight:
d$weight.s2 <- d$weight.s^2
m4.5 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1*weight.s + b2*weight.s2,
    a ~ dnorm(178, 100),
    b1 ~ dnorm(0, 10),
    b2 ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ),
  data=d
)

# look at summary table; caution: might be hard to interpret
precis(m4.5)

# simulate some heights and calc some summary stats of mu posterior
weight.seq <- seq(from=-2.2, to=2, length.out=30)
pred_dat <- list(weight.s=weight.seq, weight.s2=weight.seq^2)
mu <- link(m4.5, data=pred_dat)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob=.89)
sim.height <- sim(m4.5, data=pred_dat)
height.PI <- apply(sim.height, 2, PI, prob=0.89)

# plot
plot(height ~ weight.s, d, col=col.alpha(rangi2, 0.65))
lines(weight.seq, mu.mean)
shade(mu.PI, weight.seq, col=col.alpha("black", 0.65))
shade(height.PI, weight.seq, col=col.alpha("black", 0.25))

# to convert the x axis back to natural scale
# turn off the x-axis with the xaxt parameter 
plot(height ~ weight.s, d, col=col.alpha(rangi2, 0.5), xaxt="n")
# reconstruct the x axis
# define location of the labels
at <- c(-2, -1, 0, 1, 2)
# convert the units back toß original scale
labels <- at*sd(d$weight) + mean(d$weight)
# draw the axis
axis(side=1, at=at, labels=round(labels, 1))

