### CHAPTER 4 PROBLEMS ###

library(rethinking)
# load data
data(Howell1)
d <- Howell1 

# 4E1 - likelihood is yi ~ Normal(mu, sigma)
# 4E2 - 2 parameters in the posterior distribution (mu & sigma)
# 4E3 - Pr ~ likelihood x prior 
# 4E4 - ui = a + bxi
# 4E5 - 3 (a, b, sigma)
# 4M1 - simulate observed heights from the prior (not the posterior)

y ~ dnorm(mu, sigma)
mu ~ dnorm(0, 10)
sigma ~ dunif(0, 10)

# simulate prior distribution of heights
# by sampling from prior distributions of mu and sigma
sample_mu <- rnorm(1e4, 0, 10)
sample_sigma <- runif(1e4, 0, 10)
prior_h <- rnorm(1e4, sample_mu, sample_sigma)
dens(prior_h)


# 4M2 - translate model above into map formula
m4m2 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ),
  data=d
)

# 4M3 
# y ~ Normal(mu, sigma)
# mui = a + bxi
# a ~ Normal(0, 50)
# b ~ Uniform(0, 10)
# sigma ~ Uniform(0, 50)

# load data
data(Howell1)
d <- Howell1

# Hard 
# define & fit the model 
d$weight.c <- d$weight - mean(d$weight)
m4h1 <- map(
  alist(height ~ dnorm(mu, sigma), #likelihood
        mu <- a + b*weight.c, # define linear model
        a ~ dnorm(178, 100),
        b ~ dnorm(0, 10),
        sigma ~ dunif(0, 50)),
  data=d
)

# view model summary table 
precis(m4h1, corr=TRUE)

# simulate height values at different weights
# sample from this simulation
plot(height ~ weight.c, data=d)
abline(a=coef(m4h1)["a"], b=coef(m4h1)["b"], col="red")

?link
link.output <- link(m4h1)

weights_to_pred <- c(46.95, 43.72, 64.78, 32.59, 54.63)
weights_to_pred <- data.frame("weight"= weights_to_pred)
weights_to_pred$weight.c <- weights_to_pred$weight - mean(weights_to_pred$weight)
pred_heights <- link(m4h1, data=data.frame(weights_to_pred), 1e4)

# expected heights (mean of posterior distribution)
pred_ht_mean <- apply(pred_heights, 2, mean)

ht.HPDI <- apply(pred_heights, 2, HPDI, prob=0.89)

# 4H2
# below 18 yrs of age
d2 <- d[d$age < 18,]
# add column for centered weights
d2$weight.c <- d2$weight - mean(d2$weight)
head(d2)
dim(d2)

# fit model
m4h2 <- map(
  alist(height ~ dnorm(mu, sigma), #likelihood
        mu <- a + b*weight.c, # define linear model
        a ~ dnorm(50, 100),
        b ~ dnorm(0, 10),
        sigma ~ dunif(0, 75)),
  data=d2
)

precis(m4h2, corr=TRUE)
link2.output <- link(m4h2, data=d2)
mu <- apply(link2.output, 2, mean)
link2.hpdi <- apply(link2.output, 2, HPDI)

weights.seq <- data.frame("weight"=seq(from=3, to=65, by=1))
weights.seq$weight.c <- weights.seq$weight - mean(d$weight)

pred_output <- link(m4h2, data=data.frame(weights.seq))
hpdi.pred <- apply(pred_output, 2, HPDI)

plot(height ~ weight.c, data=d2)
abline(a=coef(m4h2)["a"], b=coef(m4h2)["b"], col="red")
shade(hpdi.pred, weights.seq$weight.c, col=col.alpha(rangi2, 0.3))
shade(link2.hpdi, d2$weight.c, col=col.alpha("black", 0.5))

# 4H3
# add a column for standardized weights 
d$weight.c <- d$weight - mean(d$weight)
d$weight.s <- (d$weight.c)/sd(d$weight)
# check
mean(d$weight.s)
sd(d$weight.s)

m4h3 <- map(
  alist(height ~ dnorm(mu, sigma),
        mu <- a + b*log(d$weight),
        a ~ dnorm(178, 100),
        b ~ dnorm(0, 100),
        sigma ~ dunif(0, 50)),
        data=d)

# interpret model

# first simulate predictions for each weight
link4h3 <- link(m4h3)
mu <- apply(link4h3, 2, mean)
hpdi.mu <- apply(link4h3, 2, HPDI, prob=0.97)

# getting an error with the code below...
weights_to_pred <- data.frame("weight"=seq(from=3, to=65, by=1))
pred_hts <- link(m4h3, data=weights_to_pred)
hpdi_pred <- apply(pred_hts, 2, HPDI, prob=0.97)

weights_to_pred$weights.log <- log(weights_to_pred$weight)

# plot
# raw data 
plot(height ~ weight, data=Howell1,
     col=col.alpha(rangi2, 0.4))
# predicted mean height
plot(d$weight, mu, col="red", type="p")
# plot 97% HPDI of predicted mean height
shade(hpdi.mu, d$weight, col=col.alpha(rangi2, 0.3))
# plot 97% HPDI of predicted heights
shade(hpdi_pred, d2$weight, col=col.alpha("black", 0.5))