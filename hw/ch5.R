# CH 5 MULTIVARIATE LINEAR MODELS

# re-install rethinking packages because of plotting error
install.packages(c("devtools","mvtnorm","loo","coda"), repos="https://cloud.r-project.org/",dependencies=TRUE)
library(devtools)
install_github("rmcelreath/rethinking")

library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce
head(d)
dim(d)

# standardize predictor MedianAgeMarriage
d$MedianAgeMarriage.s <- (d$MedianAgeMarriage - mean(d$MedianAgeMarriage))/sd(d$MedianAgeMarriage)

# fit linear model with just MedianAgeMarriage
m5.1 <- map(
  alist(Divorce ~ dnorm(mu, sigma),
        mu <- a+ bA*MedianAgeMarriage.s,
        a ~ dnorm(10, 10),
        bA ~ dnorm(0, 1),
        sigma ~ dunif(0, 10)),
  data=d
)

# compute percentile interval of mean
MAM.seq <- seq(from=-3, to=3.5, length.out=30)
mu <- link(m5.1, data=data.frame(MedianAgeMarriage.s=MAM.seq))
mu.PI <- apply(mu, 2, PI)

# plot
plot(Divorce ~ MedianAgeMarriage.s, data=d, col=rangi2)
abline(m5.1)
shade(mu.PI, MAM.seq)

precis(m5.1)

# Try fitting a similar model using just marriage rate
d$Marriage.s <- (d$Marriage - mean(d$Marriage))/sd(d$Marriage)
m5.2 <- map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu <- a + bR*d$Marriage.s,
    a ~ dnorm(10, 10),
    bR ~ dnorm(0, 1),
    sigma ~ dunif(0,10)
  ),
  data=d
)

# define and fit multvariate model where divorce rate
# is function of median age at marriage and marriage rate
m5.3 <- map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu <- a + bR*Marriage.s + bA*MedianAgeMarriage.s,
    a ~ dnorm(10,10),
    bR ~ dnorm(0, 1),
    bA ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data=d
)
library(rstan)

precis(m5.3)
precis_plot(precis(m5.3))

# predictor residual plots 
# Question: How useful are *both* marriage rate and median-marriage-age
# as predictors of divorce rate? 

# try modeling marriage rate as a function of median-marriage-age
m5.4 <- map(alist(
  Marriage.s ~ dnorm(mu, sigma),
  mu <- a + b*MedianAgeMarriage.s,
  a ~ dnorm(0, 10),
  b ~ dnorm(0, 1),
  sigma ~ dunif(0, 10)
), 
data=d)

# get predicted values for marriage rate based on MedianAgeMarriage
mu <- coef(m5.4)['a'] + coef(m5.4)['b']*d$MedianAgeMarriage.s 
# compute residuals by subtracted observed from predicted
m.resid <- d$Marriage.s - mu 
d$m.resid <- m.resid

# plot 
plot(Divorce ~ m.resid, d, col=rangi2)

# reverse and compute medianagemarriage residuals
m5.4.2 <- map(alist(
  MedianAgeMarriage.s ~ dnorm(mu, sigma),
  mu <- a + b*Marriage.s,
  a ~ dnorm(0, 10),
  b ~ dnorm(0, 1),
  sigma ~ dunif(0, 10)
),
data=d)

# generate predictions for MedianAgeMarriage as fxn of marriage rate
mu <- coef(m5.4.2)['a'] + coef(m5.4.2)['b']*d$Marriage.s
a.resid <- d$MedianAgeMarriage.s - mu

# plot
d$a.resid <- a.resid
plot(Divorce ~ a.resid, d, col=rangi2)

# counterfactual plots: examining the effect of one predictor on 
# the response while holding the other predictor constant

# prepare some counterfactual data
A.avg <- mean(d$MedianAgeMarriage.s)
R.seq <- seq(from=-3, to=3, length.out=30)
pred.data <- data.frame(
  Marriage.s=R.seq,
  MedianAgeMarriage.s=A.avg)

# compute counterfactual divorce rate
mu <- link(m5.3, data=pred.data)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

# simulate counterfactual divorce outcomes
# how is this different from map? 
R.sim <- sim(m5.3, data=pred.data, n=1e4)
R.PI <- apply(R.sim, 2, PI)

# display predictions, hide raw using "n"
plot(Divorce ~ Marriage.s, type="n", data=d)
mtext("MedianAgeMarriage.s=0")
lines(R.seq, mu.mean)
shade(mu.PI, R.seq)
shade(R.PI, R.seq)

# now do counterfactual for MedianAgeMarriage varying
R.avg <- mean(d$Marriage.s)
A.seq <- seq(from=-3, to=3, length.out=30)
pred.data <- data.frame(
  MedianAgeMarriage.s=A.seq,
  Marriage.s=R.avg)
head(pred.data)

# compute counterfactual divorce rate
mu <- link(m5.3, data=pred.data)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

# simulate
A.sim <- sim(m5.3, data=pred.data, n=1e4)
A.PI <- apply(A.sim, 2, PI)

plot(Divorce ~ MedianAgeMarriage.s, type="n", data=d)
mtext("Marriage.s=0")
lines(A.seq, mu.mean)
shade(mu.PI, A.seq)
shade(A.PI, A.seq)

# Another way of evaluating: compare simulated predictions
# to actual data
mu <- link(m5.3)
# summarize
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

# simulate new observations with original inputs
divorce.sim <- sim(m5.3, n=1e4)
divorce.PI <- apply(divorce.sim, 2, PI)

# plot predicted vs actual
plot(mu.mean ~ d$Divorce, col=rangi2, ylim=range(mu.PI),
     xlab="Observed divorce", ylab="Predicted Divorce")
abline(a=0,b=1,lty=2)
for (i in 1:nrow(d))
  lines(rep(d$Divorce[i], 2), c(mu.PI[1, i], mu.PI[2,i]),
        col=rangi2)

# model is under-predicting divorce rates for some states 
# and over-predicted for others
# mark some (UT & ID):
identify(x=d$Divorce, y=mu.mean, labels=d$Loc, cex=0.8)

# compute residuals
divorce.resid <- d$Divorce - mu.mean
# order by residual
o <- order(divorce.resid)
# make the plot
dotchart(divorce.resid[o], labels=d$loc[o], xlim=c(-6, 5),
         cex=0.6)
abline(v=0, col=col.alpha("black", 0.2))

# 5.2 Masked relationship 
library(rethinking)
data(milk)
d <- milk
str(d)

# first set up a simple bi-variate regression model
# with kcal.per.g and neocortex.perc
m5.5 <- map(alist(
  kcal.per.g ~ dnorm(mu, sigma),
  mu <- a + bn*neocortex.perc,
  a ~ dnorm(0, 100),
  bn ~ dnorm(0, 1),
  sigma ~ dunif(0, 1)
),
data=dcc
)

# drop all the NA values from d 
dcc <- d[complete.cases(d), ]

# view summary
precis(m5.5, digits=3)
# bn is pretty wide, plot predictions to see PI
np.seq <- 0:100
pred.data <- data.frame(neocortex.perc=np.seq)
mu <- link(m5.5, data=pred.data, n=1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

# plot
plot(kcal.per.g ~ neocortex.perc, data=dcc, col=rangi2)
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty=3)
lines(np.seq, mu.PI[2,], lty=2)

# repeating fit above with female body mass (d$mass)
# reveals a slightly negative correlation 
# want to use log because magnitude is what we're after 
# (still not entirely sure why this is the case)
dcc$log.mass <- log(dcc$mass)
m5.6 <- map(alist(
  kcal.per.g <- dnorm(mu, sigma),
  mu <- a + bm*log.mass,
  a ~ dnorm(0, 100),
  bm ~ dnorm(0,1),
  sigma ~ dunif(0,1)
),
data=dcc)
precis(m5.6)

# now fit a multivariate model with both 
# log.mass and neocortex.perc:
m5.7 <- map(alist(
  kcal.per.g <- dnorm(mu, sigma),
  mu <- a + bn*neocortex.perc + bm*log.mass,
  a ~ dnorm(0, 100),
  bn ~ dnorm(0,1),
  bm ~ dnorm(0,1),
  sigma ~ dunif(0,1)
),
data=dcc)
precis(m5.7)
precis_plot(precis(m5.7))

# make some counterfactual plots
# to observe each predictors contribution 
# while holding the other constant
mean.log.mass <- mean(log(dcc$mass))
n.seq <- 0:100
pred.data <- data.frame(neocortex.perc=np.seq,
                        log.mass=mean.log.mass)
mu <- link(m5.7, data=pred.data, n=1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

# plot
plot(kcal.per.g ~ neocortex.perc, data=dcc, type="n")
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty=2)
lines(np.seq, mu.PI[2,], lty=2)

# do analagous counterfactual for mass 
mean.neocortex.perc <- mean(dcc$neocortex.perc)
mass.seq <- -10:10
pred.data <- data.frame(log.mass=mass.seq,
                        neocortex.perc=mean.neocortex.perc)
mu <- link(m5.7, data=pred.data, n=1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

# plot
plot(kcal.per.g ~ log.mass, data=dcc, type="n")
lines(mass.seq, mu.mean)
lines(mass.seq, mu.PI[1,], lty=2)
lines(mass.seq, mu.PI[2,], lty=2)

# simulate a masking relationship
N <- 100  #number of cases
rho <- 0.7 # correlation between x_pos and x_neg
x_pos <- rnorm(N)
x_neg <- rnorm(N, rho*x_pos)

# 5.3 reasons to not add more and more predictors
# REASON 1: MULTICOLLINEARITY 
# simulate a dataset where height is a function of
# leg lengths (L & R)
N <- 100 # number of individuals 
height <- rnorm(N, 10, 2) # sim total height of each
leg_prop <- runif(N, 0.4, 0.5) # leg as proportion of height
leg_left <- leg_prop*height + rnorm(N, 0, 0.02)
leg_right <- leg_prop*height + rnorm(N, 0, 0.02)
d <- data.frame(height, leg_left, leg_right)

# now try to predict height using both 
# leg_left and leg_right as predictors 
m5.8 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bl*leg_left + br*leg_right,
    a ~ dnorm(10, 100),
    bl ~ dnorm(2, 10),
    br ~ dnorm(2, 10),
    sigma ~ dunif(0, 10)
  ),
  data=d
)
precis(m5.8)
precis_plot(precis(m5.8))
# the standard deviations for bl and br are huge!!
# the posterior distribution for each is very wide
# take a look at how samples from the posterior for both
# are correlated: 
post <- extract.samples(m5.8)
plot(bl ~ br, post, col=col.alpha(rangi2,0.1), pch=16)

# plot the sum
sum_blbr <- post$bl + post$br
dens(sum_blbr, col=rangi2, lwd=2, xlab="sum of bl and br")
mean(sum_blbr)

# try fitting a regression with just one variable
# it will be very close to the sum
m5.9 <- map(alist(
  height ~ dnorm(mu, sigma),
  mu <- a + bl*leg_left,
  a ~ dnorm(10, 100),
  bl ~ dnorm(2, 10),
  sigma ~ dunif(0,10)
),
data=d)
precis(m5.9)

# multicollinearity in the milk dataset
library(rethinking)
data(milk)
d <- milk

plot(perc.fat ~ perc.lactose, data=milk)
# start by modeling each predcitor as a single
# bivariate regression
# k.cal.g regressed on perc.fat
m5.10 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bf*perc.fat,
    a ~ dnorm(0.6, 10),
    bf ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data=d
)

# k.cal.g regressed on perc.lactose
m5.11 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bl*perc.lactose,
    a ~ dnorm(0.6, 10),
    bl ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data=d
)

precis(m5.10, digits=3)
precis(m5.11, digits=3)
# each on it's own is a strong predictor of k.cal.g,
# albeit in exactly the opposite direction. 
# look what happens when both are included in a multivariate model:
m5.12 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bf*perc.fat + bl*perc.lactose,
    a ~ dnorm(0.6, 10),
    bf ~ dnorm(0, 1),
    bl ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data=d
)
precis_plot(precis(5.12))
precis(m5.12, digits=3)

# plot pairs
pairs( ~ kcal.per.g + perc.fat + perc.lactose, data=d, col=rangi2)
# compute correlation between perc.fat and perc.lactose:
cor(d$perc.fat, d$perc.lactose)
# how much correlation is too much? Difficult to say, but can 
# quantify this by running a simulation in which a fake predictor
# "x" that is correlated at different percents is included in the model

# code to do this:
# define a function that generates random values
# that are correlated with perc.fat
sim.col1 <- function(r=0.9) {
  d$x <- rnorm(nrow(d), mean=r*d$perc.fat,
               sd=sqrt(1-r^2)*var(d$perc.fat) ) 
  # fit regression
  m <- lm(kcal.per.g ~ perc.fat + x, data=d)
  sqrt(diag(vcov(m)))[2] #stdev of parameter
}

rep.sim.col1 <- function(r=0.9, n=100) {
  stddev <- replicate(n, sim.col1(r))
  mean(stddev)
}

r.seq <- seq(from=0, to=0.99, by=0.01)
stddev <- sapply(r.seq, function(z) rep.sim.col1(r=z, n=100))
plot(stddev ~ r.seq, type='l', col=rangi2, lwd=2, xlab="correlation")

# 5.3.3 - post-treatment bias
# illustrate with another example, simulate some data:
# number of plans
N <- 100
# simulate initial heights
h0 <- rnorm(N, 10, 2)
# assignment treatments and simulate fungus and growth
treatment <- rep(0:1, each=N/2)
# simulate presence of fungus as decreasing in probability with 
# presence of treatment!
fungus <- rbinom(N, size=1, prob=0.5 - treatment*0.4)
h1 <- h0 + rnorm(N, 5-3*fungus)
d <-data.frame(h0=h0, h1=h1, treatment=treatment, fungus=fungus)

# question: what is the impact of treatment on growth? 
# fit model with all variables
m5.13 <- map(alist(
  h1 ~ dnorm(mu, sigma),
  mu <- a + bh*h0 + bt*treatment + bf*fungus,
  a ~ dnorm(0,100),
  c(bh,bt,bf) ~ dnorm(0, 10),
  sigma ~ dunif(0,10)
),
data=d)
precis(m5.13)
# above summary shows that coeff for bt is small and negative!
# here, fungus is a "post-treatment" variable
# because it is a *consequence* of the treatment
# it should be ommitted from the model!

# re-fit without fungus:
m5.14 <- map(
  alist(
    h1 ~ dnorm(mu, sigma),
    mu <- a + bh*h0 + bt*treatment,
    a ~ dnorm(0,100),
    c(bh,bt) ~ dnorm(0,10),
    sigma ~ dunif(0,10)
  ),
  data=d
)
precis(m5.14)

# in experimental studies, post-treatment variables are easier to identify
# harder in observational studies

# 5.4 CATEGORICAL VARIABLES aka FACTORS 
# How does the computer deal with them? 

# 5.4.1 Binary categories
# e.g. male vs female (0 or 1)
# mathematically, results in a parameter
# being turn "on" or "off" depending on value of categorical variable
data(Howell1)
d<-Howell1
str(d)

m5.15 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bm*male,
    a ~ dnorm(178,100),
    bm ~ dnorm(0,10),
    sigma ~ dunif(0,50)),
data=d)
precis(m5.15)

# now a just becomes the mean of female height
# and bm is the average difference between males and females

# to get a sense of the width of the posterior
# can't simplly add a and bm, because they are correlated!
# can sample from posterior instead
post <- extract.samples(m5.15)
mu.male <- post$a + post$bm
PI(mu.male)

# intercept ends up being value of mean of the 
# 0 category, b (slope) ends up being difference
# of the means

# 5.4.2 Many categories: For k categories, need k-1 dummy variables
# explore with taxonomic group in milk dataset
data(milk)
d <- milk
unique(d$clade)

# make dummy variables:
d$clade.NWM <- ifelse(d$clade=="New World Monkey", 1, 0)
d$clade.NWM
d$clade.OWM <- ifelse(d$clade=="Old World Monkey", 1, 0)
d$clade.S <- ifelse(d$clade=="Strepsirrhine", 1, 0)

# let's try to define a linear model for kcal.per.g regressed on clade:
m5.16 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + b.NWM*clade.NWM + b.OWM*clade.OWM + b.S*clade.S,
    a ~ dnorm(0.6,10),
    b.NWM ~ dnorm(0,1),
    b.OWM ~ dnorm(0,1),
    b.S ~ dnorm(0,1),
    sigma ~ dunif(0,10)
  ), 
  data=d
)
precis(m5.16)

# sample posterior
post <- extract.samples(m5.16)
# compute averages for each category
mu.ape <- post$a
mu.NWM <- post$a + post$b.NWM
mu.OWM <- post$a + post$b.OWM
mu.S <- post$a + post$b.S

precis(data.frame(mu.ape, 
                  mu.NWM,
                  mu.OWM,
                  mu.S), digits=3)

# Another way to deal with multiple categories UNIQUE INTERCEPTS
# can also specify a parameter of interceps (unique intercepts)
# for each categorical variable (category) using an "index variable"
d$clade_id <- coerce_index(d$clade)
?coerce_index

# then specify a unique vector of intercepts for each in map:
m5.16_alt <- map(alist(
  kcal.per.g ~ dnorm(mu, sigma),
  mu <- a[clade_id],
  a[clade_id] ~ dnorm(0.6, 10),
  sigma ~ dunif(0,10)
), data=d)
precis(m5.16_alt, depth=2)

# 5.5 ORDINARY LEAST SQUARES (aka OLS) and lm
# "OLS" solves for parameter values that minimize sum of squared residuals
# can use R's lm to get functionally equivalent answers as map,
# provided you are okay with flat priors 

# Design formulas: a notation to simplify the definition of a model
# example:
mtest <- lm(kcal.per.g ~ 1 + perc.fat, data=d)

# some important quirks:
## intercepts are optional (i.e., y ~ 1 + x EQUAL TO y ~ x)
## should mark categorical variables using the 'as.factor' function
## Transform variables before passing to lm.
## lm will not give a posterior distribution for sigma. Will report "residual standard error"

# can use "glimmer" function to translate lm design formulas
# into map formulas with flat priors:
data(cars)
glimmer(dist ~ speed, data=cars)
?glimmer