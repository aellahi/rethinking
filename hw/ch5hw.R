library(rethinking)

# 5M1 - create your own spurious correlation
N <- 100 # number of observations
rho = 0.85
rho2 = rho/3
one <- rnorm(N)
# x1 is positively correlated with y 
y <- rnorm(N, rho*one, sqrt(1-rho^2))
# now define a 2nd predictor, x2,
# that is negatively correlated with y
two <- rnorm(N, rho2*y, sqrt(1-rho2^2))

# visually inspect correlations
plot(y ~ one)
plot(y ~ two)
plot(two ~ one)

# create a dataframe
d <- data.frame(one, two, y=y)
dim(d)
head(d)

# calculate correlation of predictors to each other
m <- map(alist(
  y ~ dnorm(mu, sigma),
  mu <- a + b1*one,
  a ~ dnorm(0,2),
  b1 ~ dnorm(0,1),
  sigma ~ dunif(0,10)
),
data=d)
precis(mOne)

# fit bivariate model with either x
mOne <- map(alist(
  y ~ dnorm(mu, sigma),
  mu <- a + b1*one,
  a ~ dnorm(0,2),
  b1 ~ dnorm(0,1),
  sigma ~ dunif(0,10)
),
data=d)
precis(mOne)

mTwo <- map(alist(
  y ~ dnorm(mu, sigma),
  mu <- a + b2*two,
  a ~ dnorm(0,2),
  b2 ~ dnorm(0,1),
  sigma ~ dunif(0,10)
),
data=d)
precis(mTwo)
precis_plot(precis(mTwo))

# now fit a model with both one and two
m5m1 <- map(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- a + b1*one + b2*two,
    a ~ dnorm(0, 10),
    c(b1,b2) ~ dnorm(0,1),
    sigma ~ dunif(0, 10)
    ), 
  data=d)
precis(m5m1, digits=3)
precis_plot(precis(m5m1))

# Conclusion: the strength of the correlation of each individual predictor
# "dilutes" the strength of both predictors together,
# but not equally...if one is very strongly correlated, it is *less*
# diluted than the one that is weakly correlated.
# direction of correlation does not matter. If both are strongly correlated,
# then they are equally "diluted"

precis_plot(precis(m5m1))

# 5M2 - create your own masked relationship 
# outcome should be correlated with both predictors but 
# in opposite directions, plus correlated with one another
N <- 100 # number of observations
rho = 0.85
one <- rnorm(N)
# x1 is positively correlated with y 
y <- rnorm(N, rho*one, sqrt(1-rho^2))
# now define a 2nd predictor, x2,
# that is negatively correlated with y
two <- rnorm(N, -1*rho*y, sqrt(1-rho^2))

# visually inspect correlations
plot(y ~ one)
plot(y ~ two)
plot(two ~ one)
cor(one, two)

# create a dataframe
d <- data.frame(one, two, y=y)
dim(d)
head(d)

# 5M3 - How might a higher divorce rate lead to higher marriage rate?
# Perhaps those that marry young and divorce re-enter the pool of marriage-
# eligable people and re-marry because they still want to marry? 

# Hard problems
data(foxes)
head(foxes)
str(foxes)

# standardize variables
foxes$weight.s <- (foxes$weight-mean(foxes$weight))/sd(foxes$weight)
foxes$area.s <- (foxes$area - mean(foxes$area))/sd(foxes$area)
head(foxes)

# body weight as a function of territory size (area)
mA <- map(
  alist(
    foxes$weight.s ~ dnorm(mu, sigma),
    mu <- a + ba*foxes$area.s,
    a ~ dnorm(1,10),
    ba ~ dnorm(0,1),
    sigma ~ dunif(0,10)
  ),
  data=foxes
)
precis(mA, digits=4)


# body weight as a function of groupsize 
# group size is a categorical variable we can convert to an index
foxes$groupsize.i <- coerce_index(foxes$group)