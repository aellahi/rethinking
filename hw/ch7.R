# CH 7: INTERACTIONS

library(rethinking)
data(rugged)
d <- rugged
head(d)

# question: How does GDP change as function of ruggedness?
# make log version of outcome (GDP)
d$log_gdp <- log(d$rgdppc_2000)

# extract countries with GDP data
dd <- d[complete.cases(d$rgdppc_2000),]
dim(dd)
dim(d)

# split countries into Africa and not-Africa
d.A1 <- dd[dd$cont_africa==1 , ] #Africa
d.A0 <- dd[dd$cont_africa==0 ,] # not Africa

# fit regression models one for each split

m7.1 <- map(alist(
  log_gdp ~ dnorm(mu, sigma),
  mu <- a + bR*rugged,
  a ~ dnorm(8, 100),
  bR ~ dnorm(0,1),
  sigma ~ dunif(0, 10)),
  data=d.A1
)

m7.2 <- map(alist(
  log_gdp ~ dnorm(mu, sigma),
  mu <- a + bR*rugged,
  a ~ dnorm(8, 100),
  bR ~ dnorm(0, 1),
  sigma ~ dunif(0, 10)),
  data=d.A0)

plot(log_gdp ~ rugged, data=d.A1)
plot(log_gdp ~ rugged, data=d.A0, col=rangi2)
precis(m7.1)

# Observation: In Africa, terrain ruggedness is positively correlated with GDP. In non-African cuntries, terrain ruggedness is negatively correlated with GDP.

# generate predicted line
rugged.seq <- seq(from=0, to=7, by=0.5)
m7.1_pred <- link(m7.1, data=data.frame(rugged=rugged.seq))
dim(m7.1_pred)
mu_pred <- apply(m7.1_pred, 2, mean)
hpdi_mu <- apply(m7.1_pred, 2, HPDI)

plot(log_gdp ~ rugged, data=d.A1)
lines(rugged.seq, mu_pred)
shade(hpdi_mu, rugged.seq)

m7.2_pred <- link(m7.2, data=data.frame(rugged=rugged.seq))
mu2_pred <- apply(m7.2_pred, 2, mean)
mu2_hpdi <- apply(m7.2_pred, 2, HPDI)

plot(log_gdp ~ rugged, data=d.A0)
lines(rugged.seq, mu2_pred)
shade(mu2_hpdi, rugged.seq)

precis(m7.2)
precis(m7.1)

# How to recover this effect using the entire dataset?

# first fit model with *all* data
m7.3 <- map(alist(
  log_gdp ~ dnorm(mu, sigma),
  mu <- a + bR*rugged,
  a ~ dnorm(8, 100),
  bR ~ dnorm(0, 1),
  sigma ~ dunif(0, 10)
), data=dd)

precis(m7.3)

# now fit with all data and make cont_africa a categorical variable
m7.4 <- map(alist(
  log_gdp ~ dnorm(mu, sigma),
  mu <- a + bR*rugged + bA*cont_africa,
  a ~ dnorm(8,100),
  bR ~ dnorm(0, 1),
  bA ~ dnorm(0, 1),
  sigma ~ dunif(0, 10)), data=dd)

precis(m7.4)

compare(m7.3, m7.4)

# Mode m7.4 (with Africa as categorical variable) is picking up on something important about Africa (as evidenced by the model's superiority to m7.3 when compared). Now fix Africa as either 0 or 1 and see if posterior predicts can pick up on different slopes for each:

rugged.seq <- seq(from=-1, to=8, by=0.25)
# fix cont_africa =0 and compute mu over these samples
mu.NotAfrica <- link(m7.4, data=data.frame(cont_africa=0, rugged=rugged.seq))

# compute mu with cont_africa=1
mu.Africa <- link(m7.4, data=data.frame(cont_africa=1, rugged=rugged.seq))

# summarize means and intervals
mu.NotAfrica.mean <- apply(mu.NotAfrica, 2, mean)
hpdi.NotAfrica <- apply(mu.NotAfrica, 2, HPDI, prob=0.95)

mu.Africa.mean <- apply(mu.Africa, 2, mean)
hpdi.Africa <- apply(mu.Africa, 2, HPDI, prob=0.95)

# plot
plot(log_gdp ~ rugged, data=d.A0)
lines(rugged.seq, mu.NotAfrica.mean)
par(new=TRUE)
plot(log_gdp ~ rugged, data=d.A1, col="blue")
lines(rugged.seq, mu.Africa.mean, col='blue')

# define model with interaction term between bR & bA

m7.5 <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + gamma*rugged + bA*cont_africa,
    gamma <- bR + bAR*cont_africa,
    a ~ dnorm(8, 100),
    bA ~ dnorm(0, 1),
    bR ~ dnorm(0, 1),
    bAR ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data=dd
)
precis(m7.5)

# compare this model to the previous two
compare(m7.3, m7.4, m7.5)

# to generate predictions for African and non-African countries
rugged.seq <- seq(from=-1, to=8, by=0.25)
# fix cont_africa =0 and compute mu over these samples
mu.NotAfrica <- link(m7.5, data=data.frame(cont_africa=0, rugged=rugged.seq))

# compute mu with cont_africa=1
mu.Africa <- link(m7.5, data=data.frame(cont_africa=1, rugged=rugged.seq))

# summarize means and intervals
mu.NotAfrica.mean <- apply(mu.NotAfrica, 2, mean)
hpdi.NotAfrica <- apply(mu.NotAfrica, 2, HPDI, prob=0.95)

mu.Africa.mean <- apply(mu.Africa, 2, mean)
hpdi.Africa <- apply(mu.Africa, 2, HPDI, prob=0.95)

# plot
par(new=FALSE)
d.A1 <- dd[dd$cont_africa==1,]
plot(log(rgdppc_2000) ~ rugged, data=d.A1,
     col=rangi2, ylab="log GDP year 2000",
     xlab="Terrain Ruggedness Index")
mtext("African Nations", 3)
lines(rugged.seq, mu.Africa.mean, col=rangi2)
shade(hpdi.Africa, rugged.seq, col=col.alpha(rangi2, 0.3))

# plot non-african nations with regression
d.A0 <- dd[dd$cont_africa==0,]
plot(log(rgdppc_2000) ~ rugged, data=d.A0,
     ylab="log GDP year 2000", xlab="Terrain Ruggedness Index")
mtext("Non African Nations", 3)
lines(rugged.seq, mu.NotAfrica.mean)
shade(hpdi.NotAfrica, rugged.seq)