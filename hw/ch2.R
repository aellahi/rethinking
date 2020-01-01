# chapter 2 problems
# 2M1 
# (1) W, W, W

# define W (# of "successes")
w <- 3

# define N, number of trials
N <- 3

# define grid
p_grid <- seq(from=0, to=1, length.out=20)

# define prior
prior <- rep(1,20)

# compute likelihood at each value in grid
likelihood <- dbinom(w, size=N, prob=p_grid)

# compute posterior, or product of likelihood & prior
unstd.posterior <- likelihood * prior

# standardize prior so it sums to 1
posterior <- unstd.posterior/sum(unstd.posterior)

# plot 
plot(p_grid, posterior, type="b",
     xlab="Probability of Water", ylab="posterior probability")
mtext("20 points")

# (2) W, W, W, L
# repeat above except:
N <- 4

# (3) L, W, W, L, W, W, W
w <- 5
N <- 7

# 2M2 prior = 0 when p < 0.5 and positive when p >= 0.5
prior <- ifelse(p_grid >= 0.5, 1, 0)

# 2M3 Two globes, one earth and one mars. 
# pEarth = 0.70 for water; 0.3 for land
# pMars for water = 0, 1 for land

# Use Bayes' theorem! 
# Pr(Earth|land) = P(land|Earth) * P(Earth) / Pr(Land)
# P(land|Earth) = 0.3
# P(Earth) = 0.5 (either Mars or Earth)
# Pr(Land) = avg of likelihood for each scenario
# so calculate likelihood on Earth, then likelihood on Mars, and avg

pr.land <- (dbinom(1, 1, 0.3) + dbinom(1, 1, 1))/2 
print(pr.land)

# plug
pr.earth_land = 0.3 * 0.5 / pr.land
print(pr.earth_land)

# 2M4: 3 cards 



## 2H1

# find posterior for plausibility of each pandas species following the first birth of twins
species.1.likelihood <- .1
species.2.likelihood <- .2
likelihood <- c(species.1.likelihood, species.2.likelihood)
prior <- c(1, 1)
unstandardized.posterior <- likelihood * prior
posterior <- unstandardized.posterior / sum(unstandardized.posterior)

# probability next birth is set of twins
posterior[1] * .1 + posterior[2] * .2


