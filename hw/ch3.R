# Chapter 3 notes Sampling from the imaginary 

library(rethinking)

# generate a posterior distribution 
# p_grid repesents your parameters 
p_grid <- seq(from=0, to=1, length.out=1000)
prior <- rep(1, 1000)
likelihood <- dbinom(6, size=9, prob=p_grid)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)

# sample from it 
samples <- sample(p_grid, prob=posterior, size=1e4, replace=TRUE)
?sample

# what is the posterior probability that the proportion of water on earth is 0.5?
# add all of the posterior values for p_grid values less than 0.5. Add because 
# that is what you do with probability values 
sum(posterior[p_grid < 0.5])

# do this with sampling
sum(samples< 0.5)/1e4

# how to get the value with the most density? 
# the mode? or median? 

# to get probability that marks the lower 80% of the distribution
# the quantile function produces the value that marks the probability 
# in the input 
quantile(samples, 0.8)

# to get boundaries for 10% and 90% quantile
quantile(samples, c(0.1, 0.9))

# get the value with the max posterior
p_grid[which.max(posterior)]

?which.max

# loss functions: proportional to the distance d of a parameter value from the "true" 
# parameter value 

### Ch 3 Practice ###

# Easy #

p_grid <- seq(from=0, to=1, length.out=1000)
prior <- rep(1, 1000)
likelihood <- dbinom(6, size=9, prob=p_grid)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)
set.seed(100)
samples <- sample(p_grid, prob=posterior, size=1e4, replace=TRUE)

# 3E1 How much posterior probability lies below p = 0.2?
below_0.2 <- sum(samples < 0.2)/length(samples)
below_0.2

# 3E2 How much posterior probability lies above p = 0.8?
above_0.8 <- sum(samples > 0.8)/length(samples)
above_0.8

# 3E3 How much posterior probability lies between p = 0.2 and p = 0.8?
bw <- sum(samples > 0.2 & samples < 0.8)/length(samples)
bw

# 3E4 20% of the posterior probability lies below which value of p? 
quantile(samples, 0.2)

# 3E5 20% of the posterior probability lies above which value of p? 
quantile(samples, 0.8)

# 3E6 Which values of p contain the narrowest interval equal to 66% of the posterior
# probability? 
HPDI(samples, 0.66)

# 3E7 Which values of p contain 66% of the posterior probability, assuming equal posterior
# probability both below and above the interval? 
PI(samples, 0.66)

# Medium # 
# 3M1 
p_grid <- seq(from=0, to=1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(8, size=15, prob=p_grid)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)

# 3M2
samples <- sample(p_grid, prob=posterior, size=1e4, replace = TRUE)
hpdi_0.9 <- HPDI(samples, 0.9)
hpdi_0.9

# 3M3
# to simulate predicted observations for a single value of p, use random binomial sampling:
# w <- rbinom(n=1e4, size=9, prob=p)
?rbinom
w <- rbinom(1e4, size=15, prob=samples)
p_obs_8 = sum(w==8)/length(w)

#3M4
w <- rbinom(1e4, size=9, prob=samples)
p_obs_6 = sum(w==6)/length(w)

#3M5
p_grid <- seq(from=0, to=1, length.out = 1000)
prior <- ifelse(p_grid > 0.5, 1, 0)
likelihood <- dbinom(8, size=15, prob=p_grid)
posterior <- likelihood * prior
posterior <- posterior/sum(posterior)

# re-sample
samples <- sample(p_grid, prob=posterior, size=1e4, replace = TRUE)
x <- HPDI(samples, 0.9)
x

w <- rbinom(1e4, size=15, prob=samples)
p_obs_8 = sum(w==8)/length(w)
p_ob
w <- rbinom(1e4, size=9, prob=samples)
p_obs_6 = sum(w==6)/length(w)
p_obs_6

# hard
# 3H1 
data(homework)
all_births = c(birth1, birth2)
prior <- rep(1, 1000)
p_grid <- seq(from=0, to=1, length.out=1000)
likelihood <- dbinom(sum(all_births), size=length(all_births), prob=p_grid)

posterior <- prior * likelihood
posterior <- posterior/sum(posterior)
plot(p_grid, posterior)

highest_p <- p_grid[which.max(posterior)]
highest_p 

# 3H2
samples <- sample(p_grid, size=1e4, prob=posterior, replace=TRUE)

highest.50 <- HPDI(samples, prob=0.5)
highest.89 <- HPDI(samples, prob=0.89)
highest.97 <- HPDI(samples, prob=0.97)

# 3H3
b1 <- rbinom(1e4, size=200, prob=samples)
b1
# ?dens
dens(b1, col="red")
par(new=TRUE)
dens(samples)

# does the simulation include the actual as a likely outcome?
# the simulated outcome is slightly right-shifted

# 3H4
sum(birth1)/length(birth1)

b2 <- rbinom(1e4, size=100, prob=samples)
dens(b2, col="green")
par(new=TRUE)
dens(samples)

# 3H5. The model assumes that sex of first and second births are independent. 
# To check this assumption, focus now on second births that followed female first borns. 
# Compare 10,000 simulated counts  of boys to only those second births that followed girls. 
# To do this correctly, you need to count the  number of first borns who were girls 
# and simulate that many births, 10,000 times. Compare the  counts of boys in your simulations 
# to the actual observed count of boys following girls. How does the  model look in this light? 
# Any guesses what is going on in these data? 
  
female_first_borns = ifelse(birth1 == 1, 0, 1)
female_first_borns
sum(female_first_borns) # 49
p_first_born_female = sum(female_first_borns)/length(female_first_borns)
p_first_born_female

# observed count of boys following first born girls
boys_after_girls <- ifelse((female_first_borns + birth2)==2, 1, 0)
boys_after_girls
sum(boys_after_girls) # 39 

# simulate female first-borns 10,000 times
first_born_girls_sim = rbinom(1e4, size=100, prob=samples)
first_born_girls_sim
boys_in_sim = # substract 100 from each number in first_born_girls_sim
  
# generate posterior for first-born being female
prior <- rep(1, 1000)
p_grid <- seq(from=0, to=1, length.out=1000)
likelihood <- dbinom(49, size=length(female_first_borns), prob=p_grid)

f_posterior = p_grid * likelihood
f_posterior = f_posterior/sum(f_posterior)



