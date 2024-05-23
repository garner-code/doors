# lydia barnes, may 2024

library(zeallot) #unpack/destructure with %<-%
source(file.path(getwd(), "src-learn", "get_maggi.R"))

set.seed(17)

# synthesise data
ps <- c(.1, .3, .6, .9) #reasonable steps in the probability of successes over runs of trials
n <- 25 #trials per run
data <- unlist(lapply(ps, rbinom, n=n, size=1)) # n * length(ps) trials drawn from a binomial distribution

# calculate recency-weighted probability of finding strategy s
c(alphas,betas,beta_map,beta_variance) %<-% get_maggi(data)

# view final beta distribution
plot(1:length(data),alphas,type="l",col="darkgreen")
points(1:length(data),betas,type="l",col="navy")