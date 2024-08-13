## K. Garner, 2024
### simulate data from a Dirichlet distribution and recover the parameters, 
## plus compute entropy
## this code is meant to serve as a basis for a script to run over participants,
## as the coding great Lydia Barnes sees fit
rm(list=ls())

###### some helpful things
library(withr)
library(tidyverse)
library(rstan) # installation guide here - https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
library(brms) # use regular install.packages()
library(tidybayes)
library(wesanderson)

##########
# extra info:
# https://www.andrewheiss.com/blog/2023/09/18/understanding-dirichlet-beta-intuition/
# also: https://builtin.com/data-science/dirichlet-distribution
##########

############################################
## step 1. use a dirichlet distribution to pick from the six possible routines
## at chance
n_routines = 6 # there are 6 possible routines
alpha_k <- rep(n_routines, times=n_routines) # shape parameter, like the beta, 
# here our prior says that each route is likely to happen 1/6 of the time
# aka I am generating data under the null hypothesis
n_observations <- 160 # assuming the total counts is 160
# distribution counts, but for each possible routine

# each trial is a random draw from 
sim_data <- with_seed(42, {
  round(apply(rdirichlet(n=1e5, alpha=alpha_k),2,mean)*n_observations, 0) |>
    data.frame() |>
    pivot_longer(everything(), names_to = "r", values_to="counts") 
})
sim_data$r <- paste("r", 1:n_routines, sep="")

############################################
## step 2
# first thing I want to do is compute the alphas for the
# dirichlet distribution (i.e. add the prior to all the
# counts), and visualise the results as a beta
# distribution for each alpha parameter - i.e. the probability of each
# routine being selected
dir_prior <- 1 # uniform prior (note this is the prior we'll use 
# for modelling the data)
sim_data <- sim_data %>% mutate(posterior_alphas = counts + dir_prior)

# now I have the posterior alphas, I can simulate the distribution
# and visualise the results
with_seed(42, {
  rdirichlet(n = 1e5, alpha = sim_data$posterior_alphas) |> 
    data.frame() |> 
    set_names(paste("α", 1:6, sep="")) |> 
    pivot_longer(everything(), values_to="p", names_to="alpha") |> 
    ggplot(aes(x = p, fill = alpha)) +
    geom_density(bounds = c(0, 1), color = NA) +
    scale_fill_manual(values = wes_palette("IsleofDogs1"), guide = "none") +
    labs(x = "P") +
    facet_wrap(vars(alpha)) 
})

############################################
## step 3 - forming our beliefs about the data

## implementation 1: a bayes factor, the strength of which tells
## us how unlikely the null probabilities are, relative to the observed 
## probabilities for that participant
# Now I want to learn 3 things about the data
# 1) the probability of the alternate hypothesis (that the expected values
# are larger than 1/6, relative to the probability
# that all thetas are likely to be around 1/6
# note that a larger value is greater evidence for the alternate hypothesis
null_hyp <- rep(1/n_routines, times=n_routines)
null_llike <- ddirichlet(null_hyp, alpha=sim_data$posterior_alphas, log=TRUE) 
# log likelihood of the null

exp_theta <- sim_data$posterior_alphas/sum(sim_data$posterior_alphas) 
# exp value of thetas (given observations)

exp_llike <- ddirichlet(exp_theta, alpha=sim_data$posterior_alphas, log=TRUE)
lBF <- exp_llike - null_llike

## but the other thing we can do is ask, what is the probability that
## any given theta will be greater than chance?




## and the last thing we want to compute is the entropy over the expected
## probability vector, which gives a nice number quantifying how routiney someone
## is

# 2) I also want the log likelihood of the bayes factor

# 2) the expected value of the thetas for that participant
exp_theta

# 3) the entropy over probabilities for that participant, as our individual diff
# measure.

