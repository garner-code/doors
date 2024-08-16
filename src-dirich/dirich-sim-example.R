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

## inference #1: a bayes factor, the strength of which tells
## us how likely the null probabilities are, relative to the observed 
## probabilities for that participant
## the probability of the alternate hypothesis (that the expected values
# are larger than 1/6, relative to the probability
# that all thetas are likely to be around 1/6
# note that a larger value is greater evidence for the alternate hypothesis
null_hyp <- rep(1/n_routines, times=n_routines)
null_llike <- ddirichlet(null_hyp, alpha=sim_data$posterior_alphas, log=TRUE) 
# log likelihood of the null, save for each participant for future calcs,
# as I have had an idea for a second level stat that I think is better than
# the binomial idea we chatted about

exp_theta <- sim_data$posterior_alphas/sum(sim_data$posterior_alphas) 
# exp value of thetas (given observations) # as above

exp_llike <- ddirichlet(exp_theta, alpha=sim_data$posterior_alphas, log=TRUE)
lBF <- exp_llike - null_llike # this is the BF in log p form 
BF <- exp(lBF) # this is our bayes factor


## inference #2: but the other thing we can do is ask, what is the probability that
## any given theta will be greater than chance? putting here to chat through/might be
## the sort of thing a reviewer asks for

# first step is to simulate the participant's distribution many, many times, to
# get an idea of the like spread of thetas, given the observations for that participant,
# then we calculate the proportion of times that the random draws were less than or
# == to chance, any prob value < .05/6 would be considered statistically above chance 
hyp_dat <- with_seed(42, {
  rdirichlet(n = 1e5, alpha = sim_data$posterior_alphas) |> 
    data.frame() |> 
    set_names(paste("α", 1:6, sep="")) |> 
    pivot_longer(everything(), values_to="p", names_to="alpha") |> 
    group_by(alpha) |>
    summarise(prob = mean(p <= 1/n_routines)) 
})

## and the last thing we want to compute is the entropy over the expected
## thetas, which gives a nice number quantifying how routiney someone
## is
# Entropy is defined as:
# H(X) = logB(alpha) + (alpha0 - K)digamma(alpha0) - sum_j=1toK(alpha_j - 1)digamma(alpha_j)
dir_entropy <- function(alphas){
  # see definition at https://en.wikipedia.org/wiki/Dirichlet_distribution
  alpha0 <- sum(alphas)
  K <- length(alphas)
  log_beta_alpha <- sum(lgamma(alphas)) - lgamma(alpha0)
  log_beta_alpha + (alpha0 - K)*digamma(alpha0) - sum((alphas-1)*digamma(alphas))
}

with(sim_data, dir_entropy(posterior_alphas))