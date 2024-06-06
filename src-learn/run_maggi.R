# lydia barnes, may 2024
# applies maggi algorithm. algorithm estimates probability of using a given strategy, weighting by recency.

library(zeallot) #unpack/destructure with %<-%
source(file.path(getwd(), "src-learn", "get_maggi.R"))
source(file.path(getwd(), "src-learn", "format_data_for_maggi.R"))

set.seed(17)
simulation <- FALSE

if (simulation){
  # synthesise data
  ps <- c(.1, .3, .6, .9) #reasonable steps in the probability of successes over runs of trials
  n <- 25 #trials per run
  data <- unlist(lapply(ps, rbinom, n=n, size=1)) # n * length(ps) trials drawn from a binomial distribution
}else{
  # read real data
  strategies <- format_data_for_maggi(nsub=2,nses=1,ncontext=1,specific_doors=FALSE)
  data <- strategies %>% pull(learn_four)
}

# calculate recency-weighted probability of finding strategy s
c(alphas,betas,beta_map,beta_variance) %<-% get_maggi(data)

# view the final beta distribution
increments <- seq(0,1,by=.01)
plot(increments,dbeta(increments,alphas[length(data)],betas[length(data)]),type="l",col="darkgreen") 

# view alphas and betas over time
plot(1:length(data),beta_map,type="l",col="darkgreen")
#points(1:length(data),betas,type="l",col="navy")
