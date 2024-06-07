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
  c(alphas,betas,beta_map,beta_variance) %<-% get_maggi(data)
  
}else{
  # read real data
  strategies <- format_data_for_maggi(nsub=1,nses=1,ncontext=1,method="by_event",specific_doors=FALSE,competitive=TRUE,evaluate_all=FALSE)
  
  colours <- c("darkgreen","limegreen","gold","orange")
  plot(1:nrow(strategies),rep(0,1,nrow(strategies)),type="l",col="black",ylim=c(0,1))
  i <- 0
  for (strategy in names(strategies)[2:length(names(strategies))]){
    i <- i+1
    data <- strategies %>% pull(strategy)
    
    # calculate recency-weighted probability of finding strategy s
    c(alphas,betas,beta_map,beta_variance) %<-% get_maggi(data)
    
    # view alphas and betas over time
    points(1:length(data),beta_map,type="l",col=colours[i])
  }
  
}

# view the final beta distribution
#increments <- seq(0,1,by=.01)
#plot(increments,dbeta(increments,alphas[length(data)],betas[length(data)]),type="l",col="darkgreen") 


