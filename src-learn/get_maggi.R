get_maggi <- function(data,alpha=1,beta=1,decay=.9){
  # mode: (alphas-1) / (alphas+betas-2)
  # mean: alphas / (alphas+betas)

  # initialise empty arrays
  alphas <- rep(0,length(data))
  betas <- alphas
  s <- alphas
  f <- alphas 
  
  for (i in 1:length(data)){
    
    if(i==1){
      # record success and failure
      s[i] <- data[i] 
      f[i] <- 1-data[i] 
    }else{
      # record success and failure, modified by success and failure at our last check
      s[i] <- decay*s[i-1] + data[i]
      f[i] <- decay*f[i-1] + 1-data[i]
    }
    
    # update the distribution's parameters
    alphas[i] <- alpha+s[i]
    betas[i] <- beta+f[i]
  }

  beta_map <- (alphas-1) / (alphas+betas-2) # maximum a posteriori probability i.e. the mode
  beta_variance <- alphas*betas / ((alphas+betas)^2 * (alphas+betas+1))
  
  return(list(alphas,betas,beta_map,beta_variance))

}
