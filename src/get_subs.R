get_subs <- function(exp,version){
  
  if(version=='20240325'){
    if(exp == 'exp_ts'){
      subs <- c('sub-01','sub-03')
    }else{
      subs <- c('sub-01','sub-03')
    }
    
  }else if(version=='20240411'){
    if(exp == 'exp_ts'){
      subs <- c('sub-04','sub-08','sub-20')
    }else{
      subs <- c('')
    }
  }
  
}