get_learned_doors <- function(data){
  
  door_lc <- c()
  for (su in unique(data$sub)){
    
    partial_doors <- data %>% filter(sub==su,ses==3,transfer==2,door_cc==1) %>% pull(door) %>% unique()
    full_doors <- data %>% filter(sub==su,ses==3,transfer==1,door_cc==1) %>% pull(door) %>% unique()
    house_1 <- data %>% filter(sub==su,ses==2,context==1,door_cc==1) %>% pull(door) %>% unique()
    house_2 <- data %>% filter(sub==su,ses==2,context==2,door_cc==1) %>% pull(door) %>% unique()
    learned_doors <- c(house_1[is.na(match(house_1,partial_doors))], house_2[is.na(match(house_2,partial_doors))])
    learned_doors_not_reused <- c(house_1[is.na(match(house_1,full_doors))], house_2[is.na(match(house_2,full_doors))])

    tmp <- data %>% filter(sub==su) %>% mutate(door_lc = case_when(ses==1~NA,ses==2~NA,transfer==1~door_oc,.default=0) )
    
    # for full transfer trials, find when someone clicked on one of the learned doors that's not included in the full transfer door set
    idx <- intersect( which(tmp$door %in% learned_doors_not_reused), intersect(which(tmp$ses==3),which(tmp$transfer==1)) )
    tmp$door_lc[idx] <- 1
    
    # for partial transfer trials, find when someone clicked on one of the learned doors
    idx <- intersect(which(tmp$door %in% learned_doors),intersect(which(tmp$ses==3),which(tmp$transfer==2)))
    tmp$door_lc[idx] <- 1
    
    door_lc <- c(door_lc,tmp$door_lc)

  }
  return(door_lc)
}