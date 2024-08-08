get_context_changes <- function(grp_data){
  
  nswitches <- c()
  for (s in unique(grp_data$sub)){
    for (ss in unique(grp_data$ses)){
      tmp <- grp_data %>% filter(sub==s,ses==ss)
      for (trial in unique(tmp$t)){
        data <- tmp %>% filter(t==trial)
        switch <- data$switch[[1]]
        if(switch==0){
          door_cc <- c(1,data$door_cc)
          door_oc <- c(0,data$door_oc)
        }else if(switch==1){
          door_cc <- c(0,data$door_cc)
          door_oc <- c(1,data$door_oc)
        }
        door_nc <- c(0,data$door_nc)
        cc_switches <- diff(door_cc)
        oc_switches <- diff(door_oc)
        nc_switches <- diff(door_nc)
        switches <- data.frame(cc_switches,oc_switches,nc_switches)
        switches <- switches %>% mutate(switch = case_when(cc_switches==1~1,oc_switches==1~1,nc_switches==1~1,.default=0))
        if(switch==1){
          nswitches <- c(nswitches, sum(switches$switch)-1)
        }else{
          nswitches <- c(nswitches,sum(switches$switch))
        }
      }
    }
  }
  
  return(nswitches)
}