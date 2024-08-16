# lydia barnes, august 2024
# counts how often clicks on a trial move between current-context and other-context door sets

# on cued trials, we assume that people start in the cued context. on uncued train phase trials, we assume that they start in the context that they were in in the previous trial.
# this requires that they make at least one context change on switch trials to find the target, so we also subtract one from those trials.

get_context_changes <- function(grp_data){
  
  nswitches <- c()
  for (s in unique(grp_data$sub)){
    for (ss in unique(grp_data$ses)){
      tmp <- grp_data %>% filter(sub==s,ses==ss) #get the data for this subject and session
      for (trial in unique(tmp$t)){
        data <- tmp %>% filter(t==trial) #reduce to the data for this trial
        switch <- data$switch[[1]] #check whether it's a switch trial
        if(switch==1 && ss==2){
          door_cc <- c(0,data$door_cc) #on uncued switch trials, assume that they start in the other context.
          door_oc <- c(1,data$door_oc)
        }else{
          door_cc <- c(1,data$door_cc) #on cued trials or uncued stay trials, assume that they start in the current context
          door_oc <- c(0,data$door_oc)
        }
        door_nc <- c(0,data$door_nc)
        
        cc_switches <- diff(door_cc) #check when they switch out of a door set (current context, other context, or never relevant doors)
        oc_switches <- diff(door_oc)
        nc_switches <- diff(door_nc)
        
        switches <- data.frame(cc_switches,oc_switches,nc_switches) #align all the context change types
        switches <- switches %>% 
          mutate(switch = case_when(cc_switches==1~1,oc_switches==1~1,.default=0))
        if(switch==1 && ss==2){
          nswitches <- c(nswitches, sum(switches$switch)-1)
        }else{
          nswitches <- c(nswitches,sum(switches$switch))
        }
      }
    }
  }
  
  return(nswitches)
}