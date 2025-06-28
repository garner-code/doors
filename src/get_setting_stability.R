# lydia barnes, august 2024
# if someone is deep into a set of doors, they may struggle to respond to a cue to change contexts.
# this could be independent from, or anti-correlated with, how likely they are to make a setting error later in a trial
# both of these things are bundled into setting_errors. this function separates the two.

# ah, ok, so what this function should do is classify all the 
get_setting_stability <- function(data){
  
  data <- data %>% 
    group_by(sub,ses,t) %>% 
    mutate(
      scca = case_when(diff(c(1,door_cc))>0~1,.default=0), # if you go from not selecting a door from the current context to doing so, put a 1 - putting a 1 at the front guarantees that if you are already in the current context, you will not get a '1' in scca
      sccb = case_when(diff(c(0,door_cc))>0~1,.default=0), # if you go from not selecting a door from the current context to doing so, put a 1 - putting a 0 at the front guarantees that if you start in the current context, you will get a '1' in scca
      select_cc = case_when(ses==2 & switch==1 ~ sccb, .default=scca), #changes into correct context - if this is a trial where there was a switch, then you use sccb, where there is a 1 if you are hitting cc immediately
      soca = case_when(diff(c(0,door_oc))>0~1,.default=0), # this does the opposite to the above - if you start the trial hitting doors from the other context then put a 1
      socb = case_when(diff(c(1,door_oc))>0~1,.default=0), # 
      select_oc = case_when(ses==2 & switch==1 ~ socb, .default=soca), #changes into other context
      select_oc_late = case_when(diff(c(0,t))==1~0,.default=select_oc) # if it is the first event in the trial then put a 0, if not put that they selected the other context
    ) %>% 
    ungroup()
  
  x <- data %>%
    group_by(sub,ses,t) %>% 
    mutate(
      t_cc = cumsum(door_cc),
      t_oc = cumsum(door_oc)
      ) %>% 
    ungroup()
  
  return(data)
}