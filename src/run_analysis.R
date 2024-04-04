# lydia barnes, march 2024
# this script extracts, formats, and summarises data from the 'doors' project.

# TODO:
# remove redundancy bw door_correct and door_cc
# save full click and hover data as well as trial- and subject-grouped results
# discuss: rt to first correct click onset, or offset?
# produce alternate results grouped by door identity instead of nclicks
# add functionality to track stereotypy

###
# sources
library(rstudioapi)
library(tidyverse)
wd <- dirname(rstudioapi::getSourceEditorContext()$path)
source(file.path(wd,'get_data.R'))
source(file.path(wd,'get_subs.R'))
source(file.path(wd,'get_transitions.R'))

# essentials
getwd()
project_path <- getwd() #if you open the project thru doors.Rproj, your working directory will automatically be the project path
data_path <- '/Users/lydiabarnes/Documents/academe/data/doors' #if data are within the project directory, update to file.path(project_path,'data')

# settings
exp <- 'exp_lt' #experiment: 'exp_ts' (task-conding) or 'exp_lt' (learning transfer)
subs <- get_subs(exp)
sess <- c('ses-train','ses-test') #session: 'ses-learn','ses-train','ses-test'. usually want 'ses-test'.
mes <- 'clicks' #measure: 'clicks' or 'hovers'. usually want 'clicks'.
if(mes=='clicks'){idx <- 1}else{idx <- 2}
version <- '20240325' #pilot: 20240325
apply_threshold <- FALSE #only retain events that lasted more than a given duration?
min_dur <- 0.1 #minimum duration 

###
# format the raw data 
grp_data <- data.frame(
  sub = integer(),
  ses = integer(),
  t = integer(),
  context = integer(),
  door = integer(),
  door_correct = integer(),
  on = numeric(),
  off = numeric(),
  door_cc = integer(),
  door_oc = integer(),
  switch = integer(),
  train_type = integer()
)
for(sub in subs){
  for(ses in sess){
    
    train_type <- NA
    if(ses == 'ses-test'){
      train_type <- grp_data %>% filter(sub == sub & ses == 2) %>% select(train_type)
    }
    data <- get_data(data_path,exp,sub,ses,train_type,apply_threshold,min_dur)
    grp_data <- rbind(grp_data,data[[idx]]) 
  }
}

###
# extract results: accuracy and RT (time to trial end)
#   by trial
res <- grp_data %>% group_by(sub,ses,t,context,train_type) %>% summarise(
  switch = max(switch),
  n_clicks = n(),
  n_correct = sum(door_correct),
  n_cc = sum(door_cc),
  n_oc = sum(door_oc),
  accuracy = n_correct/n_clicks,
)
rt <- grp_data %>% group_by(sub,ses,t,context,train_type) %>% filter(door_correct==1) %>% summarise(rt = min(off)) #time to first correct click offset
res$rt <- rt$rt
fnl <- file.path(project_path,'res',paste(paste(version,exp,mes,'trl',sep='_'),'.csv',sep = ""))
write_csv(res,fnl)

#   by subject
res <- res %>% group_by(sub,ses,context,switch,train_type) %>% summarise(
  n_clicks = mean(n_clicks),
  n_cc = mean(n_cc),
  n_oc = mean(n_oc),
  rt = mean(rt),
  accuracy = mean(accuracy)
) 
fnl <- file.path(project_path,'res',paste(paste(version,exp,mes,'avg',sep='_'),'.csv',sep = ""))
write_csv(res,fnl) 

