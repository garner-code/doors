# lydia barnes, march 2024
# this script runs the analysis for data from the 'doors' project.

# TODO:
# find out how to export renv dependencies
# extend to allow analysing train and test data together

###
# sources
library(rstudioapi)
library(tidyverse)
wd <- dirname(rstudioapi::getSourceEditorContext()$path)
source(file.path(wd,'get_data.R'))
source(file.path(wd,'get_subs.R'))

# essentials
project_path <- '/Users/lydiabarnes/Documents/academe/projects/doors'
data_path <- '/Users/lydiabarnes/Documents/academe/data/doors'

# settings
exp <- 'exp_ts' #experiment: 'exp_ts' (task-conding) or 'exp_lt' (learning transfer)
subs <- get_subs(exp)
ses <- 'ses-train' #session: 'ses-learn','ses-train','ses-test'. usually want 'ses-test'.
mes <- 'clicks' #measure: 'clicks' or 'hovers'. usually want 'clicks'.
if(mes=='clicks'){idx <- 1}else{idx <- 2}
version <- '20240325' #pilot: 20240325

###
# format the raw data 
grp_data <- data.frame(
  sub = integer(),
  test = integer(),
  t = integer(),
  cond = integer(),
  onset = numeric(),
  door = integer(),
  cond = integer(),
  door_correct = integer(),
  offset = numeric()
)
for(sub in subs){
  data <- get_data(data_path,exp,sub,ses)
  grp_data <- rbind(grp_data,data[[idx]])
}

###
# extract results: accuracy and RT (time to trial end)
#   by trial
res <- grp_data %>% group_by(sub,group,t,cond) %>% summarise(
  n_clicks = n(),
  n_correct = sum(door_correct),
  accuracy = n_correct/n_clicks,
  rt = max(offset)
) %>% select(!n_clicks:n_correct)
fnl <- file.path(project_path,'res',paste(version,'_trl.csv',sep = ""))
write_csv(res,fnl)

#   by subject
res <- res %>% group_by(sub,group,cond) %>% summarise(rt = mean(rt),accuracy = mean(accuracy)) 
fnl <- file.path(project_path,'res',paste(version,'_avg.csv',sep = ""))
write_csv(res,fnl) 

