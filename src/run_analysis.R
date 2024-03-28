# lydia barnes, march 2024
# this script extracts, formats, and summarises data from the 'doors' project.

# TODO:
# extend to allow analysing train and test data together

###
# sources
library(rstudioapi)
library(tidyverse)
wd <- dirname(rstudioapi::getSourceEditorContext()$path)
source(file.path(wd,'get_data.R'))
source(file.path(wd,'get_subs.R'))

# essentials
getwd()
project_path <- getwd() #if you open the project thru doors.Rproj, your working directory will automatically be the project path
data_path <- '/Users/lydiabarnes/Documents/academe/data/doors' #if data are within the project directory, update to file.path(project_path,'data')

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
  cond = numeric(),
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
res <- grp_data %>% group_by(sub,group,t) %>% summarise(
  cond = max(cond),
  n_clicks = n(),
  n_correct = sum(door_correct),
  accuracy = n_correct/n_clicks,
  rt = max(offset)
) %>% select(!n_clicks:n_correct)
fnl <- file.path(project_path,'res',paste(paste(version,exp,ses,mes,'trl',sep='_'),'.csv',sep = ""))
write_csv(res,fnl)

#   by subject
res <- res %>% group_by(sub,group,cond) %>% summarise(rt = mean(rt),accuracy = mean(accuracy)) 
fnl <- file.path(project_path,'res',paste(paste(version,exp,ses,mes,'avg',sep='_'),'.csv',sep = ""))
write_csv(res,fnl) 

