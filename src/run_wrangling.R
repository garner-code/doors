# lydia barnes, march 2024 this script extracts, formats, and summarises data from the 'doors'
# project.

### sources
library(tidyverse)
source(file.path(getwd(), "src", "get_subs.R"))
source(file.path(getwd(), "src", "get_switch.R"))
source(file.path(getwd(), "src", "get_data.R"))

### settings

# !you will want to update these settings a lot during piloting, when the task code or the way you
# test changes, or when you test participants on different subsets of the task phases
version <- "study-01" # pilot-data-00 (train and test), pilot-data-01 (learn and train), pilot-data-02 (learn and train, learn phase split into two parts)
exp <- "exp_lt" # experiment: 'exp_ts' (task-switching) or 'exp_lt' (learning transfer)
sess <- c("ses-learn","ses-train","ses-test") # session: 'ses-learn','ses-train','ses-test'. can select one (e.g. ses <- c('ses-learn')) or multiple (e.g. ses <- c('ses-train','ses-test'))

# !you can change the following settings if you want to, but the defaults will usually be fine
mes <- "clicks" # measure: 'clicks' or 'hovers'. usually want 'clicks'.
if (mes == "clicks") {
  idx <- 1
} else {
  idx <- 2
}
apply_threshold <- FALSE # only retain events that lasted more than a given duration?
min_dur <- 0.1 # minimum duration

### paths

# !if you open the project thru doors.Rproj, your working directory will automatically be the
# project path
project_path <- getwd()
if (!dir.exists(file.path(project_path, "res"))) {
  # check that the results directory exists. if it doesn't, create it.
  dir.create(file.path(project_path, "res"))
}

# !you will need to change the data path to match the location of OneDrive on your personal
# computer
data_path <- file.path("/Users/lydiabarnes/OneDrive - UNSW/task switch and transfer/data-sandpit", version)
if (!dir.exists(data_path)) {
  stop(paste0(data_path, " does not exist"))
}

### load an up-to-date list of participants
subs <- get_subs(exp, version)

### extract events from the raw data

# make an empty data frame with all the variables (columns) that we will want
grp_data <- data.frame(
  sub = integer(), ses = integer(), t = integer(), context = integer(), door = integer(),
  door_cc = integer(), on = numeric(), off = numeric(), subses = integer(), door_oc = integer(), 
  switch = integer(), train_type = integer(), transfer = integer(), original_house = integer()
)

# for each subject and session, use the function 'get_data' to load their raw data and attach it to
# our 'grp_data' data frame with one measurement (row) per event (click or hover)
for (sub in subs) {
  print(sub)
  
  sid <- as.numeric(substring(sub,5,7))
  for (ses in sess) {
    train_type <- NA
    context_one_doors <- NA
    if (ses == "ses-test") {
      train_type <- grp_data %>%
        filter(sub == sid, ses == 2) %>%
        select(train_type) %>% 
        unique() %>% 
        pull()
      context_one_doors <- grp_data %>% 
        filter(sub==sid,ses==ses,context==1,door_cc==1) %>% 
        select(door) %>% 
        unique() %>% 
        pull()
    }
    data <- get_data(data_path, exp, sub, ses, train_type, context_one_doors, apply_threshold, min_dur) # load and format raw data
    grp_data <- rbind(grp_data, data[[idx]]) # add to the 'grp_data' data frame so we end up with all subjects and sessions in one spreadsheet
  }
}

# save the formatted data
fnl <- file.path(project_path, "res", paste(paste(version, exp, mes, "evt", sep = "_"), ".csv", sep = ""))
write_csv(grp_data, fnl)

### extract accuracy and response time averages from event data

# by trial
res <- grp_data %>%
  group_by(sub, ses, t, context, train_type, transfer) %>%
  summarise(
    switch = max(switch), n_clicks = n(), n_cc = sum(door_cc), n_oc = sum(door_oc), 
    accuracy = n_cc / n_clicks,
  )
rt <- grp_data %>%
  group_by(sub, ses, t, context, train_type, transfer) %>%
  filter(door_cc == 1) %>%
  summarise(rt = min(off)) # time to first correct click offset
res$rt <- rt$rt

other_accuracy <- 4-res$n_clicks >= 0
res$other_accuracy <- other_accuracy

fnl <- file.path(project_path, "res", paste(paste(version, exp, mes, "trl", sep = "_"), ".csv", sep = ""))
write_csv(res, fnl)

# by subject
res <- res %>%
  group_by(sub, ses, context, switch, train_type, transfer) %>% 
  summarise_all(mean)
fnl <- file.path(project_path, "res", paste(paste(version, exp, mes, "avg", sep = "_"), ".csv", sep = ""))
write_csv(res, fnl)
