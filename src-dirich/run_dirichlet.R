# lydia barnes, september 2024

# reads and formats click data 
# converts click data to count data for each of six possible paths
# generates dirichlet distribution from path count data
# extracts parameters and estimates entropy (our routineness measure)

rm(list=ls())

library(tidyverse)
source(file.path("src-dirich", "count_paths.R"))
source(file.path("src-dirich", "make_dirichlet.R"))
project_path <- getwd()
exp <- "exp_ts" # experiment: 'exp_ts' (task-switching) or 'exp_lt' (learning transfer)

# -------------------------------------------------------------------------
# load data
fnl <- file.path(project_path, "res", paste(paste(exp, "evt", sep = "_"), ".csv", sep = ""))
data <- read.csv(fnl)

# -------------------------------------------------------------------------
# loop through participants and contexts
routineness <- data.frame()
for (sid in unique(data$sub)){
  for (ctx in unique(data$context)){
    # get their train phase data, context-correct stay trial clicks only
    clicks <- data %>% filter(sub==sid,ses==2,context==ctx,door_cc==1,switch==0)
    counts <- count_paths(clicks)
    tmp <- make_dirichlet(counts)
    routineness <- rbind(routineness,cbind(sub=sid,context=ctx,train_type=unique(clicks$train_type),tmp))
  }
}

fnl <- file.path(project_path, "res", paste(paste(exp, "routines", sep = "_"), ".csv", sep = ""))
write_csv(routineness, fnl)
