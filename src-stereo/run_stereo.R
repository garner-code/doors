# lydia barnes, may 2024 counts, clusters, and/or traces stereotypical behaviour during training to
# explain performance at test

# TODO: 
# generate shortest paths for three-click trials
# generate shortest paths for contexts used in 'test' phase (for exp_lt)
# save only the true shortest path, not the shortest path starting at each door
# fix path comparison so that...
#   if they clicked once, path and shortest path are 0
#   if they clicked twice, path and shortest path are the distance between the two doors
#   if they clicked three times or four times, we (a) check whether they matched a shortest 
# path and (b) check the difference between their chosen path and the true shortest
# verify that participants are not able to out-perform the optimal path!

### sources
library(tidyverse)
source(file.path(getwd(), "src-stereo", "count_stereo.R"))


### settings
version <- "study-01"  #pilot-data-00 (train and test), pilot-data-01 (learn and train), pilot-data-02 (learn and train, learn phase split into two parts)
exp <- "exp_lt"  #experiment: 'exp_ts' (task-switching) or 'exp_lt' (learning transfer)
mes <- "clicks"  #measure: 'clicks' or 'hovers'. usually want 'clicks'.

### paths
project_path <- getwd()
data_path <- file.path(project_path, "res")
if (!dir.exists(data_path)) {
    stop(paste0(data_path, " does not exist"))
}

### load event data
fnl <- file.path(data_path, paste(paste(version, exp, mes, "evt", sep = "_"), ".csv", sep = "") )
data <- read_csv(fnl, show_col_types = FALSE)

### load shortest path data
fnl <- file.path(data_path, paste(paste(version, exp, mes, "opt-path", sep = "_"), ".csv", sep = ""))
opt <- read_csv(fnl, show_col_types = FALSE)
opt <- opt %>% 
  filter(algorithm == 'tsp')

fnl <- file.path(project_path,'src-paths','graph.csv')
graph <- read_csv(fnl,col_names = FALSE,show_col_types = FALSE)

### extract stereotypy metrics
counts <- count_stereo(data,opt,graph)

# save to file
fnl <- file.path(project_path, "res", paste(paste(version, exp, mes, "reclicks", sep = "_"), ".csv",
    sep = ""))
write_csv(counts[[1]], fnl)

fnl <- file.path(project_path, "res", paste(paste(version, exp, mes, "transitions", sep = "_"), ".csv",
    sep = ""))
write_csv(counts[[2]], fnl)

fnl <- file.path(project_path, "res", paste(paste(version,exp,mes,"travelled",sep="_"),".csv",sep=""))
write_csv(counts[[3]],fnl)

### cluster participants based on stereotypy and accuracy

### use maggi method to trace change in stereotypy (extent or type)
