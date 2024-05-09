# lydia barnes, may 2024 counts, clusters, and/or traces stereotypical behaviour during training to
# explain performance at test

# TODO: 
# add option to numerically compare true and optimal paths

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
fnl <- file.path(
    data_path, paste(
        paste(version, exp, mes, "evt", sep = "_"),
        ".csv", sep = ""
    )
)
data <- read_csv(fnl, show_col_types = FALSE)

### extract stereotypy metrics
counts <- count_stereo(data)

# save to file
fnl <- file.path(
  project_path, "res", paste(
    paste(version, exp, mes, "reclicks", sep = "_"),
    ".csv", sep = ""
  )
)
write_csv(counts[[1]], fnl)

fnl <- file.path(
  project_path, "res", paste(
    paste(version, exp, mes, "transitions", sep = "_"),
    ".csv", sep = ""
  )
)
write_csv(counts[[2]], fnl)


### cluster participants based on stereotypy and accuracy

### use maggi method to trace change in stereotypy (extent or type)
