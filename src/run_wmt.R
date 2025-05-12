library(data.table)
library(tidyverse)
library(magrittr)
library(ggplot2)
# library(ggthemes)
# library(ggpubr)
library(ggsci)

# settings
exp <- "multitasking"
version <- "pilot-data" 

# set paths
data_path <- "C:/Users/cstone/OneDrive - UNSW/Documents/Projects/honours_projects/data"
exp_path <- str_glue("/{exp}/{version}")
file_path <- file.path(data_path + exp_path)
if (!dir.exists(data_path)) {
  stop(paste0(data_path, " does not exist"))
}

# load behavioural data for wmt trials in 
pat_wmt <- '.*(mts_beh.tsv)'
files_wmt <- list.files(file_path, pattern = pat_wmt, recursive = T)
dt_wmt <- rbindlist(lapply(file.path(file_path, files_wmt), fread), fill = TRUE)

# compute accuracy
dt_wmt[, acc := case_when(resp == cresp ~ 1,  T ~ 0)] # if response == correct response, acc = 1 else acc = 0
dt_wmt[, cond := case_when((context == 1 | context == 2) ~ 'oc', T ~ 'nc')] # is this correct? target locations for each house are always context 1 and 2, so the non-house locations are contexts 3 and 4?

# save data file
project_path <- getwd()
fnl <- file.path(project_path, "res", paste(paste(exp, "mts", sep = "_"), ".csv", sep = ""))
write_csv(dt_wmt, fnl)

# calculate averages
dt_wmt[stage == 4, 
       .(MeanAcc = mean(acc),
         MeanRT = mean(rt)),
       by = sub]

dt_wmt[stage == 3, 
       .(MeanAcc = mean(acc),
         MeanRT = mean(rt)),
       by = .(sub, cond)]

# plot
label_sz <- 20
mk_sz <- 2
dt_wmt[stage == 3, 
       .(MeanAcc = mean(acc),
         MeanRT = mean(rt)),
       by = .(sub, cond)] %>% 
  ggplot(aes(x = cond, y = MeanAcc)) +
  geom_violin() +
  geom_point(position = position_jitter(width=.25)) + 
  stat_summary(fun.data = "mean_cl_normal", 
               geom = "pointrange", 
               position = position_dodge(width = .9), 
               linewidth = 1, 
               size = mk_sz/2) +
  theme_minimal() +
  scale_x_discrete(labels = c("Neither", "Other ")) +
  labs(title = "", x = "Context", y = "Proportion correct") +
  theme(
    plot.title = element_text(size = label_sz),
    axis.text.x = element_text(size = label_sz), 
    axis.text.y = element_text(size = label_sz), 
    axis.title.x = element_text(size = label_sz), 
    axis.title.y = element_text(size = label_sz), 
    legend.title = element_text(size = label_sz),
    legend.text = element_text(size = label_sz),
  )


dt_wmt[stage == 3, 
       .(MeanAcc = mean(acc),
         MeanRT = mean(rt)),
       by = .(sub, cond)] %>% 
  ggplot(aes(x = cond, y = MeanRT)) +
  geom_violin() +
  geom_point(position = position_jitter(width=.25)) + 
  stat_summary(fun.data = "mean_cl_normal", 
               geom = "pointrange", 
               position = position_dodge(width = .9), 
               linewidth = 1, 
               size = mk_sz/2) +
  theme_minimal() +
  scale_x_discrete(labels = c("Neither", "Other ")) +
  labs(title = "", x = "Context", y = "Proportion correct") +
  theme(
    plot.title = element_text(size = label_sz),
    axis.text.x = element_text(size = label_sz), 
    axis.text.y = element_text(size = label_sz), 
    axis.title.x = element_text(size = label_sz), 
    axis.title.y = element_text(size = label_sz), 
    legend.title = element_text(size = label_sz),
    legend.text = element_text(size = label_sz),
  )



