

### sources
library(tidyverse)
library(ggthemes)
library(ggExtra)

### settings
version <- "study-01"  #pilot-data-00 (train and test), pilot-data-01 (learn and train), pilot-data-02 (learn and train, learn phase split into two parts)
exp <- "exp_lt"  #experiment: 'exp_ts' (task-switching) or 'exp_lt' (learning transfer)
mes <- "clicks"  #measure: 'clicks' or 'hovers'. usually want 'clicks'.

ctx <- 1

# figure settings
title_sz <- 30
label_sz <- 30

### paths
project_path <- getwd()
data_path <- file.path(project_path, "res")
if (!dir.exists(data_path)) {
    stop(paste0(data_path, " does not exist"))
}

fnl <- file.path(project_path, "res", paste(paste(version, exp, mes, "stereotypy", sep = "_"), ".csv",
    sep = ""))
stereo <- read_csv(fnl)

### transitions
p <- stereo %>%
    filter(ses == 2, context == ctx) %>%
    ggplot() + geom_point(aes(x = transition_counts, y = accuracy, colour = factor(subses)), alpha = 0.8, size = 15) + 
    geom_text(aes(x = transition_counts,
    y = accuracy, label = sub), alpha = 0.8, size = 8, position = position_jitter(width = 0.1, height = 0.05)) +
    theme_minimal(base_size = label_sz, base_family = "Roboto") + ylim(0, 1.1) + xlim(0.5, 4.5) + labs(title = "Transitions by accuracy during training",
    x = "Transitions", y = "Accuracy", colour = "Half of session") + scale_colour_brewer(palette = "Greens",
    labels = unique(stereo$subses)) + theme(panel.background = element_rect(fill = "white", colour = "white"),
    plot.background = element_rect(fill = "white", colour = "white"))
# p <- ggMarginal(p)

fnl <- file.path(project_path, "fig", paste(paste(version, exp, mes, "transitions", paste("context",
    ctx, sep = "-"), sep = "_"), ".png", sep = ""))
ggsave(fnl, plot = p)

### re-clicks
p <- stereo %>%
    filter(ses == 2, context == ctx) %>%
    ggplot() + geom_point(aes(x = reclicks, y = accuracy, colour = factor(subses)), alpha = 0.8, size = 15) + 
    geom_text(aes(x = reclicks,
    y = accuracy, label = sub), alpha = 0.8, size = 8, position = position_jitter(width = 0.1, height = 0.05)) +
    theme_minimal(base_size = label_sz, base_family = "Roboto") + ylim(0, 1.1) + xlim(0, 8.5) + labs(title = "Switch trial re-clicks by stay trial accuracy",
    x = "Re-clicks", y = "Accuracy", colour = "Half of session") + scale_colour_brewer(palette = "Greens",
    labels = unique(stereo$subses)) + theme(panel.background = element_rect(fill = "white", colour = "white"),
    plot.background = element_rect(fill = "white", colour = "white"))

fnl <- file.path(project_path, "fig", paste(paste(version, exp, mes, "reclicks", paste("context",
    ctx, sep = "-"), sep = "_"), ".png", sep = ""))
ggsave(fnl, plot = p)

### paths

# add test phase accuracy to stereo data frame
fnl <- file.path(project_path, "res", paste(paste(version, exp, mes, "avg", sep = "_"), ".csv",
      sep = ""))
avg <- read_csv(fnl)
stereo <- stereo %>% group_by(sub,ses,context) %>% summarise_all(mean)
avg <- avg %>% filter(ses==3,switch==0)
stereo$accuracy_test <- avg$accuracy # add test accuracy to stereo results

p <- stereo %>%
  filter(ses == 2, context == ctx) %>%
  ggplot() + geom_point(aes(x = overshoot_hp, y = accuracy_test), alpha = 0.8, size = 15) + 
  geom_text(aes(x = overshoot_hp,
  y = accuracy_test, label = sub), alpha = 0.8, size = 8, position = position_jitter(width = 0.1, height = 0.05)) +
  theme_minimal(base_size = label_sz, base_family = "Roboto") + ylim(0, 1.1) + xlim(-0.1,2.1) + labs(title = "Path overshoot (train) by accuracy (test)",
  x = "Path overshoot", y = "Accuracy") + 
  theme(panel.background = element_rect(fill = "white", colour = "white"),
  plot.background = element_rect(fill = "white", colour = "white"))

fnl <- file.path(project_path, "fig", paste(paste(version, exp, mes, "hp", paste("context",
  ctx, sep = "-"), sep = "_"), ".png", sep = ""))
ggsave(fnl, plot = p)
