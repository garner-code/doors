### sources
library(tidyverse)
library(ggthemes)
library(ggExtra)

### settings
version <-
  "study-01" # pilot-data-00 (train and test), pilot-data-01 (learn and train), pilot-data-02 (learn and train, learn phase split into two parts)
exp <-
  "exp_lt" # experiment: 'exp_ts' (task-switching) or 'exp_lt' (learning transfer)
mes <-
  "clicks" # measure: 'clicks' or 'hovers'. usually want 'clicks'.

# figure settings
title_sz <- 30
label_sz <- 30

### paths
project_path <- getwd()
data_path <- file.path(project_path, "res")
if (!dir.exists(data_path)) {
  stop(paste0(data_path, " does not exist"))
}

### data
# stereotypy measures from train phase
fnl <-
  file.path(project_path, "res", paste(paste(version, exp, mes, "stereotypy", sep = "_"), ".csv",
    sep = ""
  ))
stereo <- read_csv(fnl, show_col_types = FALSE)

# general accuracy and response times for all phases
fnl <-
  file.path(project_path, "res", paste(paste(version, exp, mes, "avg", sep = "_"), ".csv",
    sep = ""
  ))
avg <- read_csv(fnl)

# add test phase accuracy to stereo data frame
stereo_test <-
  stereo %>%
  group_by(sub, ses, context) %>%
  summarise_all(mean)
stereo_test$accuracy_test <-
  avg %>%
  filter(ses == 3, switch == 0) %>%
  pull(accuracy) # add test accuracy to stereo results
stereo_test$context_test <- 
  avg %>% 
  filter(ses==3, switch==0) %>% 
  pull(context)
tmp <- stereo_test %>% 
  group_by(sub,ses) %>% 
  summarise_all(mean)
reclicks_avg <- tmp %>% pull(reclicks)
overshoot_avg <- tmp %>% pull(overshoot_tsp)
a <- stereo_test %>%
  filter(ses == 2,context_test==1) %>%
  ungroup() %>% 
  select(sub,overshoot_tsp,reclicks,accuracy_test,context_test)
a$reclicks <- reclicks_avg
a$overshoot_tsp <- overshoot_avg
b <- stereo_test %>% 
  filter(ses==2, context_test==2) %>% 
  ungroup() %>% 
  select(sub,overshoot_tsp,reclicks,accuracy_test,context_test)
b$reclicks <- reclicks_avg
b$overshoot_tsp <- overshoot_avg
stereo_test <- bind_rows(a,b)

test_labels <- c("Full Transfer Accuracy", "Partial Transfer Accuracy")

for (ctx in 1:2) {
  p <- stereo %>% filter(ses == 2, context == ctx) %>% 
    ggplot() +
    geom_point(
      aes(
        x = transition_counts,
        y = reclicks,
        colour = accuracy
      ),
      alpha = 0.8,
      size = 15
    ) +
    geom_text(
      aes(
        x = transition_counts,
        y = reclicks, 
        label = sub
      ),
      alpha = 0.8,
      size = 8,
      position = position_jitter(width = 0.1, height = 0.05)
    ) +
    theme_minimal(base_size = label_sz, base_family = "Roboto") +
    labs(
      x = "Transitions",
      y = "Reclicks",
      colour = "Accuracy"
    ) +
    theme(
      panel.background = element_rect(fill = "white", colour = "white"),
      plot.background = element_rect(fill = "white", colour = "white")
    )
  fnl <-
    file.path(project_path, "fig", paste(
      paste(
        version,
        exp,
        mes,
        "stereo",
        paste("context",
              ctx,
              sep = "-"
        ),
        sep = "_"
      ),
      ".png",
      sep = ""
    ))
  ggsave(fnl, plot = p, width = 14, height = 14)
  
  
  ### transitions
  p <- stereo %>%
    filter(ses == 2, context == ctx) %>%
    ggplot() +
    geom_point(
      aes(
        x = transition_counts,
        y = accuracy,
        colour = factor(subses),
      ),
      alpha = 0.8,
      size = 15
    ) +
    geom_text(
      aes(
        x = transition_counts,
        y = accuracy, label = sub
      ),
      alpha = 0.8,
      size = 8,
      position = position_jitter(width = 0.1, height = 0.05)
    ) +
    theme_minimal(base_size = label_sz, base_family = "Roboto") +
    ylim(0, 1.1) +
    #xlim(0.5, 4.5) +
    labs(
      title = "Transitions by accuracy",
      x = "Transitions",
      y = "Accuracy",
      colour = "Half of session"
    ) +
    scale_colour_brewer(
      palette = "Greens",
      labels = unique(stereo$subses)
    ) +
    theme(
      panel.background = element_rect(fill = "white", colour = "white"),
      plot.background = element_rect(fill = "white", colour = "white")
    )
  fnl <-
    file.path(project_path, "fig", paste(
      paste(
        version,
        exp,
        mes,
        "transitions",
        paste("context",
          ctx,
          sep = "-"
        ),
        sep = "_"
      ),
      ".png",
      sep = ""
    ))
  ggsave(fnl, plot = p, width = 14, height = 7)
}


#===============================================================================
# analyse by test phase context (1 = full transfer, 2 = partial transfer)

# reclicks
p <- stereo_test %>%
  ggplot() +
  geom_point(
    aes(
      x = reclicks,
      y = accuracy_test,
      colour = factor(context_test)
    ),
    alpha = 0.8,
    size = 15
  ) +
  geom_text(
    aes(
      x = reclicks,
      y = accuracy_test, 
      label = sub
    ),
    alpha = 0.8,
    size = 8,
    position = position_jitter(width = 0.1, height = 0.05)
  ) +
  theme_minimal(base_size = label_sz, base_family = "Roboto") +
  ylim(0.5, 1.1) +
  xlim(0, 8.5) +
  labs(
    title = "Transfer Accuracy by Train Re-Clicks",
    x = "Re-Clicks",
    y = "Accuracy",
    colour = "Test Type"
  ) +
  scale_colour_brewer(
    palette = "Greens",
    labels = c("Full Transfer","Partial Transfer")
  ) +
  theme(
    panel.background = element_rect(fill = "white", colour = "white"),
    plot.background = element_rect(fill = "white", colour = "white")
  )
  
fnl <-
  file.path(project_path, "fig", paste(
    paste(
      version,
      exp,
      mes,
      "reclicks",
      sep = "_"
    ),
    ".png",
    sep = ""
  ))
ggsave(fnl, plot = p, width = 14, height = 7)

  
# shortest path overshoot
  p <- stereo_test %>%
    ggplot() +
    geom_point(aes(x = overshoot_tsp, y = accuracy_test,
                   colour = factor(context_test)),
      alpha = 0.8,
      size = 15
    ) +
    geom_text(
      aes(
        x = overshoot_tsp,
        y = accuracy_test, label = sub
      ),
      alpha = 0.8,
      size = 8,
      position = position_jitter(width = 0.1, height = 0.05)
    ) +
    theme_minimal(base_size = label_sz, base_family = "Roboto") +
    ylim(0.5, 1.1) +
    xlim(-0.1, 2.1) +
    labs(
      title = "Train Overshoot by Test Accuracy",
      x = "Path Overshoot", y = "Accuracy", colour = "Test Type"
    ) +
    scale_colour_brewer(
      palette = "Greens",
      labels = c("Full Transfer","Partial Transfer")
    ) +
    theme(
      panel.background = element_rect(fill = "white", colour = "white"),
      plot.background = element_rect(fill = "white", colour = "white")
    )

  fnl <-
    file.path(project_path, "fig", paste(paste(
      version, exp, mes, "tsp",
      sep = "_"
    ), ".png", sep = ""))
  ggsave(fnl, plot = p, width = 14, height = 7)
  
