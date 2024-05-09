
### sources
library(tidyverse)

### settings
version <- "study-01"  #pilot-data-00 (train and test), pilot-data-01 (learn and train), pilot-data-02 (learn and train, learn phase split into two parts)
exp <- "exp_lt"  #experiment: 'exp_ts' (task-switching) or 'exp_lt' (learning transfer)
mes <- "clicks"  #measure: 'clicks' or 'hovers'. usually want 'clicks'.

# figure settings
title_sz <- 30
label_sz <- 30

### paths
project_path <- getwd()
data_path <- file.path(project_path, "res")
if (!dir.exists(data_path)) {
  stop(paste0(data_path, " does not exist"))
}

fnl <- file.path(
  project_path, "res", paste(
    paste(version, exp, mes, "transitions", sep = "_"),
    ".csv", sep = ""
  )
)
res <- read_csv(fnl)

# make the figure
res %>% ggplot() + 
  geom_point(
  aes(x = transition_rate, y = accuracy),
  show.legend = FALSE, alpha = 1
) +
  theme_minimal() + ylim(0,1) + xlim(0,1) +
  labs(
    title = "Transition Rate by Accuracy", x = "Transition Rate", y = "Accuracy"
  ) +
  theme(
    axis.text.x = element_text(size = label_sz),
    axis.text.y = element_text(size = label_sz),
    legend.text = element_text(size = label_sz),
    plot.title = element_text(size = title_sz),
    axis.title.x = element_text(size = label_sz),
    axis.title.y = element_text(size = label_sz),
    legend.title = element_text(size = label_sz)
  )

# save it
fnl <- file.path(
  project_path, "fig", paste(
    paste(
      version, exp, mes, "transitions-accuracy",sep = "_"
    ),
    ".pdf", sep = ""
  )
)
ggsave(fnl, plot = last_plot())
