
rm(list=ls())

library(tidyverse)
project_path <- getwd()
exp <- "exp_ts" # experiment: 'exp_ts' (task-switching) or 'exp_lt' (learning transfer)
title_sz = 40
label_sz <- 20
mk_sz <- 2

fnl <- file.path(project_path, "res", paste(paste(exp, "avg", sep = "_"), ".csv", sep = ""))
A <- read.csv(fnl)
fnl <- file.path(project_path, "res", paste(paste(exp, "routines", sep = "_"), ".csv", sep = ""))
B <- read_csv(fnl, show_col_types = FALSE)

res <- inner_join(A,B,by = join_by(x$sub==y$sub,x$context==y$context))

res %>% 
  filter(ses==2) %>% 
  ggplot() +
  geom_violin(aes(x = factor(train_type.x), y = entropy, color = factor(context), fill = factor(context)),
              position = position_dodge(width = .7), alpha = .5, linewidth = .4) +
  geom_boxplot(aes(x = factor(train_type.x), y = entropy, fill = factor(context)),
               position = position_dodge(width = .7), width = .05, linewidth = .7,
               outlier.alpha = 1,outlier.shape = 21,outlier.size = 2.5,outlier.stroke = NA) +
  theme_classic() +
  scale_colour_manual(values = c("#F8CF71","#ABEBC6"),
                      name = "Context",
                      #labels = c("Non-Switch","Switch")
                      ) +
  scale_fill_manual(values = c("#F8CF71","#ABEBC6"),
                    name = "Context",
                    #labels = c("Non-Switch","Switch")
                    ) +
  scale_x_discrete(labels = c("Low", "High")) +
  labs(title = "", x = "Training Group", y = "Entropy") +
  theme(
    plot.title = element_text(size = label_sz),
    axis.text.x = element_text(size = label_sz), axis.text.y = element_text(size = label_sz), legend.text = element_text(size = label_sz),
    axis.title.x = element_text(size = label_sz), axis.title.y = element_text(size = label_sz), legend.title = element_text(size = label_sz)
  )

res %>% 
  filter(ses==2) %>% 
  ggplot() +
  geom_point(aes(x = entropy, y = context_changes, color = factor(train_type.x), fill = factor(train_type.x)),
              alpha = .5) +
  theme_classic() +
  scale_colour_manual(values = c("#F8CF71","#ABEBC6"),
                      name = "Train Type",
                      labels = c("Low Switch","High Switch")
  ) +
  scale_fill_manual(values = c("#F8CF71","#ABEBC6"),
                    name = "Train Type",
                    labels = c("Low Switch","High Switch")
  ) +
  labs(title = "", x = "Entropy", y = "Context Shifts") +
  theme(
    plot.title = element_text(size = label_sz),
    axis.text.x = element_text(size = label_sz), axis.text.y = element_text(size = label_sz), legend.text = element_text(size = label_sz),
    axis.title.x = element_text(size = label_sz), axis.title.y = element_text(size = label_sz), legend.title = element_text(size = label_sz)
  )
