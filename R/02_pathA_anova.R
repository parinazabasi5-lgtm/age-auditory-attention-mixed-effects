library(tidyverse)
library(janitor)
library(skimr)
library(lme4)
library(lmerTest)
library(emmeans)
library(performance)
library(broom.mixed)
library(afex)

afex::afex_options(type = 3)

data <- read.csv("data/raw/data.csv")
codebook <- read.csv("data/raw/Codebook.csv")
pta <- read.csv("data/raw/participants_PTA.csv")

glimpse(data)
skim(data)

n_distinct(data$ppt_num)

data %>% summarise(
  sessions = n_distinct(session),
  blocks = n_distinct(block),
  switch_levels = paste(unique(switch), collapse = ", "),
  congruency_levels = paste(unique(congruency), collapse = ", "),
  groups = paste(unique(group), collapse = ", ")
)

d <- data %>%
  mutate(
    ppt_num = factor(ppt_num),
    session = factor(session),
    block = factor(block),
    sex = factor(sex),
    group = factor(group),
    switch = factor(switch),
    congruency = factor(congruency),
    cue = factor(cue),
    correct = 1 - accuracy  # accuracy: 0=correct, 1=error
  )

d_acc <- d

d_rt <- d %>%
  filter(correct == 1) %>%
  filter(RT >= 200, RT <= 3000) %>%
  mutate(log_RT = log(RT))

tibble(
  total_trials = nrow(d),
  rt_trials_kept = nrow(d_rt),
  removed_trials = nrow(d) - nrow(d_rt),
  removed_percent = round(100 * (nrow(d) - nrow(d_rt)) / nrow(d), 2)
)

rt_cells <- d_rt %>%
  group_by(ppt_num, group, session, switch, congruency) %>%
  summarise(
    mean_logRT = mean(log_RT, na.rm = TRUE),
    .groups = "drop"
  )

head(rt_cells)

anova_rt <- aov_ez(
  id = "ppt_num",
  dv = "mean_logRT",
  data = rt_cells,
  between = "group",
  within = c("session", "switch", "congruency"),
  type = 3
)

anova_rt

rt_summary <- rt_cells %>%
  group_by(group, session, switch, congruency) %>%
  summarise(
    m  = mean(mean_logRT, na.rm = TRUE),
    sd = sd(mean_logRT, na.rm = TRUE),
    n  = sum(!is.na(mean_logRT)),
    se = sd / sqrt(n),
    .groups = "drop"
  )

rt_summary
