# R/00_setup.R

# install.packages(c(
#   "tidyverse", "janitor", "skimr",
#   "afex", "emmeans", "performance", "broom.mixed"
# ))

library(tidyverse)
library(janitor)
library(skimr)
library(afex)
library(emmeans)
library(performance)
library(broom.mixed)

set.seed(123)

dir.create("figures", showWarnings = FALSE, recursive = TRUE)
dir.create("tables",  showWarnings = FALSE, recursive = TRUE)
