library(readr)
library(dplyr)

rt_df <- read_csv("data/raw/RT_df.csv", show_col_types = FALSE)
er_df <- read_csv("data/raw/ER_df.csv", show_col_types = FALSE)

standardize <- function(df) {
  df %>%
    mutate(
      correct = if_else(accuracy == 0, 1L, 0L),
      ppt_num = factor(ppt_num),
      group = factor(group),
      switch = factor(switch),
      congruency = factor(congruency)
    )
}

rt_df <- standardize(rt_df)
er_df <- standardize(er_df)

dir.create("data/processed", showWarnings = FALSE)

write_csv(rt_df, "data/processed/rt_df_processed.csv")
write_csv(er_df, "data/processed/er_df_processed.csv")
