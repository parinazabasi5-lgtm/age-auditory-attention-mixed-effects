# R/04_pathA_anova_acc.R
source("R/00_setup.R")

for (d in c("figures", "tables", "reports")) {
  if (file.exists(d) && !dir.exists(d)) file.remove(d)
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

objs <- readRDS("data/processed/pathA_objects.rds")

# Support both layouts: d_acc (new) or ER_df (old)
if (!is.null(objs$d_acc)) {
  d_acc <- objs$d_acc
} else if (!is.null(objs$ER_df)) {
  d_acc <- objs$ER_df %>%
    dplyr::mutate(
      ppt_num    = factor(ppt_num),
      group      = factor(group),
      block      = factor(block),
      switch     = factor(switch),
      congruency = factor(congruency),
      cue        = factor(cue),
      sex        = factor(sex),
      session    = if ("session" %in% names(.)) factor(session) else factor(1),
      correct    = 1L - accuracy
    )
} else {
  stop("No accuracy data found in pathA_objects.rds (expected d_acc or ER_df).")
}

required_cols <- c("ppt_num", "group", "session", "switch", "congruency", "correct")
missing_cols <- setdiff(required_cols, names(d_acc))
if (length(missing_cols) > 0) stop("Missing required columns in d_acc: ", paste(missing_cols, collapse = ", "))

acc_cells <- d_acc %>%
  dplyr::group_by(ppt_num, group, session, switch, congruency) %>%
  dplyr::summarise(mean_correct = mean(correct, na.rm = TRUE), .groups = "drop")

anova_acc <- afex::aov_ez(
  id = "ppt_num",
  dv = "mean_correct",
  data = acc_cells,
  between = "group",
  within = c("session", "switch", "congruency"),
  type = 3
)

print(anova_acc)

acc_summary <- acc_cells %>%
  dplyr::group_by(group, session, switch, congruency) %>%
  dplyr::summarise(
    m  = mean(mean_correct, na.rm = TRUE),
    sd = sd(mean_correct, na.rm = TRUE),
    n  = sum(!is.na(mean_correct)),
    se = sd / sqrt(n),
    .groups = "drop"
  )

p_acc <- ggplot2::ggplot(acc_summary, ggplot2::aes(x = switch, y = m, group = congruency)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = m - se, ymax = m + se), width = 0.1) +
  ggplot2::facet_grid(group ~ session) +
  ggplot2::labs(
    x = "Switch",
    y = "Mean correct rate",
    title = "Accuracy (correct rate) by Switch, Congruency, Session, and Group"
  )

print(p_acc)
ggplot2::ggsave("figures/fig_acc_means.png", p_acc, width = 8, height = 4.5, dpi = 300)

anova_acc_table <- anova_acc$anova_table
table_anova_acc <- data.frame(
  Effect = rownames(anova_acc_table),
  anova_acc_table,
  row.names = NULL
)

utils::write.csv(table_anova_acc, "tables/table_anova_acc.csv", row.names = FALSE)
utils::write.csv(acc_summary, "tables/table_acc_summary_cells.csv", row.names = FALSE)
