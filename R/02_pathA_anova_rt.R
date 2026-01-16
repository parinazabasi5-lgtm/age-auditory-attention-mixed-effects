# R/02_pathA_anova_rt.R
source("R/00_setup.R")

# Ensure output directories exist
for (d in c("figures", "tables", "reports")) {
  if (file.exists(d) && !dir.exists(d)) file.remove(d)
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

objs <- readRDS("data/processed/pathA_objects.rds")

# Support both layouts (new: d_rt, old: RT_df)
if (!is.null(objs$d_rt)) {
  d_rt <- objs$d_rt
} else if (!is.null(objs$RT_df)) {
  d_rt <- objs$RT_df %>%
    dplyr::mutate(
      ppt_num    = factor(ppt_num),
      group      = factor(group),
      block      = factor(block),
      switch     = factor(switch),
      congruency = factor(congruency),
      cue        = factor(cue),
      sex        = factor(sex),
      session    = if ("session" %in% names(.)) factor(session) else factor(1),
      correct    = if ("accuracy" %in% names(.)) (1L - accuracy) else NA_integer_,
      log_RT     = if ("log_RT" %in% names(.)) log_RT else log(RT)
    ) %>%
    dplyr::filter(RT >= 200, RT <= 3000)
} else {
  stop("No RT data found in data/processed/pathA_objects.rds (expected d_rt or RT_df).")
}

required_cols <- c("ppt_num", "group", "session", "switch", "congruency", "log_RT")
missing_cols <- setdiff(required_cols, names(d_rt))
if (length(missing_cols) > 0) stop("Missing required columns in d_rt: ", paste(missing_cols, collapse = ", "))

# Cell means (participant x condition)
rt_cells <- d_rt %>%
  dplyr::group_by(ppt_num, group, session, switch, congruency) %>%
  dplyr::summarise(mean_logRT = mean(log_RT, na.rm = TRUE), .groups = "drop")

# ANOVA
anova_rt <- afex::aov_ez(
  id = "ppt_num",
  dv = "mean_logRT",
  data = rt_cells,
  between = "group",
  within = c("session", "switch", "congruency"),
  type = 3
)

print(anova_rt)

# Summary for plot/table
rt_summary <- rt_cells %>%
  dplyr::group_by(group, session, switch, congruency) %>%
  dplyr::summarise(
    m  = mean(mean_logRT, na.rm = TRUE),
    sd = sd(mean_logRT, na.rm = TRUE),
    n  = sum(!is.na(mean_logRT)),
    se = sd / sqrt(n),
    .groups = "drop"
  )

# Plot
p_rt <- ggplot2::ggplot(rt_summary, ggplot2::aes(x = switch, y = m, group = congruency)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = m - se, ymax = m + se), width = 0.1) +
  ggplot2::facet_grid(group ~ session) +
  ggplot2::labs(
    x = "Switch",
    y = "Mean log(RT)",
    title = "Mean log(RT) by Switch, Congruency, Session, and Group"
  )

print(p_rt)
ggplot2::ggsave("figures/fig_rt_means.png", p_rt, width = 8, height = 4.5, dpi = 300)

# Export tables
anova_rt_table <- anova_rt$anova_table
table_anova_rt <- data.frame(Effect = rownames(anova_rt_table), anova_rt_table, row.names = NULL)

utils::write.csv(table_anova_rt, "tables/table_anova_rt.csv", row.names = FALSE)
utils::write.csv(rt_summary, "tables/table_rt_summary_cells.csv", row.names = FALSE)
