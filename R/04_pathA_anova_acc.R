# R/03_pathA_switchcost_rt.R
source("R/00_setup.R")

for (d in c("figures", "tables", "reports")) {
  if (file.exists(d) && !dir.exists(d)) file.remove(d)
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

objs <- readRDS("data/processed/pathA_objects.rds")
d_rt <- if (!is.null(objs$d_rt)) objs$d_rt else stop("d_rt not found in pathA_objects.rds")

required_cols <- c("ppt_num", "group", "session", "switch", "congruency", "log_RT")
missing_cols <- setdiff(required_cols, names(d_rt))
if (length(missing_cols) > 0) stop("Missing required columns in d_rt: ", paste(missing_cols, collapse = ", "))

rt_cells <- d_rt %>%
  dplyr::group_by(ppt_num, group, session, switch, congruency) %>%
  dplyr::summarise(mean_logRT = mean(log_RT, na.rm = TRUE), .groups = "drop")

# switch cost = switch - repeat (within each ppt/group/session/congruency)
switch_cost <- rt_cells %>%
  tidyr::pivot_wider(names_from = switch, values_from = mean_logRT) %>%
  dplyr::mutate(
    switch_cost = .data[["switch"]] - .data[["repeat"]]
  )

switch_cost_summary <- switch_cost %>%
  dplyr::group_by(group, session, congruency) %>%
  dplyr::summarise(
    m  = mean(switch_cost, na.rm = TRUE),
    sd = sd(switch_cost, na.rm = TRUE),
    n  = sum(!is.na(switch_cost)),
    se = sd / sqrt(n),
    .groups = "drop"
  )

p_sc <- ggplot2::ggplot(switch_cost_summary, ggplot2::aes(x = session, y = m, group = congruency)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = m - se, ymax = m + se), width = 0.1) +
  ggplot2::facet_wrap(~ group) +
  ggplot2::labs(
    x = "Session",
    y = "Switch cost (logRT: switch - repeat)",
    title = "Switch cost by Session, Congruency, and Group"
  )

print(p_sc)
ggplot2::ggsave("figures/fig_rt_switch_cost.png", p_sc, width = 8, height = 4.5, dpi = 300)

# Paired t-test S1 vs S2 (within-subject)
switch_cost_wide <- switch_cost %>%
  dplyr::select(ppt_num, group, congruency, session, switch_cost) %>%
  tidyr::pivot_wider(names_from = session, values_from = switch_cost, names_prefix = "S")

switch_cost_tests <- switch_cost_wide %>%
  dplyr::group_by(group, congruency) %>%
  dplyr::summarise(
    t_test  = list(stats::t.test(S1, S2, paired = TRUE)),
    mean_s1 = mean(S1, na.rm = TRUE),
    mean_s2 = mean(S2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    t  = purrr::map_dbl(t_test, ~ unname(.x$statistic)),
    df = purrr::map_dbl(t_test, ~ unname(.x$parameter)),
    p  = purrr::map_dbl(t_test, ~ .x$p.value)
  ) %>%
  dplyr::select(group, congruency, mean_s1, mean_s2, t, df, p)

utils::write.csv(switch_cost_tests, "tables/table_rt_switch_cost_tests.csv", row.names = FALSE)
utils::write.csv(switch_cost_summary, "tables/table_rt_switch_cost_summary.csv", row.names = FALSE)
