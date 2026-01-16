# R/05_pathA_switchcost_acc.R
source("R/00_setup.R")

for (d in c("figures", "tables", "reports")) {
  if (file.exists(d) && !dir.exists(d)) file.remove(d)
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

objs <- readRDS("data/processed/pathA_objects.rds")
d_acc <- objs$d_acc
if (is.null(d_acc)) stop("d_acc is NULL. Run R/01_data_prep.R first.")

# Cell means (participant x condition)
acc_cells <- d_acc %>%
  dplyr::group_by(ppt_num, group, session, switch, congruency) %>%
  dplyr::summarise(mean_correct = mean(correct, na.rm = TRUE), .groups = "drop")

# Accuracy switch cost = switch - repeat
acc_switch_cost <- acc_cells %>%
  tidyr::pivot_wider(names_from = switch, values_from = mean_correct) %>%
  dplyr::mutate(switch_cost_acc = switch - repeat)

acc_switch_cost_summary <- acc_switch_cost %>%
  dplyr::group_by(group, session, congruency) %>%
  dplyr::summarise(
    m  = mean(switch_cost_acc, na.rm = TRUE),
    sd = sd(switch_cost_acc, na.rm = TRUE),
    n  = sum(!is.na(switch_cost_acc)),
    se = sd / sqrt(n),
    .groups = "drop"
  )

p_acc_sc <- ggplot2::ggplot(acc_switch_cost_summary, ggplot2::aes(x = session, y = m, group = congruency)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = m - se, ymax = m + se), width = 0.1) +
  ggplot2::facet_wrap(~ group) +
  ggplot2::labs(
    x = "Session",
    y = "Accuracy switch cost (switch - repeat)",
    title = "Accuracy switch cost by Session, Congruency, and Group"
  )

print(p_acc_sc)
ggplot2::ggsave("figures/fig_acc_switch_cost.png", p_acc_sc, width = 8, height = 4.5, dpi = 300)

# paired t-test S1 vs S2
acc_switch_cost_wide <- acc_switch_cost %>%
  dplyr::select(ppt_num, group, congruency, session, switch_cost_acc) %>%
  tidyr::pivot_wider(names_from = session, values_from = switch_cost_acc, names_prefix = "S")

acc_switch_cost_tests <- acc_switch_cost_wide %>%
  dplyr::group_by(group, congruency) %>%
  dplyr::summarise(
    t_test = list(stats::t.test(S1, S2, paired = TRUE)),
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

utils::write.csv(acc_switch_cost_tests, "tables/table_acc_switch_cost_tests.csv", row.names = FALSE)
utils::write.csv(acc_switch_cost_summary, "tables/table_acc_switch_cost_summary.csv", row.names = FALSE)
