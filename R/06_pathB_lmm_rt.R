# R/06_pathB_lmm_rt.R
source("R/00_setup.R")

# ----------------------------
# Helpers
# ----------------------------
`%||%` <- function(x, y) if (is.null(x)) y else x

ensure_dirs <- function(dirs = c("figures", "tables", "reports", "data/processed")) {
  for (d in dirs) {
    if (file.exists(d) && !dir.exists(d)) file.remove(d)
    dir.create(d, showWarnings = FALSE, recursive = TRUE)
  }
}
ensure_dirs()

# ----------------------------
# Load data (support both layouts)
# ----------------------------
fp <- "data/processed/pathA_objects.rds"
stopifnot(file.exists(fp))
objs <- readRDS(fp)

# Prefer analysis-ready d_rt, otherwise build from raw RT_df
d_rt <- objs$d_rt %||% NULL
if (is.null(d_rt)) {
  if (is.null(objs$RT_df)) stop("No RT data found in pathA_objects.rds (expected d_rt or RT_df).")
  
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
      correct    = if ("accuracy" %in% names(.)) 1 - accuracy else NA_integer_,
      log_RT     = if ("log_RT" %in% names(.)) log_RT else log(RT)
    )
}

# Basic checks
required_cols <- c("ppt_num", "group", "session", "switch", "congruency", "cue", "sex", "RT", "log_RT")
missing_cols <- setdiff(required_cols, names(d_rt))
if (length(missing_cols) > 0) stop("Missing required columns in d_rt: ", paste(missing_cols, collapse = ", "))

# Keep plausible RT range; keep correct-only if correct exists and includes errors
d_rt <- d_rt %>%
  dplyr::filter(RT >= 200, RT <= 3000) %>%
  { if ("correct" %in% names(.) && any(.$correct == 0, na.rm = TRUE)) dplyr::filter(., correct == 1) else . }

# Make sure factor levels are stable
d_rt <- d_rt %>%
  dplyr::mutate(
    session    = factor(session),
    switch     = factor(switch),
    congruency = factor(congruency),
    group      = factor(group)
  )

# Set contrasts for Type-III style tests (common in factorial designs)
options(contrasts = c("contr.sum", "contr.poly"))

# ----------------------------
# Fit mixed model (trial-level RT)
# ----------------------------
# Random effects: participant intercept + random slopes (uncorrelated) for within-subject factors
# Using || helps convergence vs full correlated random effects.
m_rt <- lme4::lmer(
  log_RT ~ group * session * switch * congruency + cue + sex +
    (1 + session + switch + congruency || ppt_num),
  data = d_rt,
  control = lme4::lmerControl(optimizer = "bobyqa")
)

print(summary(m_rt))

# Type-III tests (Wald)
anova_rt <- car::Anova(m_rt, type = 3)
print(anova_rt)

# ----------------------------
# Estimated marginal means + switch-cost contrasts
# ----------------------------
emm_rt <- emmeans::emmeans(m_rt, ~ switch * congruency * session | group)
emm_rt_df <- as.data.frame(emm_rt)

# Switch cost within each cell (switch - repeat)
# Use quoted names because "repeat" is a reserved word in R
sc_rt <- emmeans::contrast(
  emmeans::emmeans(m_rt, ~ switch | group * session * congruency),
  method = list(switch_cost = c("repeat" = -1, "switch" = 1))
)
sc_rt_df <- as.data.frame(sc_rt)


# ----------------------------
# Plot (EMMs)
# ----------------------------
p_rt <- ggplot2::ggplot(
  emm_rt_df,
  ggplot2::aes(x = switch, y = emmean, group = congruency)
) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = emmean - SE, ymax = emmean + SE),
    width = 0.1
  ) +
  ggplot2::facet_grid(group ~ session) +
  ggplot2::labs(
    x = "Switch",
    y = "Estimated mean log(RT)",
    title = "Path B (trial-level): LMM estimated log(RT)"
  )

print(p_rt)
ggplot2::ggsave("figures/fig_pathB_rt_emm.png", p_rt, width = 8, height = 4.5, dpi = 300)

# ----------------------------
# Export tables
# ----------------------------

# Fixed effects table (estimate, SE, t)
fixef_tab <- as.data.frame(summary(m_rt)$coefficients)
fixef_tab$Effect <- rownames(fixef_tab)
rownames(fixef_tab) <- NULL

# نظم ستون‌ها
fixef_tab <- fixef_tab[, c("Effect", "Estimate", "Std. Error", "t value")]

# Type-III Wald table
anova_rt_tab <- as.data.frame(anova_rt)
anova_rt_tab$Effect <- rownames(anova_rt_tab)
rownames(anova_rt_tab) <- NULL
anova_rt_tab <- anova_rt_tab[, c("Effect", "Chisq", "Df", "Pr(>Chisq)")]

# Save: EMMs + switch-cost contrasts too
utils::write.csv(fixef_tab,   "tables/table_pathB_lmm_rt_fixef.csv", row.names = FALSE)
utils::write.csv(anova_rt_tab,"tables/table_pathB_lmm_rt_anova_type3.csv", row.names = FALSE)
utils::write.csv(emm_rt_df,   "tables/table_pathB_lmm_rt_emmeans.csv", row.names = FALSE)
utils::write.csv(sc_rt_df,    "tables/table_pathB_lmm_rt_switch_cost_emm.csv", row.names = FALSE)

cat("Saved Path B RT outputs:\n")
cat("- figures/fig_pathB_rt_emm.png\n")
cat("- tables/table_pathB_lmm_rt_fixef.csv\n")
cat("- tables/table_pathB_lmm_rt_anova_type3.csv\n")
cat("- tables/table_pathB_lmm_rt_emmeans.csv\n")
cat("- tables/table_pathB_lmm_rt_switch_cost_emm.csv\n")

list.files("figures", pattern = "pathB_acc", full.names = TRUE)
list.files("tables",  pattern = "pathB_glmm_acc", full.names = TRUE)

# ----------------------------
# Save model objects (for reporting)
# ----------------------------
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

pathB_models_fp <- "data/processed/pathB_models.rds"
models <- if (file.exists(pathB_models_fp)) readRDS(pathB_models_fp) else list()

models$m_rt <- m_rt
saveRDS(models, pathB_models_fp)

cat("Saved/updated Path B models at:", pathB_models_fp, "\n")
