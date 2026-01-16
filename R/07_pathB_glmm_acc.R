# R/07_pathB_glmm_acc.R
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

# Prefer analysis-ready d_acc, otherwise build from raw ER_df
d_acc <- objs$d_acc %||% NULL
if (is.null(d_acc)) {
  if (is.null(objs$ER_df)) stop("No accuracy data found in pathA_objects.rds (expected d_acc or ER_df).")
  
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
      correct    = 1L - accuracy   # accuracy: 0=correct, 1=error
    )
}

# ----------------------------
# Basic checks + cleanup
# ----------------------------
required_cols <- c("ppt_num", "group", "session", "switch", "congruency", "cue", "sex", "correct")
missing_cols <- setdiff(required_cols, names(d_acc))
if (length(missing_cols) > 0) stop("Missing required columns in d_acc: ", paste(missing_cols, collapse = ", "))

d_acc <- d_acc %>%
  dplyr::mutate(
    # force to numeric 0/1; keep NA if any (then we drop NA below)
    correct    = as.integer(correct),
    session    = factor(session),
    switch     = factor(switch),
    congruency = factor(congruency),
    group      = factor(group)
  ) %>%
  dplyr::filter(!is.na(correct))

# sanity: ensure only 0/1
bad_vals <- setdiff(unique(d_acc$correct), c(0L, 1L))
if (length(bad_vals) > 0) stop("Unexpected values in correct (expected 0/1). Found: ", paste(bad_vals, collapse = ", "))

# Set contrasts for Type-III style tests
options(contrasts = c("contr.sum", "contr.poly"))

# ----------------------------
# Fit GLMM (trial-level accuracy)
# ----------------------------
m_acc <- lme4::glmer(
  correct ~ group * session * switch * congruency + cue + sex +
    (1 + session + switch + congruency || ppt_num),
  data = d_acc,
  family = binomial(link = "logit"),
  control = lme4::glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

print(summary(m_acc))

# Type-III tests (Wald chi-square)
anova_acc <- car::Anova(m_acc, type = 3)
print(anova_acc)

# ----------------------------
# Estimated marginal means (probabilities) + switch-cost contrasts
# ----------------------------
emm_acc <- emmeans::emmeans(
  m_acc,
  ~ switch * congruency * session | group,
  type = "response"
)
emm_acc_df <- as.data.frame(emm_acc)

# Switch cost (switch - repeat) on RESPONSE scale
# Important: use quoted names because "repeat" is reserved in R
sc_acc <- emmeans::contrast(
  emmeans::emmeans(m_acc, ~ switch | group * session * congruency, type = "response"),
  method = list(switch_cost = c("repeat" = -1, "switch" = 1))
)
sc_acc_df <- as.data.frame(sc_acc)

# ----------------------------
# Plot (EMMs)
# ----------------------------
p_acc <- ggplot2::ggplot(
  emm_acc_df,
  ggplot2::aes(x = switch, y = prob, group = congruency)
) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = asymp.LCL, ymax = asymp.UCL),
    width = 0.1
  ) +
  ggplot2::facet_grid(group ~ session) +
  ggplot2::labs(
    x = "Switch",
    y = "Estimated P(correct)",
    title = "Path B (trial-level): GLMM estimated accuracy"
  )

print(p_acc)
ggplot2::ggsave("figures/fig_pathB_acc_emm.png", p_acc, width = 8, height = 4.5, dpi = 300)

# ----------------------------
# Export tables
# ----------------------------

# Fixed effects table (Estimate, SE, z, p)
fixef_tab <- as.data.frame(summary(m_acc)$coefficients)
fixef_tab$Effect <- rownames(fixef_tab)
rownames(fixef_tab) <- NULL
fixef_tab <- fixef_tab[, c("Effect", "Estimate", "Std. Error", "z value", "Pr(>|z|)")]

# Type-III Wald table
anova_tab <- as.data.frame(anova_acc)
anova_tab$Effect <- rownames(anova_tab)
rownames(anova_tab) <- NULL
# car::Anova typically returns columns like Chisq, Df, Pr(>Chisq)
# keep them in a consistent order if present
keep_cols <- intersect(c("Effect", "Chisq", "Df", "Pr(>Chisq)"), names(anova_tab))
anova_tab <- anova_tab[, keep_cols, drop = FALSE]

utils::write.csv(fixef_tab,  "tables/table_pathB_glmm_acc_fixef.csv",        row.names = FALSE)
utils::write.csv(anova_tab,  "tables/table_pathB_glmm_acc_anova_type3.csv",  row.names = FALSE)
utils::write.csv(emm_acc_df, "tables/table_pathB_glmm_acc_emmeans.csv",      row.names = FALSE)
utils::write.csv(sc_acc_df,  "tables/table_pathB_glmm_acc_switch_cost_emm.csv", row.names = FALSE)

cat("Saved Path B Accuracy outputs:\n")
cat("- figures/fig_pathB_acc_emm.png\n")
cat("- tables/table_pathB_glmm_acc_fixef.csv\n")
cat("- tables/table_pathB_glmm_acc_anova_type3.csv\n")
cat("- tables/table_pathB_glmm_acc_emmeans.csv\n")
cat("- tables/table_pathB_glmm_acc_switch_cost_emm.csv\n")

# ----------------------------
# Save model objects (for reporting)
# ----------------------------
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

pathB_models_fp <- "data/processed/pathB_models.rds"
models <- if (file.exists(pathB_models_fp)) readRDS(pathB_models_fp) else list()

models$m_acc <- m_acc
saveRDS(models, pathB_models_fp)

cat("Saved/updated Path B models at:", pathB_models_fp, "\n")
