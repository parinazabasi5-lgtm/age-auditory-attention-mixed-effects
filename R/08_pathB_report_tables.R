# R/08_pathB_report_tables.R
source("R/00_setup.R")

# ----------------------------
# Helpers
# ----------------------------
ensure_dirs <- function(dirs = c("tables", "figures", "reports", "data/processed")) {
  for (d in dirs) {
    if (file.exists(d) && !dir.exists(d)) file.remove(d)
    dir.create(d, showWarnings = FALSE, recursive = TRUE)
  }
}

fmt_p <- function(p) {
  ifelse(is.na(p), NA_character_,
         ifelse(p < .001, "<.001", sprintf("%.3f", p)))
}

wald_ci <- function(est, se, z = 1.96) {
  lo <- est - z * se
  hi <- est + z * se
  cbind(lo = lo, hi = hi)
}

safe_select <- function(df, cols) df[, intersect(cols, names(df)), drop = FALSE]

ensure_dirs()

# ----------------------------
# Load models
# ----------------------------
models_fp <- "data/processed/pathB_models.rds"
if (!file.exists(models_fp)) {
  stop("Missing data/processed/pathB_models.rds. Run R/06_pathB_lmm_rt.R and R/07_pathB_glmm_acc.R with model-saving enabled.")
}

models <- readRDS(models_fp)

if (is.null(models$m_rt)) stop("m_rt not found in pathB_models.rds. Re-run R/06_pathB_lmm_rt.R after adding save step.")
if (is.null(models$m_acc)) stop("m_acc not found in pathB_models.rds. Re-run R/07_pathB_glmm_acc.R after adding save step.")

m_rt  <- models$m_rt
m_acc <- models$m_acc

# Make contrasts stable (Type-III style)
options(contrasts = c("contr.sum", "contr.poly"))

# ============================================================
# Path B — RT switch-cost report table (log scale -> ratio/%)
# ============================================================
# EMMs on log scale (default) then switch-repeat contrast
emm_rt_sw <- emmeans::emmeans(m_rt, ~ switch | group * session * congruency)
sc_rt <- emmeans::contrast(
  emm_rt_sw,
  method = list(switch_cost = c("repeat" = -1, "switch" = 1))
)
sc_rt_df <- as.data.frame(sc_rt)

# Ensure standard column names
# (emmeans usually gives: estimate, SE, df, t.ratio or z.ratio, p.value)
if (!("p.value" %in% names(sc_rt_df)) && ("p.value" %in% names(sc_rt_df) == FALSE)) {
  # nothing to do; just keep as-is
}

# Back-transform: estimate is delta on log(RT) scale
# ratio = exp(delta); percent = 100*(ratio-1)
ci_rt <- wald_ci(sc_rt_df$estimate, sc_rt_df$SE)

sc_rt_df$RT_ratio     <- exp(sc_rt_df$estimate)
sc_rt_df$RT_ratio_low <- exp(ci_rt[, "lo"])
sc_rt_df$RT_ratio_hi  <- exp(ci_rt[, "hi"])

sc_rt_df$RT_pct      <- 100 * (sc_rt_df$RT_ratio - 1)
sc_rt_df$RT_pct_low  <- 100 * (sc_rt_df$RT_ratio_low - 1)
sc_rt_df$RT_pct_high <- 100 * (sc_rt_df$RT_ratio_hi - 1)

if ("p.value" %in% names(sc_rt_df)) sc_rt_df$p.value <- fmt_p(sc_rt_df$p.value)

rt_report_cols <- c(
  "group", "session", "congruency", "contrast",
  "estimate", "SE", "df", "t.ratio", "z.ratio", "p.value",
  "RT_ratio", "RT_ratio_low", "RT_ratio_hi",
  "RT_pct", "RT_pct_low", "RT_pct_high"
)

utils::write.csv(
  safe_select(sc_rt_df, rt_report_cols),
  "tables/table_pathB_rt_switchcost_report.csv",
  row.names = FALSE
)

# ============================================================
# Path B — Accuracy switch-cost report table
#   Part A: diff in probability (response scale)
#   Part B: delta logit + OR (link scale)
#   Then merge safely
# ============================================================

# --- A) difference in probability on response scale ---
emm_acc_sw_resp <- emmeans::emmeans(m_acc, ~ switch | group * session * congruency, type = "response")
sc_acc_prob <- emmeans::contrast(
  emm_acc_sw_resp,
  method = list(switch_cost = c("repeat" = -1, "switch" = 1))
)
sc_acc_prob_df <- as.data.frame(sc_acc_prob)

# rename for clarity
sc_acc_prob_keep <- sc_acc_prob_df
names(sc_acc_prob_keep)[names(sc_acc_prob_keep) == "estimate"] <- "diff_prob"
names(sc_acc_prob_keep)[names(sc_acc_prob_keep) == "SE"]       <- "SE_diff"

if ("p.value" %in% names(sc_acc_prob_keep)) sc_acc_prob_keep$p.value <- fmt_p(sc_acc_prob_keep$p.value)

# --- B) delta logit + OR on link scale ---
emm_acc_sw_link <- emmeans::emmeans(m_acc, ~ switch | group * session * congruency, type = "link")
sc_acc_link <- emmeans::contrast(
  emm_acc_sw_link,
  method = list(switch_cost = c("repeat" = -1, "switch" = 1))
)
sc_acc_link_df <- as.data.frame(sc_acc_link)

sc_acc_or_keep <- sc_acc_link_df
names(sc_acc_or_keep)[names(sc_acc_or_keep) == "estimate"] <- "delta_logit"
names(sc_acc_or_keep)[names(sc_acc_or_keep) == "SE"]       <- "SE_delta"

ci_or <- wald_ci(sc_acc_or_keep$delta_logit, sc_acc_or_keep$SE_delta)
sc_acc_or_keep$OR      <- exp(sc_acc_or_keep$delta_logit)
sc_acc_or_keep$OR_low  <- exp(ci_or[, "lo"])
sc_acc_or_keep$OR_high <- exp(ci_or[, "hi"])

if ("p.value" %in% names(sc_acc_or_keep)) sc_acc_or_keep$p.value <- fmt_p(sc_acc_or_keep$p.value)

# --- Merge (robust) ---
merge_keys <- c("group", "session", "congruency", "contrast")
merge_keys <- intersect(merge_keys, intersect(names(sc_acc_prob_keep), names(sc_acc_or_keep)))

sc_acc_prob_keep2 <- safe_select(
  sc_acc_prob_keep,
  c(merge_keys, "diff_prob", "SE_diff", "p.value")
)

sc_acc_or_keep2 <- safe_select(
  sc_acc_or_keep,
  c(merge_keys, "delta_logit", "SE_delta", "OR", "OR_low", "OR_high", "p.value")
)

sc_acc_report <- merge(
  sc_acc_prob_keep2,
  sc_acc_or_keep2,
  by = merge_keys,
  all = TRUE,
  suffixes = c("_prob", "_or")
)

utils::write.csv(
  sc_acc_report,
  "tables/table_pathB_acc_switchcost_report.csv",
  row.names = FALSE
)

cat("Saved report-friendly Path B tables:\n")
cat("- tables/table_pathB_rt_switchcost_report.csv\n")
cat("- tables/table_pathB_acc_switchcost_report.csv\n")
