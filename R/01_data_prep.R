# R/01_data_prep.R
source("R/00_setup.R")

# --- Ensure output directories exist (and are directories, not files) ---
for (d in c("data/processed", "figures", "tables", "reports")) {
  if (file.exists(d) && !dir.exists(d)) file.remove(d)
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

# --- Read raw data ---
ER_df    <- read.csv("data/raw/ER_df.csv")
RT_df    <- read.csv("data/raw/RT_df.csv")
pta      <- read.csv("data/raw/participants_PTA.csv")
codebook <- read.csv("data/raw/Codebook.csv")

# --- Build analysis-ready RT data ---
# NOTE: In your RT_df, accuracy seems all 0 (correct-only). We still compute correct robustly.
d_rt <- RT_df %>%
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
  dplyr::filter(RT >= 200, RT <= 3000) %>%
  { 
    # If there are any error trials in RT_df, keep only correct; otherwise keep all rows
    if ("accuracy" %in% names(.) && any(.$accuracy == 1, na.rm = TRUE)) dplyr::filter(., correct == 1) else .
  }

# --- Build analysis-ready Accuracy data (trial-level) ---
# accuracy: 0=correct, 1=error  => correct = 1 - accuracy
d_acc <- ER_df %>%
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

# --- Save for downstream scripts (single, standard layout) ---
saveRDS(
  list(d_rt = d_rt, d_acc = d_acc, pta = pta, codebook = codebook),
  "data/processed/pathA_objects.rds"
)

cat("Saved: data/processed/pathA_objects.rds\n")
cat("d_rt rows:", nrow(d_rt), " | d_acc rows:", nrow(d_acc), "\n")
cat("Sessions in d_rt:\n")
print(dplyr::count(d_rt, session))
cat("Sessions in d_acc:\n")
print(dplyr::count(d_acc, session))
