# R/01_data_prep.R
source("R/00_setup.R")

# --- Ensure required directories exist (and are directories, not files) ---
for (d in c("data/processed", "figures", "tables", "reports")) {
  if (file.exists(d) && !dir.exists(d)) file.remove(d)
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

# --- 1) Read raw data ---
ER_df    <- read.csv("data/raw/ER_df.csv")
RT_df    <- read.csv("data/raw/RT_df.csv")
pta      <- read.csv("data/raw/participants_PTA.csv")
codebook <- read.csv("data/raw/Codebook.csv")

# --- 2) Build analysis-ready objects ---

# RT data:
# In your data, RT_df appears to already include correct-only trials (accuracy mostly/all 0),
# but we handle both possibilities safely.
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
    log_RT     = if ("log_RT" %in% names(.)) log_RT else log(RT)
  ) %>%
  dplyr::filter(RT >= 200, RT <= 3000) %>%
  {
    # If accuracy exists AND any errors exist, keep correct trials only
    if ("accuracy" %in% names(.) && any(.$accuracy == 1, na.rm = TRUE)) {
      dplyr::mutate(., correct = 1L - accuracy) %>%
        dplyr::filter(correct == 1)
    } else {
      dplyr::mutate(., correct = 1L)
    }
  }

# Accuracy data (trial-level):
# Your ER_df: accuracy = 0 (correct), 1 (error)  --> correct = 1 - accuracy
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

# --- 3) Save for downstream scripts ---
saveRDS(
  list(
    d_rt     = d_rt,
    d_acc    = d_acc,
    pta      = pta,
    codebook = codebook,
    ER_df    = ER_df,   # optional: raw kept for safety
    RT_df    = RT_df    # optional: raw kept for safety
  ),
  "data/processed/pathA_objects.rds"
)

cat("Saved: data/processed/pathA_objects.rds\n")
cat("d_rt rows:", nrow(d_rt), " | d_acc rows:", nrow(d_acc), "\n")
