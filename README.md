# Age auditory attention — analysis (Path A + Path B)

This repository documents analyses for the “Age auditory attention” dataset.

It currently includes two analysis tracks:

- **Path A (aggregated + repeated-measures ANOVA)**  
  Aggregate trial-level data to subject-by-condition cells, run repeated-measures ANOVA, compute switch costs, and export figures/tables.

- **Path B (trial-level mixed-effects models)**  
  Fit trial-level mixed-effects models:
  - **RT:** linear mixed model (LMM) on log-transformed RT
  - **Accuracy:** generalized linear mixed model (GLMM) with binomial (logit) link  
  Then compute estimated marginal means (EMMs) and switch-cost contrasts and export report-friendly tables.

## Data source
The dataset is publicly available (public data; safe to share).
- OSF link: https://www.psycharchives.org/en/item/6fd4f6d0-bfab-492e-b762-15825603125d

## Repository structure
- `R/` : R scripts (analysis pipeline)
- `data/raw/` : raw input CSVs (from OSF)
- `data/processed/` : processed datasets and saved model objects created by scripts
- `figures/` : exported figures (PNG)
- `tables/` : exported tables (CSV)
- `reports/` : rendered reports (Quarto/PDF/HTML) (optional; may be empty)
- `docs/` : website output for GitHub Pages (optional)
- `_quarto.yml`, `index.qmd` : Quarto project files

## How to reproduce

### 0) Open the project
Open RStudio in the project root (where the `.Rproj` file is).

### 1) Prepare analysis-ready data
Run:

1. `R/01_data_prep.R`
   - Reads raw CSVs from `data/raw/`
   - Creates analysis-ready datasets:
     - `d_rt` (trial-level RT; RT range filtered; correct-only if error trials exist)
     - `d_acc` (trial-level accuracy; `correct = 1 - accuracy`, where accuracy is 0=correct, 1=error)
   - Saves a standard object file:
     - `data/processed/pathA_objects.rds`

### 2) Path A — Aggregated ANOVAs and switch costs
Run scripts in order:

2. `R/02_pathA_anova_rt.R`  
3. `R/03_pathA_switchcost_rt.R`  
4. `R/04_pathA_anova_acc.R`  
5. `R/05_pathA_switchcost_acc.R`

Outputs:
- Figures: `figures/fig_rt_means.png`, `figures/fig_rt_switch_cost.png`, `figures/fig_acc_means.png`, `figures/fig_acc_switch_cost.png`
- Tables: `tables/table_anova_rt.csv`, `tables/table_rt_summary_cells.csv`, `tables/table_rt_switch_cost_*.csv`,
  `tables/table_anova_acc.csv`, `tables/table_acc_summary_cells.csv`, `tables/table_acc_switch_cost_*.csv`

### 3) Path B — Trial-level mixed-effects models
Run scripts in order:

6. `R/06_pathB_lmm_rt.R`  
   - Fits an LMM on log RT (trial-level):  
     `log_RT ~ group * session * switch * congruency + cue + sex + (1 + session + switch + congruency || ppt_num)`
   - Exports:
     - `figures/fig_pathB_rt_emm.png`
     - `tables/table_pathB_lmm_rt_fixef.csv`
     - `tables/table_pathB_lmm_rt_anova_type3.csv`
     - `tables/table_pathB_lmm_rt_emmeans.csv`
     - `tables/table_pathB_lmm_rt_switch_cost_emm.csv`

7. `R/07_pathB_glmm_acc.R`  
   - Fits a GLMM on trial-level accuracy (binomial logit):  
     `correct ~ group * session * switch * congruency + cue + sex + (1 + session + switch + congruency || ppt_num)`
   - Exports:
     - `figures/fig_pathB_acc_emm.png`
     - `tables/table_pathB_glmm_acc_fixef.csv`
     - `tables/table_pathB_glmm_acc_anova_type3.csv`
     - `tables/table_pathB_glmm_acc_emmeans.csv`
     - `tables/table_pathB_glmm_acc_switch_cost_emm.csv`

8. `R/08_pathB_report_tables.R`  
   - Produces report-friendly switch-cost tables:
     - **RT:** back-transforms log-scale switch cost to **RT ratio** and **percent change**
     - **Accuracy:** provides switch-cost on response scale (**Δ probability**) and link scale (**Δ logit**, **OR**)
   - Exports:
     - `tables/table_pathB_rt_switchcost_report.csv`
     - `tables/table_pathB_acc_switchcost_report.csv`

**Note on saved models:**  
Path B scripts may store model objects in:
- `data/processed/pathB_models.rds`  
This allows downstream scripts (e.g., report-table generation) to run without refitting models.

## Software
- R version: R 3.6.0+ (recommended: 4.x)
- Key packages: tidyverse (dplyr, tidyr), ggplot2, afex, lme4, emmeans, car, purrr

## Notes
- RT analyses use **log-transformed RT** and typically restrict RT to a plausible range (e.g., 200–3000 ms).
- Accuracy uses `correct = 1 - accuracy` where `accuracy` is coded as 0=correct and 1=error (as in the provided data).
- Path B uses mixed-effects random effects with `||` (uncorrelated slopes) to improve convergence.
