# Age auditory attention — analysis (Path A: aggregated + ANOVA)

This repository documents analyses for the “Age auditory attention” dataset.

It currently includes **Path A**: aggregate trials to subject-by-condition cells, run repeated-measures ANOVA, compute switch costs, and export figures and tables.  
**Path B** (trial-level mixed-effects models) will be added later.

## Data source
The dataset is publicly available on OSF (public data; safe to share):
- OSF link: https://www.psycharchives.org/en/item/6fd4f6d0-bfab-492e-b762-15825603125d

## Repository structure
- `R/` : R scripts (analysis pipeline)
- `data/raw/` : raw input CSVs (from OSF or provided exports)
- `data/processed/` : processed objects created by scripts (e.g., `.rds`)
- `figures/` : exported figures (PNG)
- `tables/` : exported tables (CSV)
- `reports/` : rendered reports (Quarto/PDF/HTML), if generated
- `docs/` : website output for GitHub Pages
- `_quarto.yml`, `index.qmd` : Quarto project files

## How to reproduce (Path A)
Open RStudio in the project root and run these scripts in order:

1. `R/01_data_prep.R`  
   - Reads raw CSVs  
   - Creates analysis-ready objects (RT + accuracy)  
   - Saves `data/processed/pathA_objects.rds`

2. `R/02_pathA_anova_rt.R`  
   - Aggregates RT to subject × condition cells  
   - Runs repeated-measures ANOVA on mean log(RT)  
   - Exports: `figures/fig_rt_means.png`, `tables/table_anova_rt.csv`, `tables/table_rt_summary_cells.csv`

3. `R/03_pathA_switchcost_rt.R`  
   - Computes RT switch cost (switch − repeat) from cell means  
   - Exports: `figures/fig_rt_switch_cost.png`, `tables/table_rt_switch_cost_summary.csv`, `tables/table_rt_switch_cost_tests.csv`

4. `R/04_pathA_anova_acc.R`  
   - Aggregates accuracy to subject × condition cells (mean correct rate)  
   - Runs repeated-measures ANOVA  
   - Exports: `figures/fig_acc_means.png`, `tables/table_anova_acc.csv`, `tables/table_acc_summary_cells.csv`

5. `R/05_pathA_switchcost_acc.R`  
   - Computes accuracy switch cost (switch − repeat) from cell means  
   - Exports: `figures/fig_acc_switch_cost.png`, `tables/table_acc_switch_cost_summary.csv`, `tables/table_acc_switch_cost_tests.csv`

## Outputs
Typical outputs produced by the pipeline:
- Figures: `figures/fig_*.png`
- Tables: `tables/table_*.csv`
- Processed objects: `data/processed/pathA_objects.rds`

## Software
- R version: R 3.6.0+
- Key packages: tidyverse (dplyr/tidyr/ggplot2), afex, emmeans, purrr

## Notes
- RT analyses use log-transformed RT (`log_RT`).
- Accuracy is analyzed as mean correct rate in Path A (aggregated); trial-level modeling will be implemented in Path B.
