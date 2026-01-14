# Age auditory attention — analysis (Path A: aggregated + ANOVA)

This repository documents analyses for the “Age auditory attention” dataset.
So far, it includes **Path A** (aggregate to subject-by-condition cells, then run repeated-measures ANOVA, compute switch costs, and produce figures/tables).
**Path B** (trial-level mixed-effects models) will be added later.

## Data source
The dataset is publicly available on OSF (public data; safe to share).
- OSF link: (https://www.psycharchives.org/en/item/6fd4f6d0-bfab-492e-b762-15825603125d)

## Repository structure
- `R/` : R scripts (analysis pipeline)
- `data/raw/` : raw input CSVs (from OSF)
- `data/processed/` : processed datasets created by scripts 
- `figures/` : exported figures (PNG/PDF)
- `reports/` : rendered reports (Quarto/PDF/HTML)
- `docs/` : website output for GitHub Pages 
- `_quarto.yml`, `index.qmd` : Quarto project files

## How to reproduce (Path A)
Open RStudio in the project root and run scripts in order:

1. `R/01_import_qc.R`
   - Imports the raw data
   - Applies basic QC
   - Creates aggregated datasets (e.g., subject × condition cells)
   - Produces ANOVA tables, switch-cost summaries, and figures

Outputs created by the current pipeline (examples):
- ANOVA tables (CSV): `table_anova_accuracy*.csv`, ...
- Switch-cost summaries (CSV): `table_*switch_cost*.csv`
- Figures: `fig_*.png` and/or exported PDFs

## Software
- R version: R 3.6.0+
- Key packages: tidyverse, afex, emmeans, ggplot2, etc.

## Notes
- The analysis currently uses log-transformed RT for RT-based outcomes.
- Accuracy is analyzed as mean correct rate in Path A (aggregated); trial-level models will be in Path B.
