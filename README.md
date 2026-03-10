# Replication Code: Alternative News Use Beyond (Dis)trust

Replication materials for:

> *"Alternative news use beyond (dis)trust – linking alternative news orientation to citizens' struggles for recognition"*
> Danish survey, N = 1,892

---

## Requirements

**R** (≥ 4.2). Install dependencies:

```r
install.packages(c(
  "haven",      # read SPSS (.sav) files
  "dplyr",
  "purrr",
  "sjmisc",
  "psych",      # EFA, reliability (alpha), KMO, Bartlett
  "MASS",
  "car",        # VIF
  "ggplot2",
  "gridExtra",
  "sandwich",   # HC3 heteroskedasticity-robust SEs
  "lmtest",     # coeftest for robust inference
  "boot",       # nonparametric bootstrap CIs (mediation)
  "writexl",
  "officer",    # Word appendices
  "flextable"
))
```

---

## Data

Place the survey data file in `data/`:

```
data/RUC_Main_Final.sav
```

The data are available from the authors on reasonable request (survey respondents were recruited via a commercial panel; redistribution is not permitted).

---

## Reproducing all paper outputs

Run from the project root:

```r
source("scripts/00_run_paper_analysis.R")
```

This executes the full pipeline in order and writes all outputs to `outputs/`.

### Pipeline steps

| Step | Script | Produces |
|------|--------|---------|
| 1 | `data_preparation.R` | Cleaned data objects used by all downstream scripts |
| 2 | `descriptive_statistics.R` | Weighted frequency tables |
| 3 | `reg_h1_msm_skepticism.R` | H1 – Mainstream media skepticism regressions |
| 4 | `reg_h2_altnews_seeking.R` | H2 – Alternative news seeking regressions |
| 5 | `reg_h3_ugt_gratifications.R` | H3 – UGT gratifications (info monitoring, identity) |
| 6 | `reg_h4_mediation.R` | H4 – Mediation via political and system trust |
| 7 | `create_figures.R` | Figures 1–4 (coefficient plots) |
| 8 | `extract_descriptive_numbers.R` | Inline descriptive numbers for paper text |
| 9 | `discriminant_validity.R` | Recognition vs trust discriminant validity |
| 10 | `create_descriptive_tables.R` | Tables 1–4 |
| 11 | `create_appendix_descriptives_only.R` | Table 3 (alt news by quartiles) |
| 12 | `create_appendices.R` | Appendices A (factor analysis), B (diagnostics), C (mediation) |

Scripts can also be run individually; each sources `data_preparation.R` as its first step.

---

## Output structure

```
outputs/
├── reg_mainstreamnews_nonrecog/   H1: models (.rds), tables (.xlsx/.csv), plots
├── reg_altnews_nonrecog/          H2: models, tables, plots
├── reg_user/                      H3: models, tables, plots
├── analysis/
│   ├── streamlined_results/       H4: mediation results, forest plot
│   ├── combined_plots/            Figures 1–4
│   └── descriptive_tables/
├── appendices/                    Appendices A, B, C
├── discriminant_validity/
├── frequencies/
└── DESCRIPTIVE_PLACEHOLDERS_FILLED.md
```

---

## Mediation analysis (H4)

The mediation script (`reg_h4_mediation.R`) uses **nonparametric percentile bootstrap confidence intervals** (1,000 resamples) via the `boot` package (Davison & Hinkley, 1997; Canty & Ripley, 2024). Significance is determined by whether the 95% bootstrap CI excludes zero. This replaces the Sobel test as the current methodological standard.

**Design:** 4 misrecognition dimensions × 2 trust mediators (political trust, system trust) × 1 outcome (alternative news orientation factor) = 8 pathways. All regressions use HC3 heteroskedasticity-robust standard errors and survey weights.

---

## Hypotheses

| | Hypothesis |
|--|------------|
| **H1** | Misrecognition predicts mainstream media skepticism |
| **H2** | Misrecognition predicts alternative news seeking |
| **H3** | Misrecognition predicts UGT gratifications (info monitoring, identity confirmation) |
| **H4** | Political and system trust mediate the misrecognition → alternative news relationship |
