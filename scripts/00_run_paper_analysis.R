# ---- PAPER ANALYSIS PIPELINE ----
# Runs all scripts that produce outputs used in the paper:
# "Alternative news use beyond (dis)trust – linking alternative news orientation
# to citizens' struggles for recognition"
#
# Paper uses: H1 (MSM skepticism), H2 (alt news seeking), H3 (UGT gratifications),
# H4 (mediation via trust), Appendices A/B/C, Figures 1-4, Tables 1-4
#
# Run from project root: source("scripts/00_run_paper_analysis.R")

cat("=== PAPER ANALYSIS PIPELINE ===\n")
cat("Alternative news orientation & recognition study\n\n")

if (!dir.exists("outputs")) {
  stop("Please run from project root directory")
}

# ---- 1. DATA PREPARATION ----
cat("1. Data preparation...\n")
source("scripts/data_prep.R")
cat("   ✓ Done\n\n")

# ---- 2. DESCRIPTIVE STATISTICS ----
cat("2. Descriptive statistics...\n")
source("scripts/descriptive_statistics.R")
cat("   ✓ Done\n\n")

# ---- 3. REGRESSION ANALYSES (H1, H2, H3) ----
cat("3. Regression analyses...\n")
cat("   - H1: Mainstream media skepticism\n")
source("scripts/reg_h1_msm_skepticism.R")
cat("   - H2: Alternative news seeking\n")
source("scripts/reg_h2_altnews_seeking.R")
cat("   - H3: UGT gratifications – info monitoring & identity\n")
source("scripts/reg_h3_ugt_gratifications.R")
cat("   ✓ Done\n\n")

# ---- 4. MEDIATION ANALYSIS (H4) ----
cat("4. Mediation analysis (H4)...\n")
source("scripts/reg_h4_mediation.R")
cat("   ✓ Done\n\n")

# ---- 5. FIGURES ----
cat("5. Coefficient plots (Figures 1-4)...\n")
source("scripts/create_figures.R")
cat("   ✓ Done\n\n")

# ---- 6. DESCRIPTIVE NUMBERS ----
cat("6. Descriptive numbers for paper text...\n")
source("scripts/extract_descriptive_numbers.R")
cat("   ✓ Done\n\n")

# ---- 7. DISCRIMINANT VALIDITY ----
cat("7. Discriminant validity (recognition vs trust)...\n")
source("scripts/discriminant_validity.R")
cat("   ✓ Done\n\n")

# ---- 8. DESCRIPTIVE TABLES ----
cat("8. Descriptive tables...\n")
source("scripts/create_descriptive_tables.R")
cat("   ✓ Done\n\n")

# ---- 8b. TABLE 3 (quartiles: trust + non-recognition + disrespect) ----
cat("8b. Table 3 (alt news by quartiles)...\n")
source("scripts/create_appendix_descriptives_only.R")
cat("   ✓ Done\n\n")

# ---- 9. APPENDICES ----
cat("9. Appendices (A, B, C)...\n")
source("scripts/create_appendices.R")
cat("   ✓ Done\n\n")

cat("=== PIPELINE COMPLETE ===\n")
cat("Outputs:\n")
cat("  - outputs/reg_mainstreamnews_nonrecog/  (H1 models, tables, plots)\n")
cat("  - outputs/reg_altnews_nonrecog/         (H2 models, tables, plots)\n")
cat("  - outputs/reg_user/                     (H3 models, tables, plots)\n")
cat("  - outputs/analysis/streamlined_results/ (H4 mediation)\n")
cat("  - outputs/analysis/combined_plots/      (Figures 1-4)\n")
cat("  - outputs/appendices/                   (Appendices A, B, C)\n")
cat("  - outputs/DESCRIPTIVE_PLACEHOLDERS_FILLED.md\n")
cat("  - outputs/discriminant_validity/\n")
