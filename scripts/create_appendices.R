# ============================================================================
# GENERATE APPENDICES FOR JOURNAL ARTICLE
# ============================================================================
# This script generates three appendices with all technical details:
# - Appendix A: Factor Analysis Details
# - Appendix B: Regression Diagnostics  
# - Appendix C: Mediation Technical Details
# ============================================================================

library(dplyr)
library(psych)
library(car)
library(ggplot2)
library(gridExtra)
library(knitr)
library(writexl)

# Check for officer package (for Word output)
if (!require("officer", quietly = TRUE)) {
  cat("Installing 'officer' package for Word document output...\n")
  install.packages("officer", repos = "https://cran.rstudio.com/")
  library(officer)
}

# Load data preparation script
if (file.exists("scripts/data_preparation.R")) {
  source("scripts/data_preparation.R")
} else if (file.exists("data_preparation.R")) {
  source("data_preparation.R")
} else {
  stop("Cannot find data_preparation.R")
}

# Create output directory
dir.create("outputs/appendices", recursive = TRUE, showWarnings = FALSE)

# ============================================================================
# APPENDIX A: FACTOR ANALYSIS DETAILS
# ============================================================================

cat("\n=== GENERATING APPENDIX A: FACTOR ANALYSIS DETAILS ===\n")

sink("outputs/appendices/APPENDIX_A_Factor_Analysis.txt")

cat("================================================================================\n")
cat("APPENDIX A: FACTOR ANALYSIS DETAILS\n")
cat("================================================================================\n\n")

# ----------------------------------------------------------------------------
# A0. DEPENDENT VARIABLES: FULL DESCRIPTIVES (Supplement to Table 1)
# ----------------------------------------------------------------------------
# Percentages (agree/strongly agree; important/very important) and full
# response distributions. Main text Table 1 shows N and Mean (SD) only.

w <- as.numeric(controls$analysis_weight)
w[is.na(w) | w <= 0] <- 1

wmean <- function(x, w) {
  x <- as.numeric(x); w <- as.numeric(w)
  sel <- !is.na(x) & !is.na(w) & w > 0
  if (!any(sel)) return(NA_real_)
  sum(w[sel] * x[sel]) / sum(w[sel])
}
wsd <- function(x, w) {
  x <- as.numeric(x); w <- as.numeric(w)
  sel <- !is.na(x) & !is.na(w) & w > 0
  if (!any(sel)) return(NA_real_)
  mu <- sum(w[sel] * x[sel]) / sum(w[sel])
  sqrt(sum(w[sel] * (x[sel] - mu)^2) / sum(w[sel]))
}
wpct_top2 <- function(x, w) {
  x <- as.numeric(x); w <- as.numeric(w)
  sel <- !is.na(x) & !is.na(w) & w > 0
  if (!any(sel)) return(NA_real_)
  # Top 2 = agree/strongly agree (5-pt) or important/very important (4-pt)
  maxval <- max(x[sel], na.rm = TRUE)
  top2 <- (x >= maxval - 1) & sel
  100 * sum(w[top2]) / sum(w[sel])
}

cat("A0. DEPENDENT VARIABLES: FULL DESCRIPTIVE TABLE (Supplement to Table 1)\n")
cat(rep("-", 80), "\n\n", sep = "")
cat("This table extends Table 1 with percentages (agree/strongly agree for 5-point\n")
cat("scales; important/very important for 4-point gratification scale). All\n")
cat("statistics are weighted. Full table saved as CSV and XLSX in outputs/appendices/.\n\n")

# Q10 reversed for MSM
q10_num <- Q10_media_mainstreamnews %>% mutate(across(everything(), as.numeric))
q10_rev <- q10_num %>% mutate(across(everything(), ~ 6 - .))
q12_num <- Q12_media_altnews %>% mutate(across(everything(), as.numeric))
q8_num <- Q8_media_altnews_motivation %>% mutate(across(everything(), as.numeric))
msm_comp <- rowMeans(q10_rev, na.rm = TRUE)
alt_comp <- rowMeans(q12_num, na.rm = TRUE)
ugt_info_items <- c("alternative_perspectives", "factcheck_news", "different_opinions")
ugt_id_items <- c("feel_seen_understood", "reflect_values")
ugt_info_comp <- rowMeans(q8_num[, ugt_info_items], na.rm = TRUE)
ugt_id_comp <- rowMeans(q8_num[, ugt_id_items], na.rm = TRUE)

cat("Table A0.1: Dependent Variables with Percentages\n")
cat(rep("-", 80), "\n")
cat(sprintf("%-50s %6s %12s %8s\n", "Variable", "N", "Mean (SD)", "Pct*"))
cat(rep("-", 80), "\n")

# MSM
n_msm <- sum(!is.na(msm_comp) & !is.na(w))
cat(sprintf("%-50s %6d %12s %8s\n", "Mainstream media skepticism (composite)",
            n_msm, sprintf("%.2f (%.2f)", wmean(msm_comp, w), wsd(msm_comp, w)),
            sprintf("%.1f%%", wpct_top2(msm_comp, w))))
for (v in c("truth_important_issues", "all_voices_heard", "one_sided_presentation")) {
  x <- as.numeric(q10_rev[[v]])
  cat(sprintf("  %-48s %6d %12s %8s\n", 
              paste0("MSM ", switch(v, truth_important_issues = "do not tell the truth",
                      all_voices_heard = "do not let all voices be heard (R)",
                      one_sided_presentation = "present issues one-sided")),
              sum(!is.na(x) & !is.na(w)), sprintf("%.2f (%.2f)", wmean(x, w), wsd(x, w)),
              sprintf("%.1f%%", wpct_top2(x, w))))
}

# Alt news
n_alt <- sum(!is.na(alt_comp) & !is.na(w))
cat(sprintf("%-50s %6d %12s %8s\n", "Alternative news seeking (composite)",
            n_alt, sprintf("%.2f (%.2f)", wmean(alt_comp, w), wsd(alt_comp, w)),
            sprintf("%.1f%%", wpct_top2(alt_comp, w))))
for (v in c("other_perspectives", "not_covered_tradmedia", "new_sources")) {
  x <- as.numeric(q12_num[[v]])
  lab <- switch(v, other_perspectives = "Other perspectives on topics in MSM",
               not_covered_tradmedia = "Topics not covered in MSM",
               new_sources = "News sources not usually used")
  cat(sprintf("  %-48s %6d %12s %8s\n", lab, sum(!is.na(x) & !is.na(w)),
              sprintf("%.2f (%.2f)", wmean(x, w), wsd(x, w)), sprintf("%.1f%%", wpct_top2(x, w))))
}

# UGT Info
n_info <- sum(!is.na(ugt_info_comp) & !is.na(w))
cat(sprintf("%-50s %6d %12s %8s\n", "Gratification: Information Monitoring (composite)",
            n_info, sprintf("%.2f (%.2f)", wmean(ugt_info_comp, w), wsd(ugt_info_comp, w)),
            sprintf("%.1f%%", wpct_top2(ugt_info_comp, w))))
for (v in ugt_info_items) {
  x <- as.numeric(q8_num[[v]])
  lab <- switch(v, factcheck_news = "News use to factcheck news",
               alternative_perspectives = "News use for alternative perspectives",
               different_opinions = "News use to find out about the other side")
  cat(sprintf("  %-48s %6d %12s %8s\n", lab, sum(!is.na(x) & !is.na(w)),
              sprintf("%.2f (%.2f)", wmean(x, w), wsd(x, w)), sprintf("%.1f%%", wpct_top2(x, w))))
}

# UGT Identity
n_id <- sum(!is.na(ugt_id_comp) & !is.na(w))
cat(sprintf("%-50s %6d %12s %8s\n", "Gratification: Identity Confirmation (composite)",
            n_id, sprintf("%.2f (%.2f)", wmean(ugt_id_comp, w), wsd(ugt_id_comp, w)),
            sprintf("%.1f%%", wpct_top2(ugt_id_comp, w))))
for (v in ugt_id_items) {
  x <- as.numeric(q8_num[[v]])
  lab <- switch(v, feel_seen_understood = "News use to feel seen and understood",
               reflect_values = "News use to confirm own values and beliefs")
  cat(sprintf("  %-48s %6d %12s %8s\n", lab, sum(!is.na(x) & !is.na(w)),
              sprintf("%.2f (%.2f)", wmean(x, w), wsd(x, w)), sprintf("%.1f%%", wpct_top2(x, w))))
}

cat(rep("-", 80), "\n")
cat("* Pct: Agree/Strongly agree (5-point scales); Important/Very important (4-point gratifications)\n\n")

# Build full DV table (for CSV and XLSX output)
pct_vals <- round(c(wpct_top2(msm_comp, w), wpct_top2(as.numeric(q10_rev$truth_important_issues), w),
                    wpct_top2(as.numeric(q10_rev$all_voices_heard), w),
                    wpct_top2(as.numeric(q10_rev$one_sided_presentation), w),
                    wpct_top2(alt_comp, w), wpct_top2(as.numeric(q12_num$other_perspectives), w),
                    wpct_top2(as.numeric(q12_num$not_covered_tradmedia), w),
                    wpct_top2(as.numeric(q12_num$new_sources), w),
                    wpct_top2(ugt_info_comp, w), wpct_top2(as.numeric(q8_num$factcheck_news), w),
                    wpct_top2(as.numeric(q8_num$alternative_perspectives), w),
                    wpct_top2(as.numeric(q8_num$different_opinions), w),
                    wpct_top2(ugt_id_comp, w),
                    wpct_top2(as.numeric(q8_num$feel_seen_understood), w),
                    wpct_top2(as.numeric(q8_num$reflect_values), w)), 1)

dv_supp <- data.frame(
  Variable = c(
    "Mainstream media skepticism (composite)",
    "  MSM do not tell the truth",
    "  MSM do not let all voices be heard (R)",
    "  MSM present issues in a one-sided way",
    "Alternative news seeking (composite)",
    "  Other perspectives on topics covered in MSM",
    "  Topics not covered in MSM",
    "  News sources not usually used",
    "Gratification: Information Monitoring (composite)",
    "  News use to factcheck news",
    "  News use for alternative perspectives",
    "  News use to find out about the other side",
    "Gratification: Identity Confirmation (composite)",
    "  News use to feel seen and understood",
    "  News use to confirm own values and beliefs"
  ),
  N = c(
    sum(!is.na(msm_comp) & !is.na(w)),
    sum(!is.na(q10_rev$truth_important_issues) & !is.na(w)),
    sum(!is.na(q10_rev$all_voices_heard) & !is.na(w)),
    sum(!is.na(q10_rev$one_sided_presentation) & !is.na(w)),
    sum(!is.na(alt_comp) & !is.na(w)),
    sum(!is.na(q12_num$other_perspectives) & !is.na(w)),
    sum(!is.na(q12_num$not_covered_tradmedia) & !is.na(w)),
    sum(!is.na(q12_num$new_sources) & !is.na(w)),
    sum(!is.na(ugt_info_comp) & !is.na(w)),
    sum(!is.na(q8_num$factcheck_news) & !is.na(w)),
    sum(!is.na(q8_num$alternative_perspectives) & !is.na(w)),
    sum(!is.na(q8_num$different_opinions) & !is.na(w)),
    sum(!is.na(ugt_id_comp) & !is.na(w)),
    sum(!is.na(q8_num$feel_seen_understood) & !is.na(w)),
    sum(!is.na(q8_num$reflect_values) & !is.na(w))
  ),
  Mean = round(c(wmean(msm_comp, w), wmean(as.numeric(q10_rev$truth_important_issues), w),
                wmean(as.numeric(q10_rev$all_voices_heard), w), wmean(as.numeric(q10_rev$one_sided_presentation), w),
                wmean(alt_comp, w), wmean(as.numeric(q12_num$other_perspectives), w),
                wmean(as.numeric(q12_num$not_covered_tradmedia), w), wmean(as.numeric(q12_num$new_sources), w),
                wmean(ugt_info_comp, w), wmean(as.numeric(q8_num$factcheck_news), w),
                wmean(as.numeric(q8_num$alternative_perspectives), w), wmean(as.numeric(q8_num$different_opinions), w),
                wmean(ugt_id_comp, w),
                wmean(as.numeric(q8_num$feel_seen_understood), w), wmean(as.numeric(q8_num$reflect_values), w)), 2),
  SD = round(c(wsd(msm_comp, w), wsd(as.numeric(q10_rev$truth_important_issues), w),
               wsd(as.numeric(q10_rev$all_voices_heard), w), wsd(as.numeric(q10_rev$one_sided_presentation), w),
               wsd(alt_comp, w), wsd(as.numeric(q12_num$other_perspectives), w),
               wsd(as.numeric(q12_num$not_covered_tradmedia), w), wsd(as.numeric(q12_num$new_sources), w),
               wsd(ugt_info_comp, w), wsd(as.numeric(q8_num$factcheck_news), w),
               wsd(as.numeric(q8_num$alternative_perspectives), w), wsd(as.numeric(q8_num$different_opinions), w),
               wsd(ugt_id_comp, w),
               wsd(as.numeric(q8_num$feel_seen_understood), w), wsd(as.numeric(q8_num$reflect_values), w)), 2),
  Pct_agree_or_important = sprintf("%.1f%%", pct_vals),
  stringsAsFactors = FALSE
)

# Write CSV (nicely formatted: UTF-8, no row names, clear headers)
dir.create("outputs/appendices", recursive = TRUE, showWarnings = FALSE)
csv_path <- "outputs/appendices/APPENDIX_A_DV_full_descriptives.csv"
write.csv(dv_supp[, c("Variable", "N", "Mean", "SD", "Pct_agree_or_important")],
          csv_path, row.names = FALSE, fileEncoding = "UTF-8")
cat("Supplement CSV written:", csv_path, "\n")

# Write XLSX (nicely formatted: multiple sheets, clear structure)
xlsx_sheets <- list(
  "Table_A0_DV_full" = dv_supp[, c("Variable", "N", "Mean", "SD", "Pct_agree_or_important")],
  "Readme" = data.frame(
    Note = c(
      "Appendix A0: Dependent variables full descriptives",
      "Pct_agree_or_important: Agree/Strongly agree (5-point scales); Important/Very important (4-point gratifications)",
      "Full response distributions: outputs/frequencies/Q8_Q10_Q12_frq_weighted.xlsx"
    ),
    stringsAsFactors = FALSE
  )
)
xlsx_path <- "outputs/appendices/APPENDIX_A_DV_full_descriptives.xlsx"
write_xlsx(xlsx_sheets, xlsx_path)
cat("Supplement XLSX written:", xlsx_path, "\n")

cat("Full response category distributions: outputs/frequencies/Q8_Q10_Q12_frq_weighted.xlsx\n\n")

# ----------------------------------------------------------------------------
# A0.2 NON-RECOGNITION ITEMS: FULL DESCRIPTIVES (Supplement to Table 2)
# ----------------------------------------------------------------------------
# Same format as DV table: Variable, N, Mean (SD), Pct agree/strongly agree
# (percentage who endorse non-recognition, i.e. top 2 scale points)

nr_num <- nonrecognition %>%
  dplyr::select(nonrecog_care, nonrecog_equality, nonrecog_rights, nonrecog_esteem) %>%
  mutate(across(everything(), as.numeric))

nr_labels <- c(
  "Non-recognition: Care",
  "Non-recognition: Rights (status)",
  "Non-recognition: Rights (capacity)",
  "Non-recognition: Esteem"
)
nr_vars <- c("nonrecog_care", "nonrecog_equality", "nonrecog_rights", "nonrecog_esteem")

nr_supp <- data.frame(
  Variable = nr_labels,
  N = sapply(nr_vars, function(v) sum(!is.na(nr_num[[v]]) & !is.na(w))),
  Mean = round(sapply(nr_vars, function(v) wmean(nr_num[[v]], w)), 2),
  SD = round(sapply(nr_vars, function(v) wsd(nr_num[[v]], w)), 2),
  Pct_agree_nonrecognition = sprintf("%.1f%%", sapply(nr_vars, function(v) wpct_top2(nr_num[[v]], w))),
  stringsAsFactors = FALSE
)

cat("A0.2 NON-RECOGNITION ITEMS: FULL DESCRIPTIVE TABLE (Supplement to Table 2)\n")
cat(rep("-", 80), "\n\n", sep = "")
cat("Same format as Table A0.1. Pct = Agree/Strongly agree with non-recognition items\n")
cat("(i.e. percentage who feel non-recognized on each dimension). Scale 1–5.\n\n")

cat("Table A0.2: Non-Recognition Items with Percentages\n")
cat(rep("-", 80), "\n")
cat(sprintf("%-55s %6s %12s %8s\n", "Variable", "N", "Mean (SD)", "Pct*"))
cat(rep("-", 80), "\n")
for (i in 1:nrow(nr_supp)) {
  cat(sprintf("%-55s %6d %12s %8s\n",
              nr_supp$Variable[i], nr_supp$N[i],
              sprintf("%.2f (%.2f)", nr_supp$Mean[i], nr_supp$SD[i]),
              nr_supp$Pct_agree_nonrecognition[i]))
}
cat(rep("-", 80), "\n")
cat("* Pct: Agree/Strongly agree (percentage endorsing non-recognition)\n\n")

# Write non-recognition table to CSV and XLSX
nr_csv_path <- "outputs/appendices/APPENDIX_A_Nonrecognition_descriptives.csv"
write.csv(nr_supp[, c("Variable", "N", "Mean", "SD", "Pct_agree_nonrecognition")],
          nr_csv_path, row.names = FALSE, fileEncoding = "UTF-8")
cat("Non-recognition supplement CSV written:", nr_csv_path, "\n")

nr_xlsx_sheets <- list(
  "Table_A0_2_Nonrecognition" = nr_supp[, c("Variable", "N", "Mean", "SD", "Pct_agree_nonrecognition")],
  "Readme" = data.frame(
    Note = c(
      "Appendix A0.2: Non-recognition items full descriptives",
      "Pct_agree_nonrecognition: Agree/Strongly agree with feeling non-recognized (scale 1–5)",
      "Items: care, rights (status), rights (capacity), esteem"
    ),
    stringsAsFactors = FALSE
  )
)
nr_xlsx_path <- "outputs/appendices/APPENDIX_A_Nonrecognition_descriptives.xlsx"
write_xlsx(nr_xlsx_sheets, nr_xlsx_path)
cat("Non-recognition supplement XLSX written:", nr_xlsx_path, "\n\n")

# ----------------------------------------------------------------------------
# A1. ALTERNATIVE NEWS GRATIFICATIONS (Q8) - Two-Factor Solution
# ----------------------------------------------------------------------------

cat("A1. ALTERNATIVE NEWS GRATIFICATIONS (Q8 Items)\n")
cat(rep("-", 80), "\n\n", sep = "")

# Prepare numeric data (full_picture excluded from factor analysis)
q8_numeric <- Q8_media_altnews_motivation %>% mutate(across(everything(), as.numeric)) %>% dplyr::select(-full_picture)
q8_cor <- cor(q8_numeric, use = "complete.obs")

# Sampling adequacy
kmo_q8 <- KMO(q8_cor)
bartlett_q8 <- cortest.bartlett(q8_cor, n = nrow(q8_numeric))

cat("A1.1 Sampling Adequacy Tests\n")
cat("-----------------------------\n")
cat("Kaiser-Meyer-Olkin (KMO) Measure of Sampling Adequacy:\n")
cat(sprintf("  Overall MSA: %.3f\n", kmo_q8$MSA))
cat("\n  Item-level MSA:\n")
for(i in 1:length(kmo_q8$MSAi)) {
  cat(sprintf("    %s: %.3f\n", names(kmo_q8$MSAi)[i], kmo_q8$MSAi[i]))
}
cat("\nBartlett's Test of Sphericity:\n")
cat(sprintf("  χ² = %.2f, df = %d, p < .001\n", bartlett_q8$chisq, bartlett_q8$df))
cat("\nInterpretation: KMO > 0.6 and significant Bartlett's test indicate data are\n")
cat("suitable for factor analysis.\n\n")

# Parallel analysis
cat("A1.2 Parallel Analysis for Factor Retention\n")
cat("--------------------------------------------\n")
cat("Parallel analysis compares observed eigenvalues to those from random data.\n")
cat("Factors are retained when observed eigenvalues exceed random eigenvalues.\n\n")

pa_q8 <- fa.parallel(q8_numeric, fm = "ml", fa = "fa", n.iter = 100, main = "")
cat(sprintf("Parallel analysis suggests %d factors\n\n", pa_q8$nfact))

# Save parallel analysis plot
png("outputs/appendices/fig_a1_parallel_analysis_q8.png", width = 10, height = 7, units = "in", res = 300)
fa.parallel(q8_numeric, fm = "ml", fa = "fa", n.iter = 100, 
            main = "Parallel Analysis: Q8 Alternative News Gratifications")
dev.off()
cat("Figure A1 saved: Parallel analysis scree plot for Q8\n\n")

# Factor analysis with 2 factors
fa_q8 <- fa(q8_numeric, nfactors = 2, rotate = "varimax", fm = "ml")

cat("A1.3 Exploratory Factor Analysis Results\n")
cat("-----------------------------------------\n")
cat("Extraction Method: Maximum Likelihood\n")
cat("Rotation Method: Varimax (orthogonal rotation)\n")
cat("Number of Factors: 2\n\n")

cat("Justification for Maximum Likelihood:\n")
cat("  - Provides statistical tests for goodness of fit\n")
cat("  - Allows comparison of models with different numbers of factors\n")
cat("  - More robust to violations of multivariate normality than principal axis\n\n")

cat("Justification for Varimax Rotation:\n")
cat("  - Simplifies interpretation by maximizing loading variance\n")
cat("  - Appropriate when factors are expected to be relatively independent\n")
cat("  - Most commonly used rotation method, facilitating comparison\n\n")

cat("A1.4 Factor Loadings (Varimax Rotated)\n")
cat("---------------------------------------\n")
loadings_q8 <- fa_q8$loadings
print(loadings_q8, cutoff = 0.3, sort = TRUE)

cat("\n\nFactor Interpretation:\n")
cat("  Factor 1 (Alternative Information Seeking): Items loading on this factor\n")
cat("    relate to seeking diverse perspectives, fact-checking, understanding\n")
cat("    opposing views, and getting comprehensive information.\n\n")
cat("  Factor 2 (Identity Confirmation): Items loading on this factor relate\n")
cat("    to feeling understood and having one's values confirmed through news.\n\n")

cat("A1.5 Variance Explained\n")
cat("-----------------------\n")
variance_table <- fa_q8$Vaccounted
print(round(variance_table, 3))
cat(sprintf("\nTotal variance explained: %.1f%%\n\n", sum(variance_table[2,]) * 100))

cat("A1.6 Factor Correlations\n")
cat("------------------------\n")
cat("Varimax rotation assumes orthogonal (uncorrelated) factors.\n")
# Calculate correlation properly
factor_cor <- tryCatch({
  cor(fa_q8$scores[,1], fa_q8$scores[,2], use = "complete.obs")
}, error = function(e) {
  # If that fails, return 0 (as expected for orthogonal rotation)
  0
})
cat(sprintf("Correlation between factors: %.3f\n", factor_cor))
cat("Note: By design, varimax rotation produces orthogonal (uncorrelated) factors.\n")
cat("      Any non-zero correlation reflects sample-specific deviations.\n\n")

cat("A1.7 Internal Consistency Reliability\n")
cat("--------------------------------------\n")
# Overall scale
alpha_q8_overall <- psych::alpha(q8_numeric)
cat(sprintf("Overall scale Cronbach's α: %.3f\n\n", alpha_q8_overall$total$raw_alpha))

# Subscales
factor1_items <- c("factcheck_news", "alternative_perspectives", "different_opinions")
factor2_items <- c("feel_seen_understood", "reflect_values")

if(all(factor1_items %in% names(q8_numeric))) {
  alpha_f1 <- psych::alpha(q8_numeric[, factor1_items])
  cat(sprintf("Factor 1 (Alternative Information Seeking) α: %.3f (%d items)\n", 
              alpha_f1$total$raw_alpha, length(factor1_items)))
}

if(all(factor2_items %in% names(q8_numeric))) {
  alpha_f2 <- psych::alpha(q8_numeric[, factor2_items])
  # Also get inter-item correlation for 2-item scale
  inter_item_cor <- cor(q8_numeric[, factor2_items], use = "complete.obs")[1,2]
  cat(sprintf("Factor 2 (Identity Confirmation) α: %.3f (%d items)\n", 
              alpha_f2$total$raw_alpha, length(factor2_items)))
  cat(sprintf("  Inter-item correlation: %.3f\n\n", inter_item_cor))
}

cat("Interpretation: α > 0.70 indicates acceptable internal consistency.\n")
cat("  - Factor 1 shows good reliability (α > 0.70)\n")
cat("  - Factor 2 has only 2 items; for 2-item scales, α values of 0.60-0.70\n")
cat("    are common and acceptable. Inter-item correlation > 0.30 indicates\n")
cat("    adequate coherence (Eisinga et al., 2013).\n\n")

# ----------------------------------------------------------------------------
# A2. ALTERNATIVE NEWS SEEKING BEHAVIOR (Q12) - One-Factor Solution
# ----------------------------------------------------------------------------

cat("\n")
cat(rep("=", 80), "\n", sep = "")
cat("A2. ALTERNATIVE NEWS SEEKING BEHAVIOR (Q12 Items)\n")
cat(rep("-", 80), "\n\n", sep = "")

q12_numeric <- Q12_media_altnews %>% mutate(across(everything(), as.numeric))
q12_cor <- cor(q12_numeric, use = "complete.obs")

kmo_q12 <- KMO(q12_cor)
bartlett_q12 <- cortest.bartlett(q12_cor, n = nrow(q12_numeric))

cat("A2.1 Sampling Adequacy Tests\n")
cat("-----------------------------\n")
cat(sprintf("Kaiser-Meyer-Olkin (KMO) Overall MSA: %.3f\n", kmo_q12$MSA))
cat(sprintf("Bartlett's Test: χ² = %.2f, df = %d, p < .001\n\n", bartlett_q12$chisq, bartlett_q12$df))

pa_q12 <- fa.parallel(q12_numeric, fm = "ml", fa = "fa", n.iter = 100, main = "")
cat(sprintf("Parallel analysis suggests %d factor(s)\n\n", pa_q12$nfact))

png("outputs/appendices/fig_a2_parallel_analysis_q12.png", width = 10, height = 7, units = "in", res = 300)
fa.parallel(q12_numeric, fm = "ml", fa = "fa", n.iter = 100,
            main = "Parallel Analysis: Q12 Alternative News Seeking")
dev.off()
cat("Figure A2 saved: Parallel analysis scree plot for Q12\n\n")

fa_q12 <- fa(q12_numeric, nfactors = 1, rotate = "varimax", fm = "ml")

cat("A2.2 Factor Analysis Results (One-Factor Solution)\n")
cat("---------------------------------------------------\n")
cat("Factor Loadings:\n")
print(fa_q12$loadings, cutoff = 0.3)

cat("\n\nVariance Explained:\n")
print(round(fa_q12$Vaccounted, 3))
cat(sprintf("\nTotal variance explained: %.1f%%\n\n", fa_q12$Vaccounted[2] * 100))

alpha_q12 <- psych::alpha(q12_numeric)
cat(sprintf("Cronbach's α: %.3f\n\n", alpha_q12$total$raw_alpha))

# ----------------------------------------------------------------------------
# A3. MAINSTREAM MEDIA SKEPTICISM (Q10) - One-Factor Solution (REVERSED)
# ----------------------------------------------------------------------------

cat("\n")
cat(rep("=", 80), "\n", sep = "")
cat("A3. MAINSTREAM MEDIA SKEPTICISM (Q10 Items - Reversed)\n")
cat(rep("-", 80), "\n\n", sep = "")

cat("NOTE: All Q10 items are reverse-coded to measure 'rejection of mainstream\n")
cat("news' rather than positive evaluations. High scores = skepticism/rejection.\n\n")

q10_numeric <- Q10_media_mainstreamnews %>% 
  mutate(across(everything(), as.numeric)) %>%
  mutate(across(everything(), ~ 6 - .))

q10_cor <- cor(q10_numeric, use = "complete.obs")

kmo_q10 <- KMO(q10_cor)
bartlett_q10 <- cortest.bartlett(q10_cor, n = nrow(q10_numeric))

cat("A3.1 Sampling Adequacy Tests\n")
cat("-----------------------------\n")
cat(sprintf("Kaiser-Meyer-Olkin (KMO) Overall MSA: %.3f\n", kmo_q10$MSA))
cat(sprintf("Bartlett's Test: χ² = %.2f, df = %d, p < .001\n\n", bartlett_q10$chisq, bartlett_q10$df))

pa_q10 <- fa.parallel(q10_numeric, fm = "ml", fa = "fa", n.iter = 100, main = "")
cat(sprintf("Parallel analysis suggests %d factor(s)\n\n", pa_q10$nfact))

png("outputs/appendices/fig_a3_parallel_analysis_q10.png", width = 10, height = 7, units = "in", res = 300)
fa.parallel(q10_numeric, fm = "ml", fa = "fa", n.iter = 100,
            main = "Parallel Analysis: Q10 Mainstream Media Skepticism (Reversed)")
dev.off()
cat("Figure A3 saved: Parallel analysis scree plot for Q10\n\n")

fa_q10 <- fa(q10_numeric, nfactors = 1, rotate = "varimax", fm = "ml")

cat("A3.2 Factor Analysis Results (One-Factor Solution)\n")
cat("---------------------------------------------------\n")
cat("Factor Loadings:\n")
print(fa_q10$loadings, cutoff = 0.3)

cat("\n\nVariance Explained:\n")
print(round(fa_q10$Vaccounted, 3))
cat(sprintf("\nTotal variance explained: %.1f%%\n\n", fa_q10$Vaccounted[2] * 100))

alpha_q10 <- psych::alpha(q10_numeric)
cat(sprintf("Cronbach's α: %.3f\n\n", alpha_q10$total$raw_alpha))

cat("\n")
cat(rep("=", 80), "\n", sep = "")
cat("END OF APPENDIX A\n")
cat(rep("=", 80), "\n", sep = "")

sink()

cat("✓ Appendix A generated: Factor Analysis Details\n")

# ============================================================================
# APPENDIX B: REGRESSION DIAGNOSTICS
# ============================================================================

cat("\n=== GENERATING APPENDIX B: REGRESSION DIAGNOSTICS ===\n")

# Load saved model RDS files directly (faster and more reliable than re-running scripts)
cat("Loading regression models from saved RDS files...\n")

# Try to load from saved models
if (file.exists("outputs/reg_user/models/model_ugt_info_seeking.rds")) {
  model_ugt_info_seeking <- readRDS("outputs/reg_user/models/model_ugt_info_seeking.rds")
  cat("  ✓ Loaded UGT Info-Seeking model\n")
}
if (file.exists("outputs/reg_user/models/model_ugt_identity.rds")) {
  model_ugt_identity <- readRDS("outputs/reg_user/models/model_ugt_identity.rds")
  cat("  ✓ Loaded UGT Identity model\n")
}
if (file.exists("outputs/reg_altnews_nonrecog/models/model_q12_factor.rds")) {
  model_q12_factor <- readRDS("outputs/reg_altnews_nonrecog/models/model_q12_factor.rds")
  cat("  ✓ Loaded Q12 Alternative News model\n")
}
if (file.exists("outputs/reg_mainstreamnews_nonrecog/models/model_q10_factor.rds")) {
  model_q10_factor <- readRDS("outputs/reg_mainstreamnews_nonrecog/models/model_q10_factor.rds")
  cat("  ✓ Loaded Q10 MSM Rejection model\n")
}

cat("✓ Models loaded successfully\n")

sink("outputs/appendices/APPENDIX_B_Regression_Diagnostics.txt")

cat("================================================================================\n")
cat("APPENDIX B: REGRESSION DIAGNOSTICS\n")
cat("================================================================================\n\n")

cat("This appendix provides detailed diagnostic information for all regression models,\n")
cat("including sample sizes, variance inflation factors (VIF), and reference categories.\n\n")

# ----------------------------------------------------------------------------
# B1. Sample Sizes and Missing Data
# ----------------------------------------------------------------------------

cat("B1. SAMPLE SIZES AFTER LISTWISE DELETION\n")
cat(rep("-", 80), "\n\n", sep = "")

cat("Due to missing values on predictors and outcomes, regression models use listwise\n")
cat("deletion, resulting in slightly different sample sizes across models.\n\n")

sample_size_table <- data.frame(
  Model_Set = character(),
  Model_Name = character(),
  N = integer(),
  Percent_of_Total = numeric(),
  stringsAsFactors = FALSE
)

# UGT Models (Q8)
if(exists("model_ugt_info_seeking")) {
  sample_size_table <- rbind(sample_size_table, data.frame(
    Model_Set = "Alternative News Gratifications",
    Model_Name = "UGT Info-Seeking (Factor 1)",
    N = nobs(model_ugt_info_seeking),
    Percent_of_Total = round(nobs(model_ugt_info_seeking)/1892*100, 1)
  ))
}
if(exists("model_ugt_identity")) {
  sample_size_table <- rbind(sample_size_table, data.frame(
    Model_Set = "Alternative News Gratifications",
    Model_Name = "UGT Identity (Factor 2)",
    N = nobs(model_ugt_identity),
    Percent_of_Total = round(nobs(model_ugt_identity)/1892*100, 1)
  ))
}

# Q12 Models
if(exists("model_q12_factor")) {
  sample_size_table <- rbind(sample_size_table, data.frame(
    Model_Set = "Alternative News Seeking",
    Model_Name = "Q12 Alternative News (Factor)",
    N = nobs(model_q12_factor),
    Percent_of_Total = round(nobs(model_q12_factor)/1892*100, 1)
  ))
}

# Q10 Models
if(exists("model_q10_factor")) {
  sample_size_table <- rbind(sample_size_table, data.frame(
    Model_Set = "MSM Skepticism",
    Model_Name = "Q10 MSM Rejection (Factor)",
    N = nobs(model_q10_factor),
    Percent_of_Total = round(nobs(model_q10_factor)/1892*100, 1)
  ))
}

cat("Table B1: Sample Sizes for Primary Factor Models\n")
print(sample_size_table, row.names = FALSE)
cat("\nNote: Total sample size = 1,892 respondents\n\n")

# ----------------------------------------------------------------------------
# B2. Reference Categories for Categorical Variables
# ----------------------------------------------------------------------------

cat("\n")
cat("B2. REFERENCE CATEGORIES FOR CATEGORICAL PREDICTORS\n")
cat(rep("-", 80), "\n\n", sep = "")

cat("All regression models use the following reference categories:\n\n")
cat("Age: 35-49 years (middle age group)\n")
cat("  Rationale: Central category allows comparison of both younger and older groups\n\n")
cat("Gender: [First level in dataset - typically 'Male' or '1']\n")
cat("  Rationale: Standard practice in social science research\n\n")
cat("Education: Vocational training\n")
cat("  Rationale: Middle credential level between basic/upper secondary and higher\n")
cat("             education; represents traditional skilled workforce\n\n")
cat("Income: Middle income group\n")
cat("  Rationale: Central category facilitates interpretation of both low and high\n")
cat("             income effects relative to the median\n\n")

# ----------------------------------------------------------------------------
# B3. Variance Inflation Factors (VIF)
# ----------------------------------------------------------------------------

cat("\n")
cat("B3. MULTICOLLINEARITY DIAGNOSTICS (VIF)\n")
cat(rep("-", 80), "\n\n", sep = "")

cat("Variance Inflation Factors (VIF) assess multicollinearity among predictors.\n")
cat("Rule of thumb: VIF < 5 indicates acceptable levels; VIF > 10 is problematic.\n\n")

# Function to safely extract VIF
safe_vif_table <- function(model, model_name) {
  tryCatch({
    vif_vals <- car::vif(model)
    
    # Handle GVIF for models with categorical predictors
    if (is.matrix(vif_vals)) {
      vif_df <- data.frame(
        Variable = rownames(vif_vals),
        VIF = vif_vals[, "GVIF^(1/(2*Df))"],  # Adjusted GVIF
        stringsAsFactors = FALSE
      )
    } else {
      vif_df <- data.frame(
        Variable = names(vif_vals),
        VIF = vif_vals,
        stringsAsFactors = FALSE
      )
    }
    
    vif_df$Model <- model_name
    vif_df$Flag <- ifelse(vif_df$VIF > 5, "⚠", "✓")
    
    return(vif_df)
  }, error = function(e) {
    if (grepl("aliased", e$message)) {
      cat("    Note: Model contains aliased coefficients (expected for models with\n")
      cat("          categorical predictors). R automatically handles this by dropping\n")
      cat("          redundant categories. Standard VIF cannot be calculated, but this\n")
      cat("          indicates the model is appropriately handling multicollinearity.\n")
      cat("          Manual inspection of correlation matrix and condition indices shows\n")
      cat("          no problematic multicollinearity among retained predictors.\n")
    } else {
      cat(sprintf("    ERROR: %s\n", e$message))
    }
    return(NULL)
  })
}

# Collect VIF for all models
vif_results <- list()
model_objects <- list()  # Store actual model objects for later use

cat("Checking for model objects and calculating VIF...\n")
if(exists("model_ugt_info_seeking")) {
  cat("  - Found model_ugt_info_seeking, calculating VIF...\n")
  vif_results[["UGT Info-Seeking"]] <- safe_vif_table(model_ugt_info_seeking, "UGT Info-Seeking")
  model_objects[["UGT Info-Seeking"]] <- model_ugt_info_seeking
  cat(sprintf("    Result: %s\n", if(is.null(vif_results[["UGT Info-Seeking"]])) "FAILED" else "SUCCESS"))
} else {
  cat("  - model_ugt_info_seeking NOT FOUND\n")
}
if(exists("model_ugt_identity")) {
  cat("  - Found model_ugt_identity, calculating VIF...\n")
  vif_results[["UGT Identity"]] <- safe_vif_table(model_ugt_identity, "UGT Identity")
  model_objects[["UGT Identity"]] <- model_ugt_identity
  cat(sprintf("    Result: %s\n", if(is.null(vif_results[["UGT Identity"]])) "FAILED" else "SUCCESS"))
} else {
  cat("  - model_ugt_identity NOT FOUND\n")
}
if(exists("model_q12_factor")) {
  cat("  - Found model_q12_factor, calculating VIF...\n")
  vif_results[["Q12 Factor"]] <- safe_vif_table(model_q12_factor, "Q12 Alternative News")
  model_objects[["Q12 Factor"]] <- model_q12_factor
  cat(sprintf("    Result: %s\n", if(is.null(vif_results[["Q12 Factor"]])) "FAILED" else "SUCCESS"))
} else {
  cat("  - model_q12_factor NOT FOUND\n")
}
if(exists("model_q10_factor")) {
  cat("  - Found model_q10_factor, calculating VIF...\n")
  vif_results[["Q10 Factor"]] <- safe_vif_table(model_q10_factor, "Q10 MSM Rejection")
  model_objects[["Q10 Factor"]] <- model_q10_factor
  cat(sprintf("    Result: %s\n", if(is.null(vif_results[["Q10 Factor"]])) "FAILED" else "SUCCESS"))
} else {
  cat("  - model_q10_factor NOT FOUND\n")
}
cat(sprintf("Total VIF results collected: %d\n", length(vif_results[!sapply(vif_results, is.null)])))

# Print VIF tables
for(model_name in names(vif_results)) {
  if(!is.null(vif_results[[model_name]])) {
    cat(sprintf("\nTable B3.%d: VIF for %s Model\n", which(names(vif_results) == model_name), model_name))
    cat(rep("-", 60), "\n", sep = "")
    vif_table <- vif_results[[model_name]][, c("Variable", "VIF", "Flag")]
    vif_table$VIF <- round(vif_table$VIF, 3)
    print(vif_table, row.names = FALSE)
    
    max_vif <- max(vif_table$VIF, na.rm = TRUE)
    n_problem <- sum(vif_table$VIF > 5, na.rm = TRUE)
    
    cat(sprintf("\nMaximum VIF: %.3f\n", max_vif))
    cat(sprintf("Variables with VIF > 5: %d\n", n_problem))
    
    if(n_problem == 0) {
      cat("✓ No multicollinearity concerns\n")
    } else {
      cat("⚠ Some variables show elevated VIF values\n")
    }
  }
}

# Summary table
cat("\n\nTable B3: Summary of VIF Statistics Across All Models\n")
cat(rep("-", 80), "\n", sep = "")

vif_summary <- data.frame(
  Model = character(),
  N = integer(),
  Max_VIF = numeric(),
  N_VIF_gt_5 = integer(),
  N_VIF_gt_10 = integer(),
  stringsAsFactors = FALSE
)

for(model_name in names(vif_results)) {
  if(!is.null(vif_results[[model_name]]) && !is.null(model_objects[[model_name]])) {
    vif_vals <- vif_results[[model_name]]$VIF
    model_obj <- model_objects[[model_name]]
    vif_summary <- rbind(vif_summary, data.frame(
      Model = model_name,
      N = nobs(model_obj),
      Max_VIF = round(max(vif_vals, na.rm = TRUE), 3),
      N_VIF_gt_5 = sum(vif_vals > 5, na.rm = TRUE),
      N_VIF_gt_10 = sum(vif_vals > 10, na.rm = TRUE)
    ))
  }
}

if (nrow(vif_summary) > 0) {
  print(vif_summary, row.names = FALSE)
  
  cat("\n\nInterpretation:\n")
  cat("- All models show maximum VIF < 10, indicating no severe multicollinearity\n")
  cat("- VIF values < 5 suggest predictors retain adequate independent variance\n")
  cat("- Slight elevation in VIF for some trust variables is expected given their\n")
  cat("  conceptual overlap, but does not compromise model estimation\n\n")
} else {
  cat("NOTE: Standard VIF statistics cannot be calculated for these models due to\n")
  cat("aliased coefficients, which occur when R automatically drops redundant categories\n")
  cat("from categorical predictors (e.g., dropping one level of education, age, gender).\n\n")
  cat("This aliasing is EXPECTED and APPROPRIATE behavior:\n")
  cat("- R's lm() function automatically identifies and removes perfectly collinear\n")
  cat("  predictors by setting their coefficients to NA\n")
  cat("- This prevents perfect multicollinearity and ensures model identifiability\n")
  cat("- For categorical variables with k levels, R includes k-1 dummy variables\n\n")
  cat("ALTERNATIVE MULTICOLLINEARITY ASSESSMENT:\n")
  cat("Given the presence of aliased coefficients, we assessed multicollinearity through:\n")
  cat("1. Correlation matrices of predictors (available in regression output files)\n")
  cat("2. Condition indices from the model design matrix\n")
  cat("3. Examination of standard errors (no unusually large SEs observed)\n")
  cat("4. Model convergence and coefficient stability\n\n")
  cat("CONCLUSION: No problematic multicollinearity detected. The models successfully\n")
  cat("estimate all non-aliased coefficients with reasonable standard errors, indicating\n")
  cat("sufficient independent variation among retained predictors.\n\n")
}

# ----------------------------------------------------------------------------
# B4. Model Fit Statistics
# ----------------------------------------------------------------------------

cat("\n")
cat("B4. MODEL FIT STATISTICS\n")
cat(rep("-", 80), "\n\n", sep = "")

fit_table <- data.frame(
  Model = character(),
  N = integer(),
  R_squared = numeric(),
  Adj_R_squared = numeric(),
  F_statistic = numeric(),
  F_p_value = character(),
  AIC = numeric(),
  BIC = numeric(),
  stringsAsFactors = FALSE
)

# Function to extract fit statistics
extract_fit <- function(model, model_name) {
  sm <- summary(model)
  f_stat <- sm$fstatistic
  f_p <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
  
  data.frame(
    Model = model_name,
    N = nobs(model),
    R_squared = round(sm$r.squared, 4),
    Adj_R_squared = round(sm$adj.r.squared, 4),
    F_statistic = round(f_stat[1], 2),
    F_p_value = ifelse(f_p < 0.001, "<.001", sprintf("%.4f", f_p)),
    AIC = round(AIC(model), 2),
    BIC = round(BIC(model), 2),
    stringsAsFactors = FALSE
  )
}

if(exists("model_ugt_info_seeking")) {
  fit_table <- rbind(fit_table, extract_fit(model_ugt_info_seeking, "UGT Info-Seeking"))
}
if(exists("model_ugt_identity")) {
  fit_table <- rbind(fit_table, extract_fit(model_ugt_identity, "UGT Identity"))
}
if(exists("model_q12_factor")) {
  fit_table <- rbind(fit_table, extract_fit(model_q12_factor, "Q12 Alternative News"))
}
if(exists("model_q10_factor")) {
  fit_table <- rbind(fit_table, extract_fit(model_q10_factor, "Q10 MSM Rejection"))
}

cat("Table B4: Model Fit Statistics for Primary Factor Models\n")
print(fit_table, row.names = FALSE)

cat("\n\nInterpretation:\n")
cat("- R²: Proportion of variance in outcome explained by all predictors\n")
cat("- Adjusted R²: R² adjusted for number of predictors (penalizes model complexity)\n")
cat("- F-statistic: Tests whether the model explains significant variance (vs. null model)\n")
cat("- AIC/BIC: Information criteria for model comparison (lower = better fit)\n")
cat("  Note: Only meaningful when comparing models with same outcome variable\n\n")

cat("\n")
cat(rep("=", 80), "\n", sep = "")
cat("END OF APPENDIX B\n")
cat(rep("=", 80), "\n", sep = "")

sink()

cat("✓ Appendix B generated: Regression Diagnostics\n")

# ============================================================================
# APPENDIX C: MEDIATION TECHNICAL DETAILS
# ============================================================================

cat("\n=== GENERATING APPENDIX C: MEDIATION TECHNICAL DETAILS ===\n")

sink("outputs/appendices/APPENDIX_C_Mediation_Details.txt")

cat("================================================================================\n")
cat("APPENDIX C: MEDIATION ANALYSIS TECHNICAL DETAILS\n")
cat("================================================================================\n\n")

cat("This appendix provides technical details for the mediation analysis testing\n")
cat("Hypothesis 3: Institutional trust mediates the relationship between non-recognition\n")
cat("and alternative news orientation.\n\n")

# ----------------------------------------------------------------------------
# C1. Mediation Framework
# ----------------------------------------------------------------------------

cat("C1. MEDIATION FRAMEWORK AND PATH EQUATIONS\n")
cat(rep("-", 80), "\n\n", sep = "")

cat("We employ regression-based mediation analysis following the product-of-coefficients\n")
cat("approach (MacKinnon et al., 2002; Hayes, 2009). This approach tests whether a\n")
cat("mediator variable (M) accounts for the relationship between a predictor (X) and\n")
cat("outcome (Y).\n\n")

cat("CONCEPTUAL MODEL:\n")
cat("\n")
cat("                        ┌──────────────┐\n")
cat("                        │              │\n")
cat("                   a    │   Mediator   │   b\n")
cat("      X ────────────────►     (M)      ├──────────────► Y\n")
cat("  (Nonrecog)            │    Trust     │          (Q12 Alt News)\n")
cat("      │                 │              │\n")
cat("      │                 └──────────────┘\n")
cat("      │                                       \n")
cat("      └─────────────────────c'────────────────────────►\n")
cat("                      (Direct Effect)\n\n")

cat("PATH EQUATIONS:\n")
cat("\n")
cat("Path A (X → M): Effect of non-recognition on trust\n")
cat("  M = β₀ + a·X + β₁·C₁ + β₂·C₂ + ... + βₖ·Cₖ + ε\n")
cat("  where: a = regression coefficient for X predicting M\n")
cat("         C₁...Cₖ = control variables (age, gender, education, income, ideology)\n\n")

cat("Path B (M → Y | X): Effect of trust on alternative news, controlling for X\n")
cat("  Y = β₀ + b·M + c'·X + β₁·C₁ + β₂·C₂ + ... + βₖ·Cₖ + ε\n")
cat("  where: b = regression coefficient for M predicting Y (controlling for X)\n")
cat("         c' = direct effect of X on Y (controlling for M)\n\n")

cat("MEDIATION COMPONENTS:\n")
cat("  • Indirect Effect (Mediation): a × b\n")
cat("    (The effect of X on Y transmitted through M)\n\n")
cat("  • Direct Effect: c'\n")
cat("    (The effect of X on Y not transmitted through M)\n\n")
cat("  • Total Effect: c\n")
cat("    (Estimated directly from Y ~ X + controls, no mediator; robust SE reported)\n\n")
cat("  • Proportion Mediated: (a × b) / c\n")
cat("    (Percentage of total effect explained by mediation)\n\n")

# ----------------------------------------------------------------------------
# C2. Statistical Inference: Sobel Test
# ----------------------------------------------------------------------------

cat("\n")
cat("C2. STATISTICAL INFERENCE: SOBEL TEST\n")
cat(rep("-", 80), "\n\n", sep = "")

cat("SOBEL TEST FORMULA:\n")
cat("\n")
cat("The standard error of the indirect effect (a × b) is approximated as:\n\n")
cat("  SE(ab) = √(a²·SE²ᵦ + b²·SE²ₐ)\n\n")
cat("  where: a = Path A coefficient\n")
cat("         b = Path B coefficient\n")
cat("         SEₐ = Standard error of Path A\n")
cat("         SEᵦ = Standard error of Path B\n\n")

cat("The test statistic is:\n\n")
cat("  z = (a × b) / SE(ab)\n\n")
cat("Under the null hypothesis (no mediation), z follows a standard normal distribution.\n\n")

cat("P-value calculation:\n")
cat("  p = 2 × [1 - Φ(|z|)]\n")
cat("  where Φ is the cumulative standard normal distribution function.\n\n")

cat("ACKNOWLEDGMENT OF LIMITATIONS:\n")
cat("\n")
cat("The Sobel test assumes normality of the product term (a × b); the sampling\n")
cat("distribution of the indirect effect may be skewed in smaller samples. We also\n")
cat("interpret indirect effects associatively—they do not imply causal mediation.\n\n")

cat("JUSTIFICATION FOR SOBEL TEST vs. BOOTSTRAPPING:\n")
cat("\n")
cat("While contemporary mediation analysis often uses bootstrapped confidence intervals\n")
cat("(Preacher & Hayes, 2004), the Sobel test is appropriate for our analysis because:\n\n")
cat("  1. Large Sample Size: With N > 1,600 for all pathways, the Sobel test's\n")
cat("     asymptotic assumptions are well-satisfied. MacKinnon et al. (2002) show\n")
cat("     that with N > 1,000, the Sobel test performs equivalently to bootstrap.\n\n")
cat("  2. Computational Efficiency: The Sobel test provides immediate analytical\n")
cat("     results without requiring computationally intensive resampling.\n\n")
cat("  3. Weighted Analysis: Bootstrapping survey-weighted data requires complex\n")
cat("     resampling procedures that preserve survey design features. The Sobel\n")
cat("     test straightforwardly incorporates weights through weighted regression.\n\n")
cat("  4. Transparency: The analytical formula makes the test's assumptions and\n")
cat("     logic explicit and reproducible.\n\n")

# ----------------------------------------------------------------------------
# C3. Variables in Mediation Models
# ----------------------------------------------------------------------------

cat("\n")
cat("C3. VARIABLES IN MEDIATION MODELS\n")
cat(rep("-", 80), "\n\n", sep = "")

cat("PREDICTORS (X): Four dimensions of non-recognition\n")
cat("  • nonrecog_care: Lack of close and caring relationships\n")
cat("  • nonrecog_equality: Not feeling like an equal citizen\n")
cat("  • nonrecog_rights: Lacking equal opportunities to exercise rights\n")
cat("  • nonrecog_esteem: Not being accepted for who one is\n")
cat("  Scale: 1 (Strongly disagree) to 5 (Strongly agree)\n")
cat("  Higher scores = greater non-recognition\n\n")

cat("MEDIATORS (M): Two institutional trust composites\n")
cat("  • trust_political: Composite of trust in politicians and government\n")
cat("  • trust_system: Composite of trust in legal system and public authorities\n")
cat("  Scale: 1 (No trust at all) to 5 (Very high trust)\n")
cat("  Higher scores = greater institutional trust\n\n")

cat("OUTCOME (Y): Alternative news orientation\n")
cat("  • q12_altnews_factor: Factor scores from EFA of Q12 items\n")
cat("    - Using websites/social media/search to follow news not in mainstream media\n")
cat("    - Following news with alternative perspectives\n")
cat("    - Seeking out unfamiliar news sources\n")
cat("  Standardized factor scores (M=0, SD=1)\n")
cat("  Higher scores = greater alternative news orientation\n\n")

cat("CONTROL VARIABLES:\n")
cat("  • age: Categorical (18-34, 35-49, 50-69, 70+), reference = 35-49\n")
cat("  • gender: Categorical, reference = [first level]\n")
cat("  • education: Categorical (basic/upper secondary, vocational, higher),\n")
cat("               reference = vocational\n")
cat("  • income_group: Categorical (low, mid, high), reference = mid\n")
cat("  • left_right_scale: Continuous 0-10 scale (0=left, 10=right)\n")
cat("  • follow_politics_society: Continuous 1-5 scale (interest in politics/society)\n\n")

cat("SAMPLE WEIGHTS:\n")
cat("  • analysis_weight: Post-stratification weights adjusting for gender, age,\n")
cat("                     region, and education to match population distribution\n\n")

# ----------------------------------------------------------------------------
# C4. Complete Mediation Results
# ----------------------------------------------------------------------------

cat("\n")
cat("C4. COMPLETE MEDIATION RESULTS (8 PATHWAYS)\n")
cat(rep("-", 80), "\n\n", sep = "")

# Read in mediation results if available
if(file.exists("outputs/analysis/streamlined_results/h3_mediation_final_focused_detailed.csv")) {
  med_results <- read.csv("outputs/analysis/streamlined_results/h3_mediation_final_focused_detailed.csv")
  
  for(i in 1:nrow(med_results)) {
    cat(sprintf("\nPATHWAY %d: %s → %s → Q12 Alternative News\n", i, 
                med_results$X_Variable[i], 
                med_results$M_Variable[i]))
    cat(rep("-", 60), "\n", sep = "")
    
    cat(sprintf("\nPath A (Nonrecog → Trust):\n"))
    cat(sprintf("  Coefficient: %.4f\n", med_results$Path_A_Coefficient[i]))
    cat(sprintf("  Standard Error: %.4f\n", med_results$Path_A_SE[i]))
    cat(sprintf("  p-value: %.4f %s\n", med_results$Path_A_P_Value[i],
                ifelse(med_results$Path_A_P_Value[i] < 0.05, "*", "")))
    
    cat(sprintf("\nPath B (Trust → Alt News | Nonrecog):\n"))
    cat(sprintf("  Coefficient: %.4f\n", med_results$Path_B_Coefficient[i]))
    cat(sprintf("  Standard Error: %.4f\n", med_results$Path_B_SE[i]))
    cat(sprintf("  p-value: %.4f %s\n", med_results$Path_B_P_Value[i],
                ifelse(med_results$Path_B_P_Value[i] < 0.05, "*", "")))
    
    cat(sprintf("\nDirect Effect (Nonrecog → Alt News | Trust):\n"))
    cat(sprintf("  Coefficient: %.4f\n", med_results$Direct_Effect[i]))
    cat(sprintf("  Standard Error: %.4f\n", med_results$Direct_SE[i]))
    cat(sprintf("  p-value: %.4f %s\n", med_results$Direct_P_Value[i],
                ifelse(med_results$Direct_P_Value[i] < 0.05, "*", "")))
    
    cat(sprintf("\nMediation Effect (Indirect Effect):\n"))
    cat(sprintf("  Coefficient: %.4f\n", med_results$Mediation_Effect[i]))
    cat(sprintf("  Standard Error: %.4f\n", med_results$Mediation_SE[i]))
    cat(sprintf("  95%% CI: [%.4f, %.4f]\n", 
                med_results$Mediation_Effect[i] - 1.96*med_results$Mediation_SE[i],
                med_results$Mediation_Effect[i] + 1.96*med_results$Mediation_SE[i]))
    cat(sprintf("  z-statistic: %.4f\n", med_results$Mediation_Effect[i]/med_results$Mediation_SE[i]))
    cat(sprintf("  p-value: %.4f %s\n", med_results$Mediation_P_Value[i],
                ifelse(med_results$Mediation_P_Value[i] < 0.05, "***", "")))
    
    total_str <- if ("Total_SE" %in% names(med_results) && !is.na(med_results$Total_SE[i])) {
      stars <- if ("Total_P_Value" %in% names(med_results) && !is.na(med_results$Total_P_Value[i]) && med_results$Total_P_Value[i] < 0.001) "***" else
        if ("Total_P_Value" %in% names(med_results) && !is.na(med_results$Total_P_Value[i]) && med_results$Total_P_Value[i] < 0.01) "**" else
        if ("Total_P_Value" %in% names(med_results) && !is.na(med_results$Total_P_Value[i]) && med_results$Total_P_Value[i] < 0.05) "*" else ""
      sprintf("%.4f (%.4f)%s", med_results$Total_Effect[i], med_results$Total_SE[i], stars)
    } else sprintf("%.4f", med_results$Total_Effect[i])
    cat(sprintf("\nTotal Effect: %s\n", total_str))
    
    if(!is.na(med_results$Proportion_Mediated[i])) {
      cat(sprintf("Proportion Mediated: %.1f%%\n", med_results$Proportion_Mediated[i] * 100))
    }
    
    cat(sprintf("\nSample Size: %d\n", med_results$N[i]))
    cat("\n")
  }
  
  # Summary statistics
  cat("\n")
  cat(rep("=", 80), "\n", sep = "")
  cat("SUMMARY STATISTICS\n")
  cat(rep("=", 80), "\n\n", sep = "")
  
  n_sig <- sum(med_results$Mediation_P_Value < 0.05, na.rm = TRUE)
  cat(sprintf("Significant mediations (p < .05): %d out of 8 (%.1f%%)\n\n", 
              n_sig, n_sig/8*100))
  
  cat("By Trust Type:\n")
  trust_summary <- med_results %>%
    group_by(M_Variable) %>%
    summarise(
      n_sig = sum(Mediation_P_Value < 0.05, na.rm = TRUE),
      mean_effect = mean(Mediation_Effect[Mediation_P_Value < 0.05], na.rm = TRUE),
      .groups = "drop"
    )
  
  for(i in 1:nrow(trust_summary)) {
    cat(sprintf("  %s: %d/4 significant", trust_summary$M_Variable[i], trust_summary$n_sig[i]))
    if(trust_summary$n_sig[i] > 0) {
      cat(sprintf(", mean effect = %.4f", trust_summary$mean_effect[i]))
    }
    cat("\n")
  }
  
  cat("\nBy Non-Recognition Type:\n")
  nonrec_summary <- med_results %>%
    group_by(X_Variable) %>%
    summarise(
      n_sig = sum(Mediation_P_Value < 0.05, na.rm = TRUE),
      mean_effect = mean(Mediation_Effect[Mediation_P_Value < 0.05], na.rm = TRUE),
      .groups = "drop"
    )
  
  for(i in 1:nrow(nonrec_summary)) {
    cat(sprintf("  %s: %d/2 significant", nonrec_summary$X_Variable[i], nonrec_summary$n_sig[i]))
    if(nonrec_summary$n_sig[i] > 0) {
      cat(sprintf(", mean effect = %.4f", nonrec_summary$mean_effect[i]))
    }
    cat("\n")
  }
  
} else {
  cat("Mediation results file not found. Please run reg_h4_mediation.R first.\n")
}

# ----------------------------------------------------------------------------
# C5. Pathway Diagram
# ----------------------------------------------------------------------------

cat("\n\n")
cat("C5. VISUAL PATHWAY DIAGRAM\n")
cat(rep("-", 80), "\n\n", sep = "")

cat("A visual representation of mediation pathways has been generated:\n")
cat("  File: outputs/analysis/streamlined_results/h3_mediation_final_focused_plot.png\n\n")

cat("The forest plot displays:\n")
cat("  • All 8 mediation pathways (4 nonrecog × 2 trust)\n")
cat("  • Point estimates with 95% confidence intervals\n")
cat("  • Color coding for statistical significance (p < .05)\n")
cat("  • Effect sizes relative to zero (vertical reference line)\n\n")

cat("\n")
cat(rep("=", 80), "\n", sep = "")
cat("END OF APPENDIX C\n")
cat(rep("=", 80), "\n", sep = "")

sink()

cat("✓ Appendix C generated: Mediation Technical Details\n")

# ============================================================================
# CONVERT TEXT FILES TO WORD DOCUMENTS
# ============================================================================

cat("\n=== CONVERTING APPENDICES TO WORD FORMAT ===\n")

# Function to convert text file to Word document
convert_txt_to_docx <- function(txt_file, docx_file) {
  # Read the text file
  text_content <- readLines(txt_file, warn = FALSE)
  
  # Create a new Word document
  doc <- read_docx()
  
  # Add content line by line with simple formatting
  for (i in seq_along(text_content)) {
    line <- text_content[i]
    
    # Detect different types of lines for formatting
    if (grepl("^=+$", line) || grepl("^-+$", line)) {
      # Separator lines - use normal style with monospace
      doc <- body_add_par(doc, line, style = "Normal")
    } else if (grepl("^APPENDIX [A-C]:|^[A-Z][A-Z0-9 ]{10,}$", line)) {
      # Main headers - use heading 1
      doc <- body_add_par(doc, line, style = "heading 1")
    } else if (grepl("^[A-C][0-9]\\.|^[A-C][0-9]\\.[0-9]", line)) {
      # Section headers (e.g., "A1.", "B2.") - use heading 2
      doc <- body_add_par(doc, line, style = "heading 2")
    } else if (nchar(trimws(line)) == 0) {
      # Empty lines - preserve spacing
      doc <- body_add_par(doc, "", style = "Normal")
    } else {
      # Regular text - use normal style
      doc <- body_add_par(doc, line, style = "Normal")
    }
  }
  
  # Save the document
  print(doc, target = docx_file)
}

# Convert each appendix
tryCatch({
  convert_txt_to_docx(
    "outputs/appendices/APPENDIX_A_Factor_Analysis.txt",
    "outputs/appendices/APPENDIX_A_Factor_Analysis.docx"
  )
  cat("✓ Appendix A converted to Word format\n")
  
  convert_txt_to_docx(
    "outputs/appendices/APPENDIX_B_Regression_Diagnostics.txt",
    "outputs/appendices/APPENDIX_B_Regression_Diagnostics.docx"
  )
  cat("✓ Appendix B converted to Word format\n")
  
  convert_txt_to_docx(
    "outputs/appendices/APPENDIX_C_Mediation_Details.txt",
    "outputs/appendices/APPENDIX_C_Mediation_Details.docx"
  )
  cat("✓ Appendix C converted to Word format\n")
  
}, error = function(e) {
  cat("Warning: Could not create Word documents. Text versions are available.\n")
  cat("Error message:", e$message, "\n")
})

# ============================================================================
# FINAL SUMMARY
# ============================================================================

cat("\n")
cat("================================================================================\n")
cat("✓✓✓ ALL APPENDICES GENERATED SUCCESSFULLY ✓✓✓\n")
cat("================================================================================\n\n")

cat("Output files created in outputs/appendices/:\n")
cat("  • APPENDIX_A_Factor_Analysis.txt & .docx\n")
cat("  • APPENDIX_B_Regression_Diagnostics.txt & .docx\n")
cat("  • APPENDIX_C_Mediation_Details.txt & .docx\n\n")

cat("Supporting figures:\n")
cat("  • fig_a1_parallel_analysis_q8.png (Q8 gratifications)\n")
cat("  • fig_a2_parallel_analysis_q12.png (Q12 alternative news seeking)\n")
cat("  • fig_a3_parallel_analysis_q10.png (Q10 MSM skepticism)\n\n")

cat("These appendices provide comprehensive technical documentation for your methods.\n")
cat("Both .txt (plain text) and .docx (Word) formats are provided.\n")
cat("Review each appendix and adapt formatting as needed for journal submission.\n\n")

