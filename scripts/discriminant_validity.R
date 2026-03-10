# =============================================================================
# DISCRIMINANT VALIDITY: Recognition vs. Institutional Trust
# =============================================================================
# Empirical check that recognition (Honneth-inspired) and institutional trust
# are distinct but related constructs. EFA, CFA, and correlation analysis.
# =============================================================================

options(repos = c(CRAN = "https://cloud.r-project.org"))

# Install packages if needed
pkgs <- c("haven", "dplyr", "psych", "lavaan", "semTools", "writexl")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p, dependencies = TRUE)
}

library(haven)
library(dplyr)
library(psych)
library(lavaan)
library(semTools)
library(writexl)

# ---- 1) LOAD DATA ----
source("scripts/data_prep.R")

# Create output directories
dir.create("outputs/discriminant_validity", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/discriminant_validity/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/discriminant_validity/plots", recursive = TRUE, showWarnings = FALSE)

# ---- 2) DEFINE ITEMS ----
# Recognition (Honneth-inspired): positive items + disrespect items
recog_items <- c(
  "recog_care", "recog_equality", "recog_rights", "recog_esteem", "recog_value_society",
  "disrespect_misperception", "disrespect_denigration", "disrespect_exclusion", "disrespect_discrimination"
)

# Trust (institutional): all 8 trust items
trust_items <- c(
  "trust_authorities", "trust_justice_system", "trust_politicians", "trust_government",
  "trust_news_media", "trust_eu", "trust_intl_org", "trust_citizens"
)

all_items <- c(recog_items, trust_items)

# Prepare data: recognition + trust (as numeric, from factors)
recog_df <- recognition %>% dplyr::select(all_of(recog_items)) %>% mutate(across(everything(), as.numeric))
trust_df <- trust %>% dplyr::select(all_of(trust_items)) %>% mutate(across(everything(), as.numeric))
fa_data <- bind_cols(recog_df, trust_df)

# Complete cases for factor analysis
fa_data_complete <- fa_data %>% filter(complete.cases(.))
n_complete <- nrow(fa_data_complete)
cat("Complete cases for EFA/CFA:", n_complete, "of", nrow(fa_data), "\n\n")

# Optional: use survey weights if available (for descriptive stats; EFA/CFA typically use unweighted)
weights_vec <- if (exists("controls") && "analysis_weight" %in% names(controls)) {
  suppressWarnings(as.numeric(controls$analysis_weight))
} else {
  rep(1, nrow(fa_data))
}

# =============================================================================
# TASK A: EXPLORATORY FACTOR ANALYSIS (EFA)
# =============================================================================

cat("===== TASK A: EXPLORATORY FACTOR ANALYSIS =====\n\n")

# Polychoric correlation matrix (appropriate for ordinal/Likert)
polycor_mat <- psych::polychoric(fa_data_complete)
poly_rho <- polycor_mat$rho

# A1: Parallel analysis
cat("--- Parallel Analysis (suggests number of factors) ---\n")
set.seed(12345)
pa_results <- psych::fa.parallel(poly_rho, n.obs = n_complete, fa = "fa", n.iter = 100)
pa_suggest <- pa_results$nfact
cat("Parallel analysis suggests:", pa_suggest, "factor(s)\n\n")

# A2: Scree plot / eigenvalues
evals <- eigen(poly_rho)$values
cat("--- Eigenvalues (scree) ---\n")
print(round(evals, 3))
n_scree <- sum(evals > 1)  # Kaiser criterion
cat("Kaiser criterion (eigenvalue > 1) suggests:", n_scree, "factor(s)\n\n")

# EFA with oblimin rotation (oblique; factors may correlate)
n_factors <- max(2, min(pa_suggest, n_scree, 4))  # between 2 and 4
cat("Running EFA with", n_factors, "factors, oblimin rotation, polychoric...\n\n")

fa_efa <- psych::fa(poly_rho, nfactors = n_factors, rotate = "oblimin", fm = "minres", n.obs = n_complete)

# Extract loadings
loadings_mat <- as.matrix(fa_efa$loadings[])
rownames(loadings_mat) <- all_items

# Create formatted loadings table
loadings_table <- as.data.frame(round(loadings_mat, 3))
loadings_table$Item <- rownames(loadings_table)
loadings_table$Domain <- ifelse(loadings_table$Item %in% recog_items, "Recognition", "Trust")

# Flag cross-loadings (>= .30 on more than one factor)
nf <- ncol(loadings_mat)
cross_load <- apply(abs(loadings_mat), 1, function(x) sum(x >= 0.30) > 1)
loadings_table$CrossLoading <- ifelse(cross_load, "Yes", "")

# Highlight loadings >= .40
for (j in 1:nf) {
  col_nm <- names(loadings_table)[j]
  if (is.numeric(loadings_table[[col_nm]])) {
    loadings_table[[paste0(col_nm, "_flag")]] <- ifelse(abs(loadings_table[[col_nm]]) >= 0.40, "***", "")
  }
}

# Reorder columns for output
out_cols <- c("Item", "Domain", names(loadings_table)[1:nf], "CrossLoading")
loadings_table_out <- loadings_table[, intersect(out_cols, names(loadings_table))]

write.csv(loadings_table_out, "outputs/discriminant_validity/tables/EFA_loadings_table.csv", row.names = FALSE)
# Also write with interpretable factor names
loadings_renamed <- loadings_table_out
names(loadings_renamed) <- gsub("^MR1$", "Trust", names(loadings_renamed))
names(loadings_renamed) <- gsub("^MR2$", "Disrespect", names(loadings_renamed))
names(loadings_renamed) <- gsub("^MR3$", "Recognition_positive", names(loadings_renamed))
write.csv(loadings_renamed, "outputs/discriminant_validity/tables/EFA_loadings_renamed.csv", row.names = FALSE)
cat("EFA loadings table saved.\n\n")

# Display
print(loadings_table_out)

# Factor correlations
cat("\n--- Factor correlations (oblimin) ---\n")
print(round(fa_efa$Phi, 3))

# =============================================================================
# TASK B: CONFIRMATORY FACTOR ANALYSIS (CFA)
# =============================================================================

cat("\n===== TASK B: CONFIRMATORY FACTOR ANALYSIS =====\n\n")

# For lavaan, we need the raw data with integers (or ordered)
fa_ordinal <- fa_data_complete  # numeric 1-5

# Model 1: One factor (all items)
model1_syntax <- paste0(
  "General =~ ", paste(all_items, collapse = " + ")
)

# Model 2: Two factors (Recognition + Trust), correlated
model2_syntax <- paste0(
  "Recognition =~ ", paste(recog_items, collapse = " + "), "\n",
  "Trust =~ ", paste(trust_items, collapse = " + "), "\n",
  "Recognition ~~ Trust"
)

# Model 3: Multi-factor - Recognition split into positive + disrespect; Trust split into political + system
# Recognition: recog_positive (5) + disrespect (4)
# Trust: political (2) + system (2) + other (4: news_media, eu, intl_org, citizens)
model3_syntax <- paste0(
  "Recog_Positive =~ recog_care + recog_equality + recog_rights + recog_esteem + recog_value_society\n",
  "Disrespect =~ disrespect_misperception + disrespect_denigration + disrespect_exclusion + disrespect_discrimination\n",
  "Trust_Political =~ trust_politicians + trust_government\n",
  "Trust_System =~ trust_authorities + trust_justice_system\n",
  "Trust_Other =~ trust_news_media + trust_eu + trust_intl_org + trust_citizens\n",
  "Recog_Positive ~~ Disrespect\n",
  "Recog_Positive ~~ Trust_Political\n",
  "Recog_Positive ~~ Trust_System\n",
  "Recog_Positive ~~ Trust_Other\n",
  "Disrespect ~~ Trust_Political\n",
  "Disrespect ~~ Trust_System\n",
  "Disrespect ~~ Trust_Other\n",
  "Trust_Political ~~ Trust_System\n",
  "Trust_Political ~~ Trust_Other\n",
  "Trust_System ~~ Trust_Other"
)

# Fit CFA with WLSMV (for ordinal)
fit1 <- tryCatch(
  cfa(model1_syntax, data = fa_ordinal, ordered = all_items, estimator = "WLSMV", mimic = "Mplus"),
  error = function(e) { message("Model 1 error: ", e$message); NULL }
)

fit2 <- tryCatch(
  cfa(model2_syntax, data = fa_ordinal, ordered = all_items, estimator = "WLSMV", mimic = "Mplus"),
  error = function(e) { message("Model 2 error: ", e$message); NULL }
)

fit3 <- tryCatch(
  cfa(model3_syntax, data = fa_ordinal, ordered = all_items, estimator = "WLSMV", mimic = "Mplus"),
  error = function(e) { message("Model 3 error: ", e$message); NULL }
)

# Extract fit indices
get_fit_row <- function(fit, model_name) {
  if (is.null(fit)) return(data.frame(Model = model_name, CFI = NA, TLI = NA, RMSEA = NA, SRMR = NA, Chi2 = NA, AIC = NA, BIC = NA, converged = FALSE))
  fi <- fitMeasures(fit, c("chisq", "cfi", "tli", "rmsea", "srmr", "aic", "bic"))
  data.frame(
    Model = model_name,
    Chi2 = round(fi["chisq"], 1),
    df = fitMeasures(fit, "df"),
    CFI = round(fi["cfi"], 3),
    TLI = round(fi["tli"], 3),
    RMSEA = round(fi["rmsea"], 3),
    SRMR = round(fi["srmr"], 3),
    AIC = round(fi["aic"], 1),
    BIC = round(fi["bic"], 1),
    converged = fit@Fit@converged
  )
}

fit_table <- rbind(
  get_fit_row(fit1, "Model 1: One factor"),
  get_fit_row(fit2, "Model 2: Two factors (Recognition + Trust)"),
  get_fit_row(fit3, "Model 3: Multi-factor (Recog+Disrespect; Political+System+Other Trust)")
)

write.csv(fit_table, "outputs/discriminant_validity/tables/CFA_fit_comparison.csv", row.names = FALSE)
cat("CFA fit table:\n")
print(fit_table)

# Save latent correlations from Model 2 if successful
if (!is.null(fit2) && fit2@Fit@converged) {
  latent_cor <- parameterEstimates(fit2) %>%
    filter(op == "~~", lhs != rhs, lhs %in% c("Recognition", "Trust"))
  cat("\nModel 2 latent correlation (Recognition ~~ Trust):\n")
  print(latent_cor[, c("lhs", "op", "rhs", "est", "se", "pvalue")])
}

# =============================================================================
# TASK C: CORRELATIONS BETWEEN CONSTRUCTS
# =============================================================================

cat("\n===== TASK C: CORRELATIONS BETWEEN DIMENSIONS =====\n\n")

# Define dimension scores (scale means)
# Recognition: care (1), rights (2: equality, rights), esteem (2: esteem, value_society), disrespect (4)
recog_care_score <- recog_df$recog_care
recog_rights_score <- rowMeans(recog_df[, c("recog_equality", "recog_rights")], na.rm = TRUE)
recog_esteem_score <- rowMeans(recog_df[, c("recog_esteem", "recog_value_society")], na.rm = TRUE)
disrespect_score <- rowMeans(recog_df[, c("disrespect_misperception", "disrespect_denigration", "disrespect_exclusion", "disrespect_discrimination")], na.rm = TRUE)

# Trust: political (2), system (2)
trust_political_score <- rowMeans(trust_df[, c("trust_politicians", "trust_government")], na.rm = TRUE)
trust_system_score <- rowMeans(trust_df[, c("trust_authorities", "trust_justice_system")], na.rm = TRUE)
trust_news_score <- trust_df$trust_news_media
trust_citizens_score <- trust_df$trust_citizens

# Combined scores for overall correlation
recog_overall <- rowMeans(recog_df, na.rm = TRUE)
trust_overall <- rowMeans(trust_df, na.rm = TRUE)

# Build correlation matrix
dim_scores <- data.frame(
  recog_care = recog_care_score,
  recog_rights = recog_rights_score,
  recog_esteem = recog_esteem_score,
  disrespect = disrespect_score,
  trust_political = trust_political_score,
  trust_system = trust_system_score,
  trust_news_media = trust_news_score,
  trust_citizens = trust_citizens_score,
  recog_overall = recog_overall,
  trust_overall = trust_overall
)

cor_pearson <- cor(dim_scores, use = "pairwise.complete.obs")
cor_spearman <- cor(dim_scores, use = "pairwise.complete.obs", method = "spearman")

# Subset: recognition dims x trust dims
recog_dims <- c("recog_care", "recog_rights", "recog_esteem", "disrespect", "recog_overall")
trust_dims <- c("trust_political", "trust_system", "trust_news_media", "trust_citizens", "trust_overall")
cor_recog_trust <- cor_pearson[recog_dims, trust_dims]

write.csv(round(cor_recog_trust, 3), "outputs/discriminant_validity/tables/Correlation_recognition_x_trust.csv")
cat("Correlation table (Recognition dims x Trust dims):\n")
print(round(cor_recog_trust, 3))

# Fornell-Larcker / discriminant validity (Model 2)
if (!is.null(fit2) && fit2@Fit@converged) {
  tryCatch({
    ave_out <- semTools::AVE(fit2)
    cat("\nAverage Variance Extracted (AVE) - Model 2:\n")
    print(ave_out)
    sqrt_ave <- sqrt(ave_out)
    cat("sqrt(AVE):", round(sqrt_ave, 3), "- should exceed latent correlation (0.235) for discriminant validity\n")
  }, error = function(e) message("AVE check skipped: ", e$message))
}

# Write combined Excel workbook
tryCatch({
  wb_list <- list(
    EFA_loadings = loadings_renamed,
    CFA_fit = fit_table,
    Correlations = as.data.frame(round(cor_recog_trust, 3))
  )
  write_xlsx(wb_list, "outputs/discriminant_validity/tables/discriminant_validity_all_tables.xlsx")
  cat("Combined Excel workbook saved: discriminant_validity_all_tables.xlsx\n")
}, error = function(e) message("Excel write skipped: ", e$message))

cat("\n===== ANALYSIS COMPLETE =====\n")
cat("Outputs saved to outputs/discriminant_validity/tables/\n")
