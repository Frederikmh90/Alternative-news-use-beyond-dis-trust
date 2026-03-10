# ---- REGRESSION ANALYSIS (Nonrecognition): Rejection of Mainstream News Media ----
# Mirrors 05_reg_mainstreamnews.R but swaps recog_* with nonrecog_* and writes
# outputs to separate nonrecog folders.
# 
# NOTE: Q10 items are REVERSED to measure "rejection of mainstream news" 
# (as indicator of alternative news orientation)
# High values = rejection/criticism of MSM, Low values = trust/acceptance of MSM

library(haven)
library(sjmisc)
library(purrr)
library(dplyr)
library(psych)
library(MASS)
library(car)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(sandwich)
library(lmtest)

if (file.exists("scripts/data_preparation.R")) {
  source("scripts/data_preparation.R")
} else if (file.exists("data_preparation.R")) {
  source("data_preparation.R")
} else {
  stop("Cannot find data_preparation.R")
}

# Reverse Q10 items to create "MSM Rejection" measure
# Original Q10: high = positive MSM perception
# Reversed: high = MSM rejection (alternative news orientation indicator)
q10_numeric <- Q10_media_mainstreamnews %>% 
  mutate(across(everything(), as.numeric)) %>%
  mutate(across(everything(), ~ 6 - .))  # Reverse all items (6 - value)

q10_cor <- cor(q10_numeric, use = "complete.obs")
fa.parallel(q10_numeric, fm = "ml", fa = "fa", n.iter = 100)
fa_1 <- fa(q10_numeric, nfactors = 1, rotate = "varimax", fm = "ml")
q10_factor_scores <- fa_1$scores; colnames(q10_factor_scores) <- "q10_msm_rejection_factor"

q10_numeric_for_composites <- Q10_media_mainstreamnews %>% 
  mutate(across(everything(), as.numeric)) %>%
  mutate(across(everything(), ~ 6 - .))  # Reverse all items

q10_overall_composite <- q10_numeric_for_composites %>% rowMeans(na.rm = TRUE)
q10_truth_rejected <- q10_numeric_for_composites$truth_important_issues
q10_voices_rejected <- q10_numeric_for_composites$all_voices_heard
q10_one_sided_agreed <- q10_numeric_for_composites$one_sided_presentation


# Predictors: Nonrecognition selection + grouped trust + controls
nonrecog_selected <- nonrecognition %>% dplyr::select(nonrecog_care, nonrecog_equality, nonrecog_rights, nonrecog_esteem)
disrespect_selected <- recognition %>% dplyr::select(disrespect_denigration, disrespect_exclusion, disrespect_discrimination)

predictors_numeric <- bind_cols(
  nonrecog_selected %>% mutate(across(everything(), as.numeric)),
  disrespect_selected %>% mutate(across(everything(), as.numeric)),
  trust_grouped %>% mutate(across(everything(), as.numeric)),
  controls %>% dplyr::select(-political_ideology, -political_ideology_simple, -fringe_vs_mainstream, -education, -income, left_right_scale, follow_politics_society) %>% mutate(across(everything(), as.numeric))
)

predictors_data <- bind_cols(
  nonrecog_selected,
  disrespect_selected,
  trust_grouped %>% dplyr::select(-trust_news_media),
  controls %>% dplyr::select(-political_ideology, -political_ideology_simple, -fringe_vs_mainstream, -education, -income, left_right_scale, follow_politics_society)
)

# Datasets and complete cases (MSM Rejection as DV)
data_factor <- bind_cols(q10_factor_scores = q10_factor_scores, predictors_data)
data_overall <- bind_cols(q10_overall = q10_overall_composite, predictors_data)
data_truth <- bind_cols(q10_truth = q10_truth_rejected, predictors_data)
data_voices <- bind_cols(q10_voices = q10_voices_rejected, predictors_data)
data_onesided <- bind_cols(q10_onesided = q10_one_sided_agreed, predictors_data)

data_factor_complete <- data_factor %>% filter(complete.cases(.))
data_overall_complete <- data_overall %>% filter(complete.cases(.))
data_truth_complete <- data_truth %>% filter(complete.cases(.))
data_voices_complete <- data_voices %>% filter(complete.cases(.))
data_onesided_complete <- data_onesided %>% filter(complete.cases(.))

# Prepare regression datasets
prep <- function(df, target) {
  df %>% mutate(
    education_group = relevel(factor(as.character(education_group), levels = c("vocational", "basic_upsecondary", "+higher")), ref = "vocational"),  # Reference: vocational
    age = relevel(as.factor(age), ref = "35-49"),  # Reference: 35-49 (middle category)
    gender = as.factor(gender),
    income_group = relevel(as.factor(income_group), ref = "mid"),  # Reference: mid (middle category)
    across(c({{target}}, starts_with("nonrecog_"), starts_with("disrespect_"), starts_with("trust_"), left_right_scale, follow_politics_society), as.numeric)
  )
}

data_factor_reg <- prep(data_factor_complete, q10_factor_scores)
data_overall_reg <- prep(data_overall_complete, q10_overall)
data_truth_reg <- prep(data_truth_complete, q10_truth)
data_voices_reg <- prep(data_voices_complete, q10_voices)
data_onesided_reg <- prep(data_onesided_complete, q10_onesided)

# Attach weights aligned with complete cases
if ("analysis_weight" %in% names(controls)) {
  data_factor_reg$analysis_weight <- controls$analysis_weight[complete.cases(data_factor)]
  data_overall_reg$analysis_weight <- controls$analysis_weight[complete.cases(data_overall)]
  data_truth_reg$analysis_weight <- controls$analysis_weight[complete.cases(data_truth)]
  data_voices_reg$analysis_weight <- controls$analysis_weight[complete.cases(data_voices)]
  data_onesided_reg$analysis_weight <- controls$analysis_weight[complete.cases(data_onesided)]
}

# Models
model_q10_factor <- lm(q10_factor_scores ~ . - analysis_weight, data = data_factor_reg, weights = analysis_weight)
model_q10_overall <- lm(q10_overall ~ . - analysis_weight, data = data_overall_reg, weights = analysis_weight)
model_q10_truth <- lm(q10_truth ~ . - analysis_weight, data = data_truth_reg, weights = analysis_weight)
model_q10_voices <- lm(q10_voices ~ . - analysis_weight, data = data_voices_reg, weights = analysis_weight)
model_q10_onesided <- lm(q10_onesided ~ . - analysis_weight, data = data_onesided_reg, weights = analysis_weight)

# Extract results and save plots (heteroskedasticity-robust HC3 SEs, survey weights applied)
extract_model_results <- function(model, model_name) {
  ct <- lmtest::coeftest(model, vcov = sandwich::vcovHC(model, type = "HC3"))
  out <- data.frame(
    Variable = rownames(ct), Coefficient = ct[,1], Std_Error = ct[,2], T_Value = ct[,3], P_Value = ct[,4],
    Model = model_name, stringsAsFactors = FALSE
  )
  out
}

all_results <- dplyr::bind_rows(
  extract_model_results(model_q10_factor, "Q10_Factor"),
  extract_model_results(model_q10_overall, "Q10_Overall"),
  extract_model_results(model_q10_truth, "Q10_Truth"),
  extract_model_results(model_q10_voices, "Q10_Voices"),
  extract_model_results(model_q10_onesided, "Q10_OneSided")
)

plot_one <- function(results, model_name, save_path){
  df <- results %>% dplyr::filter(Model == model_name, Variable != "(Intercept)") %>%
    mutate(Variable_Clean = Variable %>% gsub("^fringe_vs_mainstreamfringe$", "Fringe vote", .) %>% gsub("recog_", "Recognition: ", .) %>% gsub("nonrecog_", "Nonrecognition: ", .) %>% gsub("disrespect_", "Disrespect: ", .) %>% gsub("trust_", "Trust: ", .) %>% gsub("_", " ", .) %>% tools::toTitleCase())
  p <- ggplot(df, aes(x = reorder(Variable_Clean, Coefficient), y = Coefficient)) + geom_point(size = 3, aes(color = P_Value < 0.05)) +
    geom_errorbar(aes(ymin = Coefficient - 1.96*Std_Error, ymax = Coefficient + 1.96*Std_Error), width = 0.2) + geom_hline(yintercept=0, linetype="dashed", color="red") + coord_flip() + theme_minimal() + theme(legend.position = "none") + labs(title=paste("MSM Rejection:", model_name), x=NULL, y="Coefficient (95% CI)")
  ggsave(paste0(save_path, "coefficient_plot_", tolower(gsub(" ", "_", model_name)), ".png"), p, width=12, height=8, dpi=300)
}

# Save outputs
dir.create("outputs/reg_mainstreamnews_nonrecog/models", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/reg_mainstreamnews_nonrecog/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/reg_mainstreamnews_nonrecog/plots", recursive = TRUE, showWarnings = FALSE)

saveRDS(model_q10_factor, "outputs/reg_mainstreamnews_nonrecog/models/model_q10_factor.rds")
saveRDS(model_q10_overall, "outputs/reg_mainstreamnews_nonrecog/models/model_q10_overall.rds")
saveRDS(model_q10_truth, "outputs/reg_mainstreamnews_nonrecog/models/model_q10_truth.rds")
saveRDS(model_q10_voices, "outputs/reg_mainstreamnews_nonrecog/models/model_q10_voices.rds")
saveRDS(model_q10_onesided, "outputs/reg_mainstreamnews_nonrecog/models/model_q10_onesided.rds")

write.csv(all_results, "outputs/reg_mainstreamnews_nonrecog/tables/appendix_comprehensive_q10_nonrecog_results.csv", row.names = FALSE)

# ---- Article-ready regression tables (priority models only) ----
library(broom)

# Function to create article-ready model statistics table (HC3 robust SEs)
create_article_table <- function(model, model_name) {
  # Extract key statistics (heteroskedasticity-robust HC3 SEs)
  ct <- lmtest::coeftest(model, vcov = sandwich::vcovHC(model, type = "HC3"))
  model_summary <- summary(model)
  
  # Get coefficients with robust standard errors, t-values, and p-values
  ct_mat <- matrix(ct, ncol = 4)
  coef_df <- data.frame(
    Variable = rownames(ct),
    Coefficient = ct_mat[,1],
    Std_Error = ct_mat[,2],
    t_value = ct_mat[,3],
    p_value = ct_mat[,4],
    stringsAsFactors = FALSE
  )
  
  # Add significance stars
  coef_df$Significance <- ifelse(coef_df$p_value < 0.001, "***",
                                 ifelse(coef_df$p_value < 0.01, "**",
                                        ifelse(coef_df$p_value < 0.05, "*",
                                               ifelse(coef_df$p_value < 0.1, ".", ""))))
  
  # Model statistics
  n_obs <- nobs(model)
  r_squared <- model_summary$r.squared
  adj_r_squared <- model_summary$adj.r.squared
  f_stat <- model_summary$fstatistic[1]
  f_df1 <- model_summary$fstatistic[2]
  f_df2 <- model_summary$fstatistic[3]
  f_pvalue <- pf(f_stat, f_df1, f_df2, lower.tail = FALSE)
  residual_se <- model_summary$sigma
  aic_value <- AIC(model)
  bic_value <- BIC(model)
  n_predictors <- length(coef(model)) - 1  # Exclude intercept
  
  # Create model statistics footer
  model_stats <- data.frame(
    Variable = c("", "=== MODEL STATISTICS ===", "N", "R²", "Adjusted R²", 
                 "F-statistic", "F df1", "F df2", "F p-value", 
                 "Residual Std. Error", "AIC", "BIC", "Number of Predictors"),
    Coefficient = c("", "", n_obs, round(r_squared, 4), round(adj_r_squared, 4),
                   round(f_stat, 3), f_df1, f_df2, 
                   ifelse(f_pvalue < 0.001, "<0.001", round(f_pvalue, 4)),
                   round(residual_se, 4), round(aic_value, 2), round(bic_value, 2), n_predictors),
    Std_Error = "",
    t_value = "",
    p_value = "",
    Significance = ""
  )
  
  # Combine coefficients and statistics
  full_table <- rbind(coef_df, model_stats)
  
  return(full_table)
}

# Generate article table for priority model (Q10 MSM Rejection factor only)
cat("\nGenerating article-ready table for priority model...\n")

article_table_q10 <- create_article_table(model_q10_factor, "Q10 MSM Rejection Factor")
write.csv(article_table_q10, "outputs/reg_mainstreamnews_nonrecog/tables/article_table_q10_msm_rejection_factor.csv", row.names = FALSE)
cat("✓ Article table saved: Q10 MSM Rejection Factor\n")
pp <- "outputs/reg_mainstreamnews_nonrecog/plots/"
plot_one(all_results, "Q10_Factor", pp)
plot_one(all_results, "Q10_Overall", pp)
plot_one(all_results, "Q10_Truth", pp)
plot_one(all_results, "Q10_Voices", pp)
plot_one(all_results, "Q10_OneSided", pp)

cat("MSM Rejection analysis (nonrecognition predictors) complete. High DV = rejection of mainstream news.\n")

# ---- Publication tables (weighted) ----
dir.create("outputs/reg_mainstreamnews_nonrecog/tables", recursive = TRUE, showWarnings = FALSE)

# Table 1: Sample characteristics (reuse weights from controls)
weighted_prop_table <- function(f, w) {
  lv <- levels(f)
  total_weight <- sum(w, na.rm = TRUE)
  
  # Calculate for each level
  out <- lapply(lv, function(l) {
    idx <- which(f == l)
    c(level = l,
      count = sum(w[idx], na.rm = TRUE),
      proportion = sum(w[idx], na.rm = TRUE) / total_weight)
  })
  
  # Add missing values row
  idx_missing <- which(is.na(f))
  if(length(idx_missing) > 0) {
    out[[length(out) + 1]] <- c(
      level = "Missing (NA)",
      count = sum(w[idx_missing], na.rm = TRUE),
      proportion = sum(w[idx_missing], na.rm = TRUE) / total_weight
    )
  }
  
  out <- as.data.frame(do.call(rbind, out), stringsAsFactors = FALSE)
  out$count <- as.numeric(out$count)
  out$proportion <- as.numeric(out$proportion)
  out
}

controls_w <- controls$analysis_weight
sample_chars <- list(
  gender = weighted_prop_table(controls$gender, controls_w),
  age = weighted_prop_table(controls$age, controls_w),
  education_group = weighted_prop_table(controls$education_group, controls_w),
  income_group = weighted_prop_table(controls$income_group, controls_w),
  fringe_vs_mainstream = weighted_prop_table(controls$fringe_vs_mainstream, controls_w),
  political_ideology_simple = weighted_prop_table(controls$political_ideology_simple, controls_w)
)
sample_chars_df <- dplyr::bind_rows(lapply(names(sample_chars), function(nm){
  df <- sample_chars[[nm]]; df$variable <- nm; df
})) %>% dplyr::select(variable, level, count, proportion)
writexl::write_xlsx(sample_chars_df, "outputs/reg_mainstreamnews_nonrecog/tables/table1_sample_characteristics.xlsx")

# Table 2: Factor/construct summary for Q10 (MSM Rejection)
q10_kmo <- tryCatch({ psych::KMO(q10_numeric) $MSA }, error = function(e) NA_real_)
q10_bart <- tryCatch({ psych::cortest.bartlett(q10_numeric, n = nrow(q10_numeric))$p.value }, error = function(e) NA_real_)
q10_var <- tryCatch({ sum(fa_1$Vaccounted[2,]) }, error = function(e) NA_real_)
q10_alpha <- tryCatch({ psych::alpha(q10_numeric)$total$raw_alpha }, error = function(e) NA_real_)
factor_summary <- data.frame(
  construct = c("Q10 (MSM Rejection - reversed items)"),
  items = paste(colnames(q10_numeric), collapse = "; "),
  KMO = round(q10_kmo, 3),
  Bartlett_p = round(q10_bart, 6),
  Variance_Explained = round(q10_var, 3),
  Alpha = round(q10_alpha, 3),
  N = nrow(q10_numeric)
)
writexl::write_xlsx(factor_summary, "outputs/reg_mainstreamnews_nonrecog/tables/table2_factor_reliability_summary.xlsx")

# Table 3: Descriptives for regression variables
to_numeric <- function(df){ df %>% dplyr::mutate(across(everything(), as.numeric)) }
wmean <- function(x, w){
  x <- as.numeric(x); w <- as.numeric(w)
  sel <- !is.na(x) & !is.na(w)
  if (!any(sel)) return(NA_real_)
  sum(w[sel] * x[sel]) / sum(w[sel])
}
wsd <- function(x, w){
  x <- as.numeric(x); w <- as.numeric(w)
  sel <- !is.na(x) & !is.na(w)
  if (!any(sel)) return(NA_real_)
  mu <- sum(w[sel] * x[sel]) / sum(w[sel])
  sqrt(sum(w[sel] * (x[sel] - mu)^2) / sum(w[sel]))
}

desc_numeric <- function(df, w, varnames){
  nm <- varnames
  out <- lapply(nm, function(v){
    x <- df[[v]]
    n_valid <- sum(!is.na(x))
    n_missing <- sum(is.na(x))
    data.frame(
      variable = v, 
      mean = round(wmean(x,w), 3), 
      sd = round(wsd(x,w), 3), 
      min = min(x, na.rm=TRUE), 
      max = max(x, na.rm=TRUE), 
      N = n_valid,
      N_missing = n_missing
    )
  })
  dplyr::bind_rows(out)
}

desc_factor <- function(df, w){
  vars <- names(df)
  out <- lapply(vars, function(v){
    if(is.factor(df[[v]])||is.ordered(df[[v]])){
      tmp <- weighted_prop_table(df[[v]], w); tmp$variable <- v; tmp
    } else { NULL }
  })
  dplyr::bind_rows(out) %>% dplyr::select(variable, level, count, proportion)
}

nr_vars <- c("nonrecog_care","nonrecog_equality","nonrecog_rights","nonrecog_esteem","nonrecog_value_society")
dis_vars <- c("disrespect_denigration","disrespect_exclusion","disrespect_discrimination")
trust_vars <- c("trust_citizens","trust_political","trust_system","trust_news_media")

nr_desc <- desc_factor(nonrecognition[,nr_vars, drop=FALSE], controls_w)
dis_desc <- desc_factor(recognition[,dis_vars, drop=FALSE], controls_w)
trust_desc <- desc_numeric(trust_grouped, controls_w, trust_vars)
controls_desc <- desc_factor(controls %>% dplyr::select(gender, age, education_group, income_group, fringe_vs_mainstream, political_ideology_simple), controls_w)

descriptives_combined <- list(
  nonrecognition = nr_desc,
  disrespect = dis_desc,
  trust = trust_desc,
  controls = controls_desc
)
writexl::write_xlsx(descriptives_combined, "outputs/reg_mainstreamnews_nonrecog/tables/table3_descriptives_regression_variables.xlsx")

# Table 4: Weighted bivariate correlations
cor_df <- dplyr::bind_cols(
  q10_factor_scores = as.numeric(q10_factor_scores),
  to_numeric(nonrecognition[,nr_vars]),
  to_numeric(recognition[,dis_vars]),
  trust_grouped
)
wcor <- function(A, w){
  A <- as.matrix(A); w <- as.numeric(w)
  sel <- complete.cases(A) & !is.na(w)
  A <- A[sel,,drop=FALSE]; w <- w[sel]
  mu <- colSums(A * w)/sum(w)
  A_center <- sweep(A, 2, mu, "-")
  cov_w <- t(A_center * w) %*% A_center / sum(w)
  sd_w <- sqrt(diag(cov_w))
  cov_w / (sd_w %o% sd_w)
}
cor_mat <- round(wcor(cor_df, controls_w), 3)
writexl::write_xlsx(as.data.frame(cor_mat), "outputs/reg_mainstreamnews_nonrecog/tables/table4_bivariate_correlations_q10.xlsx")

# Table A1: VIF & N (robust to aliasing)
safe_max_vif <- function(model){
  tryCatch({
    v <- car::vif(model)
    if (is.matrix(v)) max(v[,1], na.rm = TRUE) else max(v, na.rm = TRUE)
  }, error = function(e) NA_real_)
}
safe_count_vif_gt5 <- function(model){
  tryCatch({
    v <- car::vif(model)
    if (is.matrix(v)) sum(v[,1] > 5, na.rm = TRUE) else sum(v > 5, na.rm = TRUE)
  }, error = function(e) NA_integer_)
}

vif_tbl <- data.frame(
  model = c("Q10_Factor","Q10_Overall","Q10_Truth","Q10_Voices","Q10_OneSided"),
  N = c(nrow(data_factor_reg), nrow(data_overall_reg), nrow(data_truth_reg), nrow(data_voices_reg), nrow(data_onesided_reg)),
  max_VIF = c(
    safe_max_vif(model_q10_factor),
    safe_max_vif(model_q10_overall),
    safe_max_vif(model_q10_truth),
    safe_max_vif(model_q10_voices),
    safe_max_vif(model_q10_onesided)
  )
)
vif_tbl$n_VIF_gt5 <- c(
  safe_count_vif_gt5(model_q10_factor),
  safe_count_vif_gt5(model_q10_overall),
  safe_count_vif_gt5(model_q10_truth),
  safe_count_vif_gt5(model_q10_voices),
  safe_count_vif_gt5(model_q10_onesided)
)
writexl::write_xlsx(vif_tbl, "outputs/reg_mainstreamnews_nonrecog/tables/tableA1_vif_and_model_n.xlsx")


