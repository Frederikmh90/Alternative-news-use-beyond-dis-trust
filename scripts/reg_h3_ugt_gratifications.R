# ---- REGRESSION ANALYSIS (Nonrecognition): Uses and Gratifications Theory ----
# This script mirrors 03_reg_usergratification.R but swaps recog_* with nonrecog_*
# and writes outputs to a separate nonrecog-specific folder.

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

# Load data preparation
source("scripts/data_prep.R")

# ---- 1) EFA for UGT variables ----
# NOTE: full_picture (Q8_6) removed from analysis — conceptually distinct item
# (highest mean at 2.94, 74.9% agreement; behaves differently from the other
# information monitoring items which cluster around 2.18–2.39)
ugt_all <- Q8_media_altnews_motivation %>% mutate(across(everything(), as.numeric))
ugt_numeric <- ugt_all %>% dplyr::select(-full_picture)
ugt_cor <- cor(ugt_numeric, use = "complete.obs")
KMO(ugt_cor); cortest.bartlett(ugt_cor, n = nrow(ugt_numeric))
fa.parallel(ugt_numeric, fm = "ml", fa = "fa", n.iter = 100)
fa_2 <- fa(ugt_numeric, nfactors = 2, rotate = "varimax", fm = "ml")

# Factor scores and composites (full_picture excluded)
ugt_factor_scores <- fa_2$scores
colnames(ugt_factor_scores) <- c("ugt_info_seeking", "ugt_identity")
ugt_numeric_for_composites <- ugt_all %>% dplyr::select(-full_picture)
ugt_overall_composite <- ugt_numeric_for_composites %>% rowMeans(na.rm = TRUE)
ugt_reflect_values <- ugt_numeric_for_composites$reflect_values
ugt_feel_seen <- ugt_numeric_for_composites$feel_seen_understood
ugt_alternative_perspectives <- ugt_numeric_for_composites$alternative_perspectives

# ---- 2) Predictors: Nonrecognition + Disrespect + Grouped Trust + Controls ----
nonrecog_selected <- nonrecognition %>% dplyr::select(nonrecog_care, nonrecog_equality, nonrecog_rights, nonrecog_esteem)
disrespect_selected <- recognition %>% dplyr::select(disrespect_denigration, disrespect_exclusion, disrespect_discrimination)

predictors_numeric <- bind_cols(
  nonrecog_selected %>% mutate(across(everything(), as.numeric)),
  disrespect_selected %>% mutate(across(everything(), as.numeric)),
  trust_grouped %>% mutate(across(everything(), as.numeric)),
  controls %>% dplyr::select(-political_ideology, -political_ideology_simple, -fringe_vs_mainstream, -education, -income, left_right_scale, follow_politics_society) %>% mutate(across(everything(), as.numeric))
)

# Predictor dataset
predictors_data <- bind_cols(
  nonrecog_selected,
  disrespect_selected,
  trust_grouped,
  controls %>% dplyr::select(-political_ideology, -political_ideology_simple, -fringe_vs_mainstream, -education, -income, left_right_scale, follow_politics_society)
)

# ---- 3) Prepare regression datasets ----
data_info_seeking <- bind_cols(ugt_info_seeking = ugt_factor_scores[,1], predictors_data)
data_identity <- bind_cols(ugt_identity = ugt_factor_scores[,2], predictors_data)
data_overall <- bind_cols(ugt_overall = ugt_overall_composite, predictors_data)
data_reflect <- bind_cols(ugt_reflect = ugt_reflect_values, predictors_data)
data_feel_seen <- bind_cols(ugt_feel_seen = ugt_feel_seen, predictors_data)
data_alternative <- bind_cols(ugt_alternative = ugt_alternative_perspectives, predictors_data)

# Remove incomplete cases
data_info_seeking_complete <- data_info_seeking %>% filter(complete.cases(.))
data_identity_complete <- data_identity %>% filter(complete.cases(.))
data_overall_complete <- data_overall %>% filter(complete.cases(.))
data_reflect_complete <- data_reflect %>% filter(complete.cases(.))
data_feel_seen_complete <- data_feel_seen %>% filter(complete.cases(.))
data_alternative_complete <- data_alternative %>% filter(complete.cases(.))

# Keep categorical vars as factors; convert ordinal/numeric appropriately
# First level in factor() becomes reference, so put vocational first
data_info_seeking_reg <- data_info_seeking_complete %>% mutate(
  education_group = relevel(factor(as.character(education_group), levels = c("vocational", "basic_upsecondary", "+higher")), ref = "vocational"),  # Reference: vocational
  age = relevel(as.factor(age), ref = "35-49"),  # Reference: 35-49 (middle category)
  gender = as.factor(gender),
  income_group = relevel(as.factor(income_group), ref = "mid"),  # Reference: mid (middle category)
  across(c(ugt_info_seeking, starts_with("recog_"), starts_with("nonrecog_"), starts_with("disrespect_"), starts_with("trust_"), left_right_scale, follow_politics_society), as.numeric)
)

data_identity_reg <- data_identity_complete %>% mutate(
  education_group = relevel(factor(as.character(education_group), levels = c("vocational", "basic_upsecondary", "+higher")), ref = "vocational"),  # Reference: vocational
  age = relevel(as.factor(age), ref = "35-49"),  # Reference: 35-49 (middle category)
  gender = as.factor(gender),
  income_group = relevel(as.factor(income_group), ref = "mid"),  # Reference: mid (middle category)
  across(c(ugt_identity, starts_with("recog_"), starts_with("nonrecog_"), starts_with("disrespect_"), starts_with("trust_"), left_right_scale, follow_politics_society), as.numeric)
)

data_overall_reg <- data_overall_complete %>% mutate(
  education_group = relevel(factor(as.character(education_group), levels = c("vocational", "basic_upsecondary", "+higher")), ref = "vocational"),  # Reference: vocational
  age = relevel(as.factor(age), ref = "35-49"),  # Reference: 35-49 (middle category)
  gender = as.factor(gender),
  income_group = relevel(as.factor(income_group), ref = "mid"),  # Reference: mid (middle category)
  across(c(ugt_overall, starts_with("recog_"), starts_with("nonrecog_"), starts_with("disrespect_"), starts_with("trust_"), left_right_scale, follow_politics_society), as.numeric)
)

data_reflect_reg <- data_reflect_complete %>% mutate(
  education_group = relevel(factor(as.character(education_group), levels = c("vocational", "basic_upsecondary", "+higher")), ref = "vocational"),  # Reference: vocational
  age = relevel(as.factor(age), ref = "35-49"),  # Reference: 35-49 (middle category)
  gender = as.factor(gender),
  income_group = relevel(as.factor(income_group), ref = "mid"),  # Reference: mid (middle category)
  across(c(ugt_reflect, starts_with("recog_"), starts_with("nonrecog_"), starts_with("disrespect_"), starts_with("trust_"), left_right_scale, follow_politics_society), as.numeric)
)

data_feel_seen_reg <- data_feel_seen_complete %>% mutate(
  education_group = relevel(factor(as.character(education_group), levels = c("vocational", "basic_upsecondary", "+higher")), ref = "vocational"),  # Reference: vocational
  age = relevel(as.factor(age), ref = "35-49"),  # Reference: 35-49 (middle category)
  gender = as.factor(gender),
  income_group = relevel(as.factor(income_group), ref = "mid"),  # Reference: mid (middle category)
  across(c(ugt_feel_seen, starts_with("recog_"), starts_with("nonrecog_"), starts_with("disrespect_"), starts_with("trust_"), left_right_scale, follow_politics_society), as.numeric)
)

data_alternative_reg <- data_alternative_complete %>% mutate(
  education_group = relevel(factor(as.character(education_group), levels = c("vocational", "basic_upsecondary", "+higher")), ref = "vocational"),  # Reference: vocational
  age = relevel(as.factor(age), ref = "35-49"),  # Reference: 35-49 (middle category)
  gender = as.factor(gender),
  income_group = relevel(as.factor(income_group), ref = "mid"),  # Reference: mid (middle category)
  across(c(ugt_alternative, starts_with("recog_"), starts_with("nonrecog_"), starts_with("disrespect_"), starts_with("trust_"), left_right_scale, follow_politics_society), as.numeric)
)

# Attach weights aligned with complete cases
if ("analysis_weight" %in% names(controls)) {
  data_info_seeking_reg$analysis_weight <- controls$analysis_weight[complete.cases(data_info_seeking)]
  data_identity_reg$analysis_weight <- controls$analysis_weight[complete.cases(data_identity)]
  data_overall_reg$analysis_weight <- controls$analysis_weight[complete.cases(data_overall)]
  data_reflect_reg$analysis_weight <- controls$analysis_weight[complete.cases(data_reflect)]
  data_feel_seen_reg$analysis_weight <- controls$analysis_weight[complete.cases(data_feel_seen)]
  data_alternative_reg$analysis_weight <- controls$analysis_weight[complete.cases(data_alternative)]
}

# ---- 4) Linear models ----
model_ugt_info_seeking <- lm(ugt_info_seeking ~ . - analysis_weight, data = data_info_seeking_reg, weights = analysis_weight)
model_ugt_identity <- lm(ugt_identity ~ . - analysis_weight, data = data_identity_reg, weights = analysis_weight)
model_ugt_overall <- lm(ugt_overall ~ . - analysis_weight, data = data_overall_reg, weights = analysis_weight)
model_ugt_reflect <- lm(ugt_reflect ~ . - analysis_weight, data = data_reflect_reg, weights = analysis_weight)
model_ugt_feel_seen <- lm(ugt_feel_seen ~ . - analysis_weight, data = data_feel_seen_reg, weights = analysis_weight)
model_ugt_alternative <- lm(ugt_alternative ~ . - analysis_weight, data = data_alternative_reg, weights = analysis_weight)

# ---- 4b) Results extraction and plotting (heteroskedasticity-robust HC3 SEs, survey weights applied) ----
extract_model_results <- function(model, model_name) {
  ct <- lmtest::coeftest(model, vcov = sandwich::vcovHC(model, type = "HC3"))
  df <- data.frame(
    Variable = rownames(ct),
    Coefficient = ct[, "Estimate"],
    Std_Error = ct[, "Std. Error"],
    T_Value = ct[, "t value"],
    P_Value = ct[, "Pr(>|t|)"],
    Model = model_name,
    stringsAsFactors = FALSE
  )
  df$Significance <- dplyr::case_when(
    df$P_Value < 0.001 ~ "***",
    df$P_Value < 0.01 ~ "**",
    df$P_Value < 0.05 ~ "*",
    TRUE ~ ""
  )
  df
}

create_coefficient_plot <- function(results, model_name, save_path) {
  model_results <- results %>%
    dplyr::filter(Model == model_name, Variable != "(Intercept)") %>%
    mutate(
      Variable_Clean = Variable %>%
        gsub("^fringe_vs_mainstreamfringe$", "Fringe vote", .) %>%
        gsub("recog_", "Recognition: ", .) %>%
        gsub("nonrecog_", "Nonrecognition: ", .) %>%
        gsub("disrespect_", "Disrespect: ", .) %>%
        gsub("trust_", "Trust: ", .) %>%
        gsub("_", " ", .) %>%
        tools::toTitleCase()
    )
  p <- ggplot(model_results, aes(x = reorder(Variable_Clean, Coefficient), y = Coefficient)) +
    geom_point(size = 3, aes(color = P_Value < 0.05)) +
    geom_errorbar(aes(ymin = Coefficient - 1.96 * Std_Error, ymax = Coefficient + 1.96 * Std_Error), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    coord_flip() +
    labs(title = paste("Coefficient Plot:", model_name), x = "Predictor Variables", y = "Coefficient (95% CI)") +
    theme_minimal() + theme(plot.title = element_text(size = 14, face = "bold"), legend.position = "none")
  filename <- paste0(save_path, "coefficient_plot_", tolower(gsub(" ", "_", model_name)), ".png")
  ggsave(filename, p, width = 12, height = 8, dpi = 300)
}

# ---- 5) Save outputs to separate folder ----
dir.create("outputs/reg_user/models", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/reg_user/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/reg_user/plots", recursive = TRUE, showWarnings = FALSE)

saveRDS(model_ugt_info_seeking, "outputs/reg_user/models/model_ugt_info_seeking.rds")
saveRDS(model_ugt_identity, "outputs/reg_user/models/model_ugt_identity.rds")
saveRDS(model_ugt_overall, "outputs/reg_user/models/model_ugt_overall.rds")
saveRDS(model_ugt_reflect, "outputs/reg_user/models/model_ugt_reflect.rds")
saveRDS(model_ugt_feel_seen, "outputs/reg_user/models/model_ugt_feel_seen.rds")
saveRDS(model_ugt_alternative, "outputs/reg_user/models/model_ugt_alternative.rds")

# Extract results and create plots
all_results <- dplyr::bind_rows(
  extract_model_results(model_ugt_info_seeking, "UGT_InfoSeeking"),
  extract_model_results(model_ugt_identity, "UGT_Identity"),
  extract_model_results(model_ugt_overall, "UGT_Overall"),
  extract_model_results(model_ugt_reflect, "UGT_Reflect"),
  extract_model_results(model_ugt_feel_seen, "UGT_FeelSeen"),
  extract_model_results(model_ugt_alternative, "UGT_Alternative")
)

write.csv(all_results, "outputs/reg_user/tables/appendix_comprehensive_ugt_nonrecog_results.csv", row.names = FALSE)

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

# Generate article tables for priority models
cat("\nGenerating article-ready tables for priority models...\n")

article_table_info <- create_article_table(model_ugt_info_seeking, "UGT Info-Seeking")
write.csv(article_table_info, "outputs/reg_user/tables/article_table_ugt_info_seeking.csv", row.names = FALSE)
cat("✓ Article table saved: UGT Info-Seeking\n")

article_table_identity <- create_article_table(model_ugt_identity, "UGT Identity")
write.csv(article_table_identity, "outputs/reg_user/tables/article_table_ugt_identity.csv", row.names = FALSE)
cat("✓ Article table saved: UGT Identity\n")

plot_path <- "outputs/reg_user/plots/"
create_coefficient_plot(all_results, "UGT_InfoSeeking", plot_path)
create_coefficient_plot(all_results, "UGT_Identity", plot_path)
create_coefficient_plot(all_results, "UGT_Overall", plot_path)
create_coefficient_plot(all_results, "UGT_Reflect", plot_path)
create_coefficient_plot(all_results, "UGT_FeelSeen", plot_path)
create_coefficient_plot(all_results, "UGT_Alternative", plot_path)

# ---- 4c) Publication tables (weighted) ----

dir.create("outputs/reg_user/tables", recursive = TRUE, showWarnings = FALSE)

# Weighted helpers without external packages
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

# Weighted sample characteristics (includes missing values)
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
writexl::write_xlsx(sample_chars_df, "outputs/reg_user/tables/table1_sample_characteristics.xlsx")

# Factor/construct summary
kmo_val <- tryCatch({ psych::KMO(ugt_numeric)$MSA }, error = function(e) NA_real_)
bart_p <- tryCatch({ psych::cortest.bartlett(ugt_numeric, n = nrow(ugt_numeric))$p.value }, error = function(e) NA_real_)
var_expl <- tryCatch({ sum(fa_2$Vaccounted[2,]) }, error = function(e) NA_real_)
alpha_all <- tryCatch({ psych::alpha(ugt_numeric)$total$raw_alpha }, error = function(e) NA_real_)
factor_summary <- data.frame(
  construct = c("UGT (Q8 items excl. full_picture)"),
  items = paste(colnames(ugt_numeric), collapse = "; "),
  KMO = round(kmo_val, 3),
  Bartlett_p = round(bart_p, 6),
  Variance_Explained = round(var_expl, 3),
  Alpha = round(alpha_all, 3),
  N = nrow(ugt_numeric)
)
writexl::write_xlsx(factor_summary, "outputs/reg_user/tables/table2_factor_reliability_summary.xlsx")

# Descriptives for regression variables

to_numeric <- function(df){ df %>% dplyr::mutate(across(everything(), as.numeric)) }

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
writexl::write_xlsx(descriptives_combined, "outputs/reg_user/tables/table3_descriptives_regression_variables.xlsx")

# Bivariate weighted correlations (numeric-coded) using simple weighting
cor_df <- dplyr::bind_cols(
  ugt_info_seeking = as.numeric(ugt_factor_scores[,1]),
  ugt_identity = as.numeric(ugt_factor_scores[,2]),
  to_numeric(nonrecognition[,nr_vars]),
  to_numeric(recognition[,dis_vars]),
  trust_grouped
)
# Weighted Pearson correlations via manual centering
wcor <- function(A, w){
  A <- as.matrix(A); w <- as.numeric(w)
  sel <- complete.cases(A) & !is.na(w)
  A <- A[sel,,drop=FALSE]; w <- w[sel]
  # center columns
  mu <- colSums(A * w)/sum(w)
  A_center <- sweep(A, 2, mu, "-")
  cov_w <- t(A_center * w) %*% A_center / sum(w)
  sd_w <- sqrt(diag(cov_w))
  cor_w <- cov_w / (sd_w %o% sd_w)
  cor_w
}
cor_mat <- round(wcor(cor_df, controls_w), 3)
writexl::write_xlsx(as.data.frame(cor_mat), "outputs/reg_user/tables/table4_bivariate_correlations_ugt.xlsx")

# VIF and N summary
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
  model = c("UGT_InfoSeeking","UGT_Identity","UGT_Overall","UGT_Reflect","UGT_FeelSeen","UGT_Alternative"),
  N = c(nrow(data_info_seeking_reg), nrow(data_identity_reg), nrow(data_overall_reg), nrow(data_reflect_reg), nrow(data_feel_seen_reg), nrow(data_alternative_reg)),
  max_VIF = c(
    safe_max_vif(model_ugt_info_seeking),
    safe_max_vif(model_ugt_identity),
    safe_max_vif(model_ugt_overall),
    safe_max_vif(model_ugt_reflect),
    safe_max_vif(model_ugt_feel_seen),
    safe_max_vif(model_ugt_alternative)
  )
)
vif_tbl$n_VIF_gt5 <- c(
  safe_count_vif_gt5(model_ugt_info_seeking),
  safe_count_vif_gt5(model_ugt_identity),
  safe_count_vif_gt5(model_ugt_overall),
  safe_count_vif_gt5(model_ugt_reflect),
  safe_count_vif_gt5(model_ugt_feel_seen),
  safe_count_vif_gt5(model_ugt_alternative)
)
writexl::write_xlsx(vif_tbl, "outputs/reg_user/tables/tableA1_vif_and_model_n.xlsx")

cat("03a (UGT nonrecognition) script is ready. Run to generate outputs.\n")


