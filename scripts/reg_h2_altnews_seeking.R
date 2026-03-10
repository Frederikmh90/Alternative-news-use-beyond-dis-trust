# ---- REGRESSION ANALYSIS (Nonrecognition): Alternative News Habits ----
# Mirrors 06_reg_altnews.R but swaps recog_* with nonrecog_* and writes
# outputs to separate nonrecog folders.

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

# EFA for Q12 (unchanged)
q12_numeric <- Q12_media_altnews %>% mutate(across(everything(), as.numeric))
q12_cor <- cor(q12_numeric, use = "complete.obs")
fa.parallel(q12_numeric, fm = "ml", fa = "fa", n.iter = 100)
fa_1 <- fa(q12_numeric, nfactors = 1, rotate = "varimax", fm = "ml")
q12_factor_scores <- fa_1$scores; colnames(q12_factor_scores) <- "q12_altnews_factor"
q12_numeric_for_composites <- Q12_media_altnews %>% mutate(across(everything(), as.numeric))
q12_overall_composite <- q12_numeric_for_composites %>% rowMeans(na.rm = TRUE)
q12_other_perspectives <- q12_numeric_for_composites$other_perspectives
q12_not_covered <- q12_numeric_for_composites$not_covered_tradmedia
q12_new_sources <- q12_numeric_for_composites$new_sources

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
  trust_grouped,
  controls %>% dplyr::select(-political_ideology, -political_ideology_simple, -fringe_vs_mainstream, -education, -income, left_right_scale, follow_politics_society)
)

# Datasets
data_factor <- bind_cols(q12_factor_scores = q12_factor_scores, predictors_data)
data_overall <- bind_cols(q12_overall = q12_overall_composite, predictors_data)
data_other_perspectives <- bind_cols(q12_other_perspectives = q12_other_perspectives, predictors_data)
data_not_covered <- bind_cols(q12_not_covered = q12_not_covered, predictors_data)
data_new_sources <- bind_cols(q12_new_sources = q12_new_sources, predictors_data)

data_factor_complete <- data_factor %>% filter(complete.cases(.))
data_overall_complete <- data_overall %>% filter(complete.cases(.))
data_other_perspectives_complete <- data_other_perspectives %>% filter(complete.cases(.))
data_not_covered_complete <- data_not_covered %>% filter(complete.cases(.))
data_new_sources_complete <- data_new_sources %>% filter(complete.cases(.))

prep <- function(df, target){
  df %>% mutate(
    education_group = relevel(factor(as.character(education_group), levels = c("vocational", "basic_upsecondary", "+higher")), ref = "vocational"),  # Reference: vocational
    age = relevel(as.factor(age), ref = "35-49"),  # Reference: 35-49 (middle category)
    gender = as.factor(gender),
    income_group = relevel(as.factor(income_group), ref = "mid"),  # Reference: mid (middle category)
    across(c({{target}}, starts_with("nonrecog_"), starts_with("disrespect_"), starts_with("trust_"), left_right_scale, follow_politics_society), as.numeric)
  )
}

data_factor_reg <- prep(data_factor_complete, q12_factor_scores)
data_overall_reg <- prep(data_overall_complete, q12_overall)
data_other_perspectives_reg <- prep(data_other_perspectives_complete, q12_other_perspectives)
data_not_covered_reg <- prep(data_not_covered_complete, q12_not_covered)
data_new_sources_reg <- prep(data_new_sources_complete, q12_new_sources)

# Attach weights aligned with complete cases
if ("analysis_weight" %in% names(controls)) {
  data_factor_reg$analysis_weight <- controls$analysis_weight[complete.cases(data_factor)]
  data_overall_reg$analysis_weight <- controls$analysis_weight[complete.cases(data_overall)]
  data_other_perspectives_reg$analysis_weight <- controls$analysis_weight[complete.cases(data_other_perspectives)]
  data_not_covered_reg$analysis_weight <- controls$analysis_weight[complete.cases(data_not_covered)]
  data_new_sources_reg$analysis_weight <- controls$analysis_weight[complete.cases(data_new_sources)]
}

# Models
model_q12_factor <- lm(q12_factor_scores ~ . - analysis_weight, data = data_factor_reg, weights = analysis_weight)
model_q12_overall <- lm(q12_overall ~ . - analysis_weight, data = data_overall_reg, weights = analysis_weight)
model_q12_other_perspectives <- lm(q12_other_perspectives ~ . - analysis_weight, data = data_other_perspectives_reg, weights = analysis_weight)
model_q12_not_covered <- lm(q12_not_covered ~ . - analysis_weight, data = data_not_covered_reg, weights = analysis_weight)
model_q12_new_sources <- lm(q12_new_sources ~ . - analysis_weight, data = data_new_sources_reg, weights = analysis_weight)

# Extract results and save plots (heteroskedasticity-robust HC3 SEs, survey weights applied)
extract_model_results <- function(model, model_name){
  ct <- lmtest::coeftest(model, vcov = sandwich::vcovHC(model, type = "HC3"))
  data.frame(Variable = rownames(ct), Coefficient = ct[,1], Std_Error = ct[,2], T_Value = ct[,3], P_Value = ct[,4], Model = model_name, stringsAsFactors = FALSE)
}

all_results <- dplyr::bind_rows(
  extract_model_results(model_q12_factor, "Q12_Factor"),
  extract_model_results(model_q12_overall, "Q12_Overall"),
  extract_model_results(model_q12_other_perspectives, "Q12_OtherPerspectives"),
  extract_model_results(model_q12_not_covered, "Q12_NotCovered"),
  extract_model_results(model_q12_new_sources, "Q12_NewSources")
)

plot_one <- function(results, model_name, save_path){
  df <- results %>% dplyr::filter(Model == model_name, Variable != "(Intercept)") %>%
    mutate(Variable_Clean = Variable %>% gsub("^fringe_vs_mainstreamfringe$", "Fringe vote", .) %>% gsub("recog_", "Recognition: ", .) %>% gsub("nonrecog_", "Nonrecognition: ", .) %>% gsub("disrespect_", "Disrespect: ", .) %>% gsub("trust_", "Trust: ", .) %>% gsub("_", " ", .) %>% tools::toTitleCase())
  p <- ggplot(df, aes(x = reorder(Variable_Clean, Coefficient), y = Coefficient)) + geom_point(size = 3, aes(color = P_Value < 0.05)) +
    geom_errorbar(aes(ymin = Coefficient - 1.96*Std_Error, ymax = Coefficient + 1.96*Std_Error), width = 0.2) + geom_hline(yintercept=0, linetype="dashed", color="red") + coord_flip() + theme_minimal() + theme(legend.position = "none") + labs(title=paste("Coefficient Plot:", model_name), x=NULL, y="Coefficient (95% CI)")
  ggsave(paste0(save_path, "coefficient_plot_", tolower(gsub(" ", "_", model_name)), ".png"), p, width=12, height=8, dpi=300)
}

# Save outputs
dir.create("outputs/reg_altnews_nonrecog/models", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/reg_altnews_nonrecog/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/reg_altnews_nonrecog/plots", recursive = TRUE, showWarnings = FALSE)

saveRDS(model_q12_factor, "outputs/reg_altnews_nonrecog/models/model_q12_factor.rds")
saveRDS(model_q12_overall, "outputs/reg_altnews_nonrecog/models/model_q12_overall.rds")
saveRDS(model_q12_other_perspectives, "outputs/reg_altnews_nonrecog/models/model_q12_other_perspectives.rds")
saveRDS(model_q12_not_covered, "outputs/reg_altnews_nonrecog/models/model_q12_not_covered.rds")
saveRDS(model_q12_new_sources, "outputs/reg_altnews_nonrecog/models/model_q12_new_sources.rds")

write.csv(all_results, "outputs/reg_altnews_nonrecog/tables/appendix_comprehensive_q12_nonrecog_results.csv", row.names = FALSE)

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

# Generate article table for priority model (Q12 factor only)
cat("\nGenerating article-ready table for priority model...\n")

article_table_q12 <- create_article_table(model_q12_factor, "Q12 Alternative News Factor")
write.csv(article_table_q12, "outputs/reg_altnews_nonrecog/tables/article_table_q12_factor.csv", row.names = FALSE)
cat("✓ Article table saved: Q12 Alternative News Factor\n")
pp <- "outputs/reg_altnews_nonrecog/plots/"
plot_one(all_results, "Q12_Factor", pp)
plot_one(all_results, "Q12_Overall", pp)
plot_one(all_results, "Q12_OtherPerspectives", pp)
plot_one(all_results, "Q12_NotCovered", pp)
plot_one(all_results, "Q12_NewSources", pp)

cat("06a (Altnews nonrecognition) script is ready. Run to generate outputs.\n")


# ---- Publication tables (weighted) ----
dir.create("outputs/reg_altnews_nonrecog/tables", recursive = TRUE, showWarnings = FALSE)

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
writexl::write_xlsx(sample_chars_df, "outputs/reg_altnews_nonrecog/tables/table1_sample_characteristics.xlsx")

# Factor/construct summary for Q12
q12_kmo <- tryCatch({ psych::KMO(q12_numeric) $MSA }, error = function(e) NA_real_)
q12_bart <- tryCatch({ psych::cortest.bartlett(q12_numeric, n = nrow(q12_numeric))$p.value }, error = function(e) NA_real_)
q12_var <- tryCatch({ sum(fa_1$Vaccounted[2,]) }, error = function(e) NA_real_)
q12_alpha <- tryCatch({ psych::alpha(q12_numeric)$total$raw_alpha }, error = function(e) NA_real_)
factor_summary <- data.frame(
  construct = c("Q12 (Alternative news habits)"),
  items = paste(colnames(q12_numeric), collapse = "; "),
  KMO = round(q12_kmo, 3),
  Bartlett_p = round(q12_bart, 6),
  Variance_Explained = round(q12_var, 3),
  Alpha = round(q12_alpha, 3),
  N = nrow(q12_numeric)
)
writexl::write_xlsx(factor_summary, "outputs/reg_altnews_nonrecog/tables/table2_factor_reliability_summary.xlsx")

# Descriptives for regression variables
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
writexl::write_xlsx(descriptives_combined, "outputs/reg_altnews_nonrecog/tables/table3_descriptives_regression_variables.xlsx")

# Weighted bivariate correlations
cor_df <- dplyr::bind_cols(
  q12_factor_scores = as.numeric(q12_factor_scores),
  to_numeric(nonrecognition[,nr_vars]),
  to_numeric(recognition[,dis_vars]),
  trust_grouped
)
# Manual weighted correlation function
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
writexl::write_xlsx(as.data.frame(cor_mat), "outputs/reg_altnews_nonrecog/tables/table4_bivariate_correlations_q12.xlsx")

# VIF & N (with error handling for aliased coefficients)
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
  model = c("Q12_Factor","Q12_Overall","Q12_OtherPerspectives","Q12_NotCovered","Q12_NewSources"),
  N = c(nrow(data_factor_reg), nrow(data_overall_reg), nrow(data_other_perspectives_reg), nrow(data_not_covered_reg), nrow(data_new_sources_reg)),
  max_VIF = c(
    safe_max_vif(model_q12_factor),
    safe_max_vif(model_q12_overall),
    safe_max_vif(model_q12_other_perspectives),
    safe_max_vif(model_q12_not_covered),
    safe_max_vif(model_q12_new_sources)
  )
)
vif_tbl$n_VIF_gt5 <- c(
  safe_count_vif_gt5(model_q12_factor),
  safe_count_vif_gt5(model_q12_overall),
  safe_count_vif_gt5(model_q12_other_perspectives),
  safe_count_vif_gt5(model_q12_not_covered),
  safe_count_vif_gt5(model_q12_new_sources)
)
writexl::write_xlsx(vif_tbl, "outputs/reg_altnews_nonrecog/tables/tableA1_vif_and_model_n.xlsx")


