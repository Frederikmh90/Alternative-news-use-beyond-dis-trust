# ---- DESCRIPTIVE STATISTICS TABLES (WITH MEANS & PERCENTAGES) ----
# Table 1: Independent Variables (Predictors)
# Table 2: Dependent Variables (Outcomes from 03a, 03b, 03c)
# 
# MODIFIED VERSION: Uses simple MEANS instead of factor scores
# and adds "% Agree + Strongly Agree" column

library(haven)
library(dplyr)
library(psych)
library(writexl)

source("scripts/data_prep.R")

cat("=================================================================\n")
cat("CREATING DESCRIPTIVE STATISTICS TABLES (MEANS VERSION)\n")
cat("=================================================================\n\n")

weights <- controls$analysis_weight

# =================================================================
# HELPER FUNCTIONS
# =================================================================

# Weighted mean
wmean <- function(x, w) {
  x <- as.numeric(x)
  w <- as.numeric(w)
  sel <- !is.na(x) & !is.na(w)
  if (!any(sel)) return(NA_real_)
  sum(w[sel] * x[sel]) / sum(w[sel])
}

# Weighted SD
wsd <- function(x, w) {
  x <- as.numeric(x)
  w <- as.numeric(w)
  sel <- !is.na(x) & !is.na(w)
  if (!any(sel)) return(NA_real_)
  mu <- sum(w[sel] * x[sel]) / sum(w[sel])
  sqrt(sum(w[sel] * (x[sel] - mu)^2) / sum(w[sel]))
}

# Weighted percentage for "Agree + Strongly Agree" 
# For 5-point scales: 4 = agree, 5 = strongly agree
# For 4-point scales (Q8): 3 = important, 4 = very important
# For 10-point scales (left-right): 7-10 = right
wpct_agree <- function(x, w, scale_max = 5, threshold = NULL) {
  x <- as.numeric(x)
  w <- as.numeric(w)
  sel <- !is.na(x) & !is.na(w)
  if (!any(sel)) return(NA_real_)
  
  # Use custom threshold if provided, otherwise determine based on scale
  if (!is.null(threshold)) {
    agree_idx <- sel & (x >= threshold)
  } else if (scale_max == 4) {
    # For 4-point scales: 3 and 4 are "agree"
    agree_idx <- sel & (x >= 3)
  } else if (scale_max == 10) {
    # For 10-point scales: 7-10 are "high end"
    agree_idx <- sel & (x >= 7)
  } else {
    # For 5-point scales: 4 and 5 are "agree"
    agree_idx <- sel & (x >= 4)
  }
  
  if (!any(agree_idx)) return(0)
  
  pct <- sum(w[agree_idx]) / sum(w[sel]) * 100
  return(pct)
}

# Descriptive row for numeric variables (with % agree)
desc_numeric_row <- function(x, w, var_name, scale_max = 5) {
  n_valid <- sum(!is.na(x))
  n_missing <- sum(is.na(x))
  
  data.frame(
    Variable = var_name,
    Count = n_valid,
    Mean_SD = sprintf("%.2f (%.2f)", wmean(x, w), wsd(x, w)),
    Pct_Agree = sprintf("%.1f%%", wpct_agree(x, w, scale_max)),
    N_Missing = n_missing,
    stringsAsFactors = FALSE
  )
}

# Weighted proportions for categorical variables
weighted_prop_table <- function(f, w, var_name) {
  f <- as.factor(f)
  lv <- levels(f)
  total_weight <- sum(w, na.rm = TRUE)
  
  results <- lapply(lv, function(l) {
    idx <- which(f == l)
    wt_count <- sum(w[idx], na.rm = TRUE)
    wt_prop <- wt_count / total_weight
    
    data.frame(
      Variable = var_name,
      Category = as.character(l),
      Count = length(idx),
      Percentage = sprintf("%.1f%%", wt_prop * 100),
      N_Missing = NA,
      stringsAsFactors = FALSE
    )
  })
  
  # Add missing row if there are missing values
  idx_missing <- which(is.na(f))
  if (length(idx_missing) > 0) {
    wt_count_missing <- sum(w[idx_missing], na.rm = TRUE)
    wt_prop_missing <- wt_count_missing / total_weight
    
    results[[length(results) + 1]] <- data.frame(
      Variable = var_name,
      Category = "Missing",
      Count = length(idx_missing),
      Percentage = sprintf("%.1f%%", wt_prop_missing * 100),
      N_Missing = length(idx_missing),
      stringsAsFactors = FALSE
    )
  }
  
  dplyr::bind_rows(results)
}

# =================================================================
# TABLE 1A: INDEPENDENT VARIABLES (CATEGORICAL - Full breakdown)
# =================================================================
cat("=================================================================\n")
cat("TABLE 1A: Independent Variables - Categorical Breakdown\n")
cat("=================================================================\n\n")

table1a_list <- list()

# --- Nonrecognition Variables ---
cat("Processing nonrecognition variables...\n")
nonrecog_vars <- c("nonrecog_care", "nonrecog_equality", "nonrecog_rights", "nonrecog_esteem")
for (var in nonrecog_vars) {
  table1a_list[[length(table1a_list) + 1]] <- weighted_prop_table(
    nonrecognition[[var]], weights, var
  )
}

# --- Disrespect Variables ---
cat("Processing disrespect variables...\n")
disrespect_vars <- c("disrespect_denigration", "disrespect_exclusion", "disrespect_discrimination")
for (var in disrespect_vars) {
  table1a_list[[length(table1a_list) + 1]] <- weighted_prop_table(
    recognition[[var]], weights, var
  )
}

# --- Control Variables (Categorical only) ---
cat("Processing categorical control variables...\n")

# Age (categorical)
table1a_list[[length(table1a_list) + 1]] <- weighted_prop_table(
  controls$age, weights, "age"
)

# Gender (categorical)
table1a_list[[length(table1a_list) + 1]] <- weighted_prop_table(
  controls$gender, weights, "gender"
)

# Education (categorical)
table1a_list[[length(table1a_list) + 1]] <- weighted_prop_table(
  controls$education_group, weights, "education_group"
)

# Income (categorical)
table1a_list[[length(table1a_list) + 1]] <- weighted_prop_table(
  controls$income_group, weights, "income_group"
)

# Follow Politics/Society (categorical) - get from politics_news dataframe
table1a_list[[length(table1a_list) + 1]] <- weighted_prop_table(
  politics_news$follow_politics_society, weights, "follow_politics_society"
)

# Combine Table 1A
table1a <- dplyr::bind_rows(table1a_list)

# Clean up display
table1a$Variable <- gsub("_", " ", table1a$Variable)
table1a$Variable <- tools::toTitleCase(table1a$Variable)

cat("\nTable 1A Preview (first 20 rows):\n")
print(head(table1a, 20))
cat(sprintf("\nTotal rows in Table 1A: %d\n\n", nrow(table1a)))

# =================================================================
# TABLE 1B: INDEPENDENT VARIABLES (NUMERIC SUMMARY - Like Table 2)
# =================================================================
cat("=================================================================\n")
cat("TABLE 1B: Independent Variables - Numeric Summary\n")
cat("=================================================================\n\n")

table1b_list <- list()

# --- Nonrecognition Variables (as numeric with % agree) ---
cat("Processing nonrecognition variables (numeric summary)...\n")
for (var in nonrecog_vars) {
  x_numeric <- as.numeric(nonrecognition[[var]])
  table1b_list[[length(table1b_list) + 1]] <- desc_numeric_row(
    x_numeric, weights, var, scale_max = 5
  )
}

# --- Disrespect Variables (as numeric with % agree) ---
cat("Processing disrespect variables (numeric summary)...\n")
for (var in disrespect_vars) {
  x_numeric <- as.numeric(recognition[[var]])
  table1b_list[[length(table1b_list) + 1]] <- desc_numeric_row(
    x_numeric, weights, var, scale_max = 5
  )
}

# --- Trust Variables (Composite - already numeric) ---
cat("Processing trust variables (composites)...\n")
trust_vars <- c("trust_political", "trust_system", "trust_news_media", "trust_citizens")
for (var in trust_vars) {
  x_numeric <- as.numeric(trust_grouped[[var]])
  table1b_list[[length(table1b_list) + 1]] <- desc_numeric_row(
    x_numeric, weights, var, scale_max = 5
  )
}

# --- Control Variables (Numeric only) ---
cat("Processing numeric control variables...\n")

# Left-Right Scale (1-10 scale: % right-leaning = 7-10)
lr_scale <- as.numeric(controls$left_right_scale)
table1b_list[[length(table1b_list) + 1]] <- data.frame(
  Variable = "left_right_scale",
  Count = sum(!is.na(lr_scale)),
  Mean_SD = sprintf("%.2f (%.2f)", wmean(lr_scale, weights), wsd(lr_scale, weights)),
  Pct_Agree = sprintf("%.1f%% (right: 7-10)", wpct_agree(lr_scale, weights, scale_max = 10)),
  N_Missing = sum(is.na(lr_scale)),
  stringsAsFactors = FALSE
)

# Follow Politics/Society (numeric with % agree)
table1b_list[[length(table1b_list) + 1]] <- desc_numeric_row(
  as.numeric(controls$follow_politics_society), weights, "follow_politics_society", scale_max = 5
)

# Combine Table 1B
table1b <- dplyr::bind_rows(table1b_list)

# Clean up display
table1b$Variable <- gsub("_", " ", table1b$Variable)
table1b$Variable <- tools::toTitleCase(table1b$Variable)

cat("\nTable 1B Preview:\n")
print(table1b)
cat(sprintf("\nTotal rows in Table 1B: %d\n\n", nrow(table1b)))

# =================================================================
# TABLE 2: DEPENDENT VARIABLES (OUTCOMES) - USING MEANS
# =================================================================
cat("=================================================================\n")
cat("TABLE 2: Dependent Variables (Regression Outcomes)\n")
cat("Uses MEANS (simple averages) instead of factor scores\n")
cat("=================================================================\n\n")

table2_list <- list()

# --- 03a: UGT Variables (USING MEANS) ---
cat("Processing 03a outcomes (User Gratification) - MEANS...\n")

q8_numeric <- Q8_media_altnews_motivation %>% mutate(across(everything(), as.numeric))

# NOTE: Q8 is on a 4-point scale (1-4), so scale_max = 4

# UGT Info Seeking = MEAN of factcheck_news + alternative_perspectives + different_opinions
ugt_info_items <- q8_numeric %>% dplyr::select(factcheck_news, alternative_perspectives, different_opinions)
ugt_info_mean <- rowMeans(ugt_info_items, na.rm = TRUE)

table2_list[[length(table2_list) + 1]] <- desc_numeric_row(
  ugt_info_mean, weights, "03a: UGT Info Seeking (Mean)", scale_max = 4
)

# UGT Identity Seeking = Just feel_seen_understood item
ugt_identity_mean <- q8_numeric$feel_seen_understood

table2_list[[length(table2_list) + 1]] <- desc_numeric_row(
  ugt_identity_mean, weights, "03a: UGT Identity Seeking (Mean)", scale_max = 4
)

# ALL Individual Q8 items (7 items total)
table2_list[[length(table2_list) + 1]] <- desc_numeric_row(
  q8_numeric$reflect_values, weights, "03a: Q8 - Reflect my values", scale_max = 4
)

table2_list[[length(table2_list) + 1]] <- desc_numeric_row(
  q8_numeric$feel_seen_understood, weights, "03a: Q8 - Feel seen/understood", scale_max = 4
)

table2_list[[length(table2_list) + 1]] <- desc_numeric_row(
  q8_numeric$alternative_perspectives, weights, "03a: Q8 - Alternative perspectives", scale_max = 4
)

table2_list[[length(table2_list) + 1]] <- desc_numeric_row(
  q8_numeric$factcheck_news, weights, "03a: Q8 - Factcheck news", scale_max = 4
)

table2_list[[length(table2_list) + 1]] <- desc_numeric_row(
  q8_numeric$different_opinions, weights, "03a: Q8 - Different opinions", scale_max = 4
)

table2_list[[length(table2_list) + 1]] <- desc_numeric_row(
  q8_numeric$change_society, weights, "03a: Q8 - Change society", scale_max = 4
)

# --- 03b: Q12 Alternative News Variables (USING MEANS) ---
cat("Processing 03b outcomes (Alternative News Q12) - MEANS...\n")

q12_numeric <- Q12_media_altnews %>% mutate(across(everything(), as.numeric))

# Q12 Mean = average of all three items
q12_mean <- rowMeans(q12_numeric, na.rm = TRUE)

table2_list[[length(table2_list) + 1]] <- desc_numeric_row(
  q12_mean, weights, "03b: Q12 Alt News (Mean)"
)

# Individual Q12 items
table2_list[[length(table2_list) + 1]] <- desc_numeric_row(
  q12_numeric$other_perspectives, weights, "03b: Q12 - Other perspectives"
)

table2_list[[length(table2_list) + 1]] <- desc_numeric_row(
  q12_numeric$not_covered_tradmedia, weights, "03b: Q12 - Not covered in MSM"
)

table2_list[[length(table2_list) + 1]] <- desc_numeric_row(
  q12_numeric$new_sources, weights, "03b: Q12 - New sources"
)

# --- 03c: Q10 MSM Rejection Variables (USING MEANS) ---
cat("Processing 03c outcomes (MSM Rejection Q10) - MEANS...\n")

# Reverse Q10 items (IMPORTANT!)
q10_numeric <- Q10_media_mainstreamnews %>% 
  mutate(across(everything(), as.numeric)) %>%
  mutate(across(everything(), ~ 6 - .))  # Reverse coding

# Q10 Mean = average of all three reversed items
q10_mean <- rowMeans(q10_numeric, na.rm = TRUE)

table2_list[[length(table2_list) + 1]] <- desc_numeric_row(
  q10_mean, weights, "03c: Q10 MSM Rejection (Mean)"
)

# Individual Q10 items (reversed)
table2_list[[length(table2_list) + 1]] <- desc_numeric_row(
  q10_numeric$truth_important_issues, weights, "03c: Q10 - Truth rejected (R)"
)

table2_list[[length(table2_list) + 1]] <- desc_numeric_row(
  q10_numeric$all_voices_heard, weights, "03c: Q10 - Voices rejected (R)"
)

table2_list[[length(table2_list) + 1]] <- desc_numeric_row(
  q10_numeric$one_sided_presentation, weights, "03c: Q10 - One-sided agreed (R)"
)

# Combine Table 2
table2 <- dplyr::bind_rows(table2_list)

cat("\nTable 2 Preview:\n")
print(table2)
cat(sprintf("\nTotal rows in Table 2: %d\n\n", nrow(table2)))

# =================================================================
# SAVE TABLES
# =================================================================
cat("=================================================================\n")
cat("Saving Tables\n")
cat("=================================================================\n\n")

dir.create("outputs/analysis/descriptive_tables", recursive = TRUE, showWarnings = FALSE)

# Save as Excel
write_xlsx(
  list(
    "1A_Indep_Categorical" = table1a,
    "1B_Indep_Numeric" = table1b,
    "2_Dependent" = table2
  ),
  "outputs/analysis/descriptive_tables/descriptive_statistics_tables_MEANS.xlsx"
)

# Save as CSV
write.csv(table1a, "outputs/analysis/descriptive_tables/table1a_independent_categorical.csv", row.names = FALSE)
write.csv(table1b, "outputs/analysis/descriptive_tables/table1b_independent_numeric.csv", row.names = FALSE)
write.csv(table2, "outputs/analysis/descriptive_tables/table2_dependent_variables_MEANS.csv", row.names = FALSE)

cat("✓ Tables saved:\n")
cat("  - outputs/analysis/descriptive_tables/descriptive_statistics_tables_MEANS.xlsx (3 sheets)\n")
cat("    • Sheet 1A: Independent Variables - Categorical breakdown\n")
cat("    • Sheet 1B: Independent Variables - Numeric summary (like Table 2)\n")
cat("    • Sheet 2: Dependent Variables\n")
cat("  - outputs/analysis/descriptive_tables/table1a_independent_categorical.csv\n")
cat("  - outputs/analysis/descriptive_tables/table1b_independent_numeric.csv\n")
cat("  - outputs/analysis/descriptive_tables/table2_dependent_variables_MEANS.csv\n\n")

# =================================================================
# SUMMARY
# =================================================================
cat("=================================================================\n")
cat("SUMMARY\n")
cat("=================================================================\n\n")

cat("TABLE 1A: Independent Variables - Categorical Breakdown\n")
cat(sprintf("  Total rows: %d\n", nrow(table1a)))
cat("  Shows full category breakdown with weighted percentages\n\n")

cat("TABLE 1B: Independent Variables - Numeric Summary\n")
cat(sprintf("  Total rows: %d\n", nrow(table1b)))
cat("  Variable groups:\n")
cat("    - Nonrecognition: 4 variables\n")
cat("    - Disrespect: 3 variables\n")
cat("    - Trust (composites): 4 variables\n")
cat("    - Controls (numeric): 2 variables\n\n")

cat("TABLE 2: Dependent Variables\n")
cat(sprintf("  Total rows: %d\n", nrow(table2)))
cat("  Variable groups:\n")
cat("    - 03a (Q8 UGT): 9 variables (2 composites + 7 individual items)\n")
cat("    - 03b (Q12 Alt News): 4 variables (1 composite + 3 items)\n")
cat("    - 03c (Q10 MSM): 4 variables (1 composite + 3 items)\n\n")

cat("KEY FEATURES:\n")
cat("  - All numeric summaries show: Count, Mean (SD), % Agree/High\n")
cat("  - Trust composites: % High = proportion with score ≥4 (high trust)\n")
cat("  - Nonrecog/Disrespect: % Agree = proportion responding 4 or 5\n")
cat("  - Q8 items: 4-point scale (% = Important + Very important)\n")
cat("  - Q10, Q12 items: 5-point scale (% = Agree + Strongly agree)\n")
cat("  - Left-Right scale: 0-10 scale (% = 7-10 = right-leaning)\n\n")

cat("=================================================================\n")
cat("COMPLETE\n")
cat("=================================================================\n")

