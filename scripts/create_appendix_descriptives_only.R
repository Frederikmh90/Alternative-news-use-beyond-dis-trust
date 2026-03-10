# ---- CREATE APPENDIX A0 DESCRIPTIVE TABLES ONLY ----
# Generates DV and non-recognition supplement tables (CSV + XLSX)
# Run from project root: source("scripts/create_appendix_descriptives_only.R")
# No dependency on car, psych, etc.

library(dplyr)
library(writexl)

source("scripts/data_prep.R")

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
  maxval <- max(x[sel], na.rm = TRUE)
  top2 <- (x >= maxval - 1) & sel
  100 * sum(w[top2]) / sum(w[sel])
}

# Weighted correlation matrix
wcor <- function(A, w) {
  A <- as.matrix(A)
  w <- as.numeric(w)
  sel <- complete.cases(A) & !is.na(w) & w > 0
  A <- A[sel, , drop = FALSE]
  w <- w[sel]
  mu <- colSums(A * w) / sum(w)
  A_center <- sweep(A, 2, mu, "-")
  cov_w <- (t(A_center * w) %*% A_center) / sum(w)
  sd_w <- sqrt(diag(cov_w))
  cov_w / (sd_w %o% sd_w)
}

dir.create("outputs/appendices", recursive = TRUE, showWarnings = FALSE)

# ---- 1. DV TABLE ----
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

dv_x <- c(
  list(msm_comp),
  list(as.numeric(q10_rev$truth_important_issues)), list(as.numeric(q10_rev$all_voices_heard)), list(as.numeric(q10_rev$one_sided_presentation)),
  list(alt_comp),
  list(as.numeric(q12_num$other_perspectives)), list(as.numeric(q12_num$not_covered_tradmedia)), list(as.numeric(q12_num$new_sources)),
  list(ugt_info_comp),
  list(as.numeric(q8_num$factcheck_news)), list(as.numeric(q8_num$alternative_perspectives)), list(as.numeric(q8_num$different_opinions)),
  list(ugt_id_comp),
  list(as.numeric(q8_num$feel_seen_understood)), list(as.numeric(q8_num$reflect_values))
)
dv_lab <- c("Mainstream media skepticism (composite)", "  MSM do not tell the truth", "  MSM do not let all voices be heard (R)", "  MSM present issues in a one-sided way",
            "Alternative news seeking (composite)", "  Other perspectives on topics covered in MSM", "  Topics not covered in MSM", "  News sources not usually used",
            "Gratification: Information Monitoring (composite)", "  News use to factcheck news", "  News use for alternative perspectives", "  News use to find out about the other side",
            "Gratification: Identity Confirmation (composite)", "  News use to feel seen and understood", "  News use to confirm own values and beliefs")

dv_supp <- data.frame(
  Variable = dv_lab,
  N = sapply(dv_x, function(x) sum(!is.na(x) & !is.na(w))),
  Mean = round(sapply(dv_x, function(x) wmean(x, w)), 2),
  SD = round(sapply(dv_x, function(x) wsd(x, w)), 2),
  Pct_agree_or_important = sprintf("%.1f%%", sapply(dv_x, function(x) wpct_top2(x, w))),
  stringsAsFactors = FALSE
)

write.csv(dv_supp, "outputs/appendices/APPENDIX_A_DV_full_descriptives.csv", row.names = FALSE, fileEncoding = "UTF-8")
write_xlsx(list(Table_A0_DV_full = dv_supp, Readme = data.frame(Note = "Appendix A0: Dependent variables. Pct = Agree/Strongly agree or Important/Very important.", stringsAsFactors = FALSE)),
           "outputs/appendices/APPENDIX_A_DV_full_descriptives.xlsx")
cat("Created: outputs/appendices/APPENDIX_A_DV_full_descriptives.csv\n")
cat("Created: outputs/appendices/APPENDIX_A_DV_full_descriptives.xlsx\n")

# ---- 2. NON-RECOGNITION TABLE ----
nr_num <- nonrecognition %>% select(nonrecog_care, nonrecog_equality, nonrecog_rights, nonrecog_esteem) %>% mutate(across(everything(), as.numeric))
nr_labels <- c("Non-recognition: Care", "Non-recognition: Rights (status)", "Non-recognition: Rights (capacity)", "Non-recognition: Esteem")
nr_vars <- c("nonrecog_care", "nonrecog_equality", "nonrecog_rights", "nonrecog_esteem")

nr_supp <- data.frame(
  Variable = nr_labels,
  N = sapply(nr_vars, function(v) sum(!is.na(nr_num[[v]]) & !is.na(w))),
  Mean = round(sapply(nr_vars, function(v) wmean(nr_num[[v]], w)), 2),
  SD = round(sapply(nr_vars, function(v) wsd(nr_num[[v]], w)), 2),
  Pct_agree_nonrecognition = sprintf("%.1f%%", sapply(nr_vars, function(v) wpct_top2(nr_num[[v]], w))),
  stringsAsFactors = FALSE
)

write.csv(nr_supp, "outputs/appendices/APPENDIX_A_Nonrecognition_descriptives.csv", row.names = FALSE, fileEncoding = "UTF-8")
write_xlsx(list(Table_A0_2_Nonrecognition = nr_supp, Readme = data.frame(Note = "Appendix A0.2: Non-recognition items. Pct = Agree/Strongly agree with feeling non-recognized.", stringsAsFactors = FALSE)),
           "outputs/appendices/APPENDIX_A_Nonrecognition_descriptives.xlsx")
cat("Created: outputs/appendices/APPENDIX_A_Nonrecognition_descriptives.csv\n")
cat("Created: outputs/appendices/APPENDIX_A_Nonrecognition_descriptives.xlsx\n")

# ---- 3. ALTERNATIVE NEWS ORIENTATION BY NON-RECOGNITION QUARTILES ----
# Same structure as Table 3 (trust quartiles) but for non-recognition dimensions
# Require complete Q12 (all 3 items) for alt_orient; compute quartiles per dimension
# so N reflects valid respondents for each dimension (handles missing per item)
q12_items <- c("other_perspectives", "not_covered_tradmedia", "new_sources")
q12_complete <- complete.cases(q12_num[, q12_items])
alt_orient_complete <- ifelse(q12_complete, alt_comp, NA_real_)

nr_quartile_tabs <- list()
for (i in seq_along(nr_vars)) {
  v <- nr_vars[i]
  df <- data.frame(
    nr_val = as.numeric(nr_num[[v]]),
    alt_orient = alt_orient_complete,
    w = w
  )
  df <- df[complete.cases(df) & df$w > 0, ]
  df$q <- paste0("Q", ntile(df$nr_val, 4))
  tab <- df %>% group_by(q) %>%
    summarise(
      Mean_SD = sprintf("%.2f (%.2f)", wmean(alt_orient, w), wsd(alt_orient, w)),
      N = n(),
      .groups = "drop"
    ) %>%
    mutate(Panel = nr_labels[i], Panel_var = v, Analysis_N = nrow(df))
  nr_quartile_tabs[[i]] <- tab
}
nr_quartile_all <- bind_rows(nr_quartile_tabs)

# Add quartile labels (Q1 = lowest non-recognition, Q4 = highest)
nr_quartile_all <- nr_quartile_all %>%
  mutate(Quartile = case_when(
    q == "Q1" ~ "Q1 (Lowest non-recognition)",
    q == "Q4" ~ "Q4 (Highest non-recognition)",
    TRUE ~ q
  ))

# Write to CSV and XLSX (one sheet per panel)

nr_quartile_wide <- nr_quartile_all %>%
  select(Panel, Quartile, Mean_SD, N) %>%
  arrange(Panel, Quartile)

write.csv(nr_quartile_wide, "outputs/appendices/APPENDIX_A_AltNews_by_Nonrecognition_quartiles.csv", row.names = FALSE, fileEncoding = "UTF-8")

# XLSX with one sheet per non-recognition dimension (Excel sheet names: no colons)
nr_sheet_names <- c("Care", "Rights_status", "Rights_capacity", "Esteem")
nr_quartile_sheets <- list()
for (i in seq_along(nr_labels)) {
  lab <- nr_labels[i]
  sh <- nr_sheet_names[i]
  nr_quartile_sheets[[sh]] <- nr_quartile_all %>%
    filter(Panel == lab) %>%
    select(Quartile, Mean_SD, N) %>%
    rename(`Mean alternative news orientation (SD)` = Mean_SD)
}
nr_quartile_sheets[["Readme"]] <- data.frame(
  Note = c(
    "Alternative news orientation by non-recognition quartiles (same structure as Table 3 for trust)",
    "Alternative news orientation = mean of Q12 items (complete on all 3 items); scale 1-5, higher = more frequent",
    "Q1 = lowest non-recognition (most recognized), Q4 = highest non-recognition (least recognized)",
    "N = unweighted count per quartile. Each panel uses complete cases on that non-recognition dimension + Q12 + weight.",
    "Missing: respondents with missing on any Q12 item or the non-recognition dimension are excluded from that panel."
  ),
  stringsAsFactors = FALSE
)
write_xlsx(nr_quartile_sheets, "outputs/appendices/APPENDIX_A_AltNews_by_Nonrecognition_quartiles.xlsx")
cat("Created: outputs/appendices/APPENDIX_A_AltNews_by_Nonrecognition_quartiles.csv\n")
cat("Created: outputs/appendices/APPENDIX_A_AltNews_by_Nonrecognition_quartiles.xlsx\n")

# ---- 3b. TABLE 3: Alt News by Trust + Non-recognition + DISRESPECT Quartiles ----
# Full Table 3 format: trust, non-recognition, and disrespect items
# Output: Mean (z), Delta_z (Q1-Q4), N per quartile
# Disrespect vars from recognition dataframe
dis_vars <- c("disrespect_denigration", "disrespect_exclusion", "disrespect_discrimination")
dis_labels <- c("Disrespect: Denigration", "Disrespect: Exclusion", "Disrespect: Discrimination")
dis_num <- recognition %>% select(all_of(dis_vars)) %>% mutate(across(everything(), as.numeric))

# Overall alt_orient mean and SD for z-scores (weighted, complete cases)
alt_pop <- alt_orient_complete[!is.na(alt_orient_complete) & !is.na(w) & w > 0]
alt_pop_w <- w[!is.na(alt_orient_complete) & !is.na(w) & w > 0]
alt_mean_pop <- wmean(alt_pop, alt_pop_w)
alt_sd_pop <- wsd(alt_pop, alt_pop_w)

quartile_row <- function(df, pred_var, label) {
  df <- df[complete.cases(df[, c(pred_var, "alt_orient")]) & df$w > 0, ]
  if (nrow(df) < 10) return(NULL)
  df$q <- paste0("Q", ntile(df[[pred_var]], 4))
  tab <- df %>% group_by(q) %>%
    summarise(
      mean_alt = wmean(alt_orient, w),
      z_alt = (wmean(alt_orient, w) - alt_mean_pop) / alt_sd_pop,
      n = n(),
      .groups = "drop"
    )
  q1_z <- tab$z_alt[tab$q == "Q1"]
  q4_z <- tab$z_alt[tab$q == "Q4"]
  delta_z <- if (length(q1_z) && length(q4_z)) q1_z - q4_z else NA_real_
  data.frame(
    Predictor = label,
    Q1 = sprintf("%.2f (%.2f)", tab$mean_alt[tab$q == "Q1"], tab$z_alt[tab$q == "Q1"]),
    Q2 = sprintf("%.2f (%.2f)", tab$mean_alt[tab$q == "Q2"], tab$z_alt[tab$q == "Q2"]),
    Q3 = sprintf("%.2f (%.2f)", tab$mean_alt[tab$q == "Q3"], tab$z_alt[tab$q == "Q3"]),
    Q4 = sprintf("%.2f (%.2f)", tab$mean_alt[tab$q == "Q4"], tab$z_alt[tab$q == "Q4"]),
    Delta_z = round(delta_z, 2),
    N = sprintf("%d/%d/%d/%d", tab$n[tab$q == "Q1"], tab$n[tab$q == "Q2"],
                tab$n[tab$q == "Q3"], tab$n[tab$q == "Q4"]),
    stringsAsFactors = FALSE
  )
}

# Trust quartiles (higher = more trust; Q1 = lowest trust)
tg_num <- trust_grouped %>% mutate(across(everything(), as.numeric))
tg_df <- data.frame(trust_system = tg_num$trust_system, trust_political = tg_num$trust_political,
                    alt_orient = alt_orient_complete, w = w)
trust_rows <- list(
  quartile_row(tg_df, "trust_system", "System trust"),
  quartile_row(tg_df, "trust_political", "Political trust")
)

# Non-recognition quartiles
nr_rows <- list()
for (i in seq_along(nr_vars)) {
  df <- data.frame(nr_val = as.numeric(nr_num[[nr_vars[i]]]), alt_orient = alt_orient_complete, w = w)
  nr_rows[[i]] <- quartile_row(df, "nr_val", nr_labels[i])
}

# Disrespect quartiles (higher = more disrespect; Q1 = lowest, Q4 = highest)
dis_rows <- list()
for (i in seq_along(dis_vars)) {
  df <- data.frame(dis_val = as.numeric(dis_num[[dis_vars[i]]]), alt_orient = alt_orient_complete, w = w)
  dis_rows[[i]] <- quartile_row(df, "dis_val", dis_labels[i])
}

# Combine into Table 3
table3_trust <- bind_rows(trust_rows)
table3_nr <- bind_rows(nr_rows)
table3_dis <- bind_rows(dis_rows)
table3_all <- bind_rows(
  table3_trust %>% mutate(Group = "Institutional trust (higher = more trust)"),
  table3_nr %>% mutate(Group = "Non-recognition (higher = more non-recognition)"),
  table3_dis %>% mutate(Group = "Disrespect (higher = more disrespect)")
)

dir.create("outputs/analysis/descriptive_tables", recursive = TRUE, showWarnings = FALSE)
write.csv(table3_all, "outputs/analysis/descriptive_tables/Table3_AltNews_by_quartiles_full.csv", row.names = FALSE, fileEncoding = "UTF-8")
write_xlsx(list(
  Table3_full = table3_all,
  Trust_only = table3_trust,
  Nonrecognition_only = table3_nr,
  Disrespect_only = table3_dis,
  Readme = data.frame(
    Note = c(
      "Table 3: Alternative news orientation by quartiles of trust, non-recognition, and disrespect (weighted; z-standardized)",
      "Format: Mean (z) where z = (quartile mean - population mean) / population SD",
      "Delta_z = z_Q1 - z_Q4. For trust: Q1=lowest trust, Q4=highest. For nonrecog/disrespect: Q1=lowest, Q4=highest.",
      "N = unweighted count per quartile (Q1/Q2/Q3/Q4)."
    ),
    stringsAsFactors = FALSE
  )
), "outputs/analysis/descriptive_tables/Table3_AltNews_by_quartiles_full.xlsx")
cat("Created: outputs/analysis/descriptive_tables/Table3_AltNews_by_quartiles_full.csv\n")
cat("Created: outputs/analysis/descriptive_tables/Table3_AltNews_by_quartiles_full.xlsx\n")

# ---- 4. CORRELATION MATRIX (recognition, trust, outcomes) ----
# Journal-ready compact correlation matrix for appendix
tg <- trust_grouped %>% mutate(across(everything(), as.numeric))
cor_vars <- bind_cols(
  nr_num,
  tg %>% select(trust_political, trust_system, trust_news_media, trust_citizens),
  MSM_skepticism = msm_comp,
  Alt_news_seeking = alt_comp
)
cor_mat <- wcor(cor_vars, w)

# Short labels for compact display
cor_labels <- c(
  "Care", "Rights (status)", "Rights (capacity)", "Esteem",
  "Trust political", "Trust system", "Trust media", "Trust citizens",
  "MSM skepticism", "Alt. news seeking"
)
rownames(cor_mat) <- colnames(cor_mat) <- cor_labels

# Round to 2 decimals
cor_mat_round <- round(cor_mat, 2)

# Lower triangle (compact journal format): upper triangle = blank
cor_compact <- cor_mat_round
cor_compact[upper.tri(cor_compact, diag = FALSE)] <- NA

# Full matrix for CSV/XLSX
cor_df <- as.data.frame(cor_mat_round)
cor_df <- cbind(Variable = rownames(cor_df), cor_df)

write.csv(cor_df, "outputs/appendices/APPENDIX_A_Correlation_matrix.csv", row.names = FALSE, fileEncoding = "UTF-8")

# XLSX: full matrix + compact (lower triangle) + readme
cor_compact_df <- as.data.frame(cor_compact)
cor_compact_df <- cbind(Variable = rownames(cor_compact_df), cor_compact_df)

write_xlsx(list(
  Full_matrix = cor_df,
  Compact_lower_triangle = cor_compact_df,
  Readme = data.frame(
    Note = c(
      "Appendix A: Correlation matrix (recognition dimensions, trust, outcomes)",
      "Weighted Pearson correlations; survey weights applied. Pairwise complete cases.",
      "Compact sheet: lower triangle only (diagonal = 1.00). Full matrix on first sheet.",
      "Care–Esteem = non-recognition dimensions (higher = less recognized).",
      "MSM skepticism = Q10 reversed composite; Alt. news seeking = Q12 composite."
    ),
    stringsAsFactors = FALSE
  )
), "outputs/appendices/APPENDIX_A_Correlation_matrix.xlsx")
cat("Created: outputs/appendices/APPENDIX_A_Correlation_matrix.csv\n")
cat("Created: outputs/appendices/APPENDIX_A_Correlation_matrix.xlsx\n")

cat("\nDone. All 8 files are in outputs/appendices/\n")
