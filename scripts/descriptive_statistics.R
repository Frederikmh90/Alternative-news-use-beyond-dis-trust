# ---- DESCRIPTIVE STATISTICS (Publication-ready, weighted) ----

library(dplyr)
library(psych)

# Load data management for recodes, controls, and frames
source("scripts/data_prep.R")

# Helpers (no external dependencies)
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
weighted_prop_table <- function(f, w) {
  lv <- levels(f)
  out <- lapply(lv, function(l) {
    idx <- which(f == l)
    c(level = l,
      count = sum(w[idx], na.rm = TRUE),
      proportion = sum(w[idx], na.rm = TRUE) / sum(w[!is.na(f)], na.rm = TRUE))
  })
  out <- as.data.frame(do.call(rbind, out), stringsAsFactors = FALSE)
  out$count <- as.numeric(out$count)
  out$proportion <- as.numeric(out$proportion)
  out
}
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
to_numeric <- function(df){ df %>% dplyr::mutate(across(everything(), as.numeric)) }

# Output roots
dir.create("outputs/descriptives", recursive = TRUE, showWarnings = FALSE)

# 0) Sample characteristics (weighted)
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
writexl::write_xlsx(sample_chars_df, "outputs/descriptives/table1_sample_characteristics.xlsx")

# 1) Factor/construct summaries for Q8, Q10, Q12
ugt_numeric <- Q8_media_altnews_motivation %>% mutate(across(everything(), as.numeric)) %>% dplyr::select(-full_picture)
q10_numeric <- Q10_media_mainstreamnews %>% mutate(across(everything(), as.numeric))
q12_numeric <- Q12_media_altnews %>% mutate(across(everything(), as.numeric))

fa_ugt <- tryCatch(psych::fa(ugt_numeric, nfactors = 2, fm = "ml", rotate = "varimax"), error = function(e) NULL)
fa_q10 <- tryCatch(psych::fa(q10_numeric, nfactors = 1, fm = "ml", rotate = "varimax"), error = function(e) NULL)
fa_q12 <- tryCatch(psych::fa(q12_numeric, nfactors = 1, fm = "ml", rotate = "varimax"), error = function(e) NULL)

tab_factor <- function(num_df, fa_obj, label){
  data.frame(
    construct = label,
    items = paste(colnames(num_df), collapse = "; "),
    KMO = round(tryCatch(psych::KMO(num_df)$MSA, error=function(e) NA_real_), 3),
    Bartlett_p = round(tryCatch(psych::cortest.bartlett(num_df, n = nrow(num_df))$p.value, error=function(e) NA_real_), 6),
    Variance_Explained = round(tryCatch(sum(fa_obj$Vaccounted[2,]), error=function(e) NA_real_), 3),
    Alpha = round(tryCatch(psych::alpha(num_df)$total$raw_alpha, error=function(e) NA_real_), 3),
    N = nrow(num_df)
  )
}

factor_summary <- bind_rows(
  tab_factor(ugt_numeric, fa_ugt, "UGT (Q8 items)"),
  tab_factor(q10_numeric, fa_q10, "Q10 (Mainstream news)"),
  tab_factor(q12_numeric, fa_q12, "Q12 (Alternative news habits)")
)
writexl::write_xlsx(factor_summary, "outputs/descriptives/table2_factor_reliability_summary.xlsx")

# 2) Descriptives for regression predictors (nonrecognition, disrespect, trust, controls)
nr_vars <- c("nonrecog_care","nonrecog_equality","nonrecog_esteem","nonrecog_value_society")
dis_vars <- c("disrespect_misperception","disrespect_denigration","disrespect_exclusion","disrespect_discrimination")
trust_vars <- c("trust_citizens","trust_political","trust_system","trust_news_media")

desc_factor <- function(df, w){
  vars <- names(df)
  out <- lapply(vars, function(v){
    if(is.factor(df[[v]])||is.ordered(df[[v]])){
      tmp <- weighted_prop_table(df[[v]], w); tmp$variable <- v; tmp
    } else { NULL }
  })
  dplyr::bind_rows(out) %>% dplyr::select(variable, level, count, proportion)
}

desc_numeric <- function(df, w, varnames){
  nm <- varnames
  out <- lapply(nm, function(v){
    x <- df[[v]]
    data.frame(variable=v, mean=round(wmean(x,w),3), sd=round(wsd(x,w),3), min=min(x,na.rm=TRUE), max=max(x,na.rm=TRUE), N=sum(!is.na(x)))
  })
  dplyr::bind_rows(out)
}

nr_desc <- desc_factor(nonrecognition[,nr_vars, drop=FALSE], controls_w)
dis_desc <- desc_factor(recognition[,dis_vars, drop=FALSE], controls_w)
trust_desc <- desc_numeric(trust_grouped, controls_w, trust_vars)
controls_desc <- desc_factor(controls %>% dplyr::select(gender, age, education_group, income_group, fringe_vs_mainstream, political_ideology_simple), controls_w)

writexl::write_xlsx(list(nonrecognition = nr_desc,
                         disrespect = dis_desc,
                         trust = trust_desc,
                         controls = controls_desc),
                    path = "outputs/descriptives/table3_descriptives_regression_variables.xlsx")

# 3) Weighted bivariate correlations per model family
ugt_scores <- tryCatch({
  fa_tmp <- psych::fa(ugt_numeric, nfactors = 2, fm = "ml", rotate = "varimax")
  data.frame(ugt_info_seeking = as.numeric(fa_tmp$scores[,1]), ugt_identity = as.numeric(fa_tmp$scores[,2]))
}, error = function(e) { data.frame(ugt_info_seeking = rowMeans(ugt_numeric, na.rm = TRUE), ugt_identity = rowMeans(ugt_numeric, na.rm = TRUE)) })

q10_score <- rowMeans(q10_numeric, na.rm = TRUE)
q12_score <- rowMeans(q12_numeric, na.rm = TRUE)

cor_ugt <- wcor(dplyr::bind_cols(ugt_scores, to_numeric(nonrecognition[,nr_vars]), to_numeric(recognition[,dis_vars]), trust_grouped), controls_w)
cor_q10 <- wcor(dplyr::bind_cols(q10_factor = q10_score, to_numeric(nonrecognition[,nr_vars]), to_numeric(recognition[,dis_vars]), trust_grouped), controls_w)
cor_q12 <- wcor(dplyr::bind_cols(q12_factor = q12_score, to_numeric(nonrecognition[,nr_vars]), to_numeric(recognition[,dis_vars]), trust_grouped), controls_w)

writexl::write_xlsx(as.data.frame(round(cor_ugt,3)), "outputs/descriptives/table4_bivariate_correlations_ugt.xlsx")
writexl::write_xlsx(as.data.frame(round(cor_q10,3)), "outputs/descriptives/table4_bivariate_correlations_q10.xlsx")
writexl::write_xlsx(as.data.frame(round(cor_q12,3)), "outputs/descriptives/table4_bivariate_correlations_q12.xlsx")

cat("Descriptive statistics generated in outputs/descriptives/.\n")


