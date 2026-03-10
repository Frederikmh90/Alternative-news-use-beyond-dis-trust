# ---- Extract Descriptive Placeholders for Manuscript ----
# Computes weighted Mean/SD for all PLACEHOLDER values in the results section

options(repos = c(CRAN = "https://cloud.r-project.org"))
library(haven)
library(dplyr)
library(psych)

source("scripts/data_prep.R")

# Weighted mean and SD
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
wn <- function(x, w) {
  x <- as.numeric(x); w <- as.numeric(w)
  sel <- !is.na(x) & !is.na(w) & w > 0
  sum(sel)
}

w <- suppressWarnings(as.numeric(controls$analysis_weight))
w[is.na(w) | w <= 0] <- 1  # unweighted for missing

# ---- DEPENDENT VARIABLES ----
# MSM Skepticism = Q10 REVERSED (high = rejection)
q10_num <- Q10_media_mainstreamnews %>% mutate(across(everything(), as.numeric))
q10_rev <- q10_num %>% mutate(across(everything(), ~ 6 - .))
msm_comp <- rowMeans(q10_rev, na.rm = TRUE)
msm_m <- wmean(msm_comp, w); msm_sd <- wsd(msm_comp, w); msm_n <- wn(msm_comp, w)

# Alternative news seeking = Q12 composite (mean of 3 items)
q12_num <- Q12_media_altnews %>% mutate(across(everything(), as.numeric))
alt_comp <- rowMeans(q12_num, na.rm = TRUE)
alt_m <- wmean(alt_comp, w); alt_sd <- wsd(alt_comp, w); alt_n <- wn(alt_comp, w)

# UGT factors (Q8)
ugt_num <- Q8_media_altnews_motivation %>% mutate(across(everything(), as.numeric)) %>% dplyr::select(-full_picture)
fa_ugt <- fa(ugt_num, nfactors = 2, fm = "ml", rotate = "varimax")
ugt_info <- as.numeric(fa_ugt$scores[,1]); ugt_id <- as.numeric(fa_ugt$scores[,2])
ugt_info_m <- wmean(ugt_info, w); ugt_id_m <- wmean(ugt_id, w)
ugt_info_n <- wn(ugt_info, w); ugt_id_n <- wn(ugt_id, w)
ugt_info_sd <- wsd(ugt_info, w); ugt_id_sd <- wsd(ugt_id, w)

# ---- INDEPENDENT: NON-RECOGNITION ----
nr_num <- nonrecognition %>% mutate(across(everything(), as.numeric))
nr_care_m <- wmean(nr_num$nonrecog_care, w)
nr_equality_m <- wmean(nr_num$nonrecog_equality, w)
nr_rights_m <- wmean(nr_num$nonrecog_rights, w)
nr_esteem_m <- wmean(nr_num$nonrecog_esteem, w)
nr_value_m <- wmean(nr_num$nonrecog_value_society, w)

# ---- INDEPENDENT: DISRESPECT ----
dis_num <- recognition %>% dplyr::select(starts_with("disrespect_")) %>% mutate(across(everything(), as.numeric))
dis_mean <- rowMeans(dis_num, na.rm = TRUE)
dis_m <- wmean(dis_mean, w)

# ---- TRUST ----
tg_num <- trust_grouped %>% mutate(across(everything(), as.numeric))
trust_sys_m <- wmean(tg_num$trust_system, w)
trust_pol_m <- wmean(tg_num$trust_political, w)
trust_media_m <- wmean(tg_num$trust_news_media, w)
trust_cit_m <- wmean(tg_num$trust_citizens, w)

# ---- TABLE X: Alt News Orientation by Trust Quartiles ----
# Use Q12 composite as "alternative news orientation"
tg_n <- tg_num
tg_n$alt_orient <- alt_comp
tg_n$w <- w
tg_n <- tg_n[complete.cases(tg_n[, c("trust_system", "trust_political", "alt_orient")]), ]

# System trust quartiles (use ntile for robust quartiles)
tg_n$sys_q <- paste0("Q", ntile(tg_n$trust_system, 4))
sys_tab <- tg_n %>% group_by(sys_q) %>%
  summarise(mean_alt = wmean(alt_orient, w), sd_alt = wsd(alt_orient, w), n = n(), .groups = "drop")

# Political trust quartiles
tg_n$pol_q <- paste0("Q", ntile(tg_n$trust_political, 4))
pol_tab <- tg_n %>% group_by(pol_q) %>%
  summarise(mean_alt = wmean(alt_orient, w), sd_alt = wsd(alt_orient, w), n = n(), .groups = "drop")

# Build output list for markdown
out <- list(
  msm_m = round(msm_m, 2), msm_sd = round(msm_sd, 2), msm_n = msm_n,
  alt_m = round(alt_m, 2), alt_sd = round(alt_sd, 2), alt_n = alt_n,
  ugt_info_m = round(ugt_info_m, 2), ugt_info_sd = round(ugt_info_sd, 2), ugt_info_n = ugt_info_n,
  ugt_id_m = round(ugt_id_m, 2), ugt_id_sd = round(ugt_id_sd, 2), ugt_id_n = ugt_id_n,
  nr_care_m = round(nr_care_m, 2), nr_esteem_m = round(nr_esteem_m, 2),
  nr_equality_m = round(nr_equality_m, 2), nr_rights_m = round(nr_rights_m, 2), nr_value_m = round(nr_value_m, 2),
  trust_sys_m = round(trust_sys_m, 2), trust_pol_m = round(trust_pol_m, 2),
  trust_media_m = round(trust_media_m, 2), trust_cit_m = round(trust_cit_m, 2),
  sys_tab = sys_tab, pol_tab = pol_tab
)
dir.create("outputs/descriptives", recursive = TRUE, showWarnings = FALSE)
tryCatch(saveRDS(out, "outputs/descriptives/placeholder_numbers.rds"), error = function(e) message("Could not save RDS: ", e$message))
cat("RESULTS:\n")
for (i in 1:18) cat(names(out)[i], "=", out[[i]], "\n")
cat("\nTable X - System Trust Quartiles:\n"); print(sys_tab)
cat("\nTable X - Political Trust Quartiles:\n"); print(pol_tab)
