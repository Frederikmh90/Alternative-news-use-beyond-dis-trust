# ---- DATA PREPARATION ----
# Loads and recodes all variables used by the paper analysis pipeline.
#
# Objects produced (used by downstream analysis scripts):
#   df                        raw survey data (N = 1,892)
#   nonrecognition            Q5_1–Q5_5: non-recognition items (factor, 1–5)
#   recognition               Q5_1–Q5_9: recognition + disrespect items (factor, 1–5)
#   trust                     Q4_1–Q4_8: institutional trust items (factor, 1–5)
#   trust_grouped             composite trust scores (political, system, news_media, citizens)
#   Q8_media_altnews_motivation Q8_1–Q8_7: UGT gratification items (factor, 1–4)
#   Q10_media_mainstreamnews  Q10_1–Q10_3: MSM skepticism items (factor, 1–5)
#   Q12_media_altnews         Q12_1–Q12_3: alternative news orientation items (factor, 1–5)
#   controls                  demographics, ideology, survey weight
#   politics_news             follow_politics_society (used by create_descriptive_tables.R)

library(haven)
library(dplyr)

# ---- 1. Load data ----
file_path <- if (file.exists("data/RUC_Main_Final.sav")) "data/RUC_Main_Final.sav" else
             if (file.exists("data/RUC_Main.sav"))       "data/RUC_Main.sav"       else
             if (file.exists("RUC_Main_Final.sav"))      "RUC_Main_Final.sav"      else
             stop("Data file not found. Place RUC_Main_Final.sav in the data/ folder.")

df <- read_sav(file_path)
cat("Rows:", nrow(df), " | Columns:", ncol(df), "\n")

# ---- Helper: reverse a 1–5 Likert item (6 = DK → NA) ----
rev5 <- function(x) {
  case_when(
    x == 1 ~ 5, x == 2 ~ 4, x == 3 ~ 3, x == 4 ~ 2, x == 5 ~ 1,
    x == 6 ~ NA_real_,
    TRUE    ~ NA_real_
  )
}

trust_labs  <- c("Not at all", "To a lesser degree", "To some degree",
                 "To a high degree", "To a very high degree")
agree_labs  <- c("Strongly disagree", "Disagree", "Neither/nor", "Agree", "Strongly agree")
freq_labs   <- c("Never", "Rarely", "Weekly", "Daily", "Several times a day")
imp_labs    <- c("Not important", "Slightly important", "Important", "Very important")

# ---- 2. Trust (Q4) ----
# Eight items; reverse-coded (1 = Not at all → 5 = Very high)
trust <- df %>%
  transmute(
    trust_authorities   = factor(rev5(Q4_1), levels = 1:5, labels = trust_labs),
    trust_justice_system= factor(rev5(Q4_2), levels = 1:5, labels = trust_labs),
    trust_politicians   = factor(rev5(Q4_3), levels = 1:5, labels = trust_labs),
    trust_government    = factor(rev5(Q4_4), levels = 1:5, labels = trust_labs),
    trust_news_media    = factor(rev5(Q4_5), levels = 1:5, labels = trust_labs),
    trust_eu            = factor(rev5(Q4_6), levels = 1:5, labels = trust_labs),
    trust_intl_org      = factor(rev5(Q4_7), levels = 1:5, labels = trust_labs),
    trust_citizens      = factor(rev5(Q4_8), levels = 1:5, labels = trust_labs)
  )

# Composite trust scores (numeric means used in regressions and correlations)
trust_num <- trust %>% mutate(across(everything(), as.numeric))

trust_grouped <- trust_num %>%
  transmute(
    trust_citizens  = trust_citizens,
    trust_political = rowMeans(cbind(trust_politicians, trust_government),      na.rm = TRUE),
    trust_system    = rowMeans(cbind(trust_authorities, trust_justice_system),  na.rm = TRUE),
    trust_news_media= trust_news_media
  )

# ---- 3. Non-recognition (Q5_1–Q5_5) ----
# Items are positively worded (e.g. "I have caring relationships").
# Raw values kept: higher = more agreement with inclusion item = LOWER non-recognition.
# Regressions interpret positive coefficients as "more included → higher outcome".
nonrecognition <- df %>%
  transmute(
    nonrecog_care        = factor(case_when(
      Q5_1 %in% 1:5 ~ Q5_1, Q5_1 == 6 ~ NA_real_), levels = 1:5, labels = agree_labs),
    nonrecog_equality    = factor(case_when(
      Q5_2 %in% 1:5 ~ Q5_2, Q5_2 == 6 ~ NA_real_), levels = 1:5, labels = agree_labs),
    nonrecog_rights      = factor(case_when(
      Q5_3 %in% 1:5 ~ Q5_3, Q5_3 == 6 ~ NA_real_), levels = 1:5, labels = agree_labs),
    nonrecog_esteem      = factor(case_when(
      Q5_4 %in% 1:5 ~ Q5_4, Q5_4 == 6 ~ NA_real_), levels = 1:5, labels = agree_labs),
    nonrecog_value_society = factor(case_when(
      Q5_5 %in% 1:5 ~ Q5_5, Q5_5 == 6 ~ NA_real_), levels = 1:5, labels = agree_labs)
  )

# ---- 4. Recognition + disrespect items (Q5_1–Q5_9) ----
# Positive recognition items (Q5_1–Q5_5): reverse-coded (high = high recognition)
# Disrespect items (Q5_6–Q5_9): kept raw (high = high disrespect)
recognition <- df %>%
  transmute(
    recog_care                 = factor(rev5(Q5_1), levels = 1:5, labels = agree_labs),
    recog_equality             = factor(rev5(Q5_2), levels = 1:5, labels = agree_labs),
    recog_rights               = factor(rev5(Q5_3), levels = 1:5, labels = agree_labs),
    recog_esteem               = factor(rev5(Q5_4), levels = 1:5, labels = agree_labs),
    recog_value_society        = factor(rev5(Q5_5), levels = 1:5, labels = agree_labs),
    disrespect_misperception   = factor(case_when(
      Q5_6 %in% 1:5 ~ Q5_6, Q5_6 == 6 ~ NA_real_), levels = 1:5, labels = agree_labs),
    disrespect_denigration     = factor(case_when(
      Q5_7 %in% 1:5 ~ Q5_7, Q5_7 == 6 ~ NA_real_), levels = 1:5, labels = agree_labs),
    disrespect_exclusion       = factor(case_when(
      Q5_8 %in% 1:5 ~ Q5_8, Q5_8 == 6 ~ NA_real_), levels = 1:5, labels = agree_labs),
    disrespect_discrimination  = factor(case_when(
      Q5_9 %in% 1:5 ~ Q5_9, Q5_9 == 6 ~ NA_real_), levels = 1:5, labels = agree_labs)
  )

# ---- 5. UGT motivations / gratifications (Q8) ----
# Seven reasons for using alternative news media; 4-point importance scale.
# Note: full_picture (Q8_6) is excluded from EFA in regression scripts.
Q8_media_altnews_motivation <- df %>%
  transmute(
    reflect_values       = factor(case_when(
      Q8_1 == 1 ~ 4, Q8_1 == 2 ~ 3, Q8_1 == 3 ~ 2, Q8_1 == 4 ~ 1, Q8_1 == 5 ~ NA_real_),
      levels = 1:4, labels = imp_labs),
    feel_seen_understood = factor(case_when(
      Q8_2 == 1 ~ 4, Q8_2 == 2 ~ 3, Q8_2 == 3 ~ 2, Q8_2 == 4 ~ 1, Q8_2 == 5 ~ NA_real_),
      levels = 1:4, labels = imp_labs),
    alternative_perspectives = factor(case_when(
      Q8_3 == 1 ~ 4, Q8_3 == 2 ~ 3, Q8_3 == 3 ~ 2, Q8_3 == 4 ~ 1, Q8_3 == 5 ~ NA_real_),
      levels = 1:4, labels = imp_labs),
    factcheck_news       = factor(case_when(
      Q8_4 == 1 ~ 4, Q8_4 == 2 ~ 3, Q8_4 == 3 ~ 2, Q8_4 == 4 ~ 1, Q8_4 == 5 ~ NA_real_),
      levels = 1:4, labels = imp_labs),
    different_opinions   = factor(case_when(
      Q8_5 == 1 ~ 4, Q8_5 == 2 ~ 3, Q8_5 == 3 ~ 2, Q8_5 == 4 ~ 1, Q8_5 == 5 ~ NA_real_),
      levels = 1:4, labels = imp_labs),
    full_picture         = factor(case_when(
      Q8_6 == 1 ~ 4, Q8_6 == 2 ~ 3, Q8_6 == 3 ~ 2, Q8_6 == 4 ~ 1, Q8_6 == 5 ~ NA_real_),
      levels = 1:4, labels = imp_labs),
    change_society       = factor(case_when(
      Q8_7 == 1 ~ 4, Q8_7 == 2 ~ 3, Q8_7 == 3 ~ 2, Q8_7 == 4 ~ 1, Q8_7 == 5 ~ NA_real_),
      levels = 1:4, labels = imp_labs)
  )

# ---- 6. Mainstream media skepticism (Q10) ----
# Three items measuring attitudes toward mainstream news.
# Q10_3 ("one-sided presentation") is negatively worded: kept raw (no reversal).
Q10_media_mainstreamnews <- df %>%
  transmute(
    truth_important_issues  = factor(rev5(Q10_1), levels = 1:5, labels = agree_labs),
    all_voices_heard        = factor(rev5(Q10_2), levels = 1:5, labels = agree_labs),
    one_sided_presentation  = factor(case_when(
      Q10_3 %in% 1:5 ~ Q10_3, Q10_3 == 6 ~ NA_real_),
      levels = 1:5,
      labels = c("Strongly agree", "Agree", "Neither/nor", "Disagree", "Strongly disagree"))
  )

# ---- 7. Alternative news orientation (Q12) ----
# Three behavioural items (frequency of seeking alternative news).
Q12_media_altnews <- df %>%
  transmute(
    other_perspectives    = factor(rev5(Q12_1), levels = 1:5, labels = freq_labs),
    not_covered_tradmedia = factor(rev5(Q12_2), levels = 1:5, labels = freq_labs),
    new_sources           = factor(rev5(Q12_3), levels = 1:5, labels = freq_labs)
  )

# ---- 8. Controls ----
# Income: recode DK panel codes (98/99) to NA, then split into tertiles
income_num <- as.numeric(ifelse(df$DK_PERSONALINCOME %in% c(98, 99), NA,
                                df$DK_PERSONALINCOME))
income_q   <- stats::quantile(income_num, probs = c(1/3, 2/3), na.rm = TRUE, type = 7)
income_grp <- cut(income_num,
                  breaks = c(-Inf, income_q[1], income_q[2], Inf),
                  labels = c("low", "mid", "high"), right = TRUE)

controls <- df %>%
  transmute(
    gender = factor(GENDER, levels = c(1, 2), labels = c("Male", "Female")),

    age = cut(AGE, breaks = c(17, 34, 49, 64, Inf),
              labels = c("18-34", "35-49", "50-64", "65+"), right = TRUE),

    # Fine-grained education (used in some appendices)
    education = factor(
      case_when(
        DK_EDUCATION %in% 1:3  ~ "Basic education",
        DK_EDUCATION %in% 4:5  ~ "Upper secondary",
        DK_EDUCATION == 6      ~ "Vocational education",
        DK_EDUCATION %in% 7:9  ~ "Higher education",
        DK_EDUCATION %in% 10:11~ "Advanced education",
        TRUE ~ NA_character_
      ),
      levels = c("Basic education", "Upper secondary", "Vocational education",
                 "Higher education", "Advanced education")
    ),

    # Three-level education grouping used in regressions
    education_group = factor(
      case_when(
        DK_EDUCATION %in% 1:5  ~ "basic_upsecondary",
        DK_EDUCATION == 6      ~ "vocational",
        DK_EDUCATION %in% 7:11 ~ "+higher",
        TRUE ~ NA_character_
      ),
      levels = c("basic_upsecondary", "vocational", "+higher")
    ),

    income       = income_num,
    income_group = factor(income_grp, levels = c("low", "mid", "high")),

    # Five-category political ideology (reference = "Center" in regressions)
    political_ideology = factor(
      case_when(
        Q3 %in% 1:2  ~ "Far-left",
        Q3 %in% 3:4  ~ "Center-left",
        Q3 %in% 5:6  ~ "Center",
        Q3 %in% 7:8  ~ "Center-right",
        Q3 %in% 9:10 ~ "Far-right",
        Q3 == 11     ~ NA_character_,
        TRUE         ~ NA_character_
      ),
      levels = c("Center", "Far-left", "Center-left", "Center-right", "Far-right")
    ),

    # Fringe vs mainstream (used in some descriptives)
    fringe_vs_mainstream = factor(
      case_when(
        Q3 %in% c(1, 2, 9, 10) ~ "fringe",
        Q3 %in% 3:8            ~ "mainstream",
        Q3 == 11               ~ NA_character_,
        TRUE                   ~ NA_character_
      ),
      levels = c("mainstream", "fringe")
    ),

    # Simplified three-category ideology (used in some descriptives)
    political_ideology_simple = factor(
      case_when(
        Q3 %in% 1:3  ~ "left_wing",
        Q3 %in% 4:7  ~ "center",
        Q3 %in% 8:10 ~ "right_wing",
        Q3 == 11     ~ NA_character_,
        TRUE         ~ NA_character_
      ),
      levels = c("center", "left_wing", "right_wing")
    ),

    # Continuous ideology scale 1–10 (control variable in regressions)
    left_right_scale = case_when(Q3 == 11 ~ NA_real_, TRUE ~ as.numeric(Q3)),

    # Political engagement: "It is important to me to follow politics and society"
    # Q2_1 reverse-coded: higher = more engaged
    follow_politics_society = case_when(
      Q2_1 == 1 ~ 5, Q2_1 == 2 ~ 4, Q2_1 == 3 ~ 3,
      Q2_1 == 4 ~ 2, Q2_1 == 5 ~ 1, Q2_1 == 6 ~ NA_real_
    ),

    analysis_weight = suppressWarnings(as.numeric(df$WEIGHT))
  )

# ---- 9. politics_news (minimal; follow_politics_society used by create_descriptive_tables.R) ----
politics_news <- data.frame(
  follow_politics_society = controls$follow_politics_society
)

cat("Data preparation complete. Objects available:\n")
cat("  nonrecognition, recognition, trust, trust_grouped,\n")
cat("  Q8_media_altnews_motivation, Q10_media_mainstreamnews, Q12_media_altnews,\n")
cat("  controls, politics_news\n")
