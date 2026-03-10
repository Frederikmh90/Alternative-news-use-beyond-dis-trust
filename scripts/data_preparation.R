# ---- 0) Packages ----
# Set CRAN mirror (required for package installation)
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Install the needed packages once; then you can comment these out.
pkgs <- c("haven", "sjmisc", "purrr", "writexl")
to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]

if (length(to_install)) install.packages(to_install, dependencies = TRUE)

library(haven)   # read_sav()
library(sjmisc)  # frq()
library(purrr)   # map/walk helpers
library(dplyr)
library(writexl) # write_xlsx()

# ---- 1) Load data (.sav) ----
file_path <- if (file.exists("RUC_Main.sav")) "RUC_Main.sav" else
  if (file.exists("data/RUC_Main.sav")) "data/RUC_Main.sav" else
  if (file.exists("data/RUC_Main_Final.sav")) "data/RUC_Main_Final.sav" else
  "RUC_Main.sav"
df <- read_sav(file_path)   # preserves SPSS labels
# Cross table of GENDER and Q4_5
# Correlation: Do women (2) have less trust in news media (Q4_5) than men (1)?
# Q4_5: Presumed trust in news media variable


# ---- 2) Quick sanity check ----
cat("Rows:", nrow(df), " | Columns:", ncol(df), "\n\n")

# Capture all frq() outputs and write to file
#all_frq <- lapply(names(df), function(v) frq(df[[v]], show.na = TRUE))
#cat(capture.output(all_frq), file = "frequencies_all_variables.txt", sep = "\n")


# data overview

str(df)
names(df)

#frq(df$Q1C01)
#frq(df$Q1C02)
#frq(df$Q1C03)
#frq(df$Q1C04)
#frq(df$Q1C05)
#frq(df$Q1C06)
#frq(df$Q1C98)
#frq(df$Q1C99)


#names(df)

df <- df %>%
  dplyr::select(-all_of(c(
    # Q15C variables
    "Q15C14", "Q15C15", "Q15C16", "Q15C17", "Q15C18", "Q15C19",
    "Q15C20", "Q15C21", "Q15C22", "Q15C23", "Q15C24", "Q15C25",
    "Q15C26", "Q15C27", "Q15C28", "Q15C29", "Q15C30", "Q15C31",
    "Q15C32", "Q15C33", "Q15C34", "Q15C35", "Q15C36", "Q15C37",
    "Q15C38", "Q15C39", "Q15C40", "Q15C41", "Q15C42", "Q15C43",
    "Q15C44", "Q15C45", "Q15C46", "Q15C47", "Q15C48", "Q15C49",
    "Q15C50", "Q15C51", "Q15C52", "Q15C53", "Q15C54", "Q15C55",
    "Q15C56", "Q15C57", "Q15C58", "Q15C59", "Q15C60", "Q15C61",
    "Q15C62", "Q15C63", "Q15C64", "Q15C65", "Q15C66", "Q15C67",
    "Q15C68", "Q15C69", "Q15C70", "Q15C71", "Q15C72", "Q15C73",
    "Q15C74", "Q15C75", "Q15C76", "Q15C77", "Q15C78", "Q15C79",
    "Q15C80", "Q15C81", "Q15C82", "Q15C83", "Q15C84", "Q15C85",
    "Q15C86", "Q15C87", "Q15C88", "Q15C89", "Q15C90", "Q15C91",
    "Q15C92", "Q15C93", "Q15C94", "Q15C95", "Q15C96", "Q15C97",
    
    # Q24DKC variables
    "Q24DKC02", "Q24DKC03", "Q24DKC04", "Q24DKC05", "Q24DKC06", "Q24DKC07",
    "Q24DKC08", "Q24DKC09", "Q24DKC10", "Q24DKC11", "Q24DKC12", "Q24DKC13",
    "Q24DKC14", "Q24DKC15", "Q24DKC16", "Q24DKC17", "Q24DKC18", "Q24DKC19",
    "Q24DKC20", "Q24DKC21", "Q24DKC22", "Q24DKC23", "Q24DKC24", "Q24DKC25",
    "Q24DKC26", "Q24DKC27", "Q24DKC28", "Q24DKC29", "Q24DKC30", "Q24DKC31",
    "Q24DKC32", "Q24DKC33", "Q24DKC34", "Q24DKC35", "Q24DKC36", "Q24DKC37",
    "Q24DKC38", "Q24DKC39", "Q24DKC40", "Q24DKC41", "Q24DKC42", "Q24DKC43",
    "Q24DKC44", "Q24DKC45", "Q24DKC46", "Q24DKC47", "Q24DKC48", "Q24DKC49",
    "Q24DKC50", "Q24DKC51", "Q24DKC52", "Q24DKC53", "Q24DKC54", "Q24DKC55",
    "Q24DKC56", "Q24DKC57", "Q24DKC58", "Q24DKC59", "Q24DKC60", "Q24DKC61",
    "Q24DKC62", "Q24DKC63", "Q24DKC64", "Q24DKC65", "Q24DKC66", "Q24DKC67",
    "Q24DKC68", "Q24DKC69", "Q24DKC70", "Q24DKC71", "Q24DKC72", "Q24DKC73",
    "Q24DKC74", "Q24DKC75", "Q24DKC76", "Q24DKC77", "Q24DKC78", "Q24DKC79",
    "Q24DKC80", "Q24DKC81", "Q24DKC82", "Q24DKC83", "Q24DKC84", "Q24DKC85",
    "Q24DKC86", "Q24DKC87", "Q24DKC88", "Q24DKC89", "Q24DKC90", "Q24DKC91",
    "Q24DKC92", "Q24DKC93", "Q24DKC94", "Q24DKC95", "Q24DKC96", "Q24DKC97",
    "Q24DKC98",
    
    # Q1C variables to drop
    "Q1C07", "Q1C08", "Q1C09", "Q1C10", "Q1C11", "Q1C12", "Q1C13",
    "Q1C14", "Q1C15", "Q1C16", "Q1C17", "Q1C18", "Q1C19", "Q1C20",
    "Q1C21", "Q1C22", "Q1C23", "Q1C24", "Q1C25", "Q1C26", "Q1C27",
    "Q1C28", "Q1C29", "Q1C30", "Q1C31", "Q1C32", "Q1C33", "Q1C34",
    "Q1C35", "Q1C36", "Q1C37", "Q1C38", "Q1C39", "Q1C40", "Q1C41",
    "Q1C42", "Q1C43", "Q1C44", "Q1C45", "Q1C46", "Q1C47", "Q1C48",
    "Q1C49", "Q1C50", "Q1C51", "Q1C52", "Q1C53", "Q1C54", "Q1C55",
    "Q1C56", "Q1C57", "Q1C58", "Q1C59", "Q1C60", "Q1C61", "Q1C62",
    "Q1C63", "Q1C64", "Q1C65", "Q1C66", "Q1C67", "Q1C68", "Q1C69",
    "Q1C70", "Q1C71", "Q1C72", "Q1C73", "Q1C74", "Q1C75", "Q1C76",
    "Q1C77", "Q1C78", "Q1C79", "Q1C80", "Q1C81", "Q1C82", "Q1C83",
    "Q1C84", "Q1C85", "Q1C86", "Q1C87", "Q1C88", "Q1C89", "Q1C90",
    "Q1C91", "Q1C92", "Q1C93", "Q1C94", "Q1C95", "Q1C96", "Q1C97"
  )))



### Recodings ###
library(dplyr)



### Nyhedsforbrug // news consumption
media_news_consumption <- df %>%
  transmute(
    news_public_service = factor(Q1C01,
                                 levels = c(0, 1),
                                 labels = c("Nej", "Ja")),
    news_landsdaekkende_aviser = factor(Q1C02,
                                        levels = c(0, 1),
                                        labels = c("Nej", "Ja")),
    news_lokale_medier = factor(Q1C03,
                                levels = c(0, 1),
                                labels = c("Nej", "Ja")),
    news_tabloid = factor(Q1C04,
                          levels = c(0, 1),
                          labels = c("Nej", "Ja")),
    news_online_profileret = factor(Q1C05,
                                    levels = c(0, 1),
                                    labels = c("Nej", "Ja")),
    news_online_politisk = factor(Q1C06,
                                  levels = c(0, 1),
                                  labels = c("Nej", "Ja")),
    news_andre = factor(Q1C98,
                        levels = c(0, 1),
                        labels = c("Nej", "Ja")),
    news_ingen = factor(Q1C99,
                        levels = c(0, 1),
                        labels = c("Nej", "Ja"))
  )



### Politik ###
politics_news <- df %>%
  transmute(
    follow_politics_society = factor(case_when(
      Q2_1 == 1 ~ 5,
      Q2_1 == 2 ~ 4,
      Q2_1 == 3 ~ 3,
      Q2_1 == 4 ~ 2,
      Q2_1 == 5 ~ 1,
      Q2_1 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree")),
    
    follow_news = factor(case_when(
      Q2_2 == 1 ~ 5,
      Q2_2 == 2 ~ 4,
      Q2_2 == 3 ~ 3,
      Q2_2 == 4 ~ 2,
      Q2_2 == 5 ~ 1,
      Q2_2 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree")),
    
    keep_opinions_private = factor(case_when(
      Q2_3 == 1 ~ 5,
      Q2_3 == 2 ~ 4,
      Q2_3 == 3 ~ 3,
      Q2_3 == 4 ~ 2,
      Q2_3 == 5 ~ 1,
      Q2_3 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree")),
    
    avoid_news = factor(case_when(
      Q2_4 == 1 ~ 5,
      Q2_4 == 2 ~ 4,
      Q2_4 == 3 ~ 3,
      Q2_4 == 4 ~ 2,
      Q2_4 == 5 ~ 1,
      Q2_4 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree")),
    
    active_in_associations = factor(case_when(
      Q2_5 == 1 ~ 5,
      Q2_5 == 2 ~ 4,
      Q2_5 == 3 ~ 3,
      Q2_5 == 4 ~ 2,
      Q2_5 == 5 ~ 1,
      Q2_5 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree"))
  )


# Ideologi
politics_news <- politics_news %>%
  mutate(
    ideologisk_venstre_hojre = case_when(
      df$Q3 == 11 ~ NA_real_,        # Ved ikke
      TRUE ~ as.numeric(df$Q3)
    )
  )

politics_news <- politics_news %>%
  mutate(
    ideologisk_venstre_hojre_categories = case_when(
      ideologisk_venstre_hojre %in% c(1, 2) ~ "Far-left",
      ideologisk_venstre_hojre %in% c(3, 4) ~ "Center-left",
      ideologisk_venstre_hojre %in% c(5, 6) ~ "Center",
      ideologisk_venstre_hojre %in% c(7, 8) ~ "Center-right",
      ideologisk_venstre_hojre %in% c(9, 10) ~ "Far-right",
      TRUE ~ NA_character_
    ) %>% factor(
      levels = c("Far-left", "Center-left", "Center", "Center-right", "Far-right")
    )
  )


# TRUST
trust <- df %>%
  transmute(
    trust_authorities = factor(case_when(
      Q4_1 == 1 ~ 5,
      Q4_1 == 2 ~ 4,
      Q4_1 == 3 ~ 3,
      Q4_1 == 4 ~ 2,
      Q4_1 == 5 ~ 1,
      Q4_1 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Not at all", "To a lesser degree", "To some degree", "To a high degree", "To a very high degree")),
    
    trust_justice_system = factor(case_when(
      Q4_2 == 1 ~ 5,
      Q4_2 == 2 ~ 4,
      Q4_2 == 3 ~ 3,
      Q4_2 == 4 ~ 2,
      Q4_2 == 5 ~ 1,
      Q4_2 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Not at all", "To a lesser degree", "To some degree", "To a high degree", "To a very high degree")),
    
    trust_politicians = factor(case_when(
      Q4_3 == 1 ~ 5,
      Q4_3 == 2 ~ 4,
      Q4_3 == 3 ~ 3,
      Q4_3 == 4 ~ 2,
      Q4_3 == 5 ~ 1,
      Q4_3 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Not at all", "To a lesser degree", "To some degree", "To a high degree", "To a very high degree")),
    
    trust_government = factor(case_when(
      Q4_4 == 1 ~ 5,
      Q4_4 == 2 ~ 4,
      Q4_4 == 3 ~ 3,
      Q4_4 == 4 ~ 2,
      Q4_4 == 5 ~ 1,
      Q4_4 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Not at all", "To a lesser degree", "To some degree", "To a high degree", "To a very high degree")),
    
    trust_news_media = factor(case_when(
      Q4_5 == 1 ~ 5,
      Q4_5 == 2 ~ 4,
      Q4_5 == 3 ~ 3,
      Q4_5 == 4 ~ 2,
      Q4_5 == 5 ~ 1,
      Q4_5 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Not at all", "To a lesser degree", "To some degree", "To a high degree", "To a very high degree")),
    
    trust_eu = factor(case_when(
      Q4_6 == 1 ~ 5,
      Q4_6 == 2 ~ 4,
      Q4_6 == 3 ~ 3,
      Q4_6 == 4 ~ 2,
      Q4_6 == 5 ~ 1,
      Q4_6 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Not at all", "To a lesser degree", "To some degree", "To a high degree", "To a very high degree")),
    
    trust_intl_org = factor(case_when(
      Q4_7 == 1 ~ 5,
      Q4_7 == 2 ~ 4,
      Q4_7 == 3 ~ 3,
      Q4_7 == 4 ~ 2,
      Q4_7 == 5 ~ 1,
      Q4_7 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Not at all", "To a lesser degree", "To some degree", "To a high degree", "To a very high degree")),
    
    trust_citizens = factor(case_when(
      Q4_8 == 1 ~ 5,
      Q4_8 == 2 ~ 4,
      Q4_8 == 3 ~ 3,
      Q4_8 == 4 ~ 2,
      Q4_8 == 5 ~ 1,
      Q4_8 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Not at all", "To a lesser degree", "To some degree", "To a high degree", "To a very high degree"))
  )

# Grouped trust indices for regressions (numeric 1-5)
trust_numeric <- trust %>% mutate(across(everything(), as.numeric))

trust_grouped <- trust_numeric %>%
  transmute(
    trust_citizens = trust_citizens,
    trust_political = rowMeans(cbind(trust_politicians, trust_government), na.rm = TRUE),
    trust_system = rowMeans(cbind(trust_authorities, trust_justice_system), na.rm = TRUE),
    trust_news_media = trust_news_media
  )


# Visualisering med deling af posts, som de er uenige med. 
# 
# Recognition variables (positive items)
recognition <- df %>%
  transmute(
    recog_care = factor(case_when(
      Q5_1 == 1 ~ 5,
      Q5_1 == 2 ~ 4,
      Q5_1 == 3 ~ 3,
      Q5_1 == 4 ~ 2,
      Q5_1 == 5 ~ 1,
      Q5_1 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Strongly disagree", "Disagree", "Neither/nor", "Agree", "Strongly agree")),
    
    recog_equality = factor(case_when(
      Q5_2 == 1 ~ 5,
      Q5_2 == 2 ~ 4,
      Q5_2 == 3 ~ 3,
      Q5_2 == 4 ~ 2,
      Q5_2 == 5 ~ 1,
      Q5_2 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Strongly disagree", "Disagree", "Neither/nor", "Agree", "Strongly agree")),
    
    recog_rights = factor(case_when(
      Q5_3 == 1 ~ 5,
      Q5_3 == 2 ~ 4,
      Q5_3 == 3 ~ 3,
      Q5_3 == 4 ~ 2,
      Q5_3 == 5 ~ 1,
      Q5_3 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Strongly disagree", "Disagree", "Neither/nor", "Agree", "Strongly agree")),
    
    recog_esteem = factor(case_when(
      Q5_4 == 1 ~ 5,
      Q5_4 == 2 ~ 4,
      Q5_4 == 3 ~ 3,
      Q5_4 == 4 ~ 2,
      Q5_4 == 5 ~ 1,
      Q5_4 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Strongly disagree", "Disagree", "Neither/nor", "Agree", "Strongly agree")),
    
    recog_value_society = factor(case_when(
      Q5_5 == 1 ~ 5,
      Q5_5 == 2 ~ 4,
      Q5_5 == 3 ~ 3,
      Q5_5 == 4 ~ 2,
      Q5_5 == 5 ~ 1,
      Q5_5 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Strongly disagree", "Disagree", "Neither/nor", "Agree", "Strongly agree")),
    
    # Disrespect variables (negative items)
    disrespect_misperception = factor(case_when(
      Q5_6 == 1 ~ 5,
      Q5_6 == 2 ~ 4,
      Q5_6 == 3 ~ 3,
      Q5_6 == 4 ~ 2,
      Q5_6 == 5 ~ 1,
      Q5_6 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Strongly disagree", "Disagree", "Neither/nor", "Agree", "Strongly agree")),
    
    disrespect_denigration = factor(case_when(
      Q5_7 == 1 ~ 5,
      Q5_7 == 2 ~ 4,
      Q5_7 == 3 ~ 3,
      Q5_7 == 4 ~ 2,
      Q5_7 == 5 ~ 1,
      Q5_7 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Strongly disagree", "Disagree", "Neither/nor", "Agree", "Strongly agree")),
    
    disrespect_exclusion = factor(case_when(
      Q5_8 == 1 ~ 5,
      Q5_8 == 2 ~ 4,
      Q5_8 == 3 ~ 3,
      Q5_8 == 4 ~ 2,
      Q5_8 == 5 ~ 1,
      Q5_8 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Strongly disagree", "Disagree", "Neither/nor", "Agree", "Strongly agree")),
    
    disrespect_discrimination = factor(case_when(
      Q5_9 == 1 ~ 5,
      Q5_9 == 2 ~ 4,
      Q5_9 == 3 ~ 3,
      Q5_9 == 4 ~ 2,
      Q5_9 == 5 ~ 1,
      Q5_9 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Strongly disagree", "Disagree", "Neither/nor", "Agree", "Strongly agree"))
  )

# Nonrecognition variables (using same Q5_1-Q5_5 items as recognition)
# IMPORTANT: Despite the name, these are coded to measure INCLUSION/RECOGNITION
# Q5 items are positively worded: "I have caring relationships", "I feel equal", etc.
# Raw survey: 1=Strongly agree (included), 5=Strongly disagree (excluded)
# 
# CODING: We keep raw values WITHOUT reversal:
# - High values (4-5) = agreeing with positive statements = HIGH inclusion (LOW non-recognition)
# - Low values (1-2) = disagreeing with positive statements = LOW inclusion (HIGH non-recognition)
#
# INTERPRETATION FOR REGRESSIONS:
# When nonrecog variables are used as predictors, POSITIVE coefficients mean:
# "Higher values of this predictor (= more inclusion) → Higher outcome"
nonrecognition <- df %>%
  transmute(
    nonrecog_care = factor(case_when(
      Q5_1 == 1 ~ 1,  # Agree "I have caring relationships" = LOW non-recognition (included)
      Q5_1 == 2 ~ 2,
      Q5_1 == 3 ~ 3,
      Q5_1 == 4 ~ 4,
      Q5_1 == 5 ~ 5,  # Disagree "I have caring relationships" = HIGH non-recognition (excluded)
      Q5_1 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Strongly disagree", "Disagree", "Neither/nor", "Agree", "Strongly agree")),
    
    nonrecog_equality = factor(case_when(
      Q5_2 == 1 ~ 1,  # Agree "I feel equal" = LOW non-recognition (included)
      Q5_2 == 2 ~ 2,
      Q5_2 == 3 ~ 3,
      Q5_2 == 4 ~ 4,
      Q5_2 == 5 ~ 5,  # Disagree "I feel equal" = HIGH non-recognition (excluded)
      Q5_2 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Strongly disagree", "Disagree", "Neither/nor", "Agree", "Strongly agree")),
    
    nonrecog_rights = factor(case_when(
      Q5_3 == 1 ~ 1,  # Agree "rights respected" = LOW non-recognition (included)
      Q5_3 == 2 ~ 2,
      Q5_3 == 3 ~ 3,
      Q5_3 == 4 ~ 4,
      Q5_3 == 5 ~ 5,  # Disagree "rights respected" = HIGH non-recognition (excluded)
      Q5_3 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Strongly disagree", "Disagree", "Neither/nor", "Agree", "Strongly agree")),
    
    nonrecog_esteem = factor(case_when(
      Q5_4 == 1 ~ 1,  # Agree with self-esteem item = LOW non-recognition (included)
      Q5_4 == 2 ~ 2,
      Q5_4 == 3 ~ 3,
      Q5_4 == 4 ~ 4,
      Q5_4 == 5 ~ 5,  # Disagree with self-esteem item = HIGH non-recognition (excluded)
      Q5_4 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Strongly disagree", "Disagree", "Neither/nor", "Agree", "Strongly agree")),
    
    nonrecog_value_society = factor(case_when(
      Q5_5 == 1 ~ 1,  # Agree "valued by society" = LOW non-recognition (included)
      Q5_5 == 2 ~ 2,
      Q5_5 == 3 ~ 3,
      Q5_5 == 4 ~ 4,
      Q5_5 == 5 ~ 5,  # Disagree "valued by society" = HIGH non-recognition (excluded)
      Q5_5 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Strongly disagree", "Disagree", "Neither/nor", "Agree", "Strongly agree"))
  )

# Backward compatibility: keep theme_inclusion for existing scripts
theme_inclusion <- bind_cols(recognition, nonrecognition)


theme_systemkritik <- df %>%
  transmute(
    ens_partier = factor(case_when(
      Q6_1 == 1 ~ 5,
      Q6_1 == 2 ~ 4,
      Q6_1 == 3 ~ 3,
      Q6_1 == 4 ~ 2,
      Q6_1 == 5 ~ 1,
      Q6_1 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Meget uenig", "Uenig", "Hverken/eller", "Enig", "Meget enig")),
    
    for_lidt_opmaerksomhed = factor(case_when(
      Q6_2 == 1 ~ 5,
      Q6_2 == 2 ~ 4,
      Q6_2 == 3 ~ 3,
      Q6_2 == 4 ~ 2,
      Q6_2 == 5 ~ 1,
      Q6_2 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Meget uenig", "Uenig", "Hverken/eller", "Enig", "Meget enig")),
    
    afstand_elite_folk = factor(case_when(
      Q6_3 == 1 ~ 5,
      Q6_3 == 2 ~ 4,
      Q6_3 == 3 ~ 3,
      Q6_3 == 4 ~ 2,
      Q6_3 == 5 ~ 1,
      Q6_3 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Meget uenig", "Uenig", "Hverken/eller", "Enig", "Meget enig")),
    
    magtfuld_gruppe = factor(case_when(
      Q6_4 == 1 ~ 5,
      Q6_4 == 2 ~ 4,
      Q6_4 == 3 ~ 3,
      Q6_4 == 4 ~ 2,
      Q6_4 == 5 ~ 1,
      Q6_4 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Meget uenig", "Uenig", "Hverken/eller", "Enig", "Meget enig")),
    
    grundlaeggende_omlaegning = factor(case_when(
      Q6_5 == 1 ~ 5,
      Q6_5 == 2 ~ 4,
      Q6_5 == 3 ~ 3,
      Q6_5 == 4 ~ 2,
      Q6_5 == 5 ~ 1,
      Q6_5 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Meget uenig", "Uenig", "Hverken/eller", "Enig", "Meget enig")),
    
    vold_noedvendigt = factor(case_when(
      Q6_6 == 1 ~ 5,
      Q6_6 == 2 ~ 4,
      Q6_6 == 3 ~ 3,
      Q6_6 == 4 ~ 2,
      Q6_6 == 5 ~ 1,
      Q6_6 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Meget uenig", "Uenig", "Hverken/eller", "Enig", "Meget enig"))
  )

# Create conspiracy_skepsis dataframe with reverse-coded numeric values and factor labels
theme_climate_antiwoke_health <- df %>%
  transmute(
    klimafor_menneskeskabt = factor(case_when(
      Q7_1 == 1 ~ 5,
      Q7_1 == 2 ~ 4,
      Q7_1 == 3 ~ 3,
      Q7_1 == 4 ~ 2,
      Q7_1 == 5 ~ 1,
      Q7_1 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Meget uenig", "Uenig", "Hverken/eller", "Enig", "Meget enig")),
    
    klima_vs_velfaerd = factor(case_when(
      Q7_2 == 1 ~ 5,
      Q7_2 == 2 ~ 4,
      Q7_2 == 3 ~ 3,
      Q7_2 == 4 ~ 2,
      Q7_2 == 5 ~ 1,
      Q7_2 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Meget uenig", "Uenig", "Hverken/eller", "Enig", "Meget enig")),
    
    klima_overdrevet = factor(case_when(
      Q7_3 == 1 ~ 5,
      Q7_3 == 2 ~ 4,
      Q7_3 == 3 ~ 3,
      Q7_3 == 4 ~ 2,
      Q7_3 == 5 ~ 1,
      Q7_3 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Meget uenig", "Uenig", "Hverken/eller", "Enig", "Meget enig")),
    
    vaccine_risiko = factor(case_when(
      Q7_4 == 1 ~ 5,
      Q7_4 == 2 ~ 4,
      Q7_4 == 3 ~ 3,
      Q7_4 == 4 ~ 2,
      Q7_4 == 5 ~ 1,
      Q7_4 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Meget uenig", "Uenig", "Hverken/eller", "Enig", "Meget enig")),
    
    alt_behandling_mangler = factor(case_when(
      Q7_5 == 1 ~ 5,
      Q7_5 == 2 ~ 4,
      Q7_5 == 3 ~ 3,
      Q7_5 == 4 ~ 2,
      Q7_5 == 5 ~ 1,
      Q7_5 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Meget uenig", "Uenig", "Hverken/eller", "Enig", "Meget enig")),
    
    kraenkelse_overdrevet = factor(case_when(
      Q7_6 == 1 ~ 5,
      Q7_6 == 2 ~ 4,
      Q7_6 == 3 ~ 3,
      Q7_6 == 4 ~ 2,
      Q7_6 == 5 ~ 1,
      Q7_6 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Meget uenig", "Uenig", "Hverken/eller", "Enig", "Meget enig"))
  )

### Motivation for ANM_consumption


Q8_media_altnews_motivation <- df %>%
  transmute(
    reflect_values = factor(case_when(
      Q8_1 == 1 ~ 4,
      Q8_1 == 2 ~ 3,
      Q8_1 == 3 ~ 2,
      Q8_1 == 4 ~ 1,
      Q8_1 == 5 ~ NA_real_
    ), levels = 1:4,
    labels = c("Not important", "Slightly important", "Important", "Very important")),
    
    feel_seen_understood = factor(case_when(
      Q8_2 == 1 ~ 4,
      Q8_2 == 2 ~ 3,
      Q8_2 == 3 ~ 2,
      Q8_2 == 4 ~ 1,
      Q8_2 == 5 ~ NA_real_
    ), levels = 1:4,
    labels = c("Not important", "Slightly important", "Important", "Very important")),
    
    alternative_perspectives = factor(case_when(
      Q8_3 == 1 ~ 4,
      Q8_3 == 2 ~ 3,
      Q8_3 == 3 ~ 2,
      Q8_3 == 4 ~ 1,
      Q8_3 == 5 ~ NA_real_
    ), levels = 1:4,
    labels = c("Not important", "Slightly important", "Important", "Very important")),
    
    factcheck_news = factor(case_when(
      Q8_4 == 1 ~ 4,
      Q8_4 == 2 ~ 3,
      Q8_4 == 3 ~ 2,
      Q8_4 == 4 ~ 1,
      Q8_4 == 5 ~ NA_real_
    ), levels = 1:4,
    labels = c("Not important", "Slightly important", "Important", "Very important")),
    
    different_opinions = factor(case_when(
      Q8_5 == 1 ~ 4,
      Q8_5 == 2 ~ 3,
      Q8_5 == 3 ~ 2,
      Q8_5 == 4 ~ 1,
      Q8_5 == 5 ~ NA_real_
    ), levels = 1:4,
    labels = c("Not important", "Slightly important", "Important", "Very important")),
    
    full_picture = factor(case_when(
      Q8_6 == 1 ~ 4,
      Q8_6 == 2 ~ 3,
      Q8_6 == 3 ~ 2,
      Q8_6 == 4 ~ 1,
      Q8_6 == 5 ~ NA_real_
    ), levels = 1:4,
    labels = c("Not important", "Slightly important", "Important", "Very important")),
    
    change_society = factor(case_when(
      Q8_7 == 1 ~ 4,
      Q8_7 == 2 ~ 3,
      Q8_7 == 3 ~ 2,
      Q8_7 == 4 ~ 1,
      Q8_7 == 5 ~ NA_real_
    ), levels = 1:4,
    labels = c("Not important", "Slightly important", "Important", "Very important"))
  )

## Holdningr til etablerede medier og ANM
Q10_media_mainstreamnews <- df %>%
  transmute(
    truth_important_issues = factor(case_when(
      Q10_1 == 1 ~ 5,
      Q10_1 == 2 ~ 4,
      Q10_1 == 3 ~ 3,
      Q10_1 == 4 ~ 2,
      Q10_1 == 5 ~ 1,
      Q10_1 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Strongly disagree", "Disagree", "Neither/nor", "Agree", "Strongly agree")),
    
    all_voices_heard = factor(case_when(
      Q10_2 == 1 ~ 5,
      Q10_2 == 2 ~ 4,
      Q10_2 == 3 ~ 3,
      Q10_2 == 4 ~ 2,
      Q10_2 == 5 ~ 1,
      Q10_2 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Strongly disagree", "Disagree", "Neither/nor", "Agree", "Strongly agree")),
    
    # Q10_3 is a negatively-worded item ("one-sided presentation")
    # Keep raw values (no reversal) so high values = disagree with one-sidedness = positive view
    one_sided_presentation = factor(case_when(
      Q10_3 == 1 ~ 1,
      Q10_3 == 2 ~ 2,
      Q10_3 == 3 ~ 3,
      Q10_3 == 4 ~ 4,
      Q10_3 == 5 ~ 5,
      Q10_3 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Strongly agree", "Agree", "Neither/nor", 
               "Disagree", "Strongly disagree"))
  )

## Alternative nyhedsvaner
## Alternative news habits
Q12_media_altnews <- df %>%
  transmute(
    other_perspectives = factor(case_when(
      Q12_1 == 1 ~ 5,
      Q12_1 == 2 ~ 4,
      Q12_1 == 3 ~ 3,
      Q12_1 == 4 ~ 2,
      Q12_1 == 5 ~ 1,
      Q12_1 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Never", "Rarely", "Weekly", "Daily", "Several times a day")),
    
    not_covered_tradmedia = factor(case_when(
      Q12_2 == 1 ~ 5,
      Q12_2 == 2 ~ 4,
      Q12_2 == 3 ~ 3,
      Q12_2 == 4 ~ 2,
      Q12_2 == 5 ~ 1,
      Q12_2 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Never", "Rarely", "Weekly", "Daily", "Several times a day")),
    
    new_sources = factor(case_when(
      Q12_3 == 1 ~ 5,
      Q12_3 == 2 ~ 4,
      Q12_3 == 3 ~ 3,
      Q12_3 == 4 ~ 2,
      Q12_3 == 5 ~ 1,
      Q12_3 == 6 ~ NA_real_
    ), levels = 1:5,
    labels = c("Never", "Rarely", "Weekly", "Daily", "Several times a day"))
  )

# Social media platforms
some_usage_platforms <- df %>%
  transmute(
    whatsapp = factor(case_when(
      Q15C01 == 1 ~ "Yes",
      Q15C01 == 0 ~ "No"
    ), levels = c("No", "Yes")),
    
    x_twitter = factor(case_when(
      Q15C02 == 1 ~ "Yes",
      Q15C02 == 0 ~ "No"
    ), levels = c("No", "Yes")),
    
    reddit = factor(case_when(
      Q15C03 == 1 ~ "Yes",
      Q15C03 == 0 ~ "No"
    ), levels = c("No", "Yes")),
    
    linkedin = factor(case_when(
      Q15C04 == 1 ~ "Yes",
      Q15C04 == 0 ~ "No"
    ), levels = c("No", "Yes")),
    
    youtube = factor(case_when(
      Q15C05 == 1 ~ "Yes",
      Q15C05 == 0 ~ "No"
    ), levels = c("No", "Yes")),
    
    tiktok = factor(case_when(
      Q15C06 == 1 ~ "Yes",
      Q15C06 == 0 ~ "No"
    ), levels = c("No", "Yes")),
    
    pinterest = factor(case_when(
      Q15C07 == 1 ~ "Yes",
      Q15C07 == 0 ~ "No"
    ), levels = c("No", "Yes")),
    
    fb_messenger = factor(case_when(
      Q15C08 == 1 ~ "Yes",
      Q15C08 == 0 ~ "No"
    ), levels = c("No", "Yes")),
    
    telegram = factor(case_when(
      Q15C09 == 1 ~ "Yes",
      Q15C09 == 0 ~ "No"
    ), levels = c("No", "Yes")),
    
    discord = factor(case_when(
      Q15C10 == 1 ~ "Yes",
      Q15C10 == 0 ~ "No"
    ), levels = c("No", "Yes")),
    
    facebook = factor(case_when(
      Q15C11 == 1 ~ "Yes",
      Q15C11 == 0 ~ "No"
    ), levels = c("No", "Yes")),
    
    snapchat = factor(case_when(
      Q15C12 == 1 ~ "Yes",
      Q15C12 == 0 ~ "No"
    ), levels = c("No", "Yes")),
    
    instagram = factor(case_when(
      Q15C13 == 1 ~ "Yes",
      Q15C13 == 0 ~ "No"
    ), levels = c("No", "Yes"))
  )

# Participating in conversation about politics on social media (English)

some_pol_participation <- df %>%
  transmute(
    like_pos = factor(case_when(
      Q16_1 == 1 ~ 6,   # Several times a day
      Q16_1 == 2 ~ 5,   # Daily
      Q16_1 == 3 ~ 4,   # Weekly
      Q16_1 == 4 ~ 3,   # Rarely
      Q16_1 == 5 ~ 1,   # Never
      Q16_1 == 6 ~ 98,  # Don't know
      Q16_1 == 7 ~ 99   # Not relevant
    ), levels = c(1, 3, 4, 5, 6, 98, 99),
    labels = c("Never", "Rarely", "Weekly", "Daily", "Several times a day", "Don't know", "Not relevant")),
    
    like_neg = factor(case_when(
      Q16_2 == 1 ~ 6,
      Q16_2 == 2 ~ 5,
      Q16_2 == 3 ~ 4,
      Q16_2 == 4 ~ 3,
      Q16_2 == 5 ~ 1,
      Q16_2 == 6 ~ 98,
      Q16_2 == 7 ~ 99
    ), levels = c(1, 3, 4, 5, 6, 98, 99),
    labels = c("Never", "Rarely", "Weekly", "Daily", "Several times a day", "Don't know", "Not relevant")),
    
    share_without_text = factor(case_when(
      Q16_3 == 1 ~ 6,
      Q16_3 == 2 ~ 5,
      Q16_3 == 3 ~ 4,
      Q16_3 == 4 ~ 3,
      Q16_3 == 5 ~ 1,
      Q16_3 == 6 ~ 98,
      Q16_3 == 7 ~ 99
    ), levels = c(1, 3, 4, 5, 6, 98, 99),
    labels = c("Never", "Rarely", "Weekly", "Daily", "Several times a day", "Don't know", "Not relevant")),
    
    share_with_text = factor(case_when(
      Q16_4 == 1 ~ 6,
      Q16_4 == 2 ~ 5,
      Q16_4 == 3 ~ 4,
      Q16_4 == 4 ~ 3,
      Q16_4 == 5 ~ 1,
      Q16_4 == 6 ~ 98,
      Q16_4 == 7 ~ 99
    ), levels = c(1, 3, 4, 5, 6, 98, 99),
    labels = c("Never", "Rarely", "Weekly", "Daily", "Several times a day", "Don't know", "Not relevant")),
    
    comment = factor(case_when(
      Q16_5 == 1 ~ 6,
      Q16_5 == 2 ~ 5,
      Q16_5 == 3 ~ 4,
      Q16_5 == 4 ~ 3,
      Q16_5 == 5 ~ 1,
      Q16_5 == 6 ~ 98,
      Q16_5 == 7 ~ 99
    ), levels = c(1, 3, 4, 5, 6, 98, 99),
    labels = c("Never", "Rarely", "Weekly", "Daily", "Several times a day", "Don't know", "Not relevant")),
    
    post = factor(case_when(
      Q16_6 == 1 ~ 6,
      Q16_6 == 2 ~ 5,
      Q16_6 == 3 ~ 4,
      Q16_6 == 4 ~ 3,
      Q16_6 == 5 ~ 1,
      Q16_6 == 6 ~ 98,
      Q16_6 == 7 ~ 99
    ), levels = c(98, 99, 1, 3, 4, 5, 6),
    labels = c("Don't know", "Not relevant", "Never", "Rarely", "Weekly", "Daily", "Several times a day")),
    
    flagging = factor(case_when(
      Q16_7 == 1 ~ 6,
      Q16_7 == 2 ~ 5,
      Q16_7 == 3 ~ 4,
      Q16_7 == 4 ~ 3,
      Q16_7 == 5 ~ 1,
      Q16_7 == 6 ~ 98,
      Q16_7 == 7 ~ 99
    ), levels = c(1, 3, 4, 5, 6, 98, 99),
    labels = c("Never", "Rarely", "Weekly", "Daily", "Several times a day", "Don't know", "Not relevant"))
  )

some_pol_participation <- some_pol_participation %>%
  mutate(
    poste_opslag_grouped = case_when(
      post %in% c("Daily", "Several times a day") ~ "Daily",
      post == "Weekly" ~ "Weekly",
      post == "Rarely" ~ "Monthly",
      post == "Never" ~ "Never",
      post %in% c("Don't know", "Not relevant") ~ "Don't know / Not relevant",
      TRUE ~ NA_character_
    ),
    poste_opslag_grouped = factor(
      poste_opslag_grouped,
      levels = c("Don't know / Not relevant", "Never", "Monthly", "Weekly", "Daily")
    )
  )


# check
frq(some_pol_participation$flagging)

# Share motives (English labels)
some_share_motives <- df %>%
  transmute(
    dele_enig = factor(case_when(
      Q17_1 == 1 ~ 5,   # Very often
      Q17_1 == 2 ~ 4,   # Often
      Q17_1 == 3 ~ 3,   # Sometimes
      Q17_1 == 4 ~ 2,   # Rarely
      Q17_1 == 5 ~ 1,   # Never
      Q17_1 == 6 ~ 98   # Don't know
    ), levels = c(1, 2, 3, 4, 5, 98),
    labels = c("Never", "Rarely", "Sometimes", "Often", "Very often", "Don't know")),
    
    dele_uenig = factor(case_when(
      Q17_2 == 1 ~ 5,
      Q17_2 == 2 ~ 4,
      Q17_2 == 3 ~ 3,
      Q17_2 == 4 ~ 2,
      Q17_2 == 5 ~ 1,
      Q17_2 == 6 ~ 98
    ), levels = c(1, 2, 3, 4, 5, 98),
    labels = c("Never", "Rarely", "Sometimes", "Often", "Very often", "Don't know"))
  )

# Comment motives (English labels)
some_comment_motives <- df %>%
  transmute(
    kommentere_enig = factor(case_when(
      Q18_1 == 1 ~ 5,   # Very often
      Q18_1 == 2 ~ 4,   # Often
      Q18_1 == 3 ~ 3,   # Sometimes
      Q18_1 == 4 ~ 2,   # Rarely
      Q18_1 == 5 ~ 1,   # Never
      Q18_1 == 6 ~ 98,  # Don't know
      TRUE ~ NA_real_
    ), levels = c(1, 2, 3, 4, 5, 98),
    labels = c("Never", "Rarely", "Sometimes", "Often", "Very often", "Don't know")),
    
    kommentere_uenig = factor(case_when(
      Q18_2 == 1 ~ 5,
      Q18_2 == 2 ~ 4,
      Q18_2 == 3 ~ 3,
      Q18_2 == 4 ~ 2,
      Q18_2 == 5 ~ 1,
      Q18_2 == 6 ~ 98,
      TRUE ~ NA_real_
    ), levels = c(1, 2, 3, 4, 5, 98),
    labels = c("Never", "Rarely", "Sometimes", "Often", "Very often", "Don't know"))
  )

# Last seen opposing content (flipped scale; English labels)
some_uenig_set_senest <- df %>%
  transmute(
    uenig_set_senest = factor(case_when(
      Q19 == 1 ~ 6,   # Today -> Never
      Q19 == 2 ~ 5,   # Within last 7 days -> More than half a year ago
      Q19 == 3 ~ 4,   # Within last 4 weeks -> Within the last half year
      Q19 == 4 ~ 3,   # Within the last half year -> Within the last 4 weeks
      Q19 == 5 ~ 2,   # More than half a year ago -> Within the last 7 days
      Q19 == 6 ~ 1,   # Never -> Today
      Q19 == 7 ~ 98,  # Don't know (unchanged)
      TRUE ~ NA_real_
    ), levels = c(98, 1, 2, 3, 4, 5, 6),
    labels = c(
      "Don't know",
      "Never",
      "More than half a year ago",
      "Within the last half year",
      "Within the last 4 weeks",
      "Within the last 7 days",
      "Today"
    ))
  )

# Last shared opposing content (flipped scale; English labels)
some_uenig_delt_senest <- df %>%
  transmute(
    uenig_delt_senest = factor(case_when(
      Q20 == 1 ~ 6,   # Today -> Never
      Q20 == 2 ~ 5,   # Within last 7 days -> More than half a year ago
      Q20 == 3 ~ 4,   # Within last 4 weeks -> Within the last half year
      Q20 == 4 ~ 3,   # Within the last half year -> Within the last 4 weeks
      Q20 == 5 ~ 2,   # More than half a year ago -> Within the last 7 days
      Q20 == 6 ~ 1,   # Never -> Today
      Q20 == 7 ~ 98,  # Don't know (unchanged)
      TRUE ~ NA_real_
    ), levels = c(98, 1, 2, 3, 4, 5, 6),
    labels = c(
      "Don't know",
      "Never",
      "More than half a year ago",
      "Within the last half year",
      "Within the last 4 weeks",
      "Within the last 7 days",
      "Today"
    ))
  )

# Reactions to posts (English labels)
some_reaktion_paa_opslag <- df %>%
  transmute(
    soege_info = factor(case_when(
      Q22_1 == 1 ~ 5,   # Very often
      Q22_1 == 2 ~ 4,   # Often
      Q22_1 == 3 ~ 3,   # Sometimes
      Q22_1 == 4 ~ 2,   # Rarely
      Q22_1 == 5 ~ 1,   # Never
      Q22_1 == 6 ~ 98,  # Don't know
      TRUE ~ NA_real_
    ), levels = c(1, 2, 3, 4, 5, 98),
    labels = c("Never", "Rarely", "Sometimes", "Often", "Very often", "Don't know")),
    
    genoverveje_holdning = factor(case_when(
      Q22_2 == 1 ~ 5,
      Q22_2 == 2 ~ 4,
      Q22_2 == 3 ~ 3,
      Q22_2 == 4 ~ 2,
      Q22_2 == 5 ~ 1,
      Q22_2 == 6 ~ 98,
      TRUE ~ NA_real_
    ), levels = c(1, 2, 3, 4, 5, 98),
    labels = c("Never", "Rarely", "Sometimes", "Often", "Very often", "Don't know")),
    
    indrapportere = factor(case_when(
      Q22_3 == 1 ~ 5,
      Q22_3 == 2 ~ 4,
      Q22_3 == 3 ~ 3,
      Q22_3 == 4 ~ 2,
      Q22_3 == 5 ~ 1,
      Q22_3 == 6 ~ 98,
      TRUE ~ NA_real_
    ), levels = c(1, 2, 3, 4, 5, 98),
    labels = c("Never", "Rarely", "Sometimes", "Often", "Very often", "Don't know")),
    
    blive_forarget = factor(case_when(
      Q22_4 == 1 ~ 5,
      Q22_4 == 2 ~ 4,
      Q22_4 == 3 ~ 3,
      Q22_4 == 4 ~ 2,
      Q22_4 == 5 ~ 1,
      Q22_4 == 6 ~ 98,
      TRUE ~ NA_real_
    ), levels = c(1, 2, 3, 4, 5, 98),
    labels = c("Never", "Rarely", "Sometimes", "Often", "Very often", "Don't know"))
  )

some_usage <- df %>%
  transmute(
    foelger_samfundsdebattorer = factor(case_when(
      df$Q23 == 1 ~ 1,  # Ja
      df$Q23 == 2 ~ 0,  # Nej
      TRUE ~ NA_real_
    ), levels = c(0, 1),
    labels = c("Nej", "Ja"))
  )

some_usage <- some_usage %>%
  mutate(
    q25_enig_holdninger = factor(
      case_when(
        df$Q25_1 == 1 ~ 5,
        df$Q25_1 == 2 ~ 4,
        df$Q25_1 == 3 ~ 3,
        df$Q25_1 == 4 ~ 2,
        df$Q25_1 == 5 ~ 1,
        df$Q25_1 == 6 ~ 98,
        TRUE ~ NA_real_
      ),
      levels = c(1, 2, 3, 4, 5, 98),
      labels = c("Meget vigtigt", "Vigtigt", "Hverken/eller", "Lidt vigtigt", "Overhovedet ikke vigtigt", "Ved ikke")
    ),
    
    q25_stoette_sag = factor(
      case_when(
        df$Q25_2 == 1 ~ 5,
        df$Q25_2 == 2 ~ 4,
        df$Q25_2 == 3 ~ 3,
        df$Q25_2 == 4 ~ 2,
        df$Q25_2 == 5 ~ 1,
        df$Q25_2 == 6 ~ 98,
        TRUE ~ NA_real_
      ),
      levels = c(1, 2, 3, 4, 5, 98),
      labels = c("Meget vigtigt", "Vigtigt", "Hverken/eller", "Lidt vigtigt", "Overhovedet ikke vigtigt", "Ved ikke")
    ),
    
    q25_interesse_emne_person = factor(
      case_when(
        df$Q25_3 == 1 ~ 5,
        df$Q25_3 == 2 ~ 4,
        df$Q25_3 == 3 ~ 3,
        df$Q25_3 == 4 ~ 2,
        df$Q25_3 == 5 ~ 1,
        df$Q25_3 == 6 ~ 98,
        TRUE ~ NA_real_
      ),
      levels = c(1, 2, 3, 4, 5, 98),
      labels = c("Meget vigtigt", "Vigtigt", "Hverken/eller", "Lidt vigtigt", "Overhovedet ikke vigtigt", "Ved ikke")
    ),
    
    q25_bidrage_debatten = factor(
      case_when(
        df$Q25_4 == 1 ~ 5,
        df$Q25_4 == 2 ~ 4,
        df$Q25_4 == 3 ~ 3,
        df$Q25_4 == 4 ~ 2,
        df$Q25_4 == 5 ~ 1,
        df$Q25_4 == 6 ~ 98,
        TRUE ~ NA_real_
      ),
      levels = c(1, 2, 3, 4, 5, 98),
      labels = c("Meget vigtigt", "Vigtigt", "Hverken/eller", "Lidt vigtigt", "Overhovedet ikke vigtigt", "Ved ikke")
    ),
    
    q25_foelge_uenige = factor(
      case_when(
        df$Q25_5 == 1 ~ 5,
        df$Q25_5 == 2 ~ 4,
        df$Q25_5 == 3 ~ 3,
        df$Q25_5 == 4 ~ 2,
        df$Q25_5 == 5 ~ 1,
        df$Q25_5 == 6 ~ 98,
        TRUE ~ NA_real_
      ),
      levels = c(1, 2, 3, 4, 5, 98),
      labels = c("Meget vigtigt", "Vigtigt", "Hverken/eller", "Lidt vigtigt", "Overhovedet ikke vigtigt", "Ved ikke")
    )
  )

some_usage <- some_usage %>%
  mutate(
    q26_uenig_holdninger = factor(
      case_when(
        df$Q26_1 == 1 ~ 5,
        df$Q26_1 == 2 ~ 4,
        df$Q26_1 == 3 ~ 3,
        df$Q26_1 == 4 ~ 2,
        df$Q26_1 == 5 ~ 1,
        df$Q26_1 == 6 ~ 98,
        TRUE ~ NA_real_
      ),
      levels = c(1, 2, 3, 4, 5, 98),
      labels = c("Meget vigtigt", "Vigtigt", "Hverken/eller", "Lidt vigtigt", "Overhovedet ikke vigtigt", "Ved ikke")
    ),
    
    q26_stoette_sag = factor(
      case_when(
        df$Q26_2 == 1 ~ 5,
        df$Q26_2 == 2 ~ 4,
        df$Q26_2 == 3 ~ 3,
        df$Q26_2 == 4 ~ 2,
        df$Q26_2 == 5 ~ 1,
        df$Q26_2 == 6 ~ 98,
        TRUE ~ NA_real_
      ),
      levels = c(1, 2, 3, 4, 5, 98),
      labels = c("Meget vigtigt", "Vigtigt", "Hverken/eller", "Lidt vigtigt", "Overhovedet ikke vigtigt", "Ved ikke")
    ),
    
    q26_interesse_emne_person = factor(
      case_when(
        df$Q26_3 == 1 ~ 5,
        df$Q26_3 == 2 ~ 4,
        df$Q26_3 == 3 ~ 3,
        df$Q26_3 == 4 ~ 2,
        df$Q26_3 == 5 ~ 1,
        df$Q26_3 == 6 ~ 98,
        TRUE ~ NA_real_
      ),
      levels = c(1, 2, 3, 4, 5, 98),
      labels = c("Meget vigtigt", "Vigtigt", "Hverken/eller", "Lidt vigtigt", "Overhovedet ikke vigtigt", "Ved ikke")
    ),
    
    q26_bidrage_debatten = factor(
      case_when(
        df$Q26_4 == 1 ~ 5,
        df$Q26_4 == 2 ~ 4,
        df$Q26_4 == 3 ~ 3,
        df$Q26_4 == 4 ~ 2,
        df$Q26_4 == 5 ~ 1,
        df$Q26_4 == 6 ~ 98,
        TRUE ~ NA_real_
      ),
      levels = c(1, 2, 3, 4, 5, 98),
      labels = c("Meget vigtigt", "Vigtigt", "Hverken/eller", "Lidt vigtigt", "Overhovedet ikke vigtigt", "Ved ikke")
    ),
    
    q26_tone_utilfreds = factor(
      case_when(
        df$Q26_5 == 1 ~ 5,
        df$Q26_5 == 2 ~ 4,
        df$Q26_5 == 3 ~ 3,
        df$Q26_5 == 4 ~ 2,
        df$Q26_5 == 5 ~ 1,
        df$Q26_5 == 6 ~ 98,
        TRUE ~ NA_real_
      ),
      levels = c(1, 2, 3, 4, 5, 98),
      labels = c("Meget vigtigt", "Vigtigt", "Hverken/eller", "Lidt vigtigt", "Overhovedet ikke vigtigt", "Ved ikke")
    )
  )

library(dplyr)
# Prepare income (numeric and tertile groups) before assembling controls
income_numeric <- as.numeric(ifelse(df$DK_PERSONALINCOME %in% c(98, 99), NA, df$DK_PERSONALINCOME))
income_q <- stats::quantile(income_numeric, probs = c(1/3, 2/3), na.rm = TRUE, type = 7)
income_group_vec <- cut(income_numeric, breaks = c(-Inf, income_q[1], income_q[2], Inf),
                        labels = c("low", "mid", "high"), right = TRUE)

# Survey weights: use df$WEIGHT directly
analysis_weight_vec <- suppressWarnings(as.numeric(df$WEIGHT))

df$GENDER

controls <- df %>%
  transmute(
    #poste_opslag_grouped = some_pol_participation$poste_opslag_grouped,
    
    gender = factor(GENDER, levels = c(1, 2), labels = c("Male", "Female")),
    
    # Suggested broader age groups for stability and interpretability
    # 18-34, 35-49, 50-64, 65+
    age = cut(
      AGE,
      breaks = c(17, 34, 49, 64, Inf),
      labels = c("18-34", "35-49", "50-64", "65+"),
      right = TRUE
    ),
    
    # Simplified education categories (education) - grouped by educational level
    education = factor(
      case_when(
        DK_EDUCATION %in% c(1, 2, 3) ~ "Basic education",
        DK_EDUCATION %in% c(4, 5) ~ "Upper secondary",
        DK_EDUCATION == 6 ~ "Vocational education",
        DK_EDUCATION %in% c(7, 8, 9) ~ "Higher education",
        DK_EDUCATION %in% c(10, 11) ~ "Advanced education",
        TRUE ~ NA_character_
      ),
      levels = c("Basic education", "Upper secondary", "Vocational education", "Higher education", "Advanced education")
    ),
    # New: broader education grouping with requested labels
    # basic_upsecondary = basic + upper secondary (1-5)
    # vocational = vocational (6)
    # +higher = higher + advanced (7-11)
    education_group = factor(
      case_when(
        DK_EDUCATION %in% c(1,2,3,4,5) ~ "basic_upsecondary",
        DK_EDUCATION == 6 ~ "vocational",
        DK_EDUCATION %in% c(7,8,9,10,11) ~ "+higher",
        TRUE ~ NA_character_
      ),
      levels = c("basic_upsecondary","vocational","+higher")
    ),
    
    # Income numeric and tertiles (low/mid/high)
    income = income_numeric,
    income_group = factor(income_group_vec, levels = c("low", "mid", "high")),
    
    # Add political ideology using the same categorization as in politics_news
    political_ideology = factor(
      case_when(
        Q3 %in% c(1, 2) ~ "Far-left",
        Q3 %in% c(3, 4) ~ "Center-left", 
        Q3 %in% c(5, 6) ~ "Center",
        Q3 %in% c(7, 8) ~ "Center-right",
        Q3 %in% c(9, 10) ~ "Far-right",
        Q3 == 11 ~ NA_character_,  # "Ved ikke" (Don't know)
        TRUE ~ NA_character_
      ),
      levels = c("Center", "Far-left", "Center-left", "Center-right", "Far-right")
    ),
    
    # New: fringe vs mainstream derived from Q3
    fringe_vs_mainstream = factor(
      case_when(
        Q3 %in% c(1, 2, 9, 10) ~ "fringe",
        Q3 %in% c(3, 4, 5, 6, 7, 8) ~ "mainstream",
        Q3 == 11 ~ NA_character_,
        TRUE ~ NA_character_
      ),
      levels = c("mainstream", "fringe")
    ),
    
    # New: simplified political ideology (left/center/right)
    political_ideology_simple = factor(
      case_when(
        Q3 %in% c(1, 2, 3) ~ "left_wing",
        Q3 %in% c(4, 5, 6, 7) ~ "center",
        Q3 %in% c(8, 9, 10) ~ "right_wing",
        Q3 == 11 ~ NA_character_,
        TRUE ~ NA_character_
      ),
      levels = c("center", "left_wing", "right_wing")
    ),
    
    # Continuous left-right ideology (1-10 scale) - CONTROL VARIABLE
    left_right_scale = case_when(
      Q3 == 11 ~ NA_real_,  # "Ved ikke" (Don't know)
      TRUE ~ as.numeric(Q3)
    ),
    
    # Political engagement: follow politics and society (1-5 scale) - CONTROL VARIABLE
    # Based on Q2_1: "Det er vigtigt for mig at følge med i politik og samfundsforhold"
    # Reverse coded so higher = more engagement
    follow_politics_society = case_when(
      Q2_1 == 1 ~ 5,
      Q2_1 == 2 ~ 4,
      Q2_1 == 3 ~ 3,
      Q2_1 == 4 ~ 2,
      Q2_1 == 5 ~ 1,
      Q2_1 == 6 ~ NA_real_  # Don't know
    ),
    
    # Include analysis weight for downstream models
    analysis_weight = analysis_weight_vec
  )

frq(controls$education)

mean(df$Q3, na.rm = TRUE)



# ---- Recoding overview export (xlsx) ----

# Helper to summarize factor variables in a data frame
summarize_factor_df <- function(x) {
  vars <- names(x)
  out_list <- lapply(vars, function(v) {
    col <- x[[v]]
    if (is.factor(col) || is.ordered(col)) {
      tab <- as.data.frame(prop.table(table(col, useNA = "ifany")))
      names(tab) <- c("level", "proportion")
      tab$count <- as.integer(round(tab$proportion * sum(!is.na(col))))
      tab$variable <- v
      # Reorder columns
      tab <- tab[, c("variable", "level", "count", "proportion")]
      rownames(tab) <- NULL
      return(tab)
    } else {
      return(NULL)
    }
  })
  out <- bind_rows(out_list)
  if (is.null(out) || nrow(out) == 0) {
    # Return minimal placeholder to avoid empty sheet errors
    return(data.frame(variable = character(), level = character(), count = integer(), proportion = numeric()))
  }
  out
}

# Collect recoded data frames (factor-heavy) for overview
recoding_objects <- list(
  media_news_consumption = media_news_consumption,
  politics_news = politics_news,
  trust = trust,
  recognition = recognition,
  nonrecognition = nonrecognition,
  theme_systemkritik = theme_systemkritik,
  theme_climate_antiwoke_health = theme_climate_antiwoke_health,
  Q8_media_altnews_motivation = Q8_media_altnews_motivation,
  Q10_media_mainstreamnews = Q10_media_mainstreamnews,
  Q12_media_altnews = Q12_media_altnews,
  some_usage_platforms = some_usage_platforms,
  some_pol_participation = some_pol_participation,
  some_share_motives = some_share_motives,
  some_comment_motives = some_comment_motives,
  some_uenig_set_senest = some_uenig_set_senest,
  some_uenig_delt_senest = some_uenig_delt_senest,
  some_reaktion_paa_opslag = some_reaktion_paa_opslag,
  some_usage = some_usage,
  controls = controls %>% dplyr::select(where(
    function(col) is.factor(col) || is.ordered(col)
  ))
)

# Build sheets
recoding_sheets <- lapply(recoding_objects, summarize_factor_df)

# Build variable mapping (new variable -> original source in df)
variable_mapping <- dplyr::bind_rows(
  # media_news_consumption
  data.frame(dataset = "media_news_consumption", new_variable = c(
    "news_public_service","news_landsdaekkende_aviser","news_lokale_medier","news_tabloid",
    "news_online_profileret","news_online_politisk","news_andre","news_ingen"
  ), original_source = c("Q1C01","Q1C02","Q1C03","Q1C04","Q1C05","Q1C06","Q1C98","Q1C99"), stringsAsFactors = FALSE),
  # politics_news
  data.frame(dataset = "politics_news", new_variable = c(
    "folg_politik_samf","folg_nyheder","holdninger_for_mig_selv","undgar_nyheder","aktiv_i_forening",
    "ideologisk_venstre_hojre","ideologisk_venstre_hojre_categories"
  ), original_source = c("Q2_1","Q2_2","Q2_3","Q2_4","Q2_5","Q3","Q3"), stringsAsFactors = FALSE),
  # trust
  data.frame(dataset = "trust", new_variable = c(
    "trust_authorities","trust_justice_system","trust_politicians","trust_government",
    "trust_news_media","trust_eu","trust_intl_org","trust_citizens"
  ), original_source = c("Q4_1","Q4_2","Q4_3","Q4_4","Q4_5","Q4_6","Q4_7","Q4_8"), stringsAsFactors = FALSE),
  # recognition
  data.frame(dataset = "recognition", new_variable = c(
    "recog_care","recog_equality","recog_rights","recog_esteem","recog_value_society",
    "disrespect_misperception","disrespect_denigration","disrespect_exclusion","disrespect_discrimination"
  ), original_source = c("Q5_1","Q5_2","Q5_3","Q5_4","Q5_5","Q5_6","Q5_7","Q5_8","Q5_9"), stringsAsFactors = FALSE),
  # nonrecognition
  data.frame(dataset = "nonrecognition", new_variable = c(
    "nonrecog_care","nonrecog_equality","nonrecog_rights","nonrecog_esteem","nonrecog_value_society"
  ), original_source = c("Q5_1","Q5_2","Q5_3","Q5_4","Q5_5"), stringsAsFactors = FALSE),
  # theme_systemkritik
  data.frame(dataset = "theme_systemkritik", new_variable = c(
    "ens_partier","for_lidt_opmaerksomhed","afstand_elite_folk","magtfuld_gruppe","grundlaeggende_omlaegning","vold_noedvendigt"
  ), original_source = c("Q6_1","Q6_2","Q6_3","Q6_4","Q6_5","Q6_6"), stringsAsFactors = FALSE),
  # theme_climate_antiwoke_health
  data.frame(dataset = "theme_climate_antiwoke_health", new_variable = c(
    "klimafor_menneskeskabt","klima_vs_velfaerd","klima_overdrevet","vaccine_risiko","alt_behandling_mangler","kraenkelse_overdrevet"
  ), original_source = c("Q7_1","Q7_2","Q7_3","Q7_4","Q7_5","Q7_6"), stringsAsFactors = FALSE),
  # Q8 motivations
  data.frame(dataset = "Q8_media_altnews_motivation", new_variable = c(
    "reflect_values","feel_seen_understood","alternative_perspectives","factcheck_news","different_opinions","full_picture","change_society"
  ), original_source = c("Q8_1","Q8_2","Q8_3","Q8_4","Q8_5","Q8_6","Q8_7"), stringsAsFactors = FALSE),
  # Q10 mainstream
  data.frame(dataset = "Q10_media_mainstreamnews", new_variable = c(
    "truth_important_issues","all_voices_heard","one_sided_presentation"
  ), original_source = c("Q10_1","Q10_2","Q10_3"), stringsAsFactors = FALSE),
  # Q12 altnews
  data.frame(dataset = "Q12_media_altnews", new_variable = c(
    "other_perspectives","not_covered_tradmedia","new_sources"
  ), original_source = c("Q12_1","Q12_2","Q12_3"), stringsAsFactors = FALSE),
  # some_usage_platforms
  data.frame(dataset = "some_usage_platforms", new_variable = c(
    "whatsapp","x_twitter","reddit","linkedin","youtube","tiktok","pinterest","fb_messenger","telegram","discord","facebook","snapchat","instagram"
  ), original_source = c(
    "Q15C01","Q15C02","Q15C03","Q15C04","Q15C05","Q15C06","Q15C07","Q15C08","Q15C09","Q15C10","Q15C11","Q15C12","Q15C13"
  ), stringsAsFactors = FALSE),
  # some_pol_participation
  data.frame(dataset = "some_pol_participation", new_variable = c(
    "like_pos","like_neg","share_without_text","share_with_text","comment","post","flagging","poste_opslag_grouped"
  ), original_source = c("Q16_1","Q16_2","Q16_3","Q16_4","Q16_5","Q16_6","Q16_7","derived_from_Q16_6"), stringsAsFactors = FALSE),
  # some_share_motives
  data.frame(dataset = "some_share_motives", new_variable = c("dele_enig","dele_uenig"), original_source = c("Q17_1","Q17_2"), stringsAsFactors = FALSE),
  # some_comment_motives
  data.frame(dataset = "some_comment_motives", new_variable = c("kommentere_enig","kommentere_uenig"), original_source = c("Q18_1","Q18_2"), stringsAsFactors = FALSE),
  # some_uenig_set_senest / delt_senest
  data.frame(dataset = "some_uenig_set_senest", new_variable = c("uenig_set_senest"), original_source = c("Q19"), stringsAsFactors = FALSE),
  data.frame(dataset = "some_uenig_delt_senest", new_variable = c("uenig_delt_senest"), original_source = c("Q20"), stringsAsFactors = FALSE),
  # some_reaktion_paa_opslag
  data.frame(dataset = "some_reaktion_paa_opslag", new_variable = c("soege_info","genoverveje_holdning","indrapportere","blive_forarget"), original_source = c("Q22_1","Q22_2","Q22_3","Q22_4"), stringsAsFactors = FALSE),
  # some_usage + Q25/Q26 mappings
  data.frame(dataset = "some_usage", new_variable = c(
    "foelger_samfundsdebattorer","q25_enig_holdninger","q25_stoette_sag","q25_interesse_emne_person","q25_bidrage_debatten","q25_foelge_uenige",
    "q26_uenig_holdninger","q26_stoette_sag","q26_interesse_emne_person","q26_bidrage_debatten","q26_tone_utilfreds"
  ), original_source = c(
    "Q23","Q25_1","Q25_2","Q25_3","Q25_4","Q25_5",
    "Q26_1","Q26_2","Q26_3","Q26_4","Q26_5"
  ), stringsAsFactors = FALSE),
  # controls (selected)
  data.frame(dataset = "controls", new_variable = c(
    "gender","age","education","education_group","income","income_group","political_ideology","fringe_vs_mainstream","political_ideology_simple","analysis_weight"
  ), original_source = c(
    "GENDER","AGE","DK_EDUCATION","DK_EDUCATION","DK_PERSONALINCOME","DK_PERSONALINCOME","Q3","Q3","Q3","WEIGHT"
  ), stringsAsFactors = FALSE)
)

# Append mapping as an extra sheet
recoding_sheets$variable_mapping <- variable_mapping

# ---- Build long appendix for predictors (survey_variable, variable, level, count, proportion) ----

build_appendix <- function(df_factor, dataset_name, mapping_tbl) {
  vars <- names(df_factor)
  rows <- lapply(vars, function(v) {
    col <- df_factor[[v]]
    if (is.factor(col) || is.ordered(col)) {
      tab <- as.data.frame(prop.table(table(col, useNA = "ifany")))
      names(tab) <- c("level", "proportion")
      valid_n <- sum(!is.na(col))
      tab$count <- as.integer(round(tab$proportion * valid_n))
      tab$variable <- v
      tab$dataset <- dataset_name
      # map to original survey variable
      orig <- mapping_tbl$original_source[mapping_tbl$new_variable == v]
      tab$survey_variable <- if (length(orig) && !is.na(orig[1])) orig[1] else NA_character_
      # order cols
      tab <- tab[, c("survey_variable", "variable", "level", "count", "proportion")]
      rownames(tab) <- NULL
      return(tab)
    } else {
      return(NULL)
    }
  })
  dplyr::bind_rows(rows)
}

# Select predictor datasets
predictor_mapping <- variable_mapping %>% dplyr::filter(dataset %in% c("nonrecognition", "trust", "controls", "recognition"))

appendix_nonrecog <- build_appendix(nonrecognition, "nonrecognition", predictor_mapping)
appendix_disrespect <- build_appendix(recognition %>% dplyr::select(dplyr::starts_with("disrespect_")), "recognition_disrespect", predictor_mapping)
appendix_trust <- build_appendix(trust, "trust", predictor_mapping)
appendix_controls <- build_appendix(controls %>% dplyr::select(where(function(col) is.factor(col) || is.ordered(col))), "controls", predictor_mapping)

predictor_appendix_long <- dplyr::bind_rows(appendix_nonrecog, appendix_disrespect, appendix_trust, appendix_controls)

# Place appendix as the first sheet
recoding_sheets <- c(list(predictor_appendix_long = predictor_appendix_long), recoding_sheets)

# Ensure output directory exists and write the Excel file
dir.create("outputs/recoding_overview", recursive = TRUE, showWarnings = FALSE)
write_xlsx(recoding_sheets, path = "outputs/recoding_overview/recoding_overview.xlsx")

cat("Recoding overview written to outputs/recoding_overview/recoding_overview.xlsx\n")


# ---- Weighted frequency tables for Q8 and Q12 (digits = 0) ----

# Build weighted frequency table lines (frq-like) without relying on sjmisc weights
weighted_frq_lines <- function(x, w, digits = 0, show_na = TRUE) {
  # x is a factor/ordered
  stopifnot(length(x) == length(w))
  w <- suppressWarnings(as.numeric(w))
  w[is.na(w)] <- 0
  is_na <- is.na(x)
  levs <- levels(x)
  valid_mask <- !is_na
  w_total <- sum(w)
  w_valid_total <- sum(w[valid_mask])
  counts <- if (length(levs)) sapply(levs, function(lv) sum(w[valid_mask & x == lv])) else numeric(0)
  raw_pct <- if (w_total > 0) counts / w_total * 100 else rep(NA_real_, length(counts))
  valid_pct <- if (w_valid_total > 0) counts / w_valid_total * 100 else rep(NA_real_, length(counts))
  cum_valid_pct <- if (length(valid_pct)) cumsum(valid_pct) else valid_pct
  na_count <- sum(w[is_na])
  na_raw_pct <- if (w_total > 0) na_count / w_total * 100 else NA_real_
  # Weighted mean/sd over valid using scores 1..K
  if (any(valid_mask) && length(levs) > 0) {
    scores <- as.numeric(factor(x, levels = levs))
    m <- sum(scores[valid_mask] * w[valid_mask]) / w_valid_total
    v <- sum(w[valid_mask] * (scores[valid_mask] - m)^2) / w_valid_total
    s <- sqrt(v)
  } else {
    m <- NA_real_
    s <- NA_real_
  }
  header <- c(
    "x <categorical>",
    sprintf("# total N=%s valid N=%s mean=%s sd=%s",
            format(round(w_total, digits), trim = TRUE, scientific = FALSE),
            format(round(w_valid_total, digits), trim = TRUE, scientific = FALSE),
            ifelse(is.na(m), "NA", format(round(m, digits), nsmall = 0, trim = TRUE, scientific = FALSE)),
            ifelse(is.na(s), "NA", format(round(s, digits), nsmall = 0, trim = TRUE, scientific = FALSE))
    ),
    "",
    "Value               |   N | Raw % | Valid % | Cum. %",
    "----------------------------------------------------"
  )
  fmt <- function(val, n, rp, vp, cp) {
    sprintf("%-20s | %3s | %5s | %7s | %6s",
            val,
            format(round(n, digits), trim = TRUE, scientific = FALSE),
            ifelse(is.na(rp), "  NA", format(round(rp, digits), trim = TRUE, scientific = FALSE)),
            ifelse(is.na(vp), "    NA", format(round(vp, digits), trim = TRUE, scientific = FALSE)),
            ifelse(is.na(cp), "   NA", format(round(cp, digits), trim = TRUE, scientific = FALSE))
    )
  }
  body <- character()
  for (i in seq_along(levs)) {
    body <- c(body, fmt(levs[i], counts[i], raw_pct[i], valid_pct[i], cum_valid_pct[i]))
  }
  if (show_na) {
    body <- c(body, fmt("<NA>", na_count, na_raw_pct, NA_real_, NA_real_))
  }
  c(header, body)
}

dir.create("outputs/frequencies", recursive = TRUE, showWarnings = FALSE)

# Use original survey weight; coerce to numeric if labelled
weight_vec <- suppressWarnings(as.numeric(df$WEIGHT))

# Build weighted frq() output for all Q8 and Q12 variables
q8_vars <- names(Q8_media_altnews_motivation)
q12_vars <- names(Q12_media_altnews)

lines_out <- character()
lines_out <- c(lines_out, "================ Q8: Motivations (weighted frq, digits=0) ================")
for (v in q8_vars) {
  lines_out <- c(lines_out, paste0("\n--- ", v, " ---"))
  lines_out <- c(lines_out, weighted_frq_lines(Q8_media_altnews_motivation[[v]], w = weight_vec, digits = 0, show_na = TRUE))
}

lines_out <- c(lines_out, "\n\n================ Q12: Alternative news habits (weighted frq, digits=0) ================")
for (v in q12_vars) {
  lines_out <- c(lines_out, paste0("\n--- ", v, " ---"))
  lines_out <- c(lines_out, weighted_frq_lines(Q12_media_altnews[[v]], w = weight_vec, digits = 0, show_na = TRUE))
}

# Add all mainstream news items (Q10)
lines_out <- c(lines_out, "\n\n================ Q10: Mainstream news (weighted frq, digits=0) ================")
q10_vars <- names(Q10_media_mainstreamnews)
for (v in q10_vars) {
  lines_out <- c(lines_out, paste0("\n--- ", v, " ---"))
  lines_out <- c(lines_out, weighted_frq_lines(Q10_media_mainstreamnews[[v]], w = weight_vec, digits = 0, show_na = TRUE))
}

out_path <- "outputs/frequencies/Q8_Q12_frq_weighted.txt"
writeLines(lines_out, con = out_path)
cat("Weighted frequency tables written to ", out_path, "\n", sep = "")

# Also write Excel with separate sheets for Q8, Q12, Q10 (weighted, digits=0)
q8_sheet <- list()
q8_list <- list()
for (v in q8_vars) {
  tab_lines <- weighted_frq_lines(Q8_media_altnews_motivation[[v]], w = weight_vec, digits = 0, show_na = TRUE)
  # Convert lines to a data frame with a single column for readability
  q8_list[[v]] <- data.frame(output = tab_lines, stringsAsFactors = FALSE)
}
q12_list <- list()
for (v in q12_vars) {
  tab_lines <- weighted_frq_lines(Q12_media_altnews[[v]], w = weight_vec, digits = 0, show_na = TRUE)
  q12_list[[v]] <- data.frame(output = tab_lines, stringsAsFactors = FALSE)
}
q10_list <- list()
for (v in q10_vars) {
  tab_lines <- weighted_frq_lines(Q10_media_mainstreamnews[[v]], w = weight_vec, digits = 0, show_na = TRUE)
  q10_list[[v]] <- data.frame(output = tab_lines, stringsAsFactors = FALSE)
}

freq_xlsx <- list(
  Q8_weighted_frq = bind_rows(mapply(function(name, df) {
    cbind(variable = name, df)
  }, names(q8_list), q8_list, SIMPLIFY = FALSE) %>% bind_rows()),
  Q12_weighted_frq = bind_rows(mapply(function(name, df) {
    cbind(variable = name, df)
  }, names(q12_list), q12_list, SIMPLIFY = FALSE) %>% bind_rows()),
  Q10_weighted_frq = bind_rows(mapply(function(name, df) {
    cbind(variable = name, df)
  }, names(q10_list), q10_list, SIMPLIFY = FALSE) %>% bind_rows())
)

write_xlsx(freq_xlsx, path = "outputs/frequencies/Q8_Q10_Q12_frq_weighted.xlsx")
cat("Weighted frequency Excel written to outputs/frequencies/Q8_Q10_Q12_frq_weighted.xlsx\n")