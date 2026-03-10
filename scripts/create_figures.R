# ---- CREATE 2X2 RANKED COEFFICIENT PLOT (Nonrecognition variants) ----

library(haven)
library(sjmisc)
library(purrr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(sandwich)
library(lmtest)

source("scripts/data_prep.R")

extract_coefficients <- function(model_path, model_name) {
  if (!file.exists(model_path)) return(NULL)
  model <- readRDS(model_path)
  cs <- lmtest::coeftest(model, vcov = sandwich::vcovHC(model, type = "HC3"))
  df <- data.frame(
    Variable = rownames(cs),
    Coefficient = cs[,1], Std_Error = cs[,2], P_Value = cs[,4],
    Model = model_name, stringsAsFactors = FALSE
  )
  df <- df[df$Variable != "(Intercept)", ]
  df$CI_Lower <- df$Coefficient - 1.96*df$Std_Error
  df$CI_Upper <- df$Coefficient + 1.96*df$Std_Error
  df$Significant <- df$P_Value < 0.05
  df
}

clean_variable_names <- function(x){
  x %>%
    gsub("^fringe_vs_mainstreamfringe$", "Fringe vote", .) %>%
    gsub("^left_right_scale$", "Left-Right Scale", .) %>%
    gsub("nonrecog_", "Non-recognition: ", .) %>%
    gsub("disrespect_", "Disrespect: ", .) %>%
    gsub("trust_", "Trust: ", .) %>%
    gsub("^education_group", "Education: ", .) %>%
    gsub("^income_group", "Income: ", .) %>%
    gsub("^gender", "Gender: ", .) %>%
    gsub("^age", "Age: ", .) %>%
    gsub("^political_ideology_simple", "Political Ideology: ", .) %>%
    gsub("_", " ", .) %>%
    gsub("government", "Government", .) %>%
    gsub("politicians", "Politicians", .) %>%
    gsub("authorities", "Authorities", .) %>%
    gsub("justice system", "Justice System", .) %>%
    gsub("news media", "News Media", .) %>%
    gsub("intl org", "Intl Org", .) %>%
    gsub("care", "Care", .) %>%
    gsub("equality", "Equality", .) %>%
    gsub("autonomy", "Autonomy", .) %>%
    gsub("esteem", "Esteem", .) %>%
    gsub("value society", "Value Society", .) %>%
    gsub("misperception", "Misperception", .) %>%
    gsub("denigration", "Denigration", .) %>%
    gsub("exclusion", "Exclusion", .) %>%
    gsub("political", "Political", .) %>%
    gsub("citizens", "Citizens", .) %>%
    gsub("system", "System", .) %>%
    gsub("discrimination", "Discrimination", .) %>%
    tools::toTitleCase() %>%
    gsub("Non-recognition: Rights$", "Non-recognition: Rights (Capacity)", .) %>%
    gsub("Non-recognition: Equality", "Non-recognition: Rights (Status)", .)
}

create_ranked_plot <- function(data, model_name, color){
  if (is.null(data) || nrow(data) == 0) return(NULL)
  data$Variable_Clean <- clean_variable_names(data$Variable)
  # Remove controls from plot (demographics + political controls)
  data <- data %>% filter(!grepl("^(Gender:|Age:|Education:|Political Ideology:|Fringe Vs Mainstream|Fringe Vote|Income:|Left-Right Scale)", data$Variable_Clean, ignore.case = TRUE))
  data_ranked <- data %>% arrange(desc(Coefficient))
  ggplot(data_ranked, aes(x = reorder(Variable_Clean, Coefficient), y = Coefficient)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper, color = Significant), width = 0.35, linewidth = 1.1) +
    geom_point(aes(color = Significant), size = 4) +
    scale_color_manual(values = c("TRUE" = color, "FALSE" = "grey70")) +
    coord_flip() + labs(title = model_name, x = NULL, y = "Coefficient (95% CI)") +
    theme_minimal() + theme(legend.position = "none", plot.title = element_text(size = 20, face = "bold"), axis.text.y = element_text(size = 18, color = "black"), axis.text.x = element_text(size = 14, color = "black"))
}

# Version that keeps controls
create_ranked_plot_with_controls <- function(data, model_name, color){
  if (is.null(data) || nrow(data) == 0) return(NULL)
  data$Variable_Clean <- clean_variable_names(data$Variable)
  data_ranked <- data %>% arrange(desc(Coefficient))
  ggplot(data_ranked, aes(x = reorder(Variable_Clean, Coefficient), y = Coefficient)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper, color = Significant), width = 0.35, linewidth = 1.1) +
    geom_point(aes(color = Significant), size = 4) +
    scale_color_manual(values = c("TRUE" = color, "FALSE" = "grey70")) +
    coord_flip() + labs(title = model_name, x = NULL, y = "Coefficient (95% CI)") +
    theme_minimal() + theme(legend.position = "none", plot.title = element_text(size = 20, face = "bold"), axis.text.y = element_text(size = 18, color = "black"), axis.text.x = element_text(size = 14, color = "black"), panel.grid.minor = element_blank())
}

# Load models from nonrecognition runs
ugt_info <- extract_coefficients("outputs/reg_user/models/model_ugt_info_seeking.rds", "UGT Info-Seeking")
ugt_ident <- extract_coefficients("outputs/reg_user/models/model_ugt_identity.rds", "UGT Identity-Seeking")
altnews <- extract_coefficients("outputs/reg_altnews_nonrecog/models/model_q12_factor.rds", "Alternative Information Orientation")
mainstr <- extract_coefficients("outputs/reg_mainstreamnews_nonrecog/models/model_q10_factor.rds", "Mainstream News Rejection")

p1 <- create_ranked_plot(ugt_info, "UGT Info-Seeking", "#FFD700")
p2 <- create_ranked_plot(ugt_ident, "UGT Identity-Seeking", "#4169E1")
p3 <- create_ranked_plot(altnews, "Alternative Information Orientation", "#32CD32")
p4 <- create_ranked_plot(mainstr, "Mainstream News Rejection", "#FF69B4")

# Create directory for outputs
dir.create("outputs/analysis/combined_plots", recursive = TRUE, showWarnings = FALSE)

# Save individual plots WITHOUT titles
p1_notitle <- create_ranked_plot(ugt_info, "", "#FFD700")
p2_notitle <- create_ranked_plot(ugt_ident, "", "#4169E1")
p3_notitle <- create_ranked_plot(altnews, "", "#32CD32")
p4_notitle <- create_ranked_plot(mainstr, "", "#FF69B4")

ggsave("outputs/analysis/combined_plots/individual_ugt_info_seeking_notitle.png", p1_notitle, width = 9, height = 6.5, dpi = 300, bg = "white")
ggsave("outputs/analysis/combined_plots/individual_ugt_identity_notitle.png", p2_notitle, width = 9, height = 6.5, dpi = 300, bg = "white")
ggsave("outputs/analysis/combined_plots/individual_alt_news_notitle.png", p3_notitle, width = 9, height = 6.5, dpi = 300, bg = "white")
ggsave("outputs/analysis/combined_plots/individual_msm_rejection_notitle.png", p4_notitle, width = 9, height = 6.5, dpi = 300, bg = "white")

# Save combined plot
combined <- grid.arrange(p1, p2, p3, p4, ncol = 2, top = textGrob("Coefficient Plot (Non-recognition)", gp = gpar(fontsize = 22, fontface = "bold")))
ggsave("outputs/analysis/combined_plots/coefficient_plot_2x2_nonrecog.png", combined, width = 18, height = 13, dpi = 300, bg = "white")

# Also generate version WITH controls
p1c <- create_ranked_plot_with_controls(ugt_info, "UGT Info-Seeking - With Controls", "#FFD700")
p2c <- create_ranked_plot_with_controls(ugt_ident, "UGT Identity-Seeking - With Controls", "#4169E1")
p3c <- create_ranked_plot_with_controls(altnews, "Alternative Information Orientation - With Controls", "#32CD32")
p4c <- create_ranked_plot_with_controls(mainstr, "Mainstream News Rejection - With Controls", "#FF69B4")

# Save individual plots WITH controls but WITHOUT titles
p1c_notitle <- create_ranked_plot_with_controls(ugt_info, "", "#FFD700")
p2c_notitle <- create_ranked_plot_with_controls(ugt_ident, "", "#4169E1")
p3c_notitle <- create_ranked_plot_with_controls(altnews, "", "#32CD32")
p4c_notitle <- create_ranked_plot_with_controls(mainstr, "", "#FF69B4")

ggsave("outputs/analysis/combined_plots/individual_ugt_info_seeking_withcontrols_notitle.png", p1c_notitle, width = 9, height = 8, dpi = 300, bg = "white")
ggsave("outputs/analysis/combined_plots/individual_ugt_identity_withcontrols_notitle.png", p2c_notitle, width = 9, height = 8, dpi = 300, bg = "white")
ggsave("outputs/analysis/combined_plots/individual_alt_news_withcontrols_notitle.png", p3c_notitle, width = 9, height = 8, dpi = 300, bg = "white")
ggsave("outputs/analysis/combined_plots/individual_msm_rejection_withcontrols_notitle.png", p4c_notitle, width = 9, height = 8, dpi = 300, bg = "white")

# Save combined plot with controls
combined_controls <- grid.arrange(p1c, p2c, p3c, p4c, ncol = 2, top = textGrob("Coefficient Plot (Non-recognition, With Controls)", gp = gpar(fontsize = 22, fontface = "bold")))
ggsave("outputs/analysis/combined_plots/coefficient_plot_2x2_nonrecog_withcontrols.png", combined_controls, width = 18, height = 13, dpi = 300, bg = "white")

cat("Figures created. Run after reg_h1, reg_h2, reg_h3 to produce coefficient plots.\n")