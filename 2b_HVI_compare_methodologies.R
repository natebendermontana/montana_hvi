# Compare the NJ methodology with the TOPSIS simulations
library(dplyr)
library(here)
library(tidyr)
library(ggplot2)
options(scipen = 999)

df_equal_weight <- read.csv(here("data", "df_final_formap.csv"))
df_topsis <- read.csv(here("data", "df_topsis.csv"))

# Ensure df_equal_weight has unique geo_id and the proper column
df_equal_weight <- df_equal_weight %>%
  rename(HVI_equal_weight_cat = HVI.Category,
         geo_id = GEOID) %>%
  mutate(HVI_equal_weight_cat = factor(HVI_equal_weight_cat, levels = c("Low", "Moderate_Low", "Moderate", "Moderate_High", "High"))) %>%
  distinct(geo_id, .keep_all = TRUE) %>% 
  select(geo_id, HVI_equal_weight_cat)

# Ensure df_topsis has unique geo_id and calculate predominant category for each unique geo_id
df_topsis <- df_topsis %>%
  rename(HVI_topsis_cat = rank_grouped) %>%
  mutate(HVI_topsis_cat = factor(HVI_topsis_cat, levels = c("Low", "Moderate_Low", "Moderate", "Moderate_High", "High"))) %>%
  group_by(geo_id) %>%
  summarize(predominant_cat = names(sort(table(HVI_topsis_cat), decreasing = TRUE)[1])) %>%
  ungroup()

comparison_df <- df_equal_weight %>%
  left_join(df_topsis, by = "geo_id")  # Join with the summarized df_topsis


# Calculate agreement
comparison_df <- comparison_df %>%
  mutate(agreement = HVI_equal_weight_cat == predominant_cat)

# Calculate the percentage of agreement
agreement_perc <- mean(comparison_df$agreement) * 100

ggplot(comparison_df, aes(
  x = factor(predominant_cat, levels = c("Low", "Moderate_Low", "Moderate", "Moderate_High", "High")),
  y = factor(HVI_equal_weight_cat, levels = c("Low", "Moderate_Low", "Moderate", "Moderate_High", "High"))
)) +
  geom_jitter(aes(color = agreement), width = 0.2, height = 0.2) +
  scale_color_manual(values = c("TRUE" = "#66c2a5", "FALSE" = "#fc8d62")) +
  labs(title = "Scatter Plot of Vulnerability Category Comparison",
       x = "TOPSIS Predominant Category",
       y = "Equal Weight Category",
       color = "Agreement") +
  theme_minimal()
