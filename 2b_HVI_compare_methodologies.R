# Compare the NJ methodology with the TOPSIS simulations
library(dplyr)
library(here)
library(tidyr)
library(ggplot2)
options(scipen = 999)

df_equal_weight_full <- read.csv(here("data", "df_final_formap.csv"))
df_topsis_full <- read.csv(here("data", "df_topsis.csv"))
df_topsis_weights_full <- read.csv(here("data", "df_topsis_weighted.csv"))


df_equal_weight_full <- df_equal_weight_full %>%
  rename(HVI_equal_weight_cat = HVI.Category,
         geo_id = GEOID) %>% 
  select(geo_id, HVI_equal_weight_cat, hvi_index)


# Ensure df_equal_weight has unique geo_id and the proper column
df_equal_weight <- df_equal_weight_full %>% 
  mutate(HVI_equal_weight_cat = factor(HVI_equal_weight_cat, levels = c("Low", "Moderate_Low", "Moderate", "Moderate_High", "High"))) %>%
  distinct(geo_id, .keep_all = TRUE)

# Ensure df_topsis has unique geo_id and calculate predominant category for each unique geo_id
df_topsis <- df_topsis_full %>%
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

# Scatterplot of categories
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

# another approach, using the quantitative vulnerability estimators rather than the categories
df_equal_weight_quant <- df_equal_weight_full %>%
  mutate(hvi_percentile = percent_rank(hvi_index))

df_topsis_quant <- df_topsis_full %>%
  group_by(geo_id) %>%
  summarize(median_closeness = median(Closeness)) %>%
  ungroup()

df_topsis_quant <- df_topsis_quant %>%
  mutate(closeness_percentile = percent_rank(median_closeness)) 

comparison_percentiles <- df_equal_weight_quant %>%
  select(geo_id, hvi_percentile) %>%
  left_join(df_topsis_quant %>% select(geo_id, closeness_percentile), by = "geo_id")

ggplot(comparison_percentiles, aes(x = hvi_percentile, y = closeness_percentile)) +
  geom_point() +
  scale_color_manual(values = vulnerability_colors) +
  labs(title = "Comparison of Percentile Ranks for Vulnerability Estimates",
       x = "HVI Percentile (Equal Weight Method)",
       y = "Median Closeness Percentile (Unweighted TOPSIS Method)",
       color = "HVI Percentile Category") +
  theme_minimal()

pearson_corr <- cor(comparison_percentiles$hvi_percentile, comparison_percentiles$closeness_percentile, method = "pearson")
spearman_corr <- cor(comparison_percentiles$hvi_percentile, comparison_percentiles$closeness_percentile, method = "spearman")
cat("Pearson Correlation between HVI Percentile and Closeness Percentile:", pearson_corr, "\n")
cat("Spearman Correlation between HVI Percentile and Closeness Percentile:", spearman_corr, "\n")

write.csv(comparison_percentiles, "data/df_methodology_comparison.csv", row.names = FALSE)

######################################################
######################################################
df_topsis_weights <- df_topsis_weights_full %>%
  rename(HVI_topsis_cat = rank_grouped) %>%
  mutate(HVI_topsis_cat = factor(HVI_topsis_cat, levels = c("Low", "Moderate_Low", "Moderate", "Moderate_High", "High"))) %>%
  group_by(geo_id) %>%
  summarize(predominant_cat = names(sort(table(HVI_topsis_cat), decreasing = TRUE)[1])) %>%
  ungroup()

comparison_df_weights <- df_equal_weight %>%
  left_join(df_topsis_weights, by = "geo_id")  # Join with the summarized df_topsis

# Calculate agreement
comparison_df_weights <- comparison_df_weights %>%
  mutate(agreement = HVI_equal_weight_cat == predominant_cat)


# Calculate the percentage of agreement
agreement_perc <- mean(comparison_df_weights$agreement) * 100

ggplot(comparison_df_weights, aes(
  x = factor(predominant_cat, levels = c("Low", "Moderate_Low", "Moderate", "Moderate_High", "High")),
  y = factor(HVI_equal_weight_cat, levels = c("Low", "Moderate_Low", "Moderate", "Moderate_High", "High"))
)) +
  geom_jitter(aes(color = agreement), width = 0.2, height = 0.2) +
  scale_color_manual(values = c("TRUE" = "#66c2a5", "FALSE" = "#fc8d62")) +
  labs(title = "Scatter Plot of Vulnerability Category Comparison",
       x = "Weighted TOPSIS Predominant Category",
       y = "Equal Weight Category",
       color = "Agreement") +
  theme_minimal()


# another approach, using the quantitative vulnerability estimators rather than the categories
df_equal_weight_quant <- df_equal_weight_full %>%
  mutate(hvi_percentile = percent_rank(hvi_index))

df_topsis_weights_quant <- df_topsis_weights_full %>%
  group_by(geo_id) %>%
  summarize(median_closeness = median(Closeness)) %>%
  ungroup()

df_topsis_weights_quant <- df_topsis_weights_quant %>%
  mutate(closeness_percentile = percent_rank(median_closeness)) 

comparison_weights_percentiles <- df_equal_weight_quant %>%
  select(geo_id, hvi_percentile) %>%
  left_join(df_topsis_weights_quant %>% select(geo_id, closeness_percentile), by = "geo_id")

ggplot(comparison_weights_percentiles, aes(x = hvi_percentile, y = closeness_percentile)) +
  geom_point() +
  #scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Comparison of Percentile Ranks for Vulnerability Estimates",
       x = "HVI Percentile (Equal Weight Method)",
       y = "Median Closeness Percentile (Weighted TOPSIS Method)",
       color = "HVI Percentile") +
  theme_minimal()

pearson_corr <- cor(comparison_weights_percentiles$hvi_percentile, comparison_weights_percentiles$closeness_percentile, method = "pearson")
spearman_corr <- cor(comparison_weights_percentiles$hvi_percentile, comparison_weights_percentiles$closeness_percentile, method = "spearman")
cat("Pearson Correlation between HVI Percentile and Closeness Percentile:", pearson_corr, "\n")
cat("Spearman Correlation between HVI Percentile and Closeness Percentile:", spearman_corr, "\n")


