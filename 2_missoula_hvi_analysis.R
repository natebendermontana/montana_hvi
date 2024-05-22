# data manipulation
library(MASS)
library(dplyr)
library(here)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(glue)
options(scipen = 999)

# setup
df_census <- read.csv(here("data", "census_data.csv"))
df_cdc_places <- read.csv(here("data", "cdc_places_data.csv"))
df_ejscreen <- read.csv(here("data", "ejscreen_data.csv"))
df_arcgis_data <- read.csv(here("data", "arcgis_data.csv"))

df_arcgis_data <- df_arcgis_data %>% 
  select(-XCoord, -YCoord) %>% 
  rename(geo_id = FIPS)

# mapping the pre-2020 tracts to the current tract boundaries.
# This mapping was created by manually inspecting the old v new tract boundaries in ArcGIS
# There were 9 new tracts created in Msla County for the 2020 decennial census. They were all created by
# splitting existing larger tracts into two smaller pieces, to account for population growth. As a crude workaround,
# I've simply applied the pre-2020 data to the new post-2020 tracts. For instance, data for pre-2020 tract "30063001800" has been mapped
# to the two new tracts that together make up that old tract boundary:  '30063001801' and '30063001802'
mapping_data <- data.frame(
  `geo_id` = c('30063000100', '30063000201', '30063000201', '30063000202', '30063000202',
               '30063000300', '30063000400', '30063000500', '30063000500', '30063000700',
               '30063000800', '30063000800', '30063000901', '30063000902', '30063001000',
               '30063001000', '30063001100', '30063001200', '30063001302', '30063001303',
               '30063001304', '30063001400', '30063001400', '30063001500', '30063001500',
               '30063001600', '30063001600', '30063001800', '30063001800'),
  `geo_id_2024` = c('30063000100', '30063000203', '30063000204', '30063000205', '30063000206',
                    '30063000300', '30063000400', '30063000501', '30063000502', '30063000700',
                    '30063000801', '30063000802', '30063000901', '30063000902', '30063001001',
                    '30063001002', '30063001100', '30063001200', '30063001302', '30063001303',
                    '30063001304', '30063001401', '30063001402', '30063001501', '30063001502',
                    '30063001601', '30063001602', '30063001801', '30063001802')
)

df_cdc_places <- merge(df_cdc_places, mapping_data, by.x = "geo_id", by.y = "geo_id")
df_cdc_places <- df_cdc_places %>% 
  mutate(
    geo_id_2024 = as.numeric(as.character(geo_id_2024))) %>%
  select(-geo_id) %>% # remove the pre-2020 tract IDs
  rename(geo_id = geo_id_2024,
         perc_2021_diabetes = perc_2021_DIABETES,
         perc_2021_casthma = perc_2021_CASTHMA,
         perc_2021_chd = perc_2021_CHD)

df_census <- df_census %>%
  select(starts_with("perc_"), geo_id, total_pop, area_sq_miles)


### join all the prepared dataset
df_full <- df_census %>% 
  left_join(df_cdc_places, by = "geo_id") %>% 
  select(geo_id, total_pop, area_sq_miles, perc_over_65, perc_under_5, 
         perc_disability, 
         perc_built_pre1960,
         perc_outdoor_workers,
         perc_households_alone,
         perc_pop_poverty,
         perc_unemployed,
         perc_speakenglish_less_verywell,
         perc_no_hs_degree, 
         perc_nonwhite, 
         perc_no_health_ins, 
         perc_2021_diabetes, perc_2021_casthma, perc_2021_chd)

df_full <- df_full %>% 
  left_join(df_ejscreen, by = "geo_id") %>% 
  mutate(
    pm25_statepercentile = as.numeric(as.character(pm25_statepercentile)),
    o3_statepercentile = as.numeric(as.character(o3_statepercentile))
  )

df_full <- df_full %>% 
  left_join(df_arcgis_data, by = "geo_id")

# create the impervious-to-canopy ratio variable
# The index was multiplied by -1 so that higher values correspond to more impervious coverage area and
# therefore a greater risk of an urban heat island. 
df_full <- df_full %>%
  mutate(impervious_canopy_index = 
           -1 * ((canopy_perc_mean * area_sq_miles) - (imperviousness_perc_mean * area_sq_miles)) /
           ((canopy_perc_mean * area_sq_miles) + (imperviousness_perc_mean * area_sq_miles))
  )

###
### Box Cox transformation
### 
long_data <- df_full %>%
  select(starts_with("perc_"), pm25_statepercentile, o3_statepercentile, impervious_canopy_index, surface_temp_mean) %>% 
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

# check to see if Box-Cox is indeed needed for normality
ggplot(long_data, aes(x = Value)) + 
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  facet_wrap(~Variable, scales = "free_x") +
  theme_minimal() +
  labs(title = "ORIGINAL", x = "Value", y = "Count")

df_full_transformed <- df_full # create duplicate for transformation
cols_to_transform <- setdiff(names(df_full), c("geo_id", "total_pop", "area_sq_miles", "imperviousness_perc_mean", "canopy_perc_mean"))

# Ensure all values are positive for Box-Cox transformation
df_full_transformed[cols_to_transform] <- lapply(df_full_transformed[cols_to_transform], function(x) {
  x + abs(min(x)) + 0.0001
})

# Applying Box-Cox transformation
transformed_cols <- lapply(df_full_transformed[cols_to_transform], function(y) {
  # Box-Cox transformation
  result = boxcox(y ~ 1, lambda = seq(-5, 5, 0.5))
  mylambda = result$x[which.max(result$y)]
  if (mylambda != 0) {
    y_transformed = (y^mylambda - 1) / mylambda
  } else {
    y_transformed = log(y)
  }
  return(y_transformed)
})

# Replace original columns with transformed data
df_full_transformed[cols_to_transform] <- transformed_cols

df_full_transformed <- df_full_transformed %>% 
  rename_with(.fn = ~paste0(., "_z"), .cols = all_of(cols_to_transform))

long_data <- df_full_transformed %>%
  select(starts_with("perc_"), pm25_statepercentile_z, o3_statepercentile_z, impervious_canopy_index_z, surface_temp_mean_z) %>% 
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

ggplot(long_data, aes(x = Value)) + 
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  facet_wrap(~Variable, scales = "free_x") +
  theme_minimal() +
  labs(title = "TRANSFORMED", x = "Value", y = "Count")


### standardize transformed data and create index
group_exposure <- c("o3_statepercentile_z", "pm25_statepercentile_z", "impervious_canopy_index_z", "surface_temp_mean_z")  #impervious_index #lst_averages

group_sensitivity <- c("perc_under_5_z", "perc_over_65_z", "perc_disability_z", 
                       "perc_built_pre1960_z", "perc_outdoor_workers_z",
                       "perc_households_alone_z", "perc_2021_casthma_z", 
                       "perc_2021_diabetes_z", "perc_2021_chd_z")

group_adaptive_capacity <- c("perc_no_health_ins_z", "perc_pop_poverty_z", "perc_nonwhite_z", 
                             "perc_speakenglish_less_verywell_z", "perc_no_hs_degree_z", "perc_unemployed_z")

scaled_temp <- df_full_transformed %>%
  select(-geo_id, -total_pop, -area_sq_miles, -canopy_perc_mean, -imperviousness_perc_mean) %>% 
  mutate(across(where(is.numeric), scale))

df_final <- df_full_transformed %>%
  select(geo_id, total_pop, area_sq_miles, canopy_perc_mean, imperviousness_perc_mean) %>%
  bind_cols(scaled_temp) %>%  #bring in scaled indicators
  # calculate group means
  mutate(
    exposure_group_score = rowMeans(select(., all_of(group_exposure))),
    sensitivity_group_score = rowMeans(select(., all_of(group_sensitivity))),
    ac_group_score = rowMeans(select(., all_of(group_adaptive_capacity))),
    # standardize group scores 
    z_exposure_group_score = scale(exposure_group_score, center = TRUE, scale = TRUE),
    z_sensitivity_group_score = scale(sensitivity_group_score, center = TRUE, scale = TRUE),
    z_ac_group_score = scale(ac_group_score, center = TRUE, scale = TRUE)
  ) %>% 
  mutate(
    hvi_index = rowMeans(select(., starts_with("z_")))
  )

# add original values back in
for_join <- df_full %>%
  select(starts_with("perc_"), geo_id, pm25_statepercentile, o3_statepercentile, impervious_canopy_index, surface_temp_mean) 

df_final <- df_final %>% 
  left_join(for_join, by = "geo_id")

# create the 20th percentile bins as new vars
for_ranking <- df_final %>%
  select(starts_with("perc_"), pm25_statepercentile, o3_statepercentile, impervious_canopy_index, surface_temp_mean,
         exposure_group_score, sensitivity_group_score, ac_group_score, hvi_index) %>% 
  select(-ends_with("_z"), 
         -starts_with("z_"))

# Add ranked columns for each variable in for_ranking
df_final <- df_final %>%
  mutate(across(all_of(names(for_ranking)), ~ntile(., 5), .names = "{.col}_ranked"),
         hvi_index_category = case_when(
           hvi_index_ranked == 1 ~ "Low",
           hvi_index_ranked == 2 ~ "Moderate_Low",
           hvi_index_ranked == 3 ~ "Moderate",
           hvi_index_ranked == 4 ~ "Moderate_High",
           hvi_index_ranked == 5 ~ "High",
           TRUE ~ NA_character_))

# rename variables so I don't have to do it manually in ArcGIS
new_names <- c(
  "GEOID" = "geo_id",
  "S - Original - Percent Aged Above 65" = "perc_over_65",
  "S - Ranked - Percent Aged Above 65" = "perc_over_65_ranked",
  "S - Original - Percent Aged Below 5" = "perc_under_5",
  "S - Ranked - Percent Aged Below 5" = "perc_under_5_ranked",
  "S - Original - Percent with Disability" = "perc_disability",
  "S - Ranked - Percent with Disability" = "perc_disability_ranked",
  "S - Original - Percent Homes Built Before 1960" = "perc_built_pre1960",
  "S - Ranked - Percent Homes Built Before 1960" = "perc_built_pre1960_ranked",
  "S - Original - Percent in Outdoor Occupations" = "perc_outdoor_workers",
  "S - Ranked - Percent in Outdoor Occupations" = "perc_outdoor_workers_ranked",
  "S - Original - Percent Living Alone" = "perc_households_alone",
  "S - Ranked - Percent Living Alone" = "perc_households_alone_ranked",
  "A - Original - Percent Below Poverty Line" = "perc_pop_poverty",
  "A - Ranked - Percent Below Poverty Line" = "perc_pop_poverty_ranked",
  "A - Original - Percent Unemployed" = "perc_unemployed",
  "A - Ranked - Percent Unemployed" = "perc_unemployed_ranked",
  "A - Original - Percent Linguistic Isolation" = "perc_speakenglish_less_verywell",
  "A - Ranked - Percent Linguistic Isolation" = "perc_speakenglish_less_verywell_ranked",
  "A - Original - Percent Without High School Diploma" = "perc_no_hs_degree",
  "A - Ranked - Percent Without High School Diploma" = "perc_no_hs_degree_ranked",
  "A - Original - Percent Non-White" = "perc_nonwhite",
  "A - Ranked - Percent Non-White" = "perc_nonwhite_ranked",
  "A - Original - Percent No Health Insurance" = "perc_no_health_ins",
  "A - Ranked - Percent No Health Insurance" = "perc_no_health_ins_ranked",
  "S - Original - Diabetes Prevalence" = "perc_2021_diabetes",
  "S - Ranked - Diabetes Prevalence" = "perc_2021_diabetes_ranked",
  "S - Original - Asthma Prevalence" = "perc_2021_casthma",
  "S - Ranked - Asthma Prevalence" = "perc_2021_casthma_ranked",
  "S - Original - Coronary Heart Disease Prevalence" = "perc_2021_chd",
  "S - Ranked - Coronary Heart Disease Prevalence" = "perc_2021_chd_ranked",
  "E - Original - State Percentile Annual PM2.5 Concentration" = "pm25_statepercentile",
  "E - Ranked State Percentile Annual PM2.5 Concentration" = "pm25_statepercentile_ranked",
  "E - Original - State Percentile Ozone Exceedance Days" = "o3_statepercentile",
  "E - Ranked State Percentile Ozone Exceedance Days" = "o3_statepercentile_ranked",
  "E - Original - Summer Average Temperature (°F)" = "surface_temp_mean",
  "E - Ranked Summer Average Temperature" = "surface_temp_mean_ranked",
  "E - Original - Impervious to Canopy Ratio" = "impervious_canopy_index",
  "E - Ranked Impervious to Canopy Ratio" = "impervious_canopy_index_ranked",
  "Exposure Scale (E)" = "exposure_group_score_ranked",
  "Sensitivity Scale (S)" = "sensitivity_group_score_ranked",
  "Adaptive Capacity Scale (A)" = "ac_group_score_ranked",
  "HVI Scale" = "hvi_index_ranked",
  "HVI Category" = "hvi_index_category"
)

# Rename specified columns in df_final
df_final <- df_final %>%
  rename(!!!new_names)

write.csv(df_final, "data/df_final_formap.csv", row.names = FALSE)

