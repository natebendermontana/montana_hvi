# data manipulation
library(MASS)
library(dplyr)
library(here)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(glue)
library(MCDA)
library(reshape2)
library(tibble)
library(mc2d)
library(tidycensus)
library(RColorBrewer)
options(scipen = 999)

# setup
fips_to_county <- data.frame(
  fips = c("30001", "30003", "30005", "30007", "30009", "30011", "30013", "30015", "30017", "30019", 
           "30021", "30023", "30025", "30027", "30029", "30031", "30033", "30035", "30037", "30039",
           "30041", "30043", "30045", "30047", "30049", "30051", "30053", "30055", "30057", "30059",
           "30061", "30063", "30065", "30067", "30069", "30071", "30073", "30075", "30077", "30079",
           "30081", "30083", "30085", "30087", "30089", "30091", "30093", "30095", "30097", "30099",
           "30101", "30103", "30105", "30107", "30109", "30111"),
  county = c("Beaverhead", "Big Horn", "Blaine", "Broadwater", "Carbon", "Carter", "Cascade", "Chouteau", "Custer", "Daniels",
             "Dawson", "Deer Lodge", "Fallon", "Fergus", "Flathead", "Gallatin", "Garfield", "Glacier", "Golden Valley", "Granite",
             "Hill", "Jefferson", "Judith Basin", "Lake", "Lewis and Clark", "Liberty", "Lincoln", "McCone", "Madison", "Meagher",
             "Mineral", "Missoula", "Musselshell", "Park", "Petroleum", "Phillips", "Pondera", "Powder River", "Powell", "Prairie",
             "Ravalli", "Richland", "Roosevelt", "Rosebud", "Sanders", "Sheridan", "Silver Bow", "Stillwater", "Sweet Grass", "Teton",
             "Toole", "Treasure", "Valley", "Wheatland", "Wibaux", "Yellowstone")
)


df_census <- read.csv(here("data", "census_data.csv"))
df_cdc_places_mt <- read.csv(here("data", "cdc_places_data_mt.csv"))
df_ejscreen <- read.csv(here("data", "ejscreen_data.csv"))
kh_cols <- c('State', 'County', 'Historical', 'MC_slow', 'MC_no', 'EC_slow', 'EC_no', 'Rapid_Action')
kh_df <- read.csv(here("data", "ucs_kh_df_100.csv"), skip = 3, col.names = kh_cols)

df_census <- df_census %>%
  select(starts_with("perc_"), geo_id, total_pop, area_sq_miles)

df_python_data <- read.csv(here("outputs", "df_raster_analysis_final.csv"))
df_python_data <- df_python_data %>% 
  select(-area_sq_miles) %>% 
  rename(
    imperviousness_perc_mean = mean_perc_impervious,
    canopy_perc_mean = mean_perc_canopy,
    surface_temp_mean = mean_temp_f
  )

kh_df <- kh_df %>%
  filter(State == 'MT') %>% 
  mutate(mc_killer_heat_diff = MC_no - Historical,
         County = sub(" County", "", County)) %>% 
  select(State, County, Historical, MC_no, mc_killer_heat_diff)

### join all the prepared datasets
df_full <- df_census %>% 
  left_join(df_cdc_places_mt, by = "geo_id") %>% 
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
  left_join(df_python_data, by = "geo_id")

df_full <- df_full %>%
  mutate(county_fips = substr(geo_id, 1, 5)) %>%
  left_join(fips_to_county, by = c("county_fips" = "fips"))

df_full <- df_full %>%
  left_join(kh_df %>% select(County, mc_killer_heat_diff), by = c("county" = "County"))

df_full_complete <- df_full %>% 
  filter(complete.cases(.))

#### MCDA analysis with Monte Carlo simulations
# unweighted is everything except "mc_killer_heat_diff"
# bc that var was not in the NJ approach
# cols <- c("geo_id","surface_temp_mean", "perc_over_65", "perc_under_5", "perc_disability", "perc_built_pre1960",
#           "perc_outdoor_workers", "perc_households_alone", "perc_pop_poverty", "perc_unemployed",
#           "perc_speakenglish_less_verywell", "perc_no_hs_degree", "perc_nonwhite", 
#           "perc_no_health_ins", "perc_2021_diabetes", "perc_2021_casthma", 
#           "perc_2021_chd", "pm25_statepercentile", "o3_statepercentile", 
#           "impervious_canopy_index")

cols <- c("geo_id","surface_temp_mean", "mc_killer_heat_diff", "perc_over_65", "perc_under_5", "perc_disability", "perc_built_pre1960",
          "perc_outdoor_workers", "perc_households_alone", "perc_pop_poverty", "perc_unemployed",
          "perc_speakenglish_less_verywell", "perc_no_hs_degree", "perc_nonwhite",
          "perc_no_health_ins", "perc_2021_diabetes", "perc_2021_casthma",
          "perc_2021_chd", "pm25_statepercentile", "o3_statepercentile",
          "impervious_canopy_index")
fixed_weights <- c("surface_temp_mean" = 0.25, "mc_killer_heat_diff" = 0.25)
remaining_weight <- 1 - sum(fixed_weights)

data <- df_full_complete %>%
  select(all_of(cols)) %>%
  column_to_rownames(var = "geo_id")

data_norm <- as.data.frame(scale(data, center = FALSE, scale = sqrt(colSums(data^2))))
crit <- rep("max", length(cols) - 1)  # Exclude the geo_id column. 
# All variables are set to "max" indicating that the "positive" ideal scenario we want TOPSIS to solve for is a
# census tract with "ideal" vulnerability - all of these indicator variables maxed out.

n <- 10
res <- matrix(0, nrow = n, ncol = nrow(data_norm))

set.seed(123) # For reproducibility
for (i in 1:n) {
  # for equal weight scenario
  #rand_wts <- rdirichlet(1, rep(1, length(cols) - 1)) # only geoid gets removed
  #wts <- as.vector(rand_wts)
  
  # for weighted scenario
  rand_wts <- rdirichlet(1, rep(1, length(cols) - 3)) # geoid and the two fixed vars get removed
  wts <- c(fixed_weights["surface_temp_mean"], fixed_weights["mc_killer_heat_diff"], as.vector(rand_wts)) # for weighted scenario

  
  # Conduct TOPSIS analysis
  topsis_result <- TOPSIS(performanceTable = data_norm, 
                          criteriaWeights = wts, 
                          criteriaMinMax = crit)
  
  # Store the closeness results directly
  res[i, ] <- topsis_result
}

res_df <- as.data.frame(t(res))
res_df$geo_id <- rownames(data)

# Melt the data for visualization
res_melt <- melt(res_df, variable.name = "geo_id", value.name = "Closeness")
colnames(res_melt)[2] <- "Iteration"
res_melt$county_fips <- substr(res_melt$geo_id, 1, 5)

geo_ids <- res_df$geo_id
county_fips <- substr(geo_ids, 3, 5)
census_api_key <- trimws(readLines("/Users/natebender/Desktop/repo/census_2024_api_key.txt"))
census_api_key(census_api_key, install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")
counties <- get_acs(geography = "county", variables = "B01003_001", year = 2020, survey = "acs5")

res_melt <- merge(res_melt, counties, by.x = "county_fips", by.y = "GEOID", all.x = TRUE)
res_melt$NAME <- sub(" County, Montana", "", res_melt$NAME)
res_melt <- res_melt %>% 
  rename(county_name = NAME)

#### Ranking - hardcoding the cutoffs
# lower_abs <- 0.2
# midlow_abs <- 0.4
# midhigh_abs <- 0.6
# upper_abs <- 0.8
# res_melt$rank_grouped <- rep(0, nrow(res_melt))
#cutoffs <- c(.2, .4, .6, .8)

# res_melt$rank_grouped[res_melt$Closeness >= upper_abs] <- 5
# res_melt$rank_grouped[res_melt$Closeness < upper_abs & res_melt$Closeness >= midhigh_abs] <- 4
# res_melt$rank_grouped[res_melt$Closeness < midhigh_abs & res_melt$Closeness >= midlow_abs] <- 3
# res_melt$rank_grouped[res_melt$Closeness < midlow_abs & res_melt$Closeness >= lower_abs] <- 2
# res_melt$rank_grouped[res_melt$Closeness < lower_abs] <- 1
# category_names <- c("1" = "Low", "2" = "Moderate Low", "3" = "Moderate", "4" = "Moderate High", "5" = "High")

# Percentile-based cutoffs
cutoffs <- quantile(res_melt$Closeness, probs = c(0.2, 0.4, 0.6, 0.8))
res_melt$rank_grouped[res_melt$Closeness >= cutoffs[4]] <- 5
res_melt$rank_grouped[res_melt$Closeness < cutoffs[4] & res_melt$Closeness >= cutoffs[3]] <- 4
res_melt$rank_grouped[res_melt$Closeness < cutoffs[3] & res_melt$Closeness >= cutoffs[2]] <- 3
res_melt$rank_grouped[res_melt$Closeness < cutoffs[2] & res_melt$Closeness >= cutoffs[1]] <- 2
res_melt$rank_grouped[res_melt$Closeness < cutoffs[1]] <- 1

# Convert numerical categories to descriptive names using the factor function
res_melt$rank_grouped <- factor(res_melt$rank_grouped, 
                                levels = 1:5, 
                                labels = c("Low", "Moderate Low", "Moderate", "Moderate High", "High"))

climate_regions <- list(
  Western = c("Lincoln", "Flathead", "Sanders", "Lake", "Mineral", "Missoula", "Ravalli", "Granite", "Deer Lodge", "Powell"),
  Southwestern = c("Beaverhead", "Madison", "Silver Bow", "Jefferson", "Gallatin"),
  North_Central = c("Glacier", "Toole", "Liberty", "Hill", "Blaine", "Chouteau", "Pondera", "Teton"),
  Central = c("Cascade", "Lewis and Clark", "Broadwater", "Meagher", "Judith Basin", "Fergus"),
  South_Central = c("Park", "Sweet Grass", "Stillwater", "Carbon", "Yellowstone"),
  Northeastern = c("Daniels", "Sheridan", "Roosevelt", "Valley", "Phillips"),
  Southeastern = c("Garfield", "McCone", "Richland", "Dawson", "Wibaux", "Fallon", 
                   "Prairie", "Carter", "Powder River", "Custer", "Rosebud", 
                   "Treasure", "Musselshell", "Golden Valley", "Petroleum")
)

vulnerability_colors <- c(
  "Low" = "#99d8c9",            # Light blue
  "Moderate Low" = "#c9e2f5",   # Light purple
  "Moderate" = "#fdd49e",       # Light orange
  "Moderate High" = "#fb8726",  # Medium orange
  "High" = "#a60000"            # Red
)
# vulnerability_colors <- viridis::viridis(5, option = "B")
vulnerability_colors <- rev(brewer.pal(5, "RdYlGn"))
vulnerability_colors <- setNames(vulnerability_colors, levels(res_melt$rank_grouped))

plot_climate_region <- function(region_name, region_counties) {
  region_data <- res_melt %>%
    filter(county_name %in% region_counties)

  # Calculate the median Closeness for each geo_id
  df_median_closeness <- region_data %>%
    group_by(geo_id) %>%
    summarize(median_closeness = median(Closeness, na.rm = TRUE)) %>%
    ungroup()
  
  # Determine the "predominant_rank" based on the median Closeness value
  df_median_closeness <- df_median_closeness %>%
    mutate(predominant_rank = case_when(
      median_closeness < cutoffs[1] ~ "Low",
      median_closeness < cutoffs[2] ~ "Moderate Low",
      median_closeness < cutoffs[3] ~ "Moderate",
      median_closeness < cutoffs[4] ~ "Moderate High",
      TRUE ~ "High"
    ))
  
  # Rank the Closeness values across the entire dataset
  ranked_region_data <- res_melt %>%
    group_by(Iteration) %>%
    mutate(ranked_closeness = rank(Closeness, ties.method = "average")) %>%
    ungroup() %>%
    filter(county_name %in% region_counties)
  
  # Merge the median closeness and "predominant_rank" back into the region data
  ranked_region_data <- ranked_region_data %>%
    left_join(df_median_closeness, by = "geo_id")
  
  ranked_region_data$predominant_rank <- factor(ranked_region_data$predominant_rank,
                                                levels = c("Low", "Moderate Low", "Moderate", "Moderate High", "High"))
  
  # Create a composite factor for ordering by category and then by median Closeness within each category
  ranked_region_data <- ranked_region_data %>%
    arrange(predominant_rank, desc(median_closeness)) %>%
    mutate(geo_id_ordered = factor(geo_id, levels = unique(geo_id)))
  
  # create fake data just to get the category labels + colors to appear in legend
  dummy_data <- data.frame(
    Closeness = c(-1000, -1000, -1000, -1000, -1000),
    geo_id = as.character(rep(sample(unique(region_data$geo_id), 1), 5)),
    predominant_rank = factor(c("Low", "Moderate Low", "Moderate", "Moderate High", "High"),
                              levels = c("Low", "Moderate Low", "Moderate", "Moderate High", "High"))
  )

#  print(head(ranked_region_data))
  
  # region_data <- region_data %>%
  #   arrange(predominant_rank, median_closeness) %>%
  #   mutate(geo_id_ordered = factor(geo_id, levels = unique(geo_id)))

  # closeness_range <- range(region_data$Closeness, na.rm = TRUE)
  
  rank_cutoffs <- quantile(1:nrow(data_norm), probs = c(0.2, 0.4, 0.6, 0.8))
  rank_range <- range(1, nrow(data_norm))
  
  
#   ggplot(region_data, aes(x = Closeness, y = reorder(geo_id, Closeness, FUN = median), fill = predominant_rank)) +
#     geom_boxplot(outlier.size = 1) +
#     geom_boxplot(data = dummy_data, aes(x = Closeness, y = geo_id, fill = predominant_rank)) +  # Use geom_blank to ensure all levels are included in the legend
#     theme_minimal() +
#     theme(axis.text.y = element_text(size = 9)) +
#     
#     scale_fill_manual(values = vulnerability_colors, 
#                       limits = c("Low", "Moderate Low", "Moderate", "Moderate High", "High"),
#                       drop = FALSE) +  # Include all levels in the legend, even if not present in the data
#     scale_x_continuous(breaks = cutoffs, labels = round(cutoffs, 2)) + 
#     labs(title = paste("Region: ", region_name), 
#          y = "Geo ID\n", 
#          x = "\nTOPSIS Weighted Vulnerability Score", 
#          fill = "Predominant Category") +
#     guides(fill = guide_legend(title = "Vulnerability Category")) +
#     geom_vline(xintercept = cutoffs, color = "#ff5353", linetype = "dashed", linewidth = 1.2) +
#     coord_cartesian(xlim = closeness_range) +  # Set limits to the range of actual data
#     theme(plot.title = element_text(hjust = 0.5))
# }

    ggplot(ranked_region_data, aes(x = ranked_closeness, y = reorder(geo_id, ranked_closeness, FUN = median), fill = predominant_rank)) +
      geom_boxplot(outlier.size = 1) +
      geom_boxplot(data = dummy_data, aes(x = Closeness, y = geo_id, fill = predominant_rank)) +  # Use geom_blank to ensure all levels are included in the legend
      theme_minimal() +
      theme(axis.text.y = element_text(size = 9)) +  # Adjusting font size for y-axis labels
      scale_fill_manual(values = vulnerability_colors, 
                        limits = c("Low", "Moderate Low", "Moderate", "Moderate High", "High"),
                        drop = FALSE) +  # Include all levels in the legend, even if not present in the data
      scale_x_continuous() +  # Automatically adjust x-axis to fit rank values
      labs(title = paste("Region: ", region_name), 
           y = "Geo ID\n", 
           x = "\nOverall Ranking", 
           fill = "Predominant Category") +
      guides(fill = guide_legend(title = "Vulnerability Category")) +
      geom_vline(xintercept = rank_cutoffs, color = "#ff5353", linetype = "dashed", linewidth = 1.2) +
      coord_cartesian(xlim = rank_range) +  # Set limits to the range of actual data
      theme(plot.title = element_text(hjust = 0.5))
}


plot_climate_region("Western", climate_regions$Western)
plot_climate_region("Southeastern", climate_regions$Southeastern)
plot_climate_region("Southwestern", climate_regions$Southwestern)
plot_climate_region("Northeastern", climate_regions$Northeastern)
plot_climate_region("North_Central", climate_regions$North_Central)
plot_climate_region("South_Central", climate_regions$South_Central)
plot_climate_region("Central", climate_regions$Central)

#write.csv(res_melt, "data/df_topsis.csv", row.names = FALSE)
write.csv(res_melt, "data/df_topsis_weighted.csv", row.names = FALSE)


# save plots
save_all_plots <- function() {
  # Loop over each region and save the plot
  for (region_name in names(climate_regions)) {
    plot <- plot_climate_region(region_name, climate_regions[[region_name]])
    plot <- plot + theme(rect = element_rect(fill = "transparent"))
    
    filename <- paste0("outputs/plot_topsis_weighted_", tolower(region_name), ".png")
    ggsave(
      plot = plot,
      filename = filename,
      bg = "transparent"
    )
  }
}

# Call the function to save all plots
save_all_plots()

