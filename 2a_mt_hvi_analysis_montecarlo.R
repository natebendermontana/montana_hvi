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
cols <- c("geo_id","surface_temp_mean", "mc_killer_heat_diff", "perc_over_65", "perc_under_5", "perc_disability", "perc_built_pre1960",
          "perc_outdoor_workers", "perc_households_alone", "perc_pop_poverty", "perc_unemployed",
          "perc_speakenglish_less_verywell", "perc_no_hs_degree", "perc_nonwhite", 
          "perc_no_health_ins", "perc_2021_diabetes", "perc_2021_casthma", 
          "perc_2021_chd", "pm25_statepercentile", "o3_statepercentile", 
          "impervious_canopy_index")

data <- df_full_complete %>%
  select(all_of(cols)) %>%
  column_to_rownames(var = "geo_id")

data_norm <- as.data.frame(scale(data, center = FALSE, scale = sqrt(colSums(data^2))))
crit <- rep("max", length(cols) - 1)  # Exclude the geo_id column. 
# All variables are set to "max" indicating that the "positive" ideal scenario we want TOPSIS to solve for is a
# census tract with "ideal" vulnerability - all of these indicator variables maxed out.

n <- 10000
res <- matrix(0, nrow = n, ncol = nrow(data_norm))
fixed_weights <- c("surface_temp_mean" = 0.2, "mc_killer_heat_diff" = 0.2)
remaining_weight <- 1 - sum(fixed_weights)


set.seed(123) # For reproducibility

for (i in 1:n) {
  # Generate random weights for all criteria
  rand_wts <- rdirichlet(1, rep(1, length(cols) - 3)) # remaining_weight  # 18 variables excluding the two fixed weight variables
  wts <- c(fixed_weights["surface_temp_mean"], fixed_weights["mc_killer_heat_diff"], as.vector(rand_wts))
  # wts <- as.vector(rand_wts) # if equal weighting

  
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
#state_fips <- substr(geo_ids, 1, 2)
county_fips <- substr(geo_ids, 3, 5)
#fips_codes <- paste0(state_fips, county_fips)
census_api_key <- trimws(readLines("/Users/natebender/Desktop/repo/census_2024_api_key.txt"))
census_api_key(census_api_key, install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")
counties <- get_acs(geography = "county", variables = "B01003_001", year = 2020, survey = "acs5")

res_melt <- merge(res_melt, counties, by.x = "county_fips", by.y = "GEOID", all.x = TRUE)
res_melt$NAME <- sub(" County, Montana", "", res_melt$NAME)
res_melt <- res_melt %>% 
  rename(county_name = NAME)

#### Ranking
# lower_abs <- 0.25
# mid_abs <- 0.5
# upper_abs <- 0.75
# res_melt$rank_grouped <- rep(0, nrow(res_melt))
# 
# res_melt$rank_grouped[res_melt$Closeness >= upper_abs] <- 4
# res_melt$rank_grouped[res_melt$Closeness < upper_abs & res_melt$Closeness >= mid_abs] <- 3
# res_melt$rank_grouped[res_melt$Closeness < mid_abs & res_melt$Closeness >= lower_abs] <- 2
# res_melt$rank_grouped[res_melt$Closeness < lower_abs] <- 1

# trying percentile-based cutoffs
cutoffs <- quantile(res_melt$Closeness, probs = c(0.2, 0.4, 0.6, 0.8))

res_melt <- res_melt %>%
  mutate(rank_grouped = case_when(
    Closeness < cutoffs[1] ~ "Low",
    Closeness >= cutoffs[1] & Closeness < cutoffs[2] ~ "Moderate_Low",
    Closeness >= cutoffs[2] & Closeness < cutoffs[3] ~ "Moderate",
    Closeness >= cutoffs[3] & Closeness < cutoffs[4] ~ "Moderate_High",
    Closeness >= cutoffs[4] ~ "High"
  ))

# Create a mapping for the rank_grouped to descriptive labels
# grp_dict <- c("1" = "Low", "2" = "Medium-low", "3" = "Medium-high", "4" = "High")
# res_melt$rank_grouped <- factor(res_melt$rank_grouped, levels = c(1, 2, 3, 4), labels = grp_dict)
res_melt$rank_grouped <- factor(res_melt$rank_grouped, levels = c("Low", "Moderate_Low", "Moderate", "Moderate_High", "High"))


# 
# r_western <- c("Lincoln", "Flathead", "Sanders", "Lake", "Mineral", "Missoula", "Ravalli", "Granite", "Deer Lodge", "Powell")
# r_southwestern <- c("Beaverhead", "Madison", "Silver Bow", "Jefferson", "Gallatin")
# r_north_central <- c("Glacier", "Toole", "Liberty", "Hill", "Blaine", "Chouteau", "Pondera", "Teton")
# r_central <- c("Cascade", "Lewis and Clark", "Broadwater", "Meagher", "Judith Basin", "Fergus")
# r_south_central <- c("Park", "Sweet Grass", "Stillwater", "Carbon", "Yellowstone")
# r_northeastern <- c("Daniels", "Sheridan", "Roosevelt", "Valley", "Phillips")
# r_southeastern <- c("Garfield", "McCone", "Richland", "Dawson", "Wibaux", "Fallon", "Prairie", "Carter", "Powder River", "Custer", "Rosebud", "Treasure", "Musselshell", "Golden Valley", "Petroleum")
# 
# climate_region_lookup <- c(
#   setNames(rep("Western", length(r_western)), r_western),
#   setNames(rep("Southwestern", length(r_southwestern)), r_southwestern),
#   setNames(rep("North Central", length(r_north_central)), r_north_central),
#   setNames(rep("Central", length(r_central)), r_central),
#   setNames(rep("South Central", length(r_south_central)), r_south_central),
#   setNames(rep("Northeastern", length(r_northeastern)), r_northeastern),
#   setNames(rep("Southeastern", length(r_southeastern)), r_southeastern)
# )
# 
# res_melt <- res_melt %>%
#   mutate(climate_region = climate_region_lookup[county_name])
# 
# ggplot(res_melt, aes(x = Closeness)) +
#   geom_histogram(binwidth = 0.05, fill = "#69b3a2", color = "#e9ecef", alpha = 0.9) +
#   theme_minimal() +
#   labs(title = "Histogram of Closeness Values", 
#        x = "Closeness", 
#        y = "Frequency") +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# 
# 
# # Break down by climate regions
# eastern_counties <- c(r_northeastern, r_southeastern, r_south_central)
# western_counties <- c(r_western, r_southwestern, r_central, r_north_central)
# 
# 
# region_w <- res_melt %>%
#   filter(county_name %in% western_counties)
# 
# region_e <- res_melt %>%
#   filter(county_name %in% eastern_counties)
# 
# #num_counties <- length(unique(region_w$county_name))
# 
# num_regions <- length(unique(res_melt$climate_region))
# 
# 
# predominant_category_w <- region_w %>%
#   group_by(geo_id) %>%
#   summarize(predominant_rank_w = names(sort(table(rank_grouped), decreasing = TRUE)[1]))
# region_w <- region_w %>%
#   left_join(predominant_category_w, by = "geo_id")
# 
# predominant_category_e <- region_e %>%
#   group_by(geo_id) %>%
#   summarize(predominant_rank_e = names(sort(table(rank_grouped), decreasing = TRUE)[1]))
# region_e <- region_e %>%
#   left_join(predominant_category_e, by = "geo_id")
# 
# ggplot(res_melt %>%
#          filter(county_name %in% r_southeastern), aes(x = Closeness, y = reorder(geo_id, Closeness), fill = predominant_rank_e)) +
#   geom_boxplot(outlier.size = 1) +
#   theme_minimal() +
#   theme(axis.text.y = element_text(size = 9)) +  # Adjusting font size for y-axis labels
#   scale_fill_manual(values = vulnerability_colors) +
#   labs(title = "Eastern MT", y = "Geo ID", x = "Closeness", fill = "County Name") +
#   guides(fill = guide_legend(title = "Vulnerability Category")) +
#   # Add vertical lines at .25, .5, and .75 on the x-axis
#   geom_vline(xintercept = c(0.25, 0.5, 0.75), color = "#ff5353", linetype = "dashed", linewidth=1.2) 


# ggplot(region_w, aes(x = Closeness, y = reorder(geo_id, Closeness), fill = predominant_rank_w)) +
#   geom_boxplot(outlier.size = 1) +
#   theme_minimal() +
#   theme(axis.text.y = element_text(size = 9)) +  # Adjusting font size for y-axis labels
#   scale_fill_manual(values = vulnerability_colors) +
#   labs(title = "Western MT", y = "Geo ID", x = "Closeness", fill = "County Name") +
#   guides(fill = guide_legend(title = "Vulnerability Category")) +
#   # Add vertical lines at .25, .5, and .75 on the x-axis
#   geom_vline(xintercept = c(0.25, 0.5, 0.75), color = "#ff5353", linetype = "dashed", linewidth=1.2) 

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
  "Moderate_Low" = "#c9e2f5",   # Light purple
  "Moderate" = "#fdd49e",       # Light orange
  "Moderate_High" = "#fb8726",  # Medium orange
  "High" = "#a60000"            # Red
)
# vulnerability_colors <- viridis::viridis(5, option = "B")
vulnerability_colors <- rev(brewer.pal(5, "RdYlGn"))
vulnerability_colors <- setNames(vulnerability_colors, levels(res_melt$rank_grouped))


plot_climate_region <- function(region_name, region_counties) {
  # Filter the data for the specified region
  region_data <- res_melt %>%
    filter(county_name %in% region_counties)
  
  # Calculate the predominant category for each geo_id
  predominant_category <- region_data %>%
    group_by(geo_id) %>%
    summarize(predominant_rank = names(sort(table(rank_grouped), decreasing = TRUE)[1])) %>% # Select the rank_grouped category with the highest count
    ungroup()
  
  # Merge the predominant category back to the region data
  region_data <- region_data %>%
    left_join(predominant_category, by = "geo_id")
  
  # Plot the boxplot
  ggplot(region_data, aes(x = Closeness, y = reorder(geo_id, Closeness), fill = predominant_rank)) +
    geom_boxplot(outlier.size = 1) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 9)) +  # Adjusting font size for y-axis labels
    scale_fill_manual(values = vulnerability_colors, 
                      limits = c("Low", "Moderate_Low", "Moderate", "Moderate_High", "High")) +  # Ensure correct legend order
    labs(title = paste("Region: ", region_name), y = "Geo ID", x = "Closeness", fill = "Predominant Category") +
    guides(fill = guide_legend(title = "Vulnerability Category")) +
    # Add vertical lines at the percentile cutoffs
    geom_vline(xintercept = cutoffs, color = "#ff5353", linetype = "dashed", linewidth = 1.2) +
    theme(plot.title = element_text(hjust = 0.5))
}

plot_climate_region("Western", climate_regions$Western)
plot_climate_region("Southeastern", climate_regions$Southeastern)
plot_climate_region("Southwestern", climate_regions$Southwestern)
plot_climate_region("Northeastern", climate_regions$Northeastern)
plot_climate_region("North_Central", climate_regions$North_Central)
plot_climate_region("South_Central", climate_regions$South_Central)
plot_climate_region("Central", climate_regions$Central)

# write.csv(res_melt, "data/df_topsis.csv", row.names = FALSE)
write.csv(res_melt, "data/df_topsis_weighted.csv", row.names = FALSE)

