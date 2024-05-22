# data manipulation
library(MASS)
library(dplyr)
library(here)
library(tidyr)
library(ggplot2)
library(gridExtra)
# apis and specific gov data source packages
library(httr)
library(jsonlite)
library(CDCPLACES)
library(tidycensus)
library(tigris)
library(sf)
library(RAQSAPI)
options(scipen = 999)


tictoc::tic()

# ************************************
# ************************************
###### Census ACS data ###### 
census_api_key <- trimws(readLines("/Users/natebender/Desktop/repo/census_2024_api_key.txt"))
census_api_key(census_api_key, install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")

variables <- load_variables(year = 2022, dataset = "acs5", cache = TRUE)
#bg_vars <- variables %>% 
#  filter(geography=="block group")

query_vars <- c(
  # population
  "B01001_001",   # Total population
  "B01001_020",  # Male population 65 and 66 years
  "B01001_021",  # Male population 67 to 69 years
  "B01001_022",  # Male population 70 to 74 years
  "B01001_023",  # Male population 75 to 79 years
  "B01001_024",  # Male population 80 to 84 years
  "B01001_025",  # Male population 85 years and over
  "B01001_044",  # Female population 65 and 66 years
  "B01001_045",  # Female population 67 to 69 years
  "B01001_046",  # Female population 70 to 74 years
  "B01001_047",  # Female population 75 to 79 years
  "B01001_048",  # Female population 80 to 84 years
  "B01001_049",  # Female population 85 years and over
  "B01001_003",  # Male population under 5 years
  "B01001_027",  # Female population under 5 years
  # disabilities
  "B18101_004",  # Male population under 5 years with a disability
  "B18101_007",  # Male population 5 to 17 years with a disability
  "B18101_010",  # Male population 18 to 34 years with a disability
  "B18101_013",  # Male population 35 to 64 years with a disability
  "B18101_016",  # Male population 65 to 74 years with a disability
  "B18101_019",  # Male population 75 years and over with a disability
  "B18101_023",  # Female population under 5 years with a disability
  "B18101_026",  # Female population 5 to 17 years with a disability
  "B18101_029",  # Female population 18 to 34 years with a disability
  "B18101_032",  # Female population 35 to 64 years with a disability
  "B18101_035",  # Female population 65 to 74 years with a disability
  "B18101_038",  # Female population 75 years and over with a disability
  # housing
  "B25034_001",  # Total housing units
  "B25034_009",  # Housing units built 1950 to 1959
  "B25034_010",  # Housing units built 1940 to 1949
  "B25034_011",   # Housing units built 1939 or earlier
  # outdoor workers
  "C24010_025",  # Male: Building and grounds cleaning and maintenance occupations
  "C24010_061",  # Female: Building and grounds cleaning and maintenance occupations
  "C24010_032",  # Male: Construction and extraction occupations
  "C24010_068",  # Female: Construction and extraction occupations
  "C24010_031",  # Male: Farming, fishing, and forestry occupations
  "C24010_067",  # Female: Farming, fishing, and forestry occupations
  "C24010_033",  # Male: Installation, maintenance, and repair occupations
  "C24010_069",  # Female: Installation, maintenance, and repair occupations
  "C24010_037",  # Male: Material moving occupations
  "C24010_073",  # Female: Material moving occupations
  "C24010_021",  # Male: Protective service occupations
  "C24010_057",  # Female: Protective service occupations
  "C24010_036",  # Male: Transportation occupations
  "C24010_072",  # Female: Transportation occupations
  "C24010_001",   # Total workers in the civilian employed population >= 16yrs
  # householder living alone
  "B11001_001",  # Total households
  "B11001_007", # total nonfamily households
  "B11001_009", # nonfamily, not living alone
  "B11001_008",   # Households with one person living alone
  # poverty 
  "B17001_002", # Income in the past 12 months below poverty level
  # employment status
  "B23025_001",  # Total population 16 years and over
  "B23025_005",   # Unemployed population 16 years and over
  # linguistic barrier: speak English less than "very well"
  "B16004_001",  # Total population 5 years and over
  "B16004_006", "B16004_007", "B16004_008",  # Age 5 to 17 years: Speak Spanish: English less than "very well"
  "B16004_011", "B16004_012", "B16004_013",  # Age 5 to 17 years: Speak other Indo-European languages: English less than "very well"
  "B16004_016", "B16004_017", "B16004_018",  # Age 5 to 17 years: Speak Asian and Pacific Island languages: English less than "very well"
  "B16004_021", "B16004_022", "B16004_023",  # Age 5 to 17 years: Speak other languages: English less than "very well"
  "B16004_028", "B16004_029", "B16004_030",  # Age 18 to 64 years: Speak Spanish: English less than "very well"
  "B16004_033", "B16004_034", "B16004_035",  # Age 18 to 64 years: Speak other Indo-European languages: English less than "very well"
  "B16004_038", "B16004_039", "B16004_040",  # Age 18 to 64 years: Speak Asian and Pacific Island languages: English less than "very well"
  "B16004_043", "B16004_044", "B16004_045",  # Age 18 to 64 years: Speak other languages: English less than "very well"
  "B16004_050", "B16004_051", "B16004_052",  # Age 65 years and over: Speak Spanish: English less than "very well"
  "B16004_055", "B16004_056", "B16004_057",  # Age 65 years and over: Speak other Indo-European languages: English less than "very well"
  "B16004_060", "B16004_061", "B16004_062",  # Age 65 years and over: Speak Asian and Pacific Island languages: English less than "very well"
  "B16004_065", "B16004_066", "B16004_067",   # Age 65 years and over: Speak other languages: English less than "very well"
  # educational attainment
  "B15003_001",  # Total population 25 years and over
  "B15003_002",  # No schooling completed
  "B15003_003",  # Nursery school
  "B15003_004",  # Kindergarten
  "B15003_005",  # 1st grade
  "B15003_006",  # 2nd grade
  "B15003_007",  # 3rd grade
  "B15003_008",  # 4th grade
  "B15003_009",  # 5th grade
  "B15003_010",  # 6th grade
  "B15003_011",  # 7th grade
  "B15003_012",  # 8th grade
  "B15003_013",  # 9th grade
  "B15003_014",  # 10th grade
  "B15003_015",  # 11th grade
  "B15003_016",   # 12th grade, no diploma
  # non-white percent of population
  "B02001_002",   # Race: White alone
  # health insurance coverage
  "B27001_001",  # Total population
  "B27001_005",  # Male under 6 years: No health insurance coverage
  "B27001_008",  # Male 6 to 18 years: No health insurance coverage
  "B27001_011",  # Male 19 to 25 years: No health insurance coverage
  "B27001_014",  # Male 26 to 34 years: No health insurance coverage
  "B27001_017",  # Male 35 to 44 years: No health insurance coverage
  "B27001_020",  # Male 45 to 54 years: No health insurance coverage
  "B27001_023",  # Male 55 to 64 years: No health insurance coverage
  "B27001_026",  # Male 65 to 74 years: No health insurance coverage
  "B27001_029",  # Male 75 years and over: No health insurance coverage
  "B27001_033",  # Female under 6 years: No health insurance coverage
  "B27001_036",  # Female 6 to 18 years: No health insurance coverage
  "B27001_039",  # Female 19 to 25 years: No health insurance coverage
  "B27001_042",  # Female 26 to 34 years: No health insurance coverage
  "B27001_045",  # Female 35 to 44 years: No health insurance coverage
  "B27001_048",  # Female 45 to 54 years: No health insurance coverage
  "B27001_051",  # Female 55 to 64 years: No health insurance coverage
  "B27001_054",  # Female 65 to 74 years: No health insurance coverage
  "B27001_057"   # Female 75 years and over: No health insurance coverage
)

# Get the data for the specified variable for Missoula County, Montana at the tract level
data <- get_acs(
  geography = "tract",
  variables = query_vars, 
  state = "MT",
  county = "063", # county FIPS code
  survey = "acs5",
  year = 2022
)

# Filter the metadata for the specified variable
variable_metadata <- variables %>%
  filter(name %in% query_vars)

merged_data <- data %>%
  left_join(variable_metadata, by = c("variable" = "name"))

data_wide <- merged_data %>%
  select(GEOID, variable, estimate) %>%
  spread(key = variable, value = estimate)

# Calculate the totals and percentages
data_wide <- data_wide %>%
  # Ppl over 65
  mutate(
    total_over_65 = B01001_020 + B01001_021 + B01001_022 + B01001_023 +
      B01001_024 + B01001_025 + B01001_044 + B01001_045 +
      B01001_046 + B01001_047 + B01001_048 + B01001_049,
    perc_over_65 = (total_over_65 / B01001_001) * 100,
    # Under 5
    total_under_5 = B01001_003 + B01001_027,
    perc_under_5 = (total_under_5 / B01001_001) * 100,
    # Disabilities
    total_disability = B18101_004 + B18101_007 + B18101_010 + B18101_013 +
      B18101_016 + B18101_019 + B18101_023 + B18101_026 +
      B18101_029 + B18101_032 + B18101_035 + B18101_038,
    perc_disability = (total_disability / B01001_001) * 100,
    # Year Structure Built
    total_built_pre1960 = B25034_009 + B25034_010 + B25034_011,
    perc_built_pre1960 = (total_built_pre1960 / B25034_001) * 100,
    # Likely outdoor workers
    total_outdoor_workers = C24010_025 + C24010_061 + C24010_032 + C24010_068 +
      C24010_031 + C24010_067 + C24010_033 + C24010_069 +
      C24010_037 + C24010_073 + C24010_021 + C24010_057 +
      C24010_036 + C24010_072,
    perc_outdoor_workers = (total_outdoor_workers / C24010_001) * 100,
    # living alone
    total_households = B11001_001,
    total_households_alone = B11001_008,
    perc_households_alone = (total_households_alone / B11001_001) * 100,
    # poverty
    total_poverty = B17001_002,
    perc_pop_poverty = (total_poverty / B01001_001) * 100,
    # unemployed
    total_unemployed = B23025_005,
    perc_unemployed = (total_unemployed / B23025_001) * 100,
    # linguistic barrier
    total_speakenglish_less_verywell = B16004_006 + B16004_007 + B16004_008 +
      B16004_011 + B16004_012 + B16004_013 +
      B16004_016 + B16004_017 + B16004_018 +
      B16004_021 + B16004_022 + B16004_023 +
      B16004_028 + B16004_029 + B16004_030 +
      B16004_033 + B16004_034 + B16004_035 +
      B16004_038 + B16004_039 + B16004_040 +
      B16004_043 + B16004_044 + B16004_045 +
      B16004_050 + B16004_051 + B16004_052 +
      B16004_055 + B16004_056 + B16004_057 +
      B16004_060 + B16004_061 + B16004_062 +
      B16004_065 + B16004_066 + B16004_067,
    perc_speakenglish_less_verywell = (total_speakenglish_less_verywell / B16004_001) * 100,
    # educational attainment
    total_pop_over25 = B15003_001,
    no_hs_degree = B15003_002 + B15003_003 + B15003_004 + B15003_005 +
      B15003_006 + B15003_007 + B15003_008 + B15003_009 +
      B15003_010 + B15003_011 + B15003_012 + B15003_013 +
      B15003_014 + B15003_015 + B15003_016,
    perc_no_hs_degree = (no_hs_degree / total_pop_over25) * 100,
    # non-white population
    total_nonwhite = B01001_001 - B02001_002,
    perc_nonwhite = (total_nonwhite / B01001_001) * 100,
    # health insurance
    total_no_health_ins = B27001_005 + B27001_008 + B27001_011 + B27001_014 + B27001_017 +
      B27001_020 + B27001_023 + B27001_026 + B27001_029 + B27001_033 +
      B27001_036 + B27001_039 + B27001_042 + B27001_045 + B27001_048 +
      B27001_051 + B27001_054 + B27001_057,
    perc_no_health_ins = (total_no_health_ins / B27001_001) * 100
  )

df_census <- data_wide %>% 
  select(GEOID, B01001_001, total_over_65, perc_over_65, total_under_5, perc_under_5, 
         total_disability, perc_disability,
         total_built_pre1960, perc_built_pre1960,
         total_outdoor_workers, perc_outdoor_workers,
         total_households, total_households_alone, perc_households_alone,
         total_poverty, perc_pop_poverty,
         total_unemployed, perc_unemployed,
         total_speakenglish_less_verywell, perc_speakenglish_less_verywell,
         total_pop_over25, perc_no_hs_degree,
         total_nonwhite, perc_nonwhite,
         total_no_health_ins, perc_no_health_ins
  ) %>% 
  rename(geo_id = GEOID,
         total_pop = B01001_001) %>% 
  mutate(geo_id = as.numeric(geo_id))

# grab the tract areas in sqmi
options(tigris_use_cache = TRUE)
state_fips <- "30"  # Montana
county_fips <- "063"  # Missoula County
tracts <- tracts(state = state_fips, county = county_fips, year = 2022, class = "sf")
# Project to EPSG:5070 (NAD83 / Conus Albers) which uses meters
# then convert from square meters to square miles
tracts_proj <- st_transform(tracts, 5070) %>%
  mutate(area_sq_miles = as.numeric(st_area(geometry)) * 0.000000386102) %>%   
  select(GEOID, area_sq_miles) %>%
  rename(geo_id = GEOID) %>%
  mutate(geo_id = as.numeric(geo_id)) %>%
  st_drop_geometry()

df_census <- df_census %>%
  left_join(tracts_proj, by = "geo_id")

write.csv(df_census, "data/census_data.csv", row.names = FALSE)


# ************************************
# ************************************
###### CDC PLACES data ###### 

# vars available
dict <- get_dictionary()

# Query diabetes, asthma, coronary heart disease data for Montana at the census tract level
mt_diabetes_mortality <- get_places(geography = "census",
                                    state = "MT",
                                    measure = c("DIABETES", "CASTHMA", "CHD"))

# QA plots if needed
# measure_id <- "DIABETES" # diabetes
# measure_id <- "CASTHMA" # asthma
# measure_id <- "CHD" # coronary heart disease
# mt_diabetes_mortality %>%
#   filter(measureid == measure_id & countyname == "Missoula") %>%
#   ggplot(aes(data_value, reorder(locationname, data_value))) +
#   geom_point(size = 2) +
#   geom_errorbar(aes(xmin = low_confidence_limit, xmax = high_confidence_limit)) +
#   labs(title = paste("Age-adjusted", measure_id, "Rates in Missoula County Census Tracts"),
#        y = "Census Tracts", x = paste(measure_id, "Rate (%)")) +
#   theme_minimal() +
#   theme(plot.title.position = "plot")

# filter to just what's needed
df_mslacounty <- mt_diabetes_mortality %>% 
  filter(countyname=="Missoula") %>% 
  select(locationname, measureid, data_value) %>% 
  rename(geo_id = locationname)

# pivot to wide format to create new variables for each of the three indicators
df_mslacounty <- df_mslacounty %>%
  pivot_wider(
    names_from = measureid,
    values_from = data_value,
    names_prefix = "perc_2021_"
  )

write.csv(df_mslacounty, "data/cdc_places_data.csv", row.names = FALSE)


# ************************************
# ************************************
###### EJScreen O3 (ozone) and PM2.5 state percentiles data ###### 
# 
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
df_mslacounty <- merge(df_mslacounty, mapping_data, by.x = "geo_id", by.y = "geo_id") %>% 
  select(-geo_id) %>% # remove the pre-2020 tract IDs
  rename(geo_id = geo_id_2024,
         perc_2021_diabetes = perc_2021_DIABETES,
         perc_2021_casthma = perc_2021_CASTHMA,
         perc_2021_chd = perc_2021_CHD)

tract_ids <- df_mslacounty$geo_id 

fetch_data_for_tract <- function(tract_id) {
  url <- paste0("https://ejscreen.epa.gov/mapper/ejscreenRESTbroker1.aspx?",
                "namestr=", tract_id,
                "&geometry=",
                "&distance=",
                "&unit=9035",
                "&areatype=tract",
                "&areaid=", tract_id,
                "&f=json")
  
  cat("Requesting URL:", url, "\n")  # Print the URL for debugging
  
  response <- GET(url)
  if (status_code(response) == 200) {
    content_data <- content(response, "text", encoding = "UTF-8")
    parsed_data <- fromJSON(content_data)
    # Extracting only the required fields
    if (!is.null(parsed_data$data$extras$areaid) && !is.null(parsed_data$data$main$S_E_PM25_PER)) {
      data <- data.frame(geo_id = parsed_data$data$extras$areaid,
                         pm25_statepercentile = parsed_data$data$main$S_E_PM25_PER,
                         o3_statepercentile = parsed_data$data$main$S_E_O3_PER)
    } else {
      data <- data.frame(geo_id = NA, pm25_statepercentile = NA, o3_statepercentile = NA)
    }
    return(data)
  } else {
    warning(paste("Failed to retrieve data for tract", tract_id, "Status code:", status_code(response)))
    return(data.frame(geo_id = NA, pm25_statepercentile = NA, o3_statepercentile = NA))
  }
}

if (!file.exists("data/ejscreen_data.csv")) {
  # If the file does not exist, fetch data and create the DataFrame
  df_ejresults <- bind_rows(lapply(tract_ids, fetch_data_for_tract))
  # Write the DataFrame to a CSV file
  write.csv(df_ejresults, "data/ejscreen_data.csv", row.names = FALSE)
} else {
  # If the file exists, load data from the CSV file
  df_ejresults <- read.csv("data/ejscreen_data.csv")
}

tictoc::toc()




