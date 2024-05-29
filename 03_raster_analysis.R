library(terra)
library(FedData)
library(tigris)
library(sf)
library(dplyr)
library(tictoc)
library(ggplot2)
library(here)
remotes::install_github("rspatial/luna")
library(luna)
options(scipen = 999)

ap <- available.packages()

# ignore - Get Montana boundary
# montana <- states(cb = TRUE) %>%
#   filter(NAME == "Montana") %>%
#   st_transform(crs = 4326)  # Ensure the CRS is WGS84
# montana_vect <- vect(montana)
# label <- "Montana_NLCD_2021"

montana_counties <- counties(state = "MT", cb = TRUE)
missoula_county <- montana_counties %>%
  filter(NAME == "Missoula") %>%
  st_transform(crs = 4326)  # Ensure the CRS is WGS84
missoula_vect <- vect(missoula_county)
label <- "MslaCounty_NLCD_2021"

# Tracts
montana_tracts <- tracts(state = "MT", cb = TRUE)
mslacnty_tracts <- montana_tracts %>% 
  filter(COUNTYFP == "063") %>% 
  st_transform(montana_tracts, crs = 4326)
str(mslacnty_tracts)
mslacnty_vect <- vect(mslacnty_tracts)
mslacnty_vect$GEOID <- mslacnty_tracts$GEOID

tract_areas_sqm <- expanse(mslacnty_vect, unit = "m")
tract_areas_sqmiles <- tract_areas_sqm * 3.86102e-7

# Download and crop the NLCD 2021 data
tictoc::tic()
NLCD_MT_treecanopy <- get_nlcd(
  template = missoula_vect,
  label = label,
  year = 2021,
  dataset = "canopy",  # Change to "landcover" if you want land cover data instead of canopy cover
  landmass = "L48"
)
toc()
NLCD_MT_treecanopy <- terra::project(NLCD_MT_treecanopy, "EPSG:4326")
plot(NLCD_MT_treecanopy)


tictoc::tic()
NLCD_MT_impervious <- get_nlcd(
  template = missoula_vect,
  label = label,
  year = 2021,
  dataset = "impervious",  # Change to "landcover" if you want land cover data instead of canopy cover
  landmass = "L48"
)
toc()
NLCD_MT_impervious <- terra::project(NLCD_MT_impervious, "EPSG:4326")
plot(NLCD_MT_impervious)
terra::lines(mslacnty_tracts, col = "black", lwd=1)

# avg surface temperature for summer months (jun - aug) from Landsat 8
aoi <- st_bbox(missoula_county)
aoi_vector <- c(aoi["xmin"], aoi["xmax"], aoi["ymin"], aoi["ymax"])
product <- "Landsat_8_OLI_TIRS_C1"
start_date <- "2021-01-01"
end_date <- "2021-12-31"
path <- here("data/")  # Change this to your desired directory

# Download Landsat 8 data for the defined AOI and time period
landsat_files <- getLandsat(
  product = product,
  start_date = start_date,
  end_date = end_date,
  aoi = aoi_vector,
  download = TRUE,
  path = path,
  server = "AWS"  # AWS server does not require credentials
)



# imperviousness
tictoc::tic()
zonal_stats <- terra::zonal(NLCD_MT_impervious, mslacnty_vect, fun = "mean", na.rm = TRUE, as.polygons=T)
zonal_stats_df <- as.data.frame(zonal_stats)
zonal_stats_df <- zonal_stats_df %>% 
  rename(geo_id = GEOID,
         perc_impervious = MslaCounty_NLCD_2021_NLCD_Impervious_2021) %>% 
  select(geo_id, perc_impervious)
toc()

# tree canopy
tictoc::tic()
zonal_stats <- terra::zonal(NLCD_MT_treecanopy, mslacnty_vect, fun = "mean", na.rm = TRUE, as.polygons=T)
temp_stats_df <- as.data.frame(zonal_stats)
temp_stats_df <- temp_stats_df %>% 
  rename(geo_id = GEOID,
         perc_canopy = MslaCounty_NLCD_2021_NLCD_Tree_Canopy_2021) %>% 
  select(geo_id, perc_canopy)
toc()



zonal_stats_df <- zonal_stats_df %>% 
  left_join(temp_stats_df, by = "geo_id")

zonal_stats_df <- zonal_stats_df %>%
  mutate(
    area_sqmiles = tract_areas_sqmiles,
    impervious_canopy_index = -1 * ((perc_canopy * area_sqmiles) - (perc_impervious * area_sqmiles)) / 
           ((perc_canopy * area_sqmiles) + (perc_impervious * area_sqmiles))
         )


