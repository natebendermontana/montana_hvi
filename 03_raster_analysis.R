library(terra)
library(FedData)
library(tigris)
library(sf)
library(dplyr)
library(tictoc)
library(ggplot2)

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

# Download and crop the NLCD 2021 data
# tictoc::tic()
# NLCD_MT_treecanopy <- get_nlcd(
#   template = missoula_vect,
#   label = label,
#   year = 2021,
#   dataset = "canopy",  # Change to "landcover" if you want land cover data instead of canopy cover
#   landmass = "L48"
# )
# toc()
# plot(NLCD_MT_treecanopy)


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

tictoc::tic()
zonal_stats <- terra::zonal(NLCD_MT_impervious, mslacnty_vect, fun = "mean", na.rm = TRUE)
zonal_stats_df <- as.data.frame(zonal_stats)
toc()


