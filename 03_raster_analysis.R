library(terra)
library(FedData)
library(tigris)
library(sf)
library(dplyr)
library(tictoc)
library(ggplot2)
library(here)
#remotes::install_github("rspatial/luna")
#devtools::install_github("rspatial/luna")
#library(luna)
options(scipen = 999)

retry_with_backoff <- function(expr, retries = 5, initial_delay = 1, max_delay = 32) {
  delay <- initial_delay
  for (i in seq_len(retries)) {
    result <- try(eval(expr), silent = TRUE)
    
    if (!inherits(result, "try-error")) {
      return(result)
    } else {
      error_message <- conditionMessage(result)
      if (grepl("429 Too Many Requests", error_message) || grepl("Timeout was reached", error_message)) {
        message("Retry ", i, "/", retries, " after error: ", error_message)
        Sys.sleep(delay)
        delay <- min(delay * 2, max_delay)
      } else {
        stop("Non-retryable error: ", error_message)
      }
    }
  }
  stop("Failed after ", retries, " retries.")
}

# Get Montana boundary
montana <- states(cb = TRUE) %>%
  filter(NAME == "Montana") %>%
  st_transform(crs = 4326)  # Ensure the CRS is WGS84
montana_vect <- vect(montana)
label <- "Montana_NLCD_2021"

tic("overall")

# montana_counties <- counties(state = "MT", cb = TRUE)
# missoula_county <- montana_counties %>%
#   filter(NAME == "Missoula") %>%
#   st_transform(crs = 4326)  # Ensure the CRS is WGS84
# missoula_vect <- vect(missoula_county)
# label <- "MslaCounty_NLCD_2021"

# MT Tracts
montana_tracts <- tracts(state = "MT", cb = TRUE)
montana_tracts <- montana_tracts %>% 
  st_transform(montana_tracts, crs = 4326)
mt_vect <- vect(montana_tracts)
mt_vect$GEOID <- montana_tracts$GEOID


# missoula county tracts
# mslacnty_tracts <- montana_tracts %>% 
#   filter(COUNTYFP == "063") %>% 
#   st_transform(montana_tracts, crs = 4326)
# str(mslacnty_tracts)
# mslacnty_vect <- vect(mslacnty_tracts)
# mslacnty_vect$GEOID <- mslacnty_tracts$GEOID

tract_areas_sqm <- expanse(mt_vect, unit = "m")
tract_areas_sqmiles <- tract_areas_sqm * 3.86102e-7

# Download and crop the NLCD 2021 data
tictoc::tic("treecanopy download:")
NLCD_MT_treecanopy <- retry_with_backoff({
  get_nlcd(
    template = mt_vect,
    label = label,
    year = 2021,
    dataset = "canopy",
    landmass = "L48"
  )
})
NLCD_MT_treecanopy <- terra::project(NLCD_MT_treecanopy, "EPSG:4326")
toc()
plot(NLCD_MT_treecanopy)
terra::lines(montana_vect, col = "black", lwd=1)

NLCD_MT_treecanopy_clipped <- terra::crop(NLCD_MT_treecanopy, montana_vect)
NLCD_MT_treecanopy_clipped <- terra::mask(NLCD_MT_treecanopy_clipped, montana_vect)
plot(NLCD_MT_treecanopy_clipped)
terra::lines(montana_vect, col = "black", lwd=1)


tictoc::tic("impervious download:")
NLCD_MT_impervious <- retry_with_backoff({
  get_nlcd(
    template = mt_vect,
    label = label,
    year = 2021,
    dataset = "impervious",  # Change to "landcover" if you want land cover data instead of canopy cover
    landmass = "L48"
  )
})
NLCD_MT_impervious <- terra::project(NLCD_MT_impervious, "EPSG:4326")
plot(NLCD_MT_impervious)
terra::lines(montana_vect, col = "black", lwd=1)
toc()

NLCD_MT_impervious_clipped <- terra::crop(NLCD_MT_impervious, montana_vect)
NLCD_MT_impervious_clipped <- terra::mask(NLCD_MT_impervious, montana_vect)
plot(NLCD_MT_impervious_clipped)
terra::lines(montana_vect, col = "black", lwd=1)

#plot(NLCD_MT_impervious)
#terra::lines(mslacnty_tracts, col = "black", lwd=1)

# avg surface temperature for summer months (jun - aug) from Landsat 8
# aoi <- st_bbox(missoula_county)
# aoi_vector <- c(aoi["xmin"], aoi["xmax"], aoi["ymin"], aoi["ymax"])
# product <- "Landsat_8_OLI_TIRS_C1"
# start_date <- "2021-01-01"
# end_date <- "2021-12-31"
# path <- here("data/")  # Change this to your desired directory

# Download Landsat 8 data for the defined AOI and time period
# landsat_files <- getLandsat(
#   product = product,
#   start_date = start_date,
#   end_date = end_date,
#   aoi = aoi_vector,
#   download = TRUE,
#   path = path,
#   server = "AWS"  # AWS server does not require credentials
# )



# imperviousness
tictoc::tic("imperviousness processing")
zonal_stats <- terra::zonal(NLCD_MT_impervious, mt_vect, fun = "mean", na.rm = TRUE, as.polygons=T)
zonal_stats_df <- as.data.frame(zonal_stats)
zonal_stats_df <- zonal_stats_df %>% 
  rename(geo_id = GEOID,
         perc_impervious = Montana_NLCD_2021_NLCD_Impervious_2021) %>% 
  select(geo_id, perc_impervious)
toc()

write.csv(zonal_stats_df, "data/mt_raster_imperviousness.csv", row.names = FALSE)


# tree canopy
tictoc::tic("canopy processing")
tree_stats <- terra::zonal(NLCD_MT_treecanopy, mt_vect, fun = "mean", na.rm = TRUE, as.polygons=T)
tree_stats_df <- as.data.frame(tree_stats)
tree_stats_df <- tree_stats_df %>% 
  rename(geo_id = GEOID,
         perc_canopy = Montana_NLCD_2021_NLCD_Tree_Canopy_2021) %>% 
  select(geo_id, perc_canopy)
toc()

write.csv(tree_stats_df, "data/mt_raster_canopy.csv", row.names = FALSE)


# aggregate stats and create impervious-to-canopy index
zonal_stats_df <- zonal_stats_df %>% 
  left_join(tree_stats_df, by = "geo_id")

zonal_stats_df <- zonal_stats_df %>%
  mutate(
    area_sqmiles = tract_areas_sqmiles,
    impervious_canopy_index = -1 * ((perc_canopy * area_sqmiles) - (perc_impervious * area_sqmiles)) / 
           ((perc_canopy * area_sqmiles) + (perc_impervious * area_sqmiles))
         )

write.csv(zonal_stats_df, "data/mt_full_raster_analysis.csv", row.names = FALSE)

toc()
