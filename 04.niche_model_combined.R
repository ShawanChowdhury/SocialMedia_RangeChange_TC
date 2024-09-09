# Maximising memory
options(java.parameters = "-Xmx6g")

# Loading required libraries
library(sp)
library(raster)
library(terra)
library(dismo)
library(dplyr)
library(sf)
library(spocc)
library(ENMeval)
library(stringr)
library(tidyverse)
library(ecospat)

# Reading data file
com <- read_csv("data/occ/com.csv")

# List of rasters
list <- list.files(path = "data/pred_mean/", pattern = "tif", recursive = TRUE, full.names = TRUE)

yr <- unique(com$year_group)


for (i in yr) {
  
  print(i)
  
  # Predictor variables
  # Subset by year group
  var_sub <- list[stringr::str_detect(list, i)]
  
  # Importing files
  envs <- terra::rast(var_sub)
  
  # Filtering occ data
  occs <- com %>% 
    dplyr::filter(year_group == i)
  
  # Selecting lat-lon
  occs <- occs %>% 
    dplyr::select(lon, lat)
  
  # Let's now remove occurrences that are cell duplicates -- these are
  # occurrences that share a grid cell in the predictor variable rasters.
  occs.cells <- raster::extract(envs[[1]], occs, cellnumbers = TRUE)
  occs.cellDups <- duplicated(occs.cells[,1])
  occs <- occs[!occs.cellDups,]
  
  # Converting it to the same projection (if they are not)
  occs.sf <- sf::st_as_sf(occs, coords = c("lon","lat"), crs = raster::crs(envs))
  
  # Now, we project our point data to an equal-area projection, which converts our 
  # degrees to meters, which is ideal for buffering (the next step). 
  # We use the typical Eckert IV projection.
  eckertIV <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  occs.sf <- sf::st_transform(occs.sf, crs = eckertIV)
  
  # Buffer all occurrences by 500 km, union the polygons together 
  # (for visualization), and convert back to a form that the raster package 
  # can use. Finally, we reproject the buffers back to WGS84 (lat/lon).
  # We choose 500 km here considering that this is a highly range-shifting species.
  occs.buf <- sf::st_buffer(occs.sf, dist = 500000) %>% 
    sf::st_union() %>% 
    sf::st_sf() %>%
    sf::st_transform(crs = raster::crs(envs))
  
  # Crop environmental rasters to match the study extent
  envs.bg <- raster::crop(envs, occs.buf)
  
  # Next, mask the rasters to the shape of the buffers
  envs.bg <- raster::mask(envs.bg, occs.buf)
  
  # Reading background point file
  bg <- read_csv("data/bg.csv")
  
  model <- ENMevaluate(occs = occs, envs = envs.bg, bg = bg, partitions = 'checkerboard2', 
                       tune.args = list(fc = c("L", "LQ", "H", "LQH", "LQHP", "LQHPT"), rm = seq(0.5, 4, 0.5)),
                       parallel = FALSE, algorithm = 'maxnet')
  
  # Exporting original model
  write_rds(model, file = paste0("output/sdm/com/com_model_", i, ".rds"))
  
  # Overall results
  res <- eval.results(model)
  
  # Select the model with delta AICc equal to 0, or the one with the lowest AICc score.
  # In practice, models with delta AICc scores less than 2 are usually considered 
  # statistically equivalent.
  opt.aicc <- res %>% filter(delta.AICc == 0)
  
  # Exporting model summary
  write_csv(opt.aicc, file = paste0("output/sdm/com/com_model_sum_", i, ".csv"))
  
  # Threshold value
  threshold <- opt.aicc$or.10p.avg
  
  # Suitability map
  r <- eval.predictions(model)[[opt.aicc$tune.args]]
  writeRaster(r, paste0("output/sdm/com/com_map_", i, ".tif"), 
              NAflag=-9999, overwrite = TRUE)
  
  # Binary map [equal/over the threshold value]
  r_bin <- r >= threshold
  writeRaster(r_bin, paste0("output/sdm/com/com_bin_", i, ".tif"), 
              NAflag=-9999, overwrite = TRUE)
  
}
