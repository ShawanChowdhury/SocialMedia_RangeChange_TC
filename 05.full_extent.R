# Load libraries
library(raster)

# Raster with full extent
r <- raster("data/pred_mean/ov/def.tif")
r <- r*0

# List of rasters
tifs <- list.files(path = "output/sdm/bin_maps/", pattern = ".tif", recursive = TRUE, full.names = TRUE)

for (i in tifs) {
  print(i)
  
  # Changing file name
  file_name <- gsub("output/sdm/bin_maps/", "", i)
  file_name <- gsub(".tif", "", file_name)
  
  # Changing projections
  ras <- raster(i)
  proj_ras <- projectRaster(ras, r, method = "bilinear")
  
  # Exporting raster
  # Binary map [in case they are not binary]
  ras_bin <- proj_ras > 0
  writeRaster(ras_bin, paste0("output/bin_maps_full/", file_name, ".tif"), 
              NAflag=-9999, overwrite = TRUE)
 
}
