# Load libraries
library(tidyverse)
library(sf)
library(raster)
library(terra)
library(RNetCDF)
library(ncdf4)
library(stringr)

# Importing dataset
gbif <- read_delim(file = 'data/gbif/occurrence.txt', delim = "\t")

# Exporting as CSV
write_csv(gbif, "data/gbif.csv")

# Importing data
gbif_cl <- read_csv("data/gbif.csv")

# Removing duplicate records
gbif_cl <- dplyr::distinct(gbif_cl)

# Exporting data
write_csv(gbif_cl, "data/gbif_cl.csv")

#########################################
# Resampling elevation raster to the other variables
# Importing rasters
r <- raster("data/climate/TerraClimate_aet_2006.nc")
elev <- raster("data/climate/elev.tif")

# Resampling
elev_re <- resample(elev, r, method = "bilinear")

# Exporting raster
writeRaster(elev_re, "data/climate/elev.tif", overwrite = T)

#########################################
# Cropping variables to the study extent
# Importing shapefile
dist <- st_read("data/entire_dist/entire_dist.shp")

# List of nc files
list <- list.files(path = "data/climate/", recursive = TRUE, full.names = TRUE)

for (i in list) {
  
  print(i)
  
  # Importing raster
  r <- raster(i)
  
  # Cropping layer
  r_crop <- crop(r, dist)
  r_crop <- mask(r_crop, dist)
  
  # Changing name [for nc files]
  if (str_detect(i, "nc")) {
    
    file_name <- gsub("data/climate/TerraClimate_", "", i)
    file_name <- gsub(".nc", "", file_name)
    
  } else{
    file_name <- gsub("data/climate/", "", i)
    file_name <- gsub(".tif", "", file_name)
  }
  
  # Exporting raster
  writeRaster(r_crop, paste0("data/pred/",
                             file_name, ".tif"), NAflag=-9999, overwrite = TRUE)
  
}

#########################################
# Calculating mean rasters by year groups
# List of rasters
list <- list.files(path = "data/pred/", pattern = ".tif", recursive = TRUE, full.names = TRUE)

# Creating variable names for the loop
yr <- c("yr1", "yr2", "yr3", "yr4", "yr5")

var <- c("aet", "def", "PDSI", "pet", "ppt", "q", "soil",
         "tmax", "tmin", "ws")

for (h in yr) {
  
  print(h)
  
  # Subset by year group
  yr_sub <- list[stringr::str_detect(list, h)]
  
  for (i in var) {
    
    print(i)
    
    # Subset by predictor variables
    var_sub <- yr_sub[stringr::str_detect(yr_sub, i)]
    
    # Reading multiple raster
    raster_files <- terra::rast(var_sub)
    
    # Calculating mean raster
    mean_ras <- terra::app(raster_files, mean)
    
    # Exporting raster
    writeRaster(mean_ras, paste0("data/pred_mean/", h, "_", i,
                               ".tif"), NAflag=-9999, overwrite = TRUE)
  }
}

#########################################
# Calculating mean rasters for all year groups [to measure collinearity]
# List of rasters
list <- list.files(path = "data/pred_mean/", pattern = ".tif", recursive = TRUE, full.names = TRUE)

var <- c("aet", "def", "PDSI", "pet", "ppt", "q", "soil",
         "tmax", "tmin", "ws")

for (i in var) {
  
  print(i)
  
  # Subset by predictor variables
  var_sub <- list[stringr::str_detect(list, i)]
  
  # Reading multiple raster
  raster_files <- terra::rast(var_sub)
  
  # Calculating mean raster
  mean_ras <- terra::app(raster_files, mean)
  
  # Exporting raster
  writeRaster(mean_ras, paste0("data/pred_mean/ov/", i,
                               ".tif"), NAflag=-9999, overwrite = TRUE)
}


# Predictor variable co-linearity
#attaching climatic layers
# Assessing co-linearity between predictor variables
vars<-c("aet.tif", "def.tif", "PDSI.tif", "pet.tif", "ppt.tif", "q.tif", "soil.tif",
        "tmax.tif", "tmin.tif", "ws.tif", "elev.tif")
clim.stack <- stack(paste(getwd(),"/data/pred_mean/ov/", vars, sep=""))

# Assessing correlations between predictor variables
pairs(clim.stack, maxpixels=20000)

# Highly correlated variables (>0.75): "aet.tif", "pet.tif", "q.tif", "tmin.tif"

# Predictor variables
vars<-c("def.tif", "PDSI.tif", "ppt.tif", "soil.tif",
        "tmax.tif", "ws.tif", "elev.tif")