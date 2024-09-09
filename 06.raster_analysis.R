# Load libraries
library(raster)
library(dismo)
library(terra)
library(tidyverse)
library(geosphere)

#####################################
# Calculating differences in estimated suitable habitats, and distance between centroids
#####################################
# Elevation raster
elev <- raster("data/pred_mean/ov/elev.tif")

# Centroid of yr1 [to calculate expansion rate]
yr1_gbif <- raster("output/bin_maps_full/gbif_bin_yr1.tif")
yr1_com <- raster("output/bin_maps_full/com_bin_yr1.tif")

centroid_yr1_gbif <- colMeans(xyFromCell(yr1_gbif, which(yr1_gbif[]==1)))
centroid_yr1_com <- colMeans(xyFromCell(yr1_com, which(yr1_com[]==1)))

# List of suitability maps in different year intervals
list <- list.files(path = "output/bin_maps_full/", pattern = "tif", 
                   recursive = TRUE, full.names = TRUE)

# year_groups
yr <- c("yr1", "yr2", "yr3", "yr4", "yr5")

# Create an empty
merged_data <- data.frame()

for(i in yr){
  
  print(i)
  
  # Subset by year group
  list_sub <- list[stringr::str_detect(list, i)]
  
  gbif <- list_sub[stringr::str_detect(list_sub, "gbif")]
  com <- list_sub[stringr::str_detect(list_sub, "com")]
  
  # Importing rasters
  gbif <- raster(gbif)
  com <- raster(com)
  
  # Mean elevation [gbif]
  # Converting to a dataframe
  gbif_elev <- stack(gbif, elev)
  gbif_elev_df <- as.data.frame(rasterToPoints(gbif_elev))
  colnames(gbif_elev_df) <- c("lon", "lat", "val", "elev")
  
  gbif_elev_df_pr <- gbif_elev_df %>% 
    filter(val > 0)
  
  gbif_elev <- mean(gbif_elev_df_pr$elev, na.rm = TRUE)
  
  # Mean elevation [combined]
  # Converting to a dataframe
  com_elev <- stack(com, elev)
  com_elev_df <- as.data.frame(rasterToPoints(com_elev))
  colnames(com_elev_df) <- c("lon", "lat", "val", "elev")
  
  com_elev_df_pr <- com_elev_df %>% 
    filter(val > 0)
  
  com_elev <- mean(com_elev_df_pr$elev, na.rm = TRUE)
  
  # Calculating area
  area_gbif <- cellStats(gbif, "sum") * 21.625
  area_com <- cellStats(com, "sum") * 21.625
  
  # Niche overlap
  niche_overlap <- nicheOverlap(com, gbif, stat = "I", mask = TRUE, checkNegatives = TRUE)
  
  # Calculating centroid
  centroid_gbif <- colMeans(xyFromCell(gbif, which(gbif[]==1)))
  centroid_com <- colMeans(xyFromCell(com, which(com[]==1)))
  
  #############
  # Expansion
  exp_gbif <- distm(centroid_yr1_gbif, centroid_gbif, fun = distHaversine)/1000
  exp_com <- distm(centroid_yr1_com, centroid_com, fun = distHaversine)/1000
  
  #########################
  # Elevation
  # ### GBIF
  # # Create an sf point with the correct CRS
  # lon <- centroid_gbif[1]
  # lat <- centroid_gbif[2]
  # point_sf <- st_as_sf(data.frame(lon, lat), coords = c("lon", "lat"), crs = 4326)
  # 
  # # Create a 10 m buffer around the point
  # buffer <- st_buffer(point_sf, dist = 10)
  # 
  # # Extract elevation values within the buffer
  # elev_gbif <- terra::extract(elev, buffer, fun = mean, na.rm=TRUE, bind=TRUE)
  # 
  # ### Combined
  # # Create an sf point with the correct CRS
  # lon <- centroid_com[1]
  # lat <- centroid_com[2]
  # point_sf <- st_as_sf(data.frame(lon, lat), coords = c("lon", "lat"), crs = 4326)
  # 
  # # Create a 10 m buffer around the point
  # buffer <- st_buffer(point_sf, dist = 25000)
  # 
  # # Extract elevation values within the buffer
  # elev_com <- terra::extract(elev, buffer, fun = mean, na.rm=TRUE, bind=TRUE)
  
  
  # Distance between centroids
  centroid_distance <- distm(centroid_gbif, centroid_com, fun = distHaversine)/1000
  
  # Centroid coordinates
  centroid_gbif_lon <- as.data.frame(centroid_gbif[1])
  centroid_gbif_lat <- as.data.frame(centroid_gbif[2])
  centroid_gbif_coord <-cbind(centroid_gbif_lon, centroid_gbif_lat) 
  colnames(centroid_gbif_coord) <- c("lon", "lat")
  centroid_gbif_coord$coord <- paste(centroid_gbif_coord$lon, centroid_gbif_coord$lat, sep = ",")
  
  centroid_com_lon <- as.data.frame(centroid_com[1])
  centroid_com_lat <- as.data.frame(centroid_com[2])
  centroid_com_coord <-cbind(centroid_com_lon, centroid_com_lat) 
  colnames(centroid_com_coord) <- c("lon", "lat")
  centroid_com_coord$coord <- paste(centroid_com_coord$lon, centroid_com_coord$lat, sep = ",")
  
  # Create a dataframe with all the above information
  df <- as.data.frame(i)
  colnames(df) <- "Year interval"
  
  df <- df %>% 
    mutate(area_gbif = area_gbif,
           area_com = area_com,
           area_diff = (area_com - area_gbif),
           niche_overlap = niche_overlap,
           centroid_gbif = centroid_gbif_coord$coord,
           centroid_com = centroid_com_coord$coord,
           centroid_distance = centroid_distance[,1],
           elev_gbif = gbif_elev[1],
           elev_com = com_elev[1],
           exp_gbif <- exp_gbif[1],
           exp_com = exp_com[1])
 
  merged_data <- rbind(merged_data, df)
  
}

# Exporting output (merged data frame)
write_csv(merged_data, "output/diff_suitability_maps.csv")

#####################################
# Calculating yearly expansion
#####################################
# # List of suitability maps in different year intervals
# list <- list.files(path = "output/bin_maps_full/", pattern = "tif", 
#                    recursive = TRUE, full.names = TRUE)
# 
# # Importing files
# envs <- terra::rast(list)
# 
# # Calculating centroid
# centroid_gbif_yr1 <- colMeans(xyFromCell(envs[[8]], which(envs[[8]][]==1)))
# centroid_gbif_yr2 <- colMeans(xyFromCell(envs[[9]], which(envs[[9]][]==1)))
# centroid_gbif_yr3 <- colMeans(xyFromCell(envs[[10]], which(envs[[10]][]==1)))
# centroid_gbif_yr4 <- colMeans(xyFromCell(envs[[11]], which(envs[[11]][]==1)))
# centroid_gbif_yr5 <- colMeans(xyFromCell(envs[[12]], which(envs[[12]][]==1)))
# 
# centroid_com_yr1 <- colMeans(xyFromCell(envs[[2]], which(envs[[2]][]==1)))
# centroid_com_yr2 <- colMeans(xyFromCell(envs[[3]], which(envs[[3]][]==1)))
# centroid_com_yr3 <- colMeans(xyFromCell(envs[[4]], which(envs[[4]][]==1)))
# centroid_com_yr4 <- colMeans(xyFromCell(envs[[5]], which(envs[[5]][]==1)))
# centroid_com_yr5 <- colMeans(xyFromCell(envs[[6]], which(envs[[6]][]==1)))
# 
# # Calculating distance between centroids (or yearly expansion)
# exp_yr2_gbif <- as.data.frame(distm(centroid_gbif_yr1, centroid_gbif_yr2, fun = distHaversine)/1000)
# colnames(exp_yr2_gbif) <- "exp"
# exp_yr2_gbif$yr <- "yr2"
# exp_yr2_gbif$source <- "GBIF"
# 
# exp_yr3_gbif <- as.data.frame(distm(centroid_gbif_yr1, centroid_gbif_yr3, fun = distHaversine)/1000)
# colnames(exp_yr3_gbif) <- "exp"
# exp_yr3_gbif$yr <- "yr3"
# exp_yr3_gbif$source <- "GBIF"
# 
# exp_yr4_gbif <- as.data.frame(distm(centroid_gbif_yr1, centroid_gbif_yr4, fun = distHaversine)/1000)
# colnames(exp_yr4_gbif) <- "exp"
# exp_yr4_gbif$yr <- "yr4"
# exp_yr4_gbif$source <- "GBIF"
# 
# exp_yr5_gbif <- as.data.frame(distm(centroid_gbif_yr1, centroid_gbif_yr5, fun = distHaversine)/1000)
# colnames(exp_yr5_gbif) <- "exp"
# exp_yr5_gbif$yr <- "yr5"
# exp_yr5_gbif$source <- "GBIF"
# 
# exp_yr2_com <- as.data.frame(distm(centroid_com_yr1, centroid_com_yr2, fun = distHaversine)/1000)
# colnames(exp_yr2_com) <- "exp"
# exp_yr2_com$yr <- "yr2"
# exp_yr2_com$source <- "com"
# 
# exp_yr3_com <- as.data.frame(distm(centroid_com_yr1, centroid_com_yr3, fun = distHaversine)/1000)
# colnames(exp_yr3_com) <- "exp"
# exp_yr3_com$yr <- "yr3"
# exp_yr3_com$source <- "com"
# 
# exp_yr4_com <- as.data.frame(distm(centroid_com_yr1, centroid_com_yr4, fun = distHaversine)/1000)
# colnames(exp_yr4_com) <- "exp"
# exp_yr4_com$yr <- "yr4"
# exp_yr4_com$source <- "com"
# 
# exp_yr5_com <- as.data.frame(distm(centroid_com_yr1, centroid_com_yr5, fun = distHaversine)/1000)
# colnames(exp_yr5_com) <- "exp"
# exp_yr5_com$yr <- "yr5"
# exp_yr5_com$source <- "com"
# 
# # combining dataframes
# exp <- rbind(exp_yr2_gbif, exp_yr3_gbif, exp_yr4_gbif, exp_yr5_gbif,
#              exp_yr2_com, exp_yr3_com, exp_yr4_com, exp_yr5_com)
# 
# # Exporting output
# write_csv(exp, "output/exp_yr.csv")
