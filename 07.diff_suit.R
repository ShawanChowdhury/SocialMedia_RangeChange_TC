# Load libraries
library(raster)
library(terra)
library(tidyverse)

##################################
# Differences in GBIF and combined data
##################################
######## Yr 1
# Importing suitability maps
gbif_yr1 <- raster("output/bin_maps_full/gbif_bin_yr1.tif")
com_yr1 <- raster("output/bin_maps_full/com_bin_yr1.tif")

# Calculating the difference
yr1_diff <- gbif_yr1 - 2*(com_yr1)

# Converting to a dataframe
yr1_diff_df <- as.data.frame(rasterToPoints(yr1_diff))
colnames(yr1_diff_df) <- c("lon", "lat", "val")

# Selecting required rows [always suitable and unique to combined data]
yr1_diff_df <- yr1_diff_df %>% 
  filter(val %in% c("-2", "-1")) %>% 
  mutate(year = "yr1")

# Removing unimportant data files
rm(gbif_yr1, com_yr1, yr1_diff)


######## Yr 2
# Importing suitability maps
gbif_yr2 <- raster("output/bin_maps_full/gbif_bin_yr2.tif")
com_yr2 <- raster("output/bin_maps_full/com_bin_yr2.tif")

# Calculating the difference
yr2_diff <- gbif_yr2 - 2*(com_yr2)

# Converting to a dataframe
yr2_diff_df <- as.data.frame(rasterToPoints(yr2_diff))
colnames(yr2_diff_df) <- c("lon", "lat", "val")

# Selecting required rows [always suitable and unique to combined data]
yr2_diff_df <- yr2_diff_df %>% 
  filter(val %in% c("-2", "-1")) %>% 
  mutate(year = "yr2")

# Removing unimportant data files
rm(gbif_yr2, com_yr2, yr2_diff)


######## Yr 3
# Importing suitability maps
gbif_yr3 <- raster("output/bin_maps_full/gbif_bin_yr3.tif")
com_yr3 <- raster("output/bin_maps_full/com_bin_yr3.tif")

# Calculating the difference
yr3_diff <- gbif_yr3 - 2*(com_yr3)

# Converting to a dataframe
yr3_diff_df <- as.data.frame(rasterToPoints(yr3_diff))
colnames(yr3_diff_df) <- c("lon", "lat", "val")

# Selecting required rows [always suitable and unique to combined data]
yr3_diff_df <- yr3_diff_df %>% 
  filter(val %in% c("-2", "-1")) %>% 
  mutate(year = "yr3")

# Removing unimportant data files
rm(gbif_yr3, com_yr3, yr3_diff)


######## Yr 4
# Importing suitability maps
gbif_yr4 <- raster("output/bin_maps_full/gbif_bin_yr4.tif")
com_yr4 <- raster("output/bin_maps_full/com_bin_yr4.tif")

# Calculating the difference
yr4_diff <- gbif_yr4 - 2*(com_yr4)

# Converting to a dataframe
yr4_diff_df <- as.data.frame(rasterToPoints(yr4_diff))
colnames(yr4_diff_df) <- c("lon", "lat", "val")

# Selecting required rows [always suitable and unique to combined data]
yr4_diff_df <- yr4_diff_df %>% 
  filter(val %in% c("-2", "-1")) %>% 
  mutate(year = "yr4")

# Removing unimportant data files
rm(gbif_yr4, com_yr4, yr4_diff)


######## Yr 5
# Importing suitability maps
gbif_yr5 <- raster("output/bin_maps_full/gbif_bin_yr5.tif")
com_yr5 <- raster("output/bin_maps_full/com_bin_yr5.tif")

# Calculating the difference
yr5_diff <- gbif_yr5 - 2*(com_yr5)

# Converting to a dataframe
yr5_diff_df <- as.data.frame(rasterToPoints(yr5_diff))
colnames(yr5_diff_df) <- c("lon", "lat", "val")

# Selecting required rows [always suitable and unique to combined data]
yr5_diff_df <- yr5_diff_df %>% 
  filter(val %in% c("-2", "-1")) %>% 
  mutate(year = "yr5")

# Removing unimportant data files
rm(gbif_yr5, com_yr5, yr5_diff)


###########
# Combining data frames
diff_df <- rbind(yr1_diff_df, yr2_diff_df, yr3_diff_df, yr4_diff_df, yr5_diff_df)

# Cleaning memory
rm(yr1_diff_df, yr2_diff_df, yr3_diff_df, yr4_diff_df, yr5_diff_df)

###################
# Importing predictor variables
###################
###### Yr 1
# Importing rasters
ppt <- raster("data/pred_mean/yr1/yr1_ppt.tif")
tmax <- raster("data/pred_mean/yr1/yr1_tmax.tif")
elev <- raster("data/pred_mean/yr1/elev.tif")

# Stacking files
yr <- stack(ppt, tmax, elev)

# # Converting to a dataframe
yr1_df <- as.data.frame(rasterToPoints(yr))
colnames(yr1_df) <- c("lon", "lat", "ppt", "tmax", "elev")

# Selecting required rows [always suitable and unique to combined data]
yr1_df <- yr1_df %>% 
  mutate(year = "yr1")

# cleaning memory
rm(ppt, tmax, elev, yr)

###### Yr 2
# Importing rasters
ppt <- raster("data/pred_mean/yr2/yr2_ppt.tif")
tmax <- raster("data/pred_mean/yr2/yr2_tmax.tif")
elev <- raster("data/pred_mean/yr2/elev.tif")

# Stacking files
yr <- stack(ppt, tmax, elev)

# # Converting to a dataframe
yr2_df <- as.data.frame(rasterToPoints(yr))
colnames(yr2_df) <- c("lon", "lat", "ppt", "tmax", "elev")

# Selecting required rows [always suitable and unique to combined data]
yr2_df <- yr2_df %>% 
  mutate(year = "yr2")

# cleaning memory
rm(ppt, tmax, elev, yr)

###### Yr 3
# Importing rasters
ppt <- raster("data/pred_mean/yr3/yr3_ppt.tif")
tmax <- raster("data/pred_mean/yr3/yr3_tmax.tif")
elev <- raster("data/pred_mean/yr3/elev.tif")

# Stacking files
yr <- stack(ppt, tmax, elev)

# # Converting to a dataframe
yr3_df <- as.data.frame(rasterToPoints(yr))
colnames(yr3_df) <- c("lon", "lat", "ppt", "tmax", "elev")

# Selecting required rows [always suitable and unique to combined data]
yr3_df <- yr3_df %>% 
  mutate(year = "yr3")

# cleaning memory
rm(ppt, tmax, elev, yr)

###### Yr 4
# Importing rasters
ppt <- raster("data/pred_mean/yr4/yr4_ppt.tif")
tmax <- raster("data/pred_mean/yr4/yr4_tmax.tif")
elev <- raster("data/pred_mean/yr4/elev.tif")

# Stacking files
yr <- stack(ppt, tmax, elev)

# # Converting to a dataframe
yr4_df <- as.data.frame(rasterToPoints(yr))
colnames(yr4_df) <- c("lon", "lat", "ppt", "tmax", "elev")

# Selecting required rows [always suitable and unique to combined data]
yr4_df <- yr4_df %>% 
  mutate(year = "yr4")

# cleaning memory
rm(ppt, tmax, elev, yr)

###### Yr 5
# Importing rasters
ppt <- raster("data/pred_mean/yr5/yr5_ppt.tif")
tmax <- raster("data/pred_mean/yr5/yr5_tmax.tif")
elev <- raster("data/pred_mean/yr5/elev.tif")

# Stacking files
yr <- stack(ppt, tmax, elev)

# # Converting to a dataframe
yr5_df <- as.data.frame(rasterToPoints(yr))
colnames(yr5_df) <- c("lon", "lat", "ppt", "tmax", "elev")

# Selecting required rows [always suitable and unique to combined data]
yr5_df <- yr5_df %>% 
  mutate(year = "yr5")

# cleaning memory
rm(ppt, tmax, elev, yr)

###########
# Combining data frames
yr <- rbind(yr1_df, yr2_df, yr3_df, yr4_df, yr5_df)

# Cleaning memory
rm(yr1_df, yr2_df, yr3_df, yr4_df, yr5_df)

##########
# Combining both data frames
suit_pred <- dplyr::left_join(diff_df, yr, by = c("lon", "lat", "year"))

# Exporting data
write_csv(suit_pred, "output/diff_suitability_pred.csv")
