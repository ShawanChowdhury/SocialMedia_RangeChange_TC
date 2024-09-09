# Maximising memory
options(java.parameters = "-Xmx6g")

# Loading required libraries
library(sp)
library(raster)
library(dismo)
library(dplyr)
library(sf)
library(spocc)
library(ENMeval)
library(stringr)
library(rgdal)
library(tidyverse)

#########################################
# Background points
# Attaching predictor variables
vars<-c("def.tif", "PDSI.tif", "ppt.tif", "soil.tif",
        "tmax.tif", "ws.tif", "elev.tif")

envs <- stack(paste(getwd(),"/data/pred_mean/ov/", vars, sep=""))

# Creating 10000 random background points with all occurrence records
bg <- dismo::randomPoints(envs[[7]], n = 10000) %>% as.data.frame()
colnames(bg) <- c("lon", "lat")

# Exporting background point data file [the same data points will be used in all the models]
write_csv(bg, "data/bg.csv")

#########################################
# Removing duplicate records by cells
# Importing a sample predictor variable raster
pred <- raster("data/pred_mean/ov/elev.tif")

# Reading data file
sp_data <- read_delim("data/com_int.txt", delim = ",")

gbif <- sp_data %>% 
  filter(type == "gbif")

sm <- sp_data %>% 
  filter(type == "social_media")

occ_gbif <- gbif %>% 
  dplyr::select(lon, lat)

occ_sm <- sm %>% 
  dplyr::select(lon, lat)

# Removing duplicate records [gbif]
occs.cells <- raster::extract(pred, occ_gbif, cellnumbers = TRUE)
occs.cellDups <- duplicated(occs.cells[,1])
occ_gbif <- occ_gbif[!occs.cellDups,]

# Removing duplicate records [social media]
occs.cells <- raster::extract(pred, occ_sm, cellnumbers = TRUE)
occs.cellDups <- duplicated(occs.cells[,1])
occ_sm <- occ_sm[!occs.cellDups,]

# Combining dataframes
gbif_dedup <- dplyr::left_join(occ_gbif, gbif, by = c("lon", "lat"))
sm_dedup <- dplyr::left_join(occ_sm, sm, by = c("lon", "lat"))

# Exporting outputs
write_csv(gbif_dedup, "data/occ/gbif_dedup.csv")
write_csv(sm_dedup, "data/occ/sm_dedup.csv")

#########################################
# Grouping and selecting same number of data points
# Importing data files
gbif_dedup <- read_csv("data/occ/gbif_dedup.csv")
sm_dedup <- read_csv("data/occ/sm_dedup.csv")

##################
### Year 1
# Filtering records by data source and year group
yr1_sm <- sm_dedup %>% 
  filter(year %in% c("2005", "2006", "2007", "2008")) %>% 
  mutate(year_group = "yr1")

yr1_gbif <- gbif_dedup %>% 
  filter(year %in% c("2005", "2006", "2007", "2008")) %>% 
  mutate(year_group = "yr1")

# Randomly select data points
yr1_sm <- sample_n(yr1_sm, 10)

# Combined dataframes
yr1 <- rbind(yr1_sm, yr1_gbif)

# Exporting outputs
write_csv(yr1, "data/occ/yr1_com.csv")
write_csv(yr1_gbif, "data/occ/yr1_gbif.csv")

##################
### Year 2
# Filtering records by data source and year group
yr2_sm <- sm_dedup %>% 
  filter(year %in% c("2009", "2010", "2011", "2012")) %>% 
  mutate(year_group = "yr2")

yr2_gbif <- gbif_dedup %>% 
  filter(year %in% c("2009", "2010", "2011", "2012")) %>% 
  mutate(year_group = "yr2")

# Randomly select data points
yr2_sm <- sample_n(yr2_sm, 42)

# Combined dataframes
yr2 <- rbind(yr2_sm, yr2_gbif)

# Exporting outputs
write_csv(yr2, "data/occ/yr2_com.csv")
write_csv(yr2_gbif, "data/occ/yr2_gbif.csv")

##################
### Year 3
# Filtering records by data source and year group
yr3_sm <- sm_dedup %>% 
  filter(year %in% c("2013", "2014", "2015", "2016")) %>% 
  mutate(year_group = "yr3")

yr3_gbif <- gbif_dedup %>% 
  filter(year %in% c("2013", "2014", "2015", "2016")) %>% 
  mutate(year_group = "yr3")

# Randomly select data points
yr3_sm <- sample_n(yr3_sm, 134)

# Combined dataframes
yr3 <- rbind(yr3_sm, yr3_gbif)

# Exporting outputs
write_csv(yr3, "data/occ/yr3_com.csv")
write_csv(yr3_gbif, "data/occ/yr3_gbif.csv")

##################
### Year 4
# Filtering records by data source and year group
yr4_sm <- sm_dedup %>% 
  filter(year %in% c("2017", "2018", "2019", "2020")) %>% 
  mutate(year_group = "yr4")

yr4_gbif <- gbif_dedup %>% 
  filter(year %in% c("2017", "2018", "2019", "2020")) %>% 
  mutate(year_group = "yr4")

# Randomly select data points
yr4_sm <- sample_n(yr4_sm, 399)

# Combined dataframes
yr4 <- rbind(yr4_sm, yr4_gbif)

# Exporting outputs
write_csv(yr4, "data/occ/yr4_com.csv")
write_csv(yr4_gbif, "data/occ/yr4_gbif.csv")

##################
### Year 5
# Filtering records by data source and year group
yr5_sm <- sm_dedup %>% 
  filter(year %in% c("2021", "2022", "2023", "2024")) %>% 
  mutate(year_group = "yr5")

yr5_gbif <- gbif_dedup %>% 
  filter(year %in% c("2021", "2022", "2023", "2024")) %>% 
  mutate(year_group = "yr5")

# Randomly select data points
yr5_gbif <- sample_n(yr5_gbif, 417)

# Combined dataframes
yr5 <- rbind(yr5_sm, yr5_gbif)

# Exporting outputs
write_csv(yr5, "data/occ/yr5_com.csv")
write_csv(yr5_gbif, "data/occ/yr5_gbif.csv")

##################
# Combining all outputs
gbif <- rbind(yr1_gbif, yr2_gbif, yr3_gbif, yr4_gbif, yr5_gbif)
com <- rbind(yr1, yr2, yr3, yr4, yr5)

# Exporting outputs
write_csv(gbif, "data/occ/gbif.csv")
write_csv(com, "data/occ/com.csv")
