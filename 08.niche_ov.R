# Load libraries
library(ecospat)
library(raster)
library(terra)
library(ade4)
library(tidyverse)

# List of files
occ_list <- list.files(path = "data/occ/", pattern = "csv", recursive = TRUE, full.names = TRUE)
env_list <- list.files(path = "data/pred_mean/", pattern = "tif", recursive = TRUE, full.names = TRUE)

yr <- c("yr1", "yr2", "yr3", "yr4", "yr5")

niche_ov_merged <- data.frame()
pca_var_contrib <- data.frame()

for (i in yr) {
  print(i)
  
  # Subset list
  occ_list_sub <- occ_list[stringr::str_detect(occ_list, i)]
  env_list_sub <- env_list[stringr::str_detect(env_list, i)]
  gbif <- occ_list_sub[stringr::str_detect(occ_list_sub, "gbif")]
  com <- occ_list_sub[stringr::str_detect(occ_list_sub, "com")]
  
  # Load files
  gbif <- read_csv(gbif)
  com <- read_csv(com)
  pred <- terra::rast(env_list_sub)
  pred <- stack(pred)
  names(pred) <- c("elev", "def", "pdsi", "ppt", "soil", "tmax", "ws")
  
  # Keeping only spatial points in the occurrence files
  gbif <- gbif %>% 
    dplyr::select("lon", "lat")
  com <- com %>% 
    dplyr::select("lon", "lat")
  
  # Create dataframes with the predictor variables
  pred_gbif <- cbind(as.data.frame(gbif), terra::extract(pred, gbif))
  pred_gbif <- pred_gbif[complete.cases(data.frame(pred_gbif)), ]

  pred_com <- cbind(as.data.frame(com), terra::extract(pred, com))
  pred_com <- pred_com[complete.cases(data.frame(pred_com)), ]

  # Produce global environmental background data
  globalEnv <- terra::values(pred)
  globalEnv <- globalEnv[complete.cases(globalEnv), ]

  #Prepare niche quantification
  pca.clim <- dudi.pca(globalEnv, center = TRUE, scale = TRUE, scannf = FALSE, nf = 2)
  
  #Scores for whole environment
  global.scores <- pca.clim$li
  
  #PCA scores for gbif records
  gbif.scores <-  suprow(pca.clim, data.frame(pred_gbif)[, colnames(globalEnv)])$li  
  
  #PCA scores for combined records
  combined.scores <-  suprow(pca.clim, data.frame(pred_com)[, colnames(globalEnv)])$li  
  
  #Create grid of PCA scores for gbif records
  gbif.grid <- ecospat.grid.clim.dyn(global.scores,
                                     global.scores,
                                     gbif.scores)
  
  #Create grid of PCA scores for combined records
  combined.grid <- ecospat.grid.clim.dyn(global.scores,
                                         global.scores,
                                         combined.scores)
  
  
  # Calculate niche overlap
  niche_ov <- ecospat.niche.overlap (combined.grid, gbif.grid, cor = TRUE)$D
  
  niche_ov <- as.data.frame(niche_ov)
  niche_ov <- niche_ov %>% 
    dplyr::mutate(year = i) %>% 
    dplyr::select(year, niche_ov)
  
  niche_ov_merged <- rbind(niche_ov_merged, niche_ov)
  
  # Plot niches
  # Get description of PCA axes variation in relation to original predictors
  png(file = paste0("output/figure/biplot_", i, ".png"), width = 1000, height = 1000, res = 100)
  ecospat.plot.contrib(contrib=pca.clim$co, eigen=pca.clim$eig)
  dev.off()
  
  # PCA components
  pca_co <- as.data.frame(pca.clim$co)
  pca_co <- pca_co %>% 
    dplyr::mutate(var = rownames(pca_co),
           year = i,
           Eigen = pca.clim$eig) %>% 
    dplyr::select(year, var, Comp1, Comp2, Eigen)
  pca_var_contrib <- rbind(pca_var_contrib, pca_co)
  
  # Plot overlap
  png(file = paste0("output/figure/niche_ov_", i, ".png"), width = 600, height = 600, res = 100)
  
  ecospat.plot.niche.dyn(combined.grid, gbif.grid, quant = 0.1, interest = 2, 
                         title = "", name.axis1 = "", name.axis2 = "", 
                         col.unf = "blue", colZ2 = "purple4",
                         col.exp = "cyan1", col.stab = "cyan1",
                         transparency = 70)
  
  dev.off()
  
 }

# Exporting niche overlap data
write_csv(niche_ov_merged, "output/niche_ov_merged.csv")
write_csv(pca_var_contrib, "output/pca_var_contrib.csv")


ecospat.plot.niche.dyn(combined.grid, gbif.grid, quant = 0.1, interest = 2, 
                       title = "", name.axis1 = "", name.axis2 = "", 
                       col.unf = "blue", colZ2 = "red", colZ1 = "black",
                       col.exp = "cyan1", col.stab = "cyan1",
                       transparency = 70)
