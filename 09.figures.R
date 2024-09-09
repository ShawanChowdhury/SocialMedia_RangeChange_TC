# Maximising memory
options(java.parameters = "-Xmx6g")

# Loading required libraries
library(tidyverse)
library(ggplot2)
library(raster)
library(RColorBrewer)
library(rasterVis)
library(viridis)
library(ggrepel)

# Importing data
data <- read_delim("data/com_int.txt", delim = ",")

###################################
# Year-wise growth
year_rec <- data %>% 
  group_by(year, type) %>% 
  summarise(n = NROW(type)) %>% 
  mutate(total = sum(n), per = n/total*100)

# Exporting summarised data frame
write_csv(year_rec, "output/year_growth.csv")

# Plot
ggplot(year_rec, aes(year, per, fill = type)) +
  geom_bar(stat = "identity", position = "stack") +
  xlab("") + ylab("% of occurrence records") + theme_classic() +
  scale_fill_manual(values = c("orange", "blue2")) +
  geom_hline(yintercept = 50, lty = "dashed", col = "white") +
  theme(legend.position = "top", 
        legend.title = element_blank())

# Exporting output
ggsave("output/figure/year_growth.png")

###################################
# Percent of records by country
region_rec <- data %>% 
  group_by(name, type) %>% 
  summarise(n = NROW(type)) %>% 
  mutate(total = sum(n), per = n/total*100)

# Exporting summarised data frame
write_csv(region_rec, "output/region_rec_per.csv")

# Importing file
region_rec <- read_csv("output/region_rec_per.csv")

# Plot
region_rec %>%
  group_by(name, type) %>%
  summarise(prop = sum(n)) %>%
  mutate(prop = prop.table(prop)) %>%
  arrange(type != 'social_media', prop) %>%
  pull(name) %>% unique -> levels

region_rec %>%
  mutate(name = factor(name, levels)) %>% 
  ggplot(aes(name, per, fill = type)) + 
  geom_col(position = "fill") +
  theme(legend.position = "top", 
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("orange", "blue2")) +
  geom_hline(yintercept = 0.5, lty = "dashed", col = "white") +
  theme_classic() + xlab("") + ylab("") + coord_flip()

# Exporting output
ggsave("output/figure/region_rec_per.png")

# Total records vs % of records from social media
region_rec_sm <- region_rec %>% 
  filter(type == "social_media")

ggplot(region_rec_sm, aes(total, per)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10() +
  theme_classic() + xlab("Total number of records") +
  ylab("% of social media records") +
  geom_label_repel(aes(label = name),
                   box.padding   = 0.5,
                   segment.color = 'grey50')

# Exporting output
ggsave("output/figure/total_rec_per_sm.png")

###################################
# Differences in suitability distribution
region_cell <- read_csv("output/region_cell.csv")

ggplot(region_cell, aes(fct_reorder(region, per_increase), per_increase)) +
  geom_bar(stat = "identity", position = "stack") +
  xlab("") + ylab("% increase in suitable area") + theme_classic() + coord_flip()

# Exporting output
ggsave("output/figure/region_rec_per_suitability.png")

#########################################
# Early stage records
early_stage <- read_csv("data/earlier_int.txt")

early_rec <- early_stage %>% 
  group_by(name, type) %>% 
  summarise(n = NROW(type)) %>% 
  mutate(total = sum(n), per = n/total*100)

write_csv(early_rec, "output/early_rec.csv")

early_rec %>%
  group_by(name, type) %>%
  summarise(prop = sum(n)) %>%
  mutate(prop = prop.table(prop)) %>%
  arrange(type != 'social_media', prop) %>%
  pull(name) %>% unique -> levels

early_rec %>%
  mutate(name = factor(name, levels)) %>% 
  ggplot(aes(name, per, fill = type)) + 
  geom_col(position = "fill") +
  theme(legend.position = "top", 
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("orange", "blue2")) +
  theme_classic() + xlab("") + ylab("") + coord_flip()

# Exporting output
ggsave("output/figure/early_rec.png")

##########################
# Yearly expansion
exp <- read_csv("output/exp_yr.csv")
# exp$exp <- as.factor(exp$exp)

ggplot(exp, aes(x = yr, y = exp, color = source, group = source)) +
  geom_line() +
  geom_point() +
  theme_classic() + xlab("") + ylab("") +
  scale_color_manual(values = c("darkorchid", "darkgoldenrod1")) +
  theme(legend.position = "none")

# Exporting output
ggsave("output/figure/exp_yr.png")

##########################
# Differences in area
diff <- read_csv("output/diff_suitability_maps.csv")
colnames(diff)[1] <- 'yr'

diff <- diff %>% 
  dplyr::select(yr, area_diff)

ggplot(diff, aes(x = yr, y = area_diff, group = 1)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, lty = "dashed", linewidth = 0.1) +
  theme_classic() + xlab("") + ylab("") +
  theme(legend.position = "none")

# Exporting output
ggsave("output/figure/area_diff.png")


##########################
# Plot background data
bg <- read_csv("data/bg.csv")

# Getting German map data
library(rworldmap)

world <- getMap(resolution = "low")
study_area <- world[world@data$REGION %in% c("Asia", "Australia"), ] 
  
ggplot(bg, aes(lon, lat)) +
  geom_point(size = 0.01, col = "blue") + theme_classic() +
  xlab("Longitude") +ylab("Latitude") + scale_color_viridis_c() +
  geom_polygon(data = study_area, aes(x = long, y = lat, group = group),fill = NA, colour = "grey") +
  coord_quickmap() + xlim(60.875, 158.9583) + ylim(-54.75, 53.54167) + 
  theme(legend.title = element_blank())

ggsave("output/figure/background_points_map.png")

##########################################
# Identifying differences and comparing predictor variables

# Importing data frame
suit_pred <- read_csv("output/diff_suitability_pred.csv")

# Replacing values
suit_pred <- suit_pred %>% 
  mutate(val = replace(val, val == '-2', 'Only combined'))

suit_pred <- suit_pred %>% 
  mutate(val = replace(val, val == '-1', 'Always there'))

########
# Latitude-elevation plot
ggplot(suit_pred, aes(lat, elev, col = val)) +
  geom_point(alpha = 0.05) +
  xlab("") + ylab("") + theme_classic() +
  scale_color_manual(values = c("cyan", "darkorchid"))

ggsave("output/figure/lat_elev.png")

# Temp-rainfall plot
ggplot(suit_pred, aes(tmax, ppt, col = val)) +
  geom_point(alpha = 0.01) +
  xlab("") + ylab("") + theme_classic() +
  scale_color_manual(values = c("cyan", "darkorchid"))

ggsave("output/figure/tmax_ppt.png")

########
# Boxplot
# latitude
ggplot(suit_pred, aes(val, lat, col = val)) +
  geom_boxplot() +
  xlab("") + ylab("") + theme_classic() +
  scale_color_manual(values = c("cyan", "darkorchid"))+
  theme(legend.position = "none")

ggsave("output/figure/lat.png")

# elevation
ggplot(suit_pred, aes(val, elev, col = val)) +
  geom_boxplot() +
  xlab("") + ylab("") + theme_classic() +
  scale_color_manual(values = c("cyan", "darkorchid"))+
  theme(legend.position = "none")

ggsave("output/figure/elev.png")

# tmax
ggplot(suit_pred, aes(val, tmax, col = val)) +
  geom_boxplot() +
  xlab("") + ylab("") + theme_classic() +
  scale_color_manual(values = c("cyan", "darkorchid"))+
  theme(legend.position = "none")

ggsave("output/figure/tmax.png")

# ppt
ggplot(suit_pred, aes(val, ppt, col = val)) +
  geom_boxplot() +
  xlab("") + ylab("") + theme_classic() +
  scale_color_manual(values = c("cyan", "darkorchid"))+
  theme(legend.position = "none")

ggsave("output/figure/ppt.png")

