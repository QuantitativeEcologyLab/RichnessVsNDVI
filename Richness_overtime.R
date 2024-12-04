library(sp)
library(raster)
library(terra) 
library(ncdf4)
library(sf)
library(geodata)
library(rworldmap)
library(rnaturalearth)
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(purrr)
library(broom)

#try linear regression

# Importing all of the richness data 
sp_rich24 <- rast ("~/Library/CloudStorage/OneDrive-UBC/Directed Studies/Richness data/Combined_SR_2024/Combined_SR_2024.tif")
sp_rich23 <- rast ("~/Library/CloudStorage/OneDrive-UBC/Directed Studies/Richness data/Combined_SR_2023/Combined_SR_2023.tif")
sp_rich22 <- rast ("~/Library/CloudStorage/OneDrive-UBC/Directed Studies/Richness data/Combined_SR_2022/Combined_SR_2022.tif")
sp_rich21 <- rast ("~/Library/CloudStorage/OneDrive-UBC/Directed Studies/Richness data/Combined_SR_2022/Combined_SR_2022.tif")
# Note:richness data for 2017 has a different resolution and dimension so we won't include it in the graph
sp_rich17 <- rast ("~/Library/CloudStorage/OneDrive-UBC/Directed Studies/Richness data/Richness_all_2017/Richness_all.tif")
sp_rich17


#Define the CRS
PROJ_CRS <- crs("EPSG:3005")

#Import the shape file with canada's borders
canada_shapefiles <- ne_countries (country='Canada', returnclass='sf')
canada_vect <- vect(canada_shapefiles)

#reproject to match the species diversity raster
canada_vect <- project(canada_vect, PROJ_CRS)
plot(canada_vect) #Plot to confirm it worked

#Reproject the rasters
sp_rich24 <- project(sp_rich24, PROJ_CRS)
sp_rich23 <- project(sp_rich23, sp_rich24)
sp_rich22 <- project(sp_rich22, sp_rich24)
sp_rich21 <- project(sp_rich21, sp_rich24)
sp_rich17 <- project(sp_rich17, PROJ_CRS)

# Resample the 2017 raster to match the resolution and extent of the 2024 raster
sp_rich17 <- resample(sp_rich17, sp_rich24, method = "bilinear")

#Confirm everything lines up
crs(sp_rich17) == crs(sp_rich24)
res(sp_rich17) == res(sp_rich24)
ext(sp_rich17) == ext(sp_rich24)

#Some cropping and masking to make sure everything lines up
#NOTE:CONFIRM THIS IS THE BEST WAY TO DO THINGS
sp_rich24<- mask(sp_rich24, canada_vect)
sp_rich23<- mask(sp_rich23, canada_vect)
sp_rich22<- mask(sp_rich22, canada_vect)
sp_rich21<- mask(sp_rich21, canada_vect)
sp_rich17<- mask(sp_rich17, canada_vect)


# Convert rasters to data frames
df_rich24 <- as.data.frame(sp_rich24, xy = TRUE, na.rm = TRUE)
df_rich23 <- as.data.frame(sp_rich23, xy = TRUE, na.rm = TRUE)
df_rich22 <- as.data.frame(sp_rich22, xy = TRUE, na.rm = TRUE)
df_rich21 <- as.data.frame(sp_rich21, xy = TRUE, na.rm = TRUE)
df_rich17 <- as.data.frame(sp_rich17, xy = TRUE, na.rm = TRUE)

# Rename the richness column to avoid conflicts
names(df_rich24)[3] <- "richness24"
names(df_rich23)[3] <- "richness23"
names(df_rich22)[3] <- "richness22"
names(df_rich21)[3] <- "richness21"
names(df_rich17)[3] <- "richness17"

# Combine data frames into a long format
df_long <- bind_rows(
  mutate(df_rich24, year = 2024, richness = richness24) %>% select(-richness24),
  mutate(df_rich23, year = 2023, richness = richness23) %>% select(-richness23),
  mutate(df_rich22, year = 2022, richness = richness22) %>% select(-richness22),
  mutate(df_rich21, year = 2021, richness = richness21) %>% select(-richness21),
  mutate(df_rich17, year = 2017, richness = richness17) %>% select(-richness17)
)

# Combine data frames into a long format (without 2017)
df_long <- bind_rows(
  mutate(df_rich24, year = 2024, richness = richness24) %>% select(-richness24),
  mutate(df_rich23, year = 2023, richness = richness23) %>% select(-richness23),
  mutate(df_rich22, year = 2022, richness = richness22) %>% select(-richness22),
  mutate(df_rich21, year = 2021, richness = richness21) %>% select(-richness21)
)

# Plot species richness over time (all years)
ggplot(df_long, aes(x = factor(year), y = richness)) +
  geom_boxplot() +
  labs(title = "Species Richness Over Time",
       x = "Year",
       y = "Species Richness") +
  theme_minimal()

# Aggregate the data to calculate mean richness for each year (all years)
df_aggregated <- df_long %>%
  group_by(year) %>%
  summarize(mean_richness = mean(richness, na.rm = TRUE))

# Plot species richness over time (without 2017)
ggplot(df_aggregated, aes(x = year, y = mean_richness)) +
  geom_line() +
  geom_point() +
  labs(title = "Mean Species Richness Over Time",
       x = "Year",
       y = "Mean Species Richness") +
  theme_minimal()

# Aggregate the data to calculate mean richness for each year (without 2017)
df_aggregated <- df_long %>%
  group_by(year) %>%
  summarize(mean_richness = mean(richness, na.rm = TRUE))

# Plot species richness over time (without 2017)
ggplot(df_aggregated, aes(x = year, y = mean_richness)) +
  geom_line() +
  geom_point() +
  labs(title = "Mean Species Richness Over Time (Excluding 2017)",
       x = "Year",
       y = "Mean Species Richness") +
  theme_minimal()

# Save the plot to OneDrive
one_drive_path <- ("~/Library/CloudStorage/OneDrive-UBC/Directed Studies/Richness data/richness_vs_time.png")
ggsave(rich_vs_time_line_excl17, filename = one_drive_path, width = 10, height = 6, dpi = 300)

