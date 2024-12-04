library(ggplot2)
library(viridis)
library(viridisLite)
library(dplyr)
library(hexbin)
library(geomtextpath)

#Plot species richness as a function of mean NDVI
plot(richness ~ mean_NDVI, data = test) 
#Adding trend line to the graph
abline

#Plot species richness as a function of the variance in NDVI
plot(richness ~ var_NDVI, data = test) 

#Plot species richness as a function of the coefficient of variation in NDVI
plot(richness ~ CV_NDVI, data = test) 


#Plot mean_NDVI and var_NDVI with colors as a function of species richness
plot(var_NDVI ~ mean_NDVI, data=test)

test <- data.frame(sp_rich23)
names(test) <- "richness"
test$mean_NDVI <- data.frame(predNDVI)[,1]
test$var_NDVI <- data.frame(var_res)[,1]
test$CV_NDVI <- data.frame(CV_ras)[,1]

test <- test %>%
  select(-mean_NDVI_bin, -richness_bin , -var_NDVI_bin)

test$richness <- as.numeric(test$richness)
test$mean_NDVI <- as.numeric(test$mean_NDVI)
test$var_NDVI <- as.numeric(test$var_NDVI)

# Create bins for mean_NDVI and var_NDVI
test <- test %>%
  mutate(mean_NDVI_bin = cut(mean_NDVI, breaks = seq(-0.07, 0.41, by = 0.02)),
         var_NDVI_bin = cut(var_NDVI, breaks = seq(0, 0.04, by = 0.002)))

# Group the binned data and create the geom tile plot
binned_data <- test %>%
  group_by(mean_NDVI_bin, var_NDVI_bin) %>%
  summarise(richness = mean(richness, na.rm = TRUE))

richness_plot <- ggplot(binned_data, aes(x = mean_NDVI_bin, y = var_NDVI_bin)) +
  geom_tile(aes(fill = richness)) +
  scale_fill_viridis_c(name = "Species Richness", option = "D") +
  labs(title = "Mean NDVI vs Variance in NDVI with Species Richness",
       x = "Mean NDVI",
       y = "Variance in NDVI") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())

print(richness_plot)

#Save the ggplot
ggsave(filename = "~/Library/CloudStorage/OneDrive-UBC/Species Richness data/NDVI_vs_richness_tile.png", plot = richness_plot, width = 8, height = 8, dpi = 300)

#Save the dataset
save(test, file = "~/Library/CloudStorage/OneDrive-UBC/Species Richness data/test_dataset.RData")
load("~/Library/CloudStorage/OneDrive-UBC/Species Richness data/NDVI_richness_dataset.RData")
ls()
