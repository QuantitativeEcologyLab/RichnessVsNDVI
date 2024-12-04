library(ggplot2)
library(viridisLite)
library(viridis)
library(dplyr)
library(hexbin)
library(dagitty)
library(ggdag)

#Plot species richness as a function of mean NDVI
plot(richness ~ mean_NDVI, data = test) 

test <- data.frame(sp_rich23)
names(test) <- "richness"
test$mean_NDVI <- data.frame(predNDVI)[,1]
test$var_NDVI <- data.frame(var_res)[,1]
test$CV_NDVI <- data.frame(CV_ras)[,1]

test$richness <- as.numeric(test$richness)
test$mean_NDVI <- as.numeric(test$mean_NDVI)
test$var_NDVI <- as.numeric(test$var_NDVI)

# Create the hex plot (Mean_NDVI vs richness)
hex_plot <- ggplot(test, aes(x = mean_NDVI, y = richness)) +  
geom_hex(aes(fill = ..count..), color = "white") +  
scale_fill_viridis(option = "C", direction = -1, name = "Count") +
labs(title = "Hex Plot of Mean NDVI vs Richness",
x = "Mean NDVI",
y = "Richness") +
theme_light() +
theme(axis.text.x = element_text(angle = 45, hjust = 1),
panel.grid = element_blank())

plot(hex_plot)

# Save the hex plot to OneDrive
one_drive_path <- "~/Library/CloudStorage/OneDrive-UBC/Directed Studies/mean_NDVI_vs_richness_tile.png"
ggsave(one_drive_path, 
       plot = hex_plot, 
       width = 10,   
       height = 8,   
       dpi = 300) 

# Create the violin plot
violin_plot <- ggplot(test, aes(x = factor(richness), y = mean_NDVI)) +
  geom_violin(fill = "skyblue", alpha = 0.7) +
  labs(title = "Violin Plot of Mean NDVI vs Species Richness",
       x = "Species Richness",
       y = "Mean NDVI") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the violin plot
print(violin_plot)

# Create the hex plot (var_NDVI vs richness)
hex_plot <- ggplot(test, aes(x = var_NDVI, y = richness)) +  
  geom_hex(aes(fill = ..count..), color = "white") +  
  scale_fill_viridis(option = "E", direction = -1, name = "Count") +
  labs(title = "Hex Plot of variance in NDVI vs Richness",
       x = "Variance in NDVI",
       y = "Richness") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())

plot(hex_plot)

# Save the hex plot to OneDrive
one_drive_path <- "~/Library/CloudStorage/OneDrive-UBC/Directed Studies/var_NDVI_vs_richness_tile.png"
ggsave(one_drive_path, 
       plot = hex_plot, 
       width = 10,   
       height = 8,   
       dpi = 300)    

# Create the hex plot (CV_NDVI vs richness)
hex_plot <- ggplot(test, aes(x = CV_NDVI, y = richness)) +  
  geom_hex(aes(fill = ..count..), color = "white") +  
  scale_fill_viridis(option = "D", direction = -1, name = "Count") +
  labs(title = "Hex Plot of Coefficient of variation in NDVI vs Richness",
       x = "Coefficient of variation in NDVI",
       y = "Richness") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())

plot(hex_plot)

# Save the hex plot to OneDrive
one_drive_path <- "~/Library/CloudStorage/OneDrive-UBC/Directed Studies/CV_NDVI_vs_richness_tile.png"
ggsave(one_drive_path, 
       plot = hex_plot, 
       width = 10,   
       height = 8,   
       dpi = 300)    

#Define the DAG with positive and negative relationships
dag <- dagitty("
dag {
  Mean_NDVI -> Variance_NDVI
  Mean_NDVI -> CV_NDVI
  Mean_NDVI -> Richness
  Variance_NDVI -> Richness
  CV_NDVI -> Richness
}
")

# Extract edges from the DAG as a data frame
dag_edges <- as.data.frame(dagitty::edges(dag))

# Extract the unique nodes (variables)
dag_nodes <- data.frame(
  name = unique(c(dag_edges$from, dag_edges$to)),
  x = c(1, 2, 2, 1, 1),  # Define dummy x positions for each node
  y = c(2, 3, 1, 2, 4)   # Define dummy y positions for each node
)

# Display the data for verification
print(dag_edges)
print(dag_nodes)
