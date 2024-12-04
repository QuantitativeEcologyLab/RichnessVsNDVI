library(sp)
library(raster)
library(terra) 
library(ncdf4)
library(sf)
library(geodata)
library(rworldmap)
library(rnaturalearth)

#Import the data
CV_ras <- rast("~/Library/CloudStorage/OneDrive-UBC/Species Richness data/Environmental/coeff_variation_raster (1).nc")
CV_ras
var_res<- rast ("~/Library/CloudStorage/OneDrive-UBC/Species Richness data/Environmental/var_residuals_raster (1).nc")
var_res
predNDVI<- rast("~/Library/CloudStorage/OneDrive-UBC/Species Richness data/Environmental/mean_predNDVI_raster (1).nc")
predNDVI
sp_rich23<- rast("~/Library/CloudStorage/OneDrive-UBC/Species Richness data/Combined_SR_2023/Combined_SR_2023.tif")

#Define the CRS
PROJ_CRS <- crs("EPSG:3005")

plot(sp_rich23)

#Reproject the habitat rasters
CV_ras <- project(CV_ras, PROJ_CRS)
var_res <- project(var_res, CV_ras)
predNDVI <- project(predNDVI, CV_ras)

#reproject the richness rasters
sp_rich23 <- project(sp_rich23, CV_ras)


#Import the shape file with canada's borders
canada_shapefiles <- ne_countries (country='Canada', returnclass='sf')
canada_vect <- vect(canada_shapefiles)
#reproject to match the species diversity raster
canada_vect <- project(canada_vect, PROJ_CRS)
plot(canada_vect) #Plot to confirm it worked


#Confirm everything lines up
crs(var_res) == crs(sp_rich23)
res(var_res) == res(sp_rich23)
ext(var_res) == ext(sp_rich23)

#Some cropping and masking to make sure everything lines up
#NOTE:CONFIRM THIS IS THE BEST WAY TO DO THINGS
sp_rich23<- mask(sp_rich23, canada_vect)
sp_rich23 <- mask(sp_rich23, var_res)

CV_ras<- mask(CV_ras, canada_vect)
CV_ras<- mask(CV_ras, sp_rich23)

var_res<- mask(var_res, canada_vect)
var_res<- mask(var_res, sp_rich23)

predNDVI<- mask(predNDVI, canada_vect)
predNDVI <- mask(predNDVI, sp_rich23)


test <- data.frame(sp_rich23)
names(test) <- "richness"
test$mean_NDVI <- data.frame(predNDVI)[,1]
test$var_NDVI <- data.frame(var_res)[,1]
test$CV_NDVI <- data.frame(CV_ras)[,1]


#Plot species richness as a function of mean NDVI
plot(richness ~ mean_NDVI, data = test) 

#Plot species richness as a function of the variance in NDVI
plot(richness ~ var_NDVI, data = test) 

#Plot species richness as a function of the coefficient of variation in NDVI
plot(richness ~ CV_NDVI, data = test) 

#Reproject the variance raster
var_res <- project(var_res, sp_rich23_crop)
