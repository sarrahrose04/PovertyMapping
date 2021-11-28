#Rescale poverty predictions, generate raster, and visualisation

#Load packages
library(raster)
library(tidyverse)

tmp_path <- "C:/temp"

#Define the folder where temporary raster files will be saved or create the folder if it does not exist
if(!dir.exists(tmp_path)) {
  dir.create(tmp_path)  #Create folder if not yet existing
}

#Set raster options to impove speed of calculation
rasterOptions(tmptime = 4,
              progress = "text", #displays raster operation progress bar
              timer = TRUE, #outputs raster calculation duration 
              maxmemory = 10e+9, #max no. of bytes to read into memory
              chunksize = 5e+9, #max no. of bytes to read/write in a single chunk while processing disk-based raster objects
              tmpdir = tmp_path) #location for writing temporary file

#Define CRS
WGS84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +tpwgs84=0,0,0"

#select csv file containing RR predicted poverty
pov_csv_path <- ff #paste

#set csv path's parent directory as working directory 
setwd(dirname(dirname(pov_csv_path)))

#detect country and year from filename
country_year <- str_extract(pove_csv_path,"[A-Z]{3}_[0-9]{4}")
country <- str_split(country_year,"_", simplify = T)[1]
year <- str_split(country_year, "_", simplify = T)[2]

target_var <- paste0("POV", year) #define column containing published poverty estimates

#load csv to dataframe
df_pov<- read.csv(pov_csv_path)

#Subset the predicted poverty dataframe to get the grid ID & lat and lon
centroids <- df_pov %>%
  select(id,lon,lat)

#Make a raster from centroids
#Function generates raster from regular grids like dataset used - assumes minimum distance between x & y coordinates is raster resolution
centroid_rast <- rasterFromXYZ(xyz = centroids[,c("lon","lat","id")], crs=WGS84)

#Load ML estimated population raster
pop_raster_path <- fhfhfh #paste
pop_raster <- raster(pop_raster_path)

#Check if population raster is using WGS84 CRS, otherwise reproject the raster
if(compareCRS(pop_raster,WGS84)==FALSE) {
  print("Raster CRS is not WGS84, Projecting raster to WGS84")
  pop_raster <- projectRaster(pop_raster, crs=WGS84)
}
print(crs(pop_raster))
print(paste0("Population Raster grid size: ",paste(res(pop_raster), collapse = ", ")))
print(paste0("Centroid Raster grid size: ",paste(res(centroid_rast), collapse = ", "))
      
#Calculate adjustment_factor because the 2 rasters have different resolutions (ratio)
adjustment_factor <- round(res(centroid_rast)/res(pop_raster))[1]

#aggregate population raster values to poverty grid by taking its sum 
pop_agg <- aggregate(pop_raster, fact = adjustment_factor, fun = sum)

#resample pop_agg raster to match the extent and resolution of centroid_rast
pop_agg_resampled <- resample(pop_agg, centroid_rast)

#rename raster column 
names(pop_agg_resampled) <- "gridpop"

#stack the two raster
pop_id_stack <- raster::stack(centroid_rast, pop_agg_resampled)

#convert the raster stack to dataframe
df_pop_id <- as.data.frame(pop_id_stack, na.rm=T)

#merge the aggregated pop at poverty grid with predicted poverty df
df_grid_pov <- left_join(df_pov, df_pop_id, by="id")

#Rescaling poverty estimates
#list predictions with values less than zero
df_grid_pov$prediction[df_grid_pov$prediction<0]
#list predictions with values more than 100
df_grid_pov$prediction[df_grid_pov$prediction>100]

#set all negative and more than 100 pred. values too 0.0001 and 100, respectively
df_grid_pov$prediction[df_grid_pov$prediction<0] <- 0.0001
df_grid_pov$prediction[df_grid_pov$prediction>100] <- 100

#rescale poverty predictions based on published poverty estimates
df_grid_pov <- df_grid_pov %>%
  mutate(pred_hci = prediction / 100) %>%
  mutate(svy_hci = get(target_var) / 100) %>%
  mutate(pred_hc = gridpop * pred_hci) %>%
  group_by(geocode) %>%
  mutate(pred_hc_rescale = pred_hc * (sum(svy_hc) / sum(pred_hc))) %>%
  mutate(pred_hci_rescale = pred_hc_rescale / gridpop) %>%
  ungroup()

#list rescaled predictions with values more than 1 
df_grid_pov$pred_hci_rescale[df_grid_pov$pred_hci_rescale>1]

#if any, set all rescaled values more than 1 to 1
df_grid_pov$pred_hci_rescale[df_grid_pov$pred_hci_rescale>1] <- 1

#generate raster 
pov_hci_raster <- rasterFromXYZ(xyz = df_grid_pov[,c("lon","lat","pred_hci")], crs = WGS84)
pov_hci_rescaled_raster <- rasterFromXYZ(xyz = df_grid_pov[,c("lon", "lat","pred_hci_rescale")], crs=WGS84)

#output raster
#set raster destination path
raster_path <- "Output/Poverty Raster/"

if (!dir.exists(raster_path)) {
  dir.create(raster_path, recursive = T)
}

writeRaster(pov_hci_raster , 
            filename = paste0(raster_path, paste(country_year, "pov_hci.tif", sep = "_")),
            overwrite = TRUE)
writeRaster(pov_hci_rescaled_raster , 
            filename = paste0(raster_path, paste(country_year, "pov_hci_rescaled.tif", sep = "_")),
            overwrite = TRUE)

#Visualisation 

#load packages
library(rasterVis)

#Define plotting function
plot_raster <- function(rast,p_var){
  theme_set(theme_bw())
  hci_heat <- cut(rast, p_var$category/100, include.lowest=T)
  
  plt_raster <- gplot(hci_heat) +
    geom_tile(aes(fill = as.character(value)))+
    scale_fill_brewer(name = p_var$scale_title,
                      palette = "RdY1Gn",
                      direction = -1,
                      labels = p_var$scale_label) +
    labs( title = paste0(p_var$map_title),
          x = "",
          y = "")+
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank())+
    coord_fixed()
  
  #save map as png
  ggsave(plt_raster,
         filename = p_var$filename,
         dpi = 300,
         device = "png")
  
  return(plt_raster)
}
