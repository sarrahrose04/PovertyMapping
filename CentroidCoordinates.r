#Generating Centroid Coordinates for downloading satellite imager

#load packages 
library(sf)
library(raster)
library(tidyverse) 
library(fasterize)
library(terra)

# set working directory 
#wd <- tcltk::tkchooseDirectory (caption ="Select Working Directory")
#setwd(wd)
setwd("/Users/sarrahrose/Downloads/tha_adm_rtsd_itos_20190221_SHP_PART_2")

#define country code 
country = "THA"

#calculate grid size - a product of satellite resolution & CNN input image size 
set.grid.resolution.px <- 256 #in pixel, img size required by CNN model 
satellite.granularity <- 15 #in metre/pixel; landsat res after pansharperning 

gridsize <-set.grid.resolution.px*satellite.granularity

#Select location of administrative boundary shapefile
shp_path <- "tha_admbnda_adm3_rtsd_20190221.shp"

#Read shapefile to see which points are in country border 
ADM_sf <- read_sf(shp_path)

#we use stringr package's str_extract() function to get the numeric portion of ADM3_PCODE entries 
ADM_sf$geocode <-as.numeric(str_extract(ADM_sf$ADM3_PCODE, "[0-9]+\\.*[0-9]*"))
ADM_sf$geocode

#Define CRS variables 
WGS84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
UTM_CRS <- "+proj=utm +zone=47 +datum=WGS84 +units=m +no_defs" #Thai is zone 47

#check projection info of shapefile
print(crs(ADM_sf))

#transform shapefile from WGS84 to UTM 
ADM_UTM_sf <- st_transform(ADM_sf, UTM_CRS)

#check projection info of shapefile to verify crs
print(crs(ADM_UTM_sf))

#get boundary box of shapefile 
PCS_ext <- extent(ADM_UTM_sf)
GCS_ext <- extent(ADM_sf)

#calculate conversion factor from degrees to metres using bounding box
meter_reciprocal_PCS2GCS <- (diff(PCS_ext[1:2]) / diff(GCS_ext[1:2]) + 
                               diff(PCS_ext[3:4]) / diff(GCS_ext[3:4]))/2

#Create an empty raster of grid size granularity 
#https://desktop.arcgis.com/en/arcmap/10.3/manage-data/geodatabases/raster-basics.htm 
#https://www.earthdatascience.org/courses/use-data-open-source-python/intro-raster-data-python/fundamentals-raster-data/
#https://gisgeography.com/spatial-data-types-vector-raster/

ADM_raster <- raster(GCS_ext, 
                     res = gridsize/meter_reciprocal_PCS2GCS,
                     crs = WGS84)

#Rasterize the shapefile's geocode 
geocode_raster <- fasterize(ADM_sf, ADM_raster, field = "geocode")

#Get the centroids coordinates
geocode_df <- as.data.frame(geocode_raster, xy = T)
head(geocode_df) #check the created dataframe

selected.centroids <- geocode_df %>% #create a new dataframe from geocode_df
  filter(!is.na(layer)) %>% #remove NA from layer columns; only get within country's borders
  mutate(id = 1:n()) %>% #create a new column containing grid ID
  select(id, 
         lon = x, 
         lat = y,
         geocode = layer) #layer column renamed as geocode 

#Generate filename for CSV 
file_name <- paste(country, "centroid", gridsize, "grid", sep = "_")
write.csv(selected.centroids, file = paste0(file_name, ".csv"),
          row.names = F)











