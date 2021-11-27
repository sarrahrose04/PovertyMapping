#Generating coordinates for downloading of nightlight imagery

library(tidyverse)
library(sf)
library(gdalUtilities)

shapefile_path <-fff #paste
#read shapefile
shp <- read_sf(shapefile_path)

#extract bounding box & round up values to add some buffer
xmin <- floor(st_bbox(shp)[[1]]) #round down
ymin <- floor(st_bbox(shp)[[2]])
xmax <- ceiling(st_bbox(shp)[[3]]) #round up
ymax <- ceiling(st_bbox(shp)[[4]])

NTL_file_folder <- ffh #ffhfh

wd_path <-dirname(NTL_file_folder)
setwd(wd_path)

#Filter NTL data products: 
    #for VIIRS: vcm-orm-ntl with extension avg_rade9.tif
    #for DMSP: web.stable_lights.avg_vis

#Obtain filenames of all NT sat img in folder
NTL_file_list <- list.files(path = NTL_file_folder,
                            pattern = ".tif$",
                            full.names = T)

#Use if-else statement to select the correct imagery product
  #filter for VIIRS
if(str_detect(NTL_file_folder,"SVBN_npp"))
  NTL_file <- NTL_file_list[str_detect(NTL_file_list, "vcm-orm-ntl")]
} else{
  #filter for DMPS
  NTL_file <- NTL_file_list[str_detect(NTL_file_list, "web.stable_lists.avg_vis")]
}

print(basename(NTL_file))

#Generate destination folder & output file ----
dest_path <- paste0(wd_path, "/cropped_", basename(NTL_file_folder), "/")
output_file <- paste0(dest_path, "cropped_", basename(NTL_file))

#Check if destination folders exists, otherwise create folders ----
if(!dir.exists(dest_path)) {
  dir.create(dest_path)
}

#Crop NTL image
gdal_translate(NTL_file,output_file, projwin = c(xmin,ymax,xmax,ymin))

