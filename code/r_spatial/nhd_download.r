### downloading NHD zip files ###

library(raster)
library(rts)
library(RCurl)
library(rgdal)

setwd("/mnt/research/aquaxterra/DATA/raw_data/NHD/usgs_nhd")

ftp.site <- "ftp://rockyftp.cr.usgs.gov/vdelivery/Datasets/Staged/Hydrography/NHD/HU4/HighResolution/Shape/"

huc4 <- readOGR("W:/DATA/raw_data/HUC/shapefiles/WBDHU4.shp")
huc4.list <- as.character(unique(huc4$HUC4))


huc4.list <- c('0203', '0505', '1204', '1701', '1706')
n <- length(huc4.list)

for (i in 1:n) {
  filename <- paste0("NHD_H_", huc4.list[i], "_HU4_Shape.zip")
  filepath <- paste0(ftp.site, filename)
  download.file(filepath, filename, cacheOK = FALSE, mode = "wb",
                quiet = TRUE)
  dir.out <- paste0("NHD_H_", huc4.list[i], "_Shape")
  unzip(filename, exdir = dir.out)
  file.remove("filename")
  print(paste(i, "/223 -", date()))
  
}

# running on my desktop this is taking > 5 minutes per file :[
