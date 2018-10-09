### Extract raster data to HUC polygons
# This script should be run as an array job, with one task for each HUC4 polygon
# (223 total). It extracts raster data for each HUC4, HUC8, and HUC12 polygon
# within that HUC4.

# Written by AC Smith on October 8, 2018
# Lasted edited by AC Smith, October 8, 2018

# load packages -----------------------------------------------------------

library(raster)
library(rgdal)
library(dplyr)
library(foreach)
library(doParallel)

# read in job arguments ---------------------------------------------------

slice <- as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID')) # number of huc4 polygons
n_cores <- as.numeric(Sys.getenv('SLURM_CPUS_PER_TASK')) # number cores available

args <- (commandArgs(TRUE))
rast_path <- as.character(args[1])
rast_name <- as.character(args[2])

# read in data raster and huc polygons ------------------------------------

rast_data <- raster(rast_path)

# load HUC shapefile
fp <- '/mnt/research/aquaxterra/DATA/raw_data/HUC/shapefiles/'
huc4_data <- readOGR(dsn = fp, layer = 'WBDHU4')[slice, ]
huc8_data <- readOGR(dsn = fp, layer = 'WBDHU8')
huc12_data <- readOGR(dsn = fp, layer = 'WBDHU12')

# grab only HUC 8/12 in HUC4 ----------------------------------------------

huc8_data <- huc8_data[which(substr(huc8_data@data$HUC8, 1, 4) == huc4_data@data$HUC4), ]
huc12_data <- huc12_data[which(substr(huc12_data@data$HUC12, 1, 4) == huc4_data@data$HUC4), ]

# transform everything to albers equal area -------------------------------

aea_crs <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0'

# raster
rast_data <- projectRaster(rast_data, crs = aea_crs)

# HUC polys
huc4_data <- spTransform(huc4_data, CRSobj = aea_crs)
huc8_data <- spTransform(huc8_data, CRSobj = aea_crs)
huc12_data <- spTransform(huc12_data, CRSobj = aea_crs)

# go polygon by polygon to extract raster info ----------------------------

# get name of HUC4 that is being worked on
HUC4 <- as.character(huc4_data@data$HUC4)

# crop the raster to the HUC4 level first to speed things up
rast_data <- crop(rast_data, extent(huc4_data))

# function to extract raster data for all hucs in the huc4 (including the huc4 itself)
huc_extracts <- function(huc_data, rast_data, out_file, n_cores) {
  # start cluster
  registerDoParallel(cores = n_cores)
  
  # create empty dataframe
  outdata <- data.frame(HUC = NA, 
                        rast_mean = NA,
                        rast_sd = NA,
                        rast_fifth = NA,
                        rast_nfifth = NA,
                        rast_n = NA)
  
  # extract data for each huc (4/8/12) in the huc4
  tempdata <- foreach(i = 1:length(huc_data)) %dopar% {
    message(paste(i, 'of', length(huc_data), 'extractions started'))
    
    # crop data
    out <- crop(rast_data, extent(huc_data[i, ]))
    out <- mask(out, huc_data[i, ])
    
    # calculate stats
    out_vals <- getValues(out)
    out_mean <- mean(out_vals, na.rm = TRUE)
    out_sd <- sd(out_vals, na.rm = TRUE)
    out_quants <- quantile(out_vals, probs = seq(0, 1, 0.05), na.rm = TRUE)
    out_fifth <- out_quants[[2]]
    out_nfifth <- out_quants[[20]]
    out_n <- length(out_vals[!is.na(out_vals)])
    
    # add stats to dataframe
    outdata[1,] <- c(as.character(huc_data@data[i, grep('^HUC', names(huc_data[i, ]@data))]),
                     out_mean, out_sd, out_fifth, out_nfifth, out_n)
    outdata
  }
  # stop cluster
  stopImplicitCluster()
  
  # take foreach list output and convert to dataframe
  n_rows <- length(huc_data)
  for (i in 1:n_rows) {
    outdata <- rbind(outdata, tempdata[[i]])
  }
  outdata <- outdata[-1, ]
  
  # re-name columns with correct raster name
  names(outdata) <- gsub('rast', rast_name, names(outdata))
  
  # write out outdata
  write.csv(outdata, out_file, row.names = FALSE)
  return(outdata)
}

# run the extraction function for each huc level
huc4_out <- huc_extracts(huc_data = huc4_data, rast_data = rast_data, 
                         out_file = paste('/mnt/research/aquaxterra/DATA/processed_data/baseflow/', HUC4, '_huc4_baseflow.csv', sep = ''),
                         n_cores = n_cores)
huc8_out <- huc_extracts(huc_data = huc8_data, rast_data = rast_data, 
                         out_file = paste('/mnt/research/aquaxterra/DATA/processed_data/baseflow/', HUC4, '_huc8_baseflow.csv', sep = ''),
                         n_cores = n_cores)
huc12_out <- huc_extracts(huc_data = huc12_data, rast_data = rast_data, 
                         out_file = paste('/mnt/research/aquaxterra/DATA/processed_data/baseflow/', HUC4, '_huc12_baseflow.csv', sep = ''),
                         n_cores = n_cores)

### Once all HUC4 levels are complete, you join data for all HUC4, HUC8, and HUC12 csvs
### and add those data to the summary table.
