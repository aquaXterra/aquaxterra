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
library(rgeos)

# read in job arguments ---------------------------------------------------

slice_ind <- as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID')) # beginning index of lagos slice
n_slices <- as.numeric(Sys.getenv('SLURM_ARRAY_TASK_COUNT'))
# e.g., in job script... --array=1:50
n_cores <- as.numeric(Sys.getenv('SLURM_CPUS_PER_TASK')) # number cores available

args <- (commandArgs(TRUE))
rast_path <- as.character(args[1])
rast_name <- as.character(args[2])

# read in data raster and huc polygons ------------------------------------

rast_data <- raster(rast_path)

# load HUC shapefile
fp <- '/mnt/research/aquaxterra/DATA/raw_data/LAGOS/'
lagos_data <- readOGR(dsn = fp, layer = 'IWS')
slice_length <- round(length(lagos_data)/n_slices)
slice_start <- slice_ind + ((slice_ind - 1) * slice_length)
slice_end <- slice_ind + (slice_ind * slice_length)
if (slice_ind == slice_length) {
  slice_end <- length(lagos_data)
}
lagos_data <- lagos_data[slice_start:slice_end,]

# transform everything to albers equal area -------------------------------

aea_crs <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0'

# raster
rast_data <- projectRaster(rast_data, crs = aea_crs)

# LAGOS polygons
lagos_data <- spTransform(lagos_data, CRSobj = aea_crs)

# crop polygons to us boundaries ------------------------------------------

# load albers-projected state boundaries
load(file.path('/mnt/research/aquaxterra/DATA/albers_projected_data/state_borders/', 'states_albers.RData'))
goodusabounds <- gUnaryUnion(states_albers)
goodusabounds <- spTransform(goodusabounds, CRSobj = aea_crs)

# limit lagos data to polygons that are completely contained within us bounds
inds <- gContains(goodusabounds, lagos_data, byid = TRUE)
lagos_data <- lagos_data[which(inds == TRUE), ] 

# go polygon by polygon to extract raster info ----------------------------

# crop the raster to the HUC4 level first to speed things up
rast_data <- crop(rast_data, (extent(lagos_data) + 1000)) # add 500m buffer on all sides

# function to extract raster data for all hucs in the huc4 (including the huc4 itself)
extracts <- function(lagos_data, rast_data, out_file, n_cores) {
  # start cluster
  registerDoParallel(cores = n_cores)
  
  # create empty dataframe
  outdata <- data.frame(lagoslakei = NA, 
                        rast_mean = NA,
                        rast_sd = NA,
                        rast_fifth = NA,
                        rast_nfifth = NA,
                        rast_n = NA)
  
  # extract data for each huc (4/8/12) in the huc4
  tempdata <- foreach(i = 1:length(lagos_data)) %dopar% {
    message(paste(i, 'of', length(lagos_data), 'extractions started'))
    
    # crop data
    out <- crop(rast_data, extent(lagos_data[i, ]))
    out <- mask(out, lagos_data[i, ])
    
    # calculate stats
    out_vals <- getValues(out)
    out_mean <- mean(out_vals, na.rm = TRUE)
    out_sd <- sd(out_vals, na.rm = TRUE)
    out_quants <- quantile(out_vals, probs = seq(0, 1, 0.05), na.rm = TRUE)
    out_fifth <- out_quants[[2]]
    out_nfifth <- out_quants[[20]]
    out_n <- length(out_vals[!is.na(out_vals)])
    
    # add stats to dataframe
    outdata[1,] <- c(as.character(lagos_data@data[i, grep('lagoslakei', names(lagos_data[i, ]@data))]),
                     out_mean, out_sd, out_fifth, out_nfifth, out_n)
    outdata
  }
  # stop cluster
  stopImplicitCluster()
  
  # take foreach list output and convert to dataframe
  n_rows <- length(lagos_data)
  for (i in 1:n_rows) {
    outdata <- rbind(outdata, tempdata[[i]])
  }
  outdata <- outdata[-1, ]
  
  # re-name columns with correct raster name
  names(outdata) <- gsub('rast', rast_name, names(outdata))
  
  # convert all columns to numeric and replace NaN with NA
  outdata <- as.data.frame(apply(outdata, 2, FUN = function(x) {as.numeric(x)}))
  outdata$baseflow_mean[is.nan(outdata$baseflow_mean)] <- NA
  
  # write out outdata
  write.csv(outdata, out_file, row.names = FALSE)
  return(outdata)
}

# run the extraction function for each huc level
lagos_out <- extracts(lagos_data = lagos_data, rast_data = rast_data, 
                         out_file = paste('/mnt/research/aquaxterra/DATA/processed_data/baseflow/lagos_', slice_start, '_baseflow.csv', sep = ''),
                         n_cores = n_cores)

### Once all HUC4 levels are complete, you join data for all HUC4, HUC8, and HUC12 csvs
### and add those data to the summary table.
