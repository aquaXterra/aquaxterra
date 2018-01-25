# Code to calculate lengths and areas for HUC4 (using same method as the smaller HUCs for consistency)

# Get list of directories
fp <- '/mnt/research/aquaxterra/DATA/raw_data/NHD/usgs_nhd'
alldirs <- dir(fp)

library(rgdal)
library(rgeos)
library(dplyr)

lake_summary <- list()
river_summary <- list()

pb <- txtProgressBar(0, length(alldirs), style = 3)

for (i in 1:length(alldirs)) {
  setTxtProgressBar(pb, i)
  
  fp_huc <- file.path(fp, alldirs[i], 'Shape')
  
  # Load NHD shapefiles
  
  lake_nhd <- readOGR(dsn = fp_huc, layer = 'NHDWaterbody')
  lake_lookup <- read.csv('/mnt/research/aquaxterra/CODE/R/nhd/nhd_waterbody_lookup.csv', stringsAsFactors = FALSE)
  
  river_nhd <- readOGR(dsn = fp_huc, layer = 'NHDFlowline')
  river_lookup <- read.csv('/mnt/research/aquaxterra/CODE/R/nhd/nhd_flowline_lookup.csv', stringsAsFactors = FALSE)
  
  # Project to equal area so we can calculate length or area
  aea_crs <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0'
  lake_nhd_aea <- spTransform(lake_nhd, CRSobj = aea_crs)
  river_nhd_aea <- spTransform(river_nhd, CRSobj = aea_crs)
  
  lake_area <- gArea(lake_nhd_aea, byid = TRUE)
  river_length <- gLength(river_nhd_aea, byid = TRUE)
  
  lake_data <- lake_nhd@data %>%
    rename(FCode = FCODE) %>%
    left_join(lake_lookup) %>%
    cbind(area = as.numeric(lake_area))
  
  lake_summary[[i]] <- lake_data %>%
    filter(include == 'yes') %>%
    group_by(category, subcategory, permanence) %>%
    summarize(area = sum(area))
  
  river_data <- river_nhd@data %>%
    rename(FCode = FCODE) %>%
    left_join(river_lookup) %>%
    cbind(length = as.numeric(river_length))
  
  river_summary[[i]] <- river_data %>%
    filter(include == 'yes') %>%
    group_by(category, subcategory, permanence) %>%
    summarize(length = sum(length))
}

close(pb)

huc4id <- sapply(strsplit(alldirs, '_'), '[', 3)

lake_summary <- cbind(HUC4 = rep(huc4id, sapply(lake_summary, nrow)), do.call('rbind', lake_summary))
river_summary <- cbind(HUC4 = rep(huc4id, sapply(river_summary, nrow)), do.call('rbind', river_summary))

write.csv(lake_summary, '/mnt/research/aquaxterra/DATA/reprojected_data/NHD/HUC4_lake_areas.csv', row.names = FALSE)
write.csv(river_summary, '/mnt/research/aquaxterra/DATA/reprojected_data/NHD/HUC4_river_lengths.csv', row.names = FALSE)