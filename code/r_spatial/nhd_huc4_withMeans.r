### Created by QDR
### Edited by ACS 9/5/18
### Project: aquaXterra

# Code to calculate lengths and areas for HUC4 (using same method as the smaller HUCs for consistency)
# change path to library for newer versions of packages
Sys.getenv("R_LIBS_USER")

# Get list of directories
fp <- '/mnt/research/aquaxterra/DATA/raw_data/NHD/usgs_nhd'

alldirs <- dir(fp)

library(rgdal)
library(rgeos)
library(dplyr)

# for total lengths/areas in HUC
lake_summary <- list()
river_summary <- list()

# for mean major lengths/areas in HUC
lake_means <- list()
river_means <- list()

# start the progress bar
pb <- txtProgressBar(0, length(alldirs), style = 3)

for (i in 1:length(alldirs)) {
  setTxtProgressBar(pb, i)
  
  fp_huc <- file.path(fp, alldirs[i], 'Shape')
  
  if (!file.exists(fp_huc)) {
    lake_summary[[i]] <- data.frame(category = NA, subcategory = NA, permanence = NA, area = NA)
    river_summary[[i]] <- data.frame(category = NA, subcategory = NA, permanence = NA, length = NA)
    lake_means[[i]] <- data.frame(mean_area = NA, median_area = NA, sd_area = NA, n = NA)
    river_means[[i]] <- data.frame(mean_length = NA, median_length = NA, sd_length = NA, n = NA)
  } else {
    
    # Load NHD shapefiles
    
    lake_nhd <- readOGR(dsn = fp_huc, layer = 'NHDWaterbody')
    lake_lookup <- read.csv('/mnt/research/aquaxterra/CODE/R/nhd/nhd_waterbody_lookup.csv', stringsAsFactors = FALSE)
    
    river_nhd <- readOGR(dsn = fp_huc, layer = 'NHDFlowline')
    river_lookup <- read.csv('/mnt/research/aquaxterra/CODE/R/nhd/nhd_flowline_lookup.csv', stringsAsFactors = FALSE)
    
    # Project to equal area so we can calculate length or area
    aea_crs <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0'
    lake_nhd_aea <- spTransform(lake_nhd, CRSobj = aea_crs)
    river_nhd_aea <- spTransform(river_nhd, CRSobj = aea_crs)
    
    # calculate area or length
    lake_area <- gArea(lake_nhd_aea, byid = TRUE)
    river_length <- gLength(river_nhd_aea, byid = TRUE)
    
    lake_data <- lake_nhd@data %>%
      rename(FCode = FCODE) %>% # fcode = type of hydrological feature
      left_join(lake_lookup) %>%
      cbind(area = as.numeric(lake_area))
    
    # sum area by type of lake
    lake_summary[[i]] <- lake_data %>%
      filter(include == 'yes') %>%
      group_by(category, subcategory, permanence) %>%
      summarize(area = sum(area))
    
    # get the average sizes of major lakes in the area
    lake_means[[i]] <- lake_data %>%
      filter(include == 'yes') %>%
      filter(!is.na(GNIS_NAME)) %>%
      group_by(GNIS_NAME) %>%
      summarize(area = sum(area)) %>%
      summarize(mean_area = mean(area),
                median_area = median(area),
                sd_area = sd(area),
                n = n())
    
    river_data <- river_nhd@data %>%
      rename(FCode = FCODE) %>%
      left_join(river_lookup) %>%
      cbind(length = as.numeric(river_length))
    
    # sum river lengths by type of river
    river_summary[[i]] <- river_data %>%
      filter(include == 'yes') %>%
      group_by(category, subcategory, permanence) %>%
      summarize(length = sum(length))
    
    # get average length of major rivers
    river_means[[i]] <- river_data %>%
      filter(include == 'yes') %>%
      filter(!is.na(GNIS_NAME)) %>%
      group_by(GNIS_NAME) %>%
      summarize(length = sum(length)) %>%
      summarize(mean_length = mean(length),
                median_length = median(length),
                sd_length = sd(length),
                n = n())
    
  }
}

close(pb)

save(lake_summary, river_summary, lake_means, river_means, file = '/mnt/research/aquaxterra/DATA/reprojected_data/NHD/HUC4_summaries.r')

######
# Compile output.
load('/mnt/research/aquaxterra/DATA/reprojected_data/NHD/HUC4_summaries.r')
huc4id <- sapply(strsplit(alldirs, '_'), '[', 3)

lake_summary <- cbind(HUC4 = rep(huc4id, sapply(lake_summary, nrow)), do.call('rbind', lake_summary))
river_summary <- cbind(HUC4 = rep(huc4id, sapply(river_summary, nrow)), do.call('rbind', river_summary))
lake_means <- cbind(HUC4 = rep(huc4id, sapply(lake_means, nrow)), do.call('rbind', lake_means))
river_means <- cbind(HUC4 = rep(huc4id, sapply(river_means, nrow)), do.call('rbind', river_means))

write.csv(lake_summary, '/mnt/research/aquaxterra/DATA/reprojected_data/NHD/HUC4_lake_areas.csv', row.names = FALSE)
write.csv(river_summary, '/mnt/research/aquaxterra/DATA/reprojected_data/NHD/HUC4_river_lengths.csv', row.names = FALSE)
write.csv(lake_means, '/mnt/research/aquaxterra/DATA/reprojected_data/NHD/HUC4_lake_areas_summaries.csv', row.names = FALSE)
write.csv(river_means, '/mnt/research/aquaxterra/DATA/reprojected_data/NHD/HUC4_river_lengths_summaries.csv', row.names = FALSE)
