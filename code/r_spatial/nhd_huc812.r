# Code to calculate length and area of streams and lakes in parallel for HUC8 and HUC12 for each HUC4.
# Do separately for HUC8 river length, HUC8 lake area, HUC12 river length, HUC12 lake area

slice <- as.numeric(Sys.getenv('PBS_ARRAYID'))

# Boilerplate code to get the arguments passed in
# args are huclevel either "huc8" or "huc12" and watertype either "river" or "lake"
args=(commandArgs(TRUE))

for (i in 1:length(args)) {
  eval(parse(text=args[[i]]))
}

# Get correct directory
fp <- '/mnt/research/aquaxterra/DATA/raw_data/NHD/usgs_nhd'
alldirs <- dir(fp)
fp_huc <- file.path(fp, alldirs[slice], 'Shape')

library(rgdal)
library(rgeos)
library(dplyr)

# Load NHD shapefile
if (watertype == 'lake') {
  nhd <- readOGR(dsn = fp_huc, layer = 'NHDWaterbody')
  lookup <- read.csv('/mnt/research/aquaxterra/CODE/R/nhd/nhd_waterbody_lookup.csv', stringsAsFactors = FALSE)
} else {
  nhd <- readOGR(dsn = fp_huc, layer = 'NHDFlowline')
  lookup <- read.csv('/mnt/research/aquaxterra/CODE/R/nhd/nhd_flowline_lookup.csv', stringsAsFactors = FALSE)
}

# Load HUC shapefile
if (huclevel == 'huc8') {
  huc <- readOGR(dsn = fp_huc, layer = 'WBDHU8')
} else {
  huc <- readOGR(dsn = fp_huc, layer = 'WBDHU12')
}

# Calculate intersections of all NHD polygons or lines with each HUC
nhd_huc_intersect <- list()

for (i in 1:length(huc)) {
  print(paste(i, 'of', length(huc), 'intersections.'))
  nhd_huc_intersect[[i]] <- gIntersection(nhd, huc[i,], byid = TRUE, drop_lower_td = TRUE)
}

# Project to equal area so we can calculate length or area
aea_crs <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0'
nhd_aea <- lapply(nhd_huc_intersect, spTransform, CRSobj = aea_crs)

# Calculate length or area
if (watertype == 'lake') {
  nhd_size <- lapply(nhd_aea, gArea, byid = TRUE)
} else {
  nhd_size <- lapply(nhd_aea, gLength, byid = TRUE)
}

# Function to replace the pasted IDs with correct IDs in each list element.
# Add 1 because the IDs are 0 indexed but the data frame is 1 indexed
correct_ids <- function(x) {
  as.numeric(unlist(lapply(strsplit(names(x), split = ' '), '[', 1))) + 1
}

nhd_huc_data <- lapply(nhd_size, function(x) {
  ids <- correct_ids(x)
  nhd@data[ids, ] %>%
    rename(FCode = FCODE) %>%
    left_join(lookup) %>%
    cbind(size = as.numeric(x))
})

nhd_huc_summary <- lapply(nhd_huc_data, function(x) {
  x %>%
    filter(include == 'yes') %>%
    group_by(category, subcategory, permanence) %>%
    summarize(size = sum(size))
})

hucid <- as.character(huc@data$HUC)
huc4id <- substr(as.character(hucid[1]), 1, 4)

nhd_huc_summary <- cbind(HUC = rep(hucid, sapply(nhd_huc_summary, nrow)), do.call('rbind', nhd_huc_summary))

write.csv(nhd_huc_summary, file = paste0('/mnt/research/aquaxterra/CODE/R/nhd/csvs/', huc4id, '_', huclevel, '_', watertype, '.csv'), row.names = FALSE)