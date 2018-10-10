# Code to calculate length and area of streams and lakes in parallel for HUC8 and HUC12 for each HUC4.
# Do separately for HUC8 river length, HUC8 lake area, HUC12 river length, HUC12 lake area

# Alternate version created 20 Aug 2018: optimized for speed by using gIntersects() which returns logical.

slice <- as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))
max_n_seconds <- 3600
n_cores <- 5

# Boilerplate code to get the arguments passed in
# args are huclevel either "huc8" or "huc12" and watertype either "river" or "lake"
args <- (commandArgs(TRUE))

watertype <- as.character(args[1])
huclevel <- as.character(args[2])

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

# Edit 20 Aug: manually get rid of Great Lakes. Only get rid of the HUC that is an exact match to the Great Lakes names.
# These are so big that they cause the gIntersection call to get stuck
greatlakes <- paste('Lake', c('Huron','Ontario','Michigan','Erie','Superior'))
huc <- subset(huc, !NAME %in% greatlakes)

# There are also a couple huge islands in Canadian part of the Great Lakes that aren't really part of the HUC system.
# They are not split into smaller catchments as they should be, so they are too large for gIntersection to handle
bigislands <- c('Joseph Island-Frontal Lake Huron', 'Manitoulin Island', 'Amherst Island-Frontal Lake Ontario')
huc <- subset(huc, !NAME %in% bigislands)

library(foreach)
library(doParallel)
library(R.utils)
registerDoParallel(cores = n_cores)

# Calculate intersections of all NHD polygons or lines with each HUC
# Edit 20 Aug: print message both when started and finished
# Edit 20 Aug: split into 2 steps: 1 step to do a logical with gIntersects 
# Edit 20 Aug: add a timeout catcher so that it will not hang for more than 1 hour on a single HUC. (set max_n_seconds above)
nhd_huc_intersect <- foreach (i = 1:length(huc)) %dopar% {
  message(paste(i, 'of', length(huc), 'intersections started'))
  inter <- SpatialPolygonsDataFrame(SpatialPolygons(list()), data=data.frame()) # Create empty SPDF which will not be filled in unless the operation completes before timeout.
  # The code below will quit after a predefined amount of time.
  tryCatch(
	expr = {
		withTimeout({
		  inter_subset <- gIntersects(nhd, huc[i,], byid = TRUE)[1, ]
		  if (sum(inter_subset) > 0) inter <- gIntersection(nhd[inter_subset,], huc[i,], byid = TRUE, drop_lower_td = TRUE)
		  message(paste(i, 'of', length(huc), 'intersections finished'))
		},
		timeout = max_n_seconds)
	},
	TimeoutException = function(ex) message(paste(i, 'of', length(huc), 'intersections timed out!!!'))
  )
 
  inter
}

nhd_aea <- list()
nhd_size <- list()

# Project to equal area so we can calculate length or area
aea_crs <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0'

for (i in 1:length(huc)) {
  nhd_aea[[i]] <- try(spTransform(nhd_huc_intersect[[i]], CRSobj = aea_crs),  TRUE)
  
  if (inherits(nhd_aea[[i]], 'try-error')) {
	nhd_size[[i]] <- NA
  } else {
	# Calculate length or area
	if (watertype == 'lake') {
	  nhd_size[[i]] <- gArea(nhd_aea[[i]], byid = TRUE)
	} else {
	  nhd_size[[i]] <- gLength(nhd_aea[[i]], byid = TRUE)
	}
  }
  
}



# Function to replace the pasted IDs with correct IDs in each list element.
# Add 1 because the IDs are 0 indexed but the data frame is 1 indexed
correct_ids <- function(x) {
  as.numeric(unlist(lapply(strsplit(names(x), split = ' '), '[', 1))) + 1
}

nhd_huc_data <- lapply(nhd_size, function(x) {
  if (is.null(x)) return(NA)
  ids <- correct_ids(x)
  nhd@data$FCODE <- as.numeric(as.character(nhd@data$FCODE)) # convert to numeric
  nhd@data[ids, ] %>%
    rename(FCode = FCODE) %>%
    left_join(lookup) %>%
    cbind(size = as.numeric(x))
})

nhd_huc_summary <- lapply(nhd_huc_data, function(x) {
  if (is.null(x)) return(data.frame(category = NA, subcategory = NA, permanence = NA, size = NA))
  x %>%
    filter(include == 'yes') %>%
    group_by(category, subcategory, permanence) %>%
    summarize(size = sum(size)) %>%
	as.data.frame
})

hucid <- as.character(huc@data$HUC)
huc4id <- substr(as.character(hucid[1]), 1, 4)

nhd_huc_summary <- cbind(HUC = rep(hucid, sapply(nhd_huc_summary, nrow)), do.call('rbind', nhd_huc_summary))

write.csv(nhd_huc_summary, file = paste0('/mnt/research/aquaxterra/CODE/R/nhd/csvs/', huc4id, '_', huclevel, '_', watertype, '.csv'), row.names = FALSE)
