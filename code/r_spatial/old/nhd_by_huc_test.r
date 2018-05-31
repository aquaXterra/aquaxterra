# Look at shape files of USGS NHD to see about how to work with them

fp <- '/mnt/research/aquaxterra/DATA/raw_data/NHD/usgs_nhd'

example_dir <- file.path(fp, 'NHD_H_0101_Shape/Shape')

library(rgdal)
library(rgeos)

wb0101 <- readOGR(dsn = example_dir, layer = 'NHDWaterbody')
fl0101 <- readOGR(dsn = example_dir, layer = 'NHDFlowline')

# Correct row IDs
row.names(wb0101@data) <- 1:nrow(wb0101@data)
row.names(fl0101@data) <- 1:nrow(fl0101@data)

h8 <- readOGR(dsn = example_dir, layer = 'WBDHU8')
h12 <- readOGR(dsn = example_dir, layer = 'WBDHU12')

# See https://philmikejones.wordpress.com/2015/09/01/clipping-polygons-in-r/


flowlinesbyhuc8 <- gIntersection(fl0101, h8, byid = TRUE, drop_lower_td = TRUE) # takes a long time but does not seem to use much ram
waterbodybyhuc8 <- gIntersection(wb0101, h8, byid = TRUE, drop_lower_td = TRUE) # takes a long time but does not seem to use much ram

# Try to do it for a smaller example so it will run faster.
example_h12 <- h12[1, ]
wb_h12_1 <- gIntersection(wb0101, example_h12, byid = TRUE, drop_lower_td = TRUE)
fl_h12_1 <- gIntersection(fl0101, example_h12, byid = TRUE, drop_lower_td = TRUE)

# Plot to see if it worked correctly
plot(wb_h12_1)
plot(fl_h12_1)

# Reconstruct data associated with each one.


# Do as a loop.
fl_h8_all <- list()
wb_h8_all <- list()

for (i in 1:length(h8)) {
  print(i)
  fl_h8_all[[i]] <- gIntersection(fl0101, h8[i,], byid = TRUE, drop_lower_td = TRUE)
  wb_h8_all[[i]] <- gIntersection(wb0101, h8[i,], byid = TRUE, drop_lower_td = TRUE)
}

fl_n <- sapply(fl_h8_all, length)
wb_n <- sapply(wb_h8_all, length)
# Almost all flowlines and waterbodies are in one huc. However some are not.



# Calculate lengths and areas of flowlines and waterbodies, respectively

# First reproject everything into an equal area projection.
aea_crs <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0'
fl0101_aea <- spTransform(fl0101, aea_crs)
wb0101_aea <- spTransform(wb0101, aea_crs)

fl_h8_aea <- lapply(fl_h8_all, spTransform, CRSobj = aea_crs)
wb_h8_aea <- lapply(wb_h8_all, spTransform, CRSobj = aea_crs)

# All
fl_length_all <- gLength(fl0101_aea, byid = TRUE)
wb_area_all <- gArea(wb0101_aea, byid = TRUE)

# By huc8
fl_h8_length <- lapply(fl_h8_aea, gLength, byid = TRUE)
wb_h8_area <- lapply(wb_h8_aea, gArea, byid = TRUE)

# compare sums
sum(fl_length_all)
sum(sapply(fl_h8_length, sum)) # Almost the same (some errors introduced but probably OK)

sum(wb_area_all)
sum(sapply(wb_h8_area, sum))

# Flag each of the lakes and rivers as natural or manmade.
# Load lookup tables:
fl_lookup <- read.csv('/mnt/research/aquaxterra/CODE/R/nhd/nhd_flowline_lookup.csv', stringsAsFactors = FALSE)
wb_lookup <- read.csv('/mnt/research/aquaxterra/CODE/R/nhd/nhd_waterbody_lookup.csv', stringsAsFactors = FALSE)

# Replace the pasted IDs with correct IDs in each list element.
correct_ids <- function(x) {
  as.numeric(unlist(lapply(strsplit(names(x), split = ' '), '[', 1)))
}

library(dplyr)

wb_h8_data <- lapply(wb_h8_area, function(x) {
  ids <- correct_ids(x)
  wb0101@data[ids, ] %>%
    rename(FCode = FCODE) %>%
    left_join(wb_lookup) %>%
    cbind(area = as.numeric(x))
})

wb_h8_summary <- lapply(wb_h8_data, function(x) {
  x %>%
    filter(include == 'yes') %>%
    group_by(category, subcategory, permanence) %>%
    summarize(area = sum(area))
})