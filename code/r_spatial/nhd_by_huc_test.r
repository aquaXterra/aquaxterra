# Look at shape files of USGS NHD to see about how to work with them

fp <- '/mnt/research/aquaxterra/DATA/raw_data/NHD/usgs_nhd'

example_dir <- file.path(fp, 'NHD_H_0101_Shape/Shape')

library(rgdal)
library(rgeos)

wb0101 <- readOGR(dsn = example_dir, layer = 'NHDWaterbody')
fl0101 <- readOGR(dsn = example_dir, layer = 'NHDFlowline')

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

for (i in 1:length(h8)) {
  print(i)
  fl_h8_all[[i]] <- gIntersection(fl0101, h8[i,], byid = TRUE, drop_lower_td = TRUE)
}

save(fl_h8_all, file = '/mnt/research/aquaxterra/DATA/raw_data/NHD/testintersection.RData')
