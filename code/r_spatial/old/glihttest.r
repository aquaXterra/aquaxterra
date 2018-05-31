# Check whether gliht raster overlaps with any BBS points.
# Use Acadia raster as a test.

fp <- '/mnt/research/aquaxterra/DATA/raw_data/BBS/gliht_test'
library(raster)
acadia <- raster(dir(fp)[1])

# Load bbs stops and convert projection.
library(rgdal)
bbsstops <- readOGR(dsn = '/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_stops', layer = 'bbsStopsPRISMproj')

# Reproject gliht to longlat. Make into a bounding box.
acadia_poly <- as(acadia, 'SpatialPolygons')
acadia_poly <- rasterToPolygons(acadia)
acadia_box <- as(extent(acadia),'SpatialPolygons')
proj4string(acadia_box) <- acadia@crs
acadia_box <- spTransform(acadia_box, bbsstops@proj4string)

# Check overlap of the two. How many bbs stops are contained in the Acadia bounding box?
overlap_test <- over(bbsstops, acadia_box)
# None overlap. Probably bad example.

#############################

# Edit 30 Jan.
# Load all the GLIHT footprints.

fp <- '/mnt/research/aquaxterra/DATA/raw_data'
library(raster)
library(rgdal)

bbsstops <- readOGR(dsn = file.path(fp,'BBS/bbs_stops'), layer = 'bbsStopsPRISMproj')
gliht <- readOGR(dsn = file.path(fp,'G-LiHT'), layer = 'las_shp')

# Attempt to calculate overlap (hopefully this function does not take a long time to run)
# The two do not have the same datum.
bbsstops <- spTransform(bbsstops, CRSobj = gliht@proj4string)
bbs_gliht_overlay <- over(bbsstops, gliht)