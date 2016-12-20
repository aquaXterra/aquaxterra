# Project BBS data to Albers equal area projection (same as the one the modis data is in)
# Author: QDR
# Project: Aquaxterra
# Date created: 19 Dec. 2016

fp <- '/mnt/research/aquaxterra'
bbs_div <- read.csv(file = file.path(fp, 'DATA/raw_data/BBS/bbs_div_reduced2015.csv'))

aea_crs <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0'
latlong_crs <- '+proj=longlat +ellps=WGS72 +no_defs'

library(rgdal)

# Must get rid of NA values in coords to do transform
valid_coord <- !is.na(bbs_div$POINT_X)

bbs_latlong <- SpatialPoints(coords = with(bbs_div[valid_coord,], data.frame(x = POINT_X, y = POINT_Y)), proj4string = CRS(latlong_crs))
bbs_aea <- spTransform(bbs_latlong, CRSobj = CRS(aea_crs))

new_coords <- matrix(NA, nrow=nrow(bbs_div), ncol=2)
new_coords[valid_coord,] <- bbs_aea@coords

bbs_aea_coords <- data.frame(x_aea = new_coords[,1], y_aea = new_coords[,2])
write.csv(bbs_aea_coords, file = file.path(fp, 'DATA/raw_data/BBS/bbs_aea_coords.csv'))

# Also do this by route location.
bbs_div_bystop <- 