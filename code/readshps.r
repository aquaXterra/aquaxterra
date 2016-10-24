library(rgdal)
setwd('/mnt/research/aquaxterra/DATA/reprojected_data')

huc12 <- readOGR(dsn = 'BBS_SpatialJoin', layer = 'HUC12')
bbsbuffers <- readOGR(dsn = 'BBS_SpatialJoin', layer = 'bbs_buffers_Albers')
iws <- readOGR(dsn = 'BBS_SpatialJoin', layer= 'IWS')