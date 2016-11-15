# Read SHPs and get info on area
# Author: QDR
# Project: Aquaxterra
# Last modified: 02 Nov 2016

library(rgdal)
setwd('/mnt/research/aquaxterra/DATA/reprojected_data')

huc12 <- readOGR(dsn = 'BBS_SpatialJoin', layer = 'HUC12')
bbsbuffers <- readOGR(dsn = 'BBS_SpatialJoin', layer = 'bbs_buffers_Albers')
iws <- readOGR(dsn = 'BBS_SpatialJoin', layer= 'IWS')

# Calculate area of the polygons in huc12

library(raster)
crs(huc12)
hucarea <- area(huc12)
# Output in a simple format
hucarea <- data.frame(Name = huc12$Name, HUC12 = huc12$HUC12, area = hucarea)

# Turns out it already contains an area. Just output it.

hucout <- huc12@data[, c('AreaAcres','AreaSqKm','States','HUC12','Name')]

# Extract the huc4 and huc8 info from the huc12 column

hucnames <- as.character(huc12@data$HUC12)
huc4names <- as.numeric(substr(hucnames, 1, 4))
huc8names <- as.numeric(substr(hucnames, 1, 8))

hucout$HUC4 <- huc4names
hucout$HUC8 <- huc8names

# Sum the areas of each huc

library(dplyr)
huc4area <- hucout %>% group_by(HUC4) %>% summarize(AreaSqKm = sum(AreaSqKm))
huc8area <- hucout %>% group_by(HUC8) %>% summarize(AreaSqKm = sum(AreaSqKm))


write.csv(hucout, file = '~/data/aquaxterra/huc12out.csv', row.names = FALSE)
write.csv(huc4area, file = '~/data/aquaxterra/huc4area.csv', row.names = FALSE)
write.csv(huc8area, file = '~/data/aquaxterra/huc8area.csv', row.names = FALSE)