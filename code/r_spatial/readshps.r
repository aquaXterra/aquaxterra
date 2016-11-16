# Read SHPs and get info on area
# Author: QDR
# Project: Aquaxterra
# Last modified: 16 Nov 2016

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

# Added 16 Nov
# Try to get rid of some of the big lakes.
huc12big <- subset(huc12, AreaSqKm > 2500)

# HUType already notes which of the HUCs are water. Metadata is found here: https://pubs.usgs.gov/tm/11/a3/pdf/tm11-a3.pdf
huc12water <- subset(huc12, HUType == 'W') # Water, includes lakes and bays. All should be removed.
huc12closedbasin <- subset(huc12, HUType == 'C') # Closed basin, includes areas of the Great Basin but also includes Great Salt Lake. Not all should be removed.
huc12namedlakes <- subset(huc12, grepl('Lake|lake', huc12@data$Name)) # Doesn't work because many of these are land areas with Lake in the name, such as frontage of lakes
huc12endorheiclakes <- subset(huc12, grepl('Lake|lake', huc12@data$Name) & HUType=='C') # Most of these are lakes, including things like Great Salt Lake and Crater Lake. 
# It looks like all the lake ones that also contain land area have a hyphen in the name, so we can get rid of all the ones with hyphens

huc12_toremove <- huc12@data$HUType == 'W' | (grepl('Lake|lake', huc12@data$Name) & huc12@data$HUType=='C' & !grepl('-', huc12@data$Name))

huc12nolakes <- subset(huc12, !huc12_toremove)
