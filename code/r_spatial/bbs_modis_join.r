# Extract environmental variables corresponding to each of the BBS points
# Author: QDR
# Project: Aquaxterra
# Date created: 19 Dec. 2016

library(raster)
library(rgdal)

fp <- '/mnt/research/aquaxterra/DATA/reprojected_data'

bbs_aea_coords <- read.csv('/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_aea_coords.csv')

modisnames <- dir(file.path(fp, 'MODIS'))
modisextract <- list()

for (i in modisnames) {
	modis_i <- raster(file.path(fp, i))
	modisextract[[length(modisextract) + 1]] <- extract(modis_i, bbs_aea_coords)
}