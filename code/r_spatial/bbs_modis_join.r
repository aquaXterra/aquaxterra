# Extract environmental variables corresponding to each of the BBS points
# Author: QDR
# Project: Aquaxterra
# Date created: 19 Dec. 2016
# Last modified: 22 Dec. 2016: Match the routes and years properly

library(raster)
library(rgdal)

fp <- '/mnt/research/aquaxterra/DATA/reprojected_data'

bbs_aea_coords <- read.csv('/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_aea_coords.csv')
bbs_aea_coords_byroute <- read.csv('/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_aea_coords_byroute.csv')

bbs_div <- read.csv(file = '/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_div_reduced2015.csv')

# Get coordinates for each stop without repeating years.
library(dplyr)
bbs_aea_coords_bylocation <- cbind(bbs_div[,1:3], bbs_aea_coords) %>% group_by(rteNo, Stop) %>% summarize(x_aea = x_aea[1], y_aea = y_aea[1])
extract_coords <- bbs_aea_coords_bylocation[,c('x_aea','y_aea')]

lainames <- dir(file.path(fp, 'MODIS/MOD15A2_LAI_FPAR'), pattern = '*.tif$')
laiextract <- list()
#laiextract_byroute <- list()

for (i in lainames) {
	lai_i <- raster(file.path(fp, 'MODIS/MOD15A2_LAI_FPAR', i))
	laiextract[[length(laiextract) + 1]] <- extract(lai_i, extract_coords)
	#laiextract_byroute[[length(laiextract_byroute) + 1]] <- extract(lai_i, bbs_aea_coords_byroute)
	print(i)
}

laiextract <- as.data.frame(do.call('cbind', laiextract))
#laiextract_byroute <- as.data.frame(do.call('cbind', laiextract_byroute))
names(laiextract) <- substr(lainames, 1, nchar(lainames)-4)
#names(laiextract_byroute) <- substr(lainames, 1, nchar(lainames)-4)

nppnames <- dir(file.path(fp, 'MODIS/MOD17A3_gpp_npp'), pattern = '*.tif$')
nppextract <- list()
#nppextract_byroute <- list()

for (i in nppnames) {
	npp_i <- raster(file.path(fp, 'MODIS/MOD17A3_gpp_npp', i))
	nppextract[[length(nppextract) + 1]] <- extract(npp_i, extract_coords)
	#nppextract_byroute[[length(nppextract_byroute) + 1]] <- extract(npp_i, bbs_aea_coords_byroute)
	print(i)
}

nppextract <- as.data.frame(do.call('cbind', nppextract))
#nppextract_byroute <- as.data.frame(do.call('cbind', nppextract_byroute))
names(nppextract) <- substr(nppnames, 1, nchar(nppnames)-4)
#names(nppextract_byroute) <- substr(nppnames, 1, nchar(nppnames)-4)

# Combine lai and npp by year
lai_npp <- cbind(as.data.frame(bbs_aea_coords_bylocation), laiextract, nppextract)

library(reshape2)
lai_npp_long <- melt(lai_npp[,-(3:4)], id.vars = 1:2)
lai_npp_long <- mutate(lai_npp_long, variable = as.character(variable), year = substr(variable, nchar(variable)-3, nchar(variable))) %>%
	mutate(variable = substr(variable, 1, nchar(variable)-5))

write.csv(lai_npp_long, '/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_lai_npp.csv', row.names = FALSE)
	
lc2001 <- raster(file.path(fp, 'MODIS/MCD12Q1_2001_landcover/MosaicRP_2001-01-01.Land_Cover_Type_1.tif'))
lc2011 <- raster(file.path(fp, 'MODIS/MCD12Q1_2011_landcover/MosaicRP_2011-01-01.Land_Cover_Type_1.tif'))

lc2001extract <- extract(lc2001, extract_coords)
#lc2001extract_byroute <- extract(lc2001, bbs_aea_coords_byroute)
lc2011extract <- extract(lc2011, extract_coords)
#lc2011extract_byroute <- extract(lc2011, bbs_aea_coords_byroute)

landcoverdf <- data.frame(bbs_aea_coords_bylocation[,1:2], variable = 'MosaicRP_Land_Cover_Type_1', value = c(lc2001extract, lc2011extract), year = rep(c(2001,2011), each=length(lc2001extract)))
lai_npp_long <- mutate(lai_npp_long, year=as.numeric(year))

bbs_modis <- rbind(lai_npp_long, landcoverdf)

write.csv(bbs_modis, '/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_modis.csv', row.names = FALSE)
#write.csv(bbs_modis_byroute, '/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_modis_byroute.csv', row.names = FALSE)

# Also extract the nlcd and prism

prismnames <- dir(file.path(fp, 'PRISM_bioclim/4km'), pattern = '*.tif$')
prismextract <- list()
#prismextract_byroute <- list()

# Split up into 5 tasks
# task <- 1
# x <- round(seq(0,209,length.out=6))
# firstrow <- x[task] + 1
# lastrow <- x[task + 1]

for (i in prismnames) {
	prism_i <- raster(file.path(fp, 'PRISM_bioclim/4km', i))
	prismextract[[length(prismextract) + 1]] <- extract(prism_i, extract_coords)
	#prismextract_byroute[[length(prismextract_byroute) + 1]] <- extract(prism_i, bbs_aea_coords_byroute)
	print(i)
}

prismextract <- as.data.frame(do.call('cbind', prismextract))
#prismextract_byroute <- as.data.frame(do.call('cbind', prismextract_byroute))
names(prismextract) <- substr(prismnames, 1, nchar(prismnames)-4)
#names(prismextract_byroute) <- substr(prismnames[firstrow:lastrow], 1, nchar(prismnames[firstrow:lastrow])-4)

prism_long <- cbind(data.frame(bbs_aea_coords_bylocation), prismextract)
prism_long <- melt(prism_long[,-(3:4)], id.vars = 1:2)
prism_long <- mutate(prism_long, variable = as.character(variable), year = substr(variable, nchar(variable)-9, nchar(variable)-6)) %>%
	mutate(variable = substr(variable, nchar(variable)-4, nchar(variable)))


# write.csv(prism_long, paste0('/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_prism',task,'.csv'), row.names = FALSE)
#write.csv(prismextract_byroute, paste0('/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_prism_byroute',task,'.csv'), row.names = FALSE)

nlcdnames <- dir(file.path(fp, 'NLCD'), pattern = '*.tif$')
nlcdextract <- list()
#nlcdextract_byroute <- list()

for (i in nlcdnames) {
	nlcd_i <- raster(file.path(fp, 'NLCD', i))
	nlcdextract[[length(nlcdextract) + 1]] <- extract(nlcd_i, extract_coords)
	#nlcdextract_byroute[[length(nlcdextract_byroute) + 1]] <- extract(nlcd_i, bbs_aea_coords_byroute)
	print(i)
}

nlcdextract <- as.data.frame(do.call('cbind', nlcdextract))
#nlcdextract_byroute <- as.data.frame(do.call('cbind', nlcdextract_byroute))
names(nlcdextract) <- substr(nlcdnames, 1, nchar(nlcdnames)-4)
#names(nlcdextract_byroute) <- substr(nlcdnames, 1, nchar(nlcdnames)-4)

nlcdextract <- data.frame(bbs_aea_coords_bylocation[,1:2], variable = 'NLCD_Land_Cover', value = c(nlcdextract$nlcd_2001, nlcdextract$nlcd_2006, nlcdextract$nlcd_2011), year = rep(c(2001,2006,2011), each=nrow(nlcdextract)))

write.csv(nlcdextract, '/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_nlcd.csv', row.names = FALSE)
#write.csv(nlcdextract_byroute, '/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_nlcd_byroute.csv', row.names = FALSE)

###############
# Read all and combine

# bbs_modis <- read.csv('/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_modis.csv')
# bbs_modis_byroute <- read.csv('/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_modis_byroute.csv')

# prism_dfs <- list()
# prism_dfs_byroute <- list()

# for (i in 1:5) {
	# prism_dfs[[i]] <- read.csv(paste0('/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_prism',i,'.csv'))
	# prism_dfs_byroute[[i]] <- read.csv(paste0('/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_prism_byroute',i,'.csv'))
# }

# bbs_nlcd <- read.csv('/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_nlcd.csv')
# bbs_nlcd_byroute <- read.csv('/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_nlcd_byroute.csv')

# bbs_allcovariates <- cbind(bbs_modis, do.call('cbind',prism_dfs), bbs_nlcd)
# bbs_allcovariates_byroute <- cbind(bbs_modis_byroute, do.call('cbind',prism_dfs_byroute), bbs_nlcd_byroute)

bbs_allcovariates <- rbind(bbs_modis, nlcdextract, prism_long)
bbs_allcovariates_byroute <- bbs_allcovariates %>% select(-Stop) %>% group_by(year, rteNo, variable) %>% summarize_all(.funs=mean, na.rm=TRUE) %>% select(rteNo, variable, value, year)

write.csv(bbs_allcovariates, '/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_allcovariates.csv', row.names = FALSE)
write.csv(bbs_allcovariates_byroute, '/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_allcovariates_byroute.csv', row.names = FALSE)