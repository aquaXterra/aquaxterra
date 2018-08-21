# Combine all NHD CSVs from individual HUC8 and HUC12 into single CSV for each HUC and waterbody type.
# QDR/aquaXterra/21 Aug 2018

library(purrr)
library(dplyr)

fpin <- '/mnt/research/aquaxterra/CODE/R/nhd/csvs'
fpout <- '/mnt/research/aquaxterra/DATA/reprojected_data/NHD'

options(scipen = 999) # Prevents HUC codes from writing as scientific notation.

for (i in c('huc8', 'huc12')) {
	for (j in c('lake', 'river')) {
		# List file names and read them into one data frame
		filenames <- list.files(fpin, pattern = paste(i, j, sep = '_'), full.names = TRUE)
		dat <- map_dfr(filenames, read.csv, stringsAsFactors = FALSE)
		# Format the data frame including adding leading zeroes to HUC identifier
		names(dat)[1] <- toupper(i)
		names(dat)[5] <- ifelse(j == 'lake', 'area', 'length')
		if (i == 'huc8') dat <- dat %>% mutate(HUC8 = as.character(HUC8),
											   HUC8 = if_else(nchar(HUC8) == 8, HUC8, paste0('0', HUC8)))
		if (i == 'huc12') dat <- dat %>% mutate(HUC12 = as.character(HUC12),
											    HUC12 = if_else(nchar(HUC12) == 12, HUC12, paste0('0', HUC12)))
		# Write the CSVs
		outputfilename <- paste0(toupper(i), '_', ifelse(j == 'lake', 'lake_areas', 'river_lengths'), '.csv')
		write.csv(dat, file.path(fpout, outputfilename), row.names = FALSE)
	}
}
