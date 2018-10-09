# Combine all NHD CSVs from individual HUC8 and HUC12 into single CSV for each HUC and waterbody type.
# QDR/aquaXterra/21 Aug 2018

library(purrr)
library(dplyr)
library(data.table)

fpin <- '/mnt/research/aquaxterra/CODE/R/nhd/csvs'
fpout <- '/mnt/research/aquaxterra/DATA/albers_projected_data/NHD'

options(scipen = 999) # Prevents HUC codes from writing as scientific notation.

for (i in c('huc8', 'huc12')) {
	for (j in c('lake', 'river')) {
		# List file names and read them into one data frame
		filenames <- list.files(fpin, pattern = paste(i, j, sep = '_'), full.names = TRUE)
		# sort out mean sizes vs. total sizes
		filenames_totals <- filenames[grep(paste(j, '.csv', sep = ''), filenames)]
		filenames_means <- filenames[grep(paste(j, 'means.csv', sep = ''), filenames)]
		dat_totals <- map_dfr(filenames_totals, read.csv, stringsAsFactors = FALSE)
		dat_means <- map_dfr(filenames_means, read.csv, stringsAsFactors = FALSE)
		# Format the data frame including adding leading zeroes to HUC identifier
		names(dat_totals)[1] <- toupper(i)
		names(dat_means)[1] <- toupper(i)
		names(dat_totals)[5] <- ifelse(j == 'lake', 'area', 'length') # change size to area or length
		names(dat_means) <- gsub('size', ifelse(j == 'lake', 'area', 'length'), names(dat_means))
		if (i == 'huc8') {
		  dat_totals <- dat_totals %>% mutate(HUC8 = as.character(HUC8),
											 HUC8 = if_else(nchar(HUC8) == 8, HUC8, paste0('0', HUC8)))
		  dat_means <- dat_means %>% mutate(HUC8 = as.character(HUC8),
		                  HUC8 = if_else(nchar(HUC8) == 8, HUC8, paste0('0', HUC8)))
		}
		if (i == 'huc12') {
		  dat_totals <- dat_totals %>% mutate(HUC12 = as.character(HUC12),
											HUC12 = if_else(nchar(HUC12) == 12, HUC12, paste0('0', HUC12)))
		  dat_means <- dat_means %>% mutate(HUC12 = as.character(HUC12),
		                  HUC12 = if_else(nchar(HUC12) == 12, HUC12, paste0('0', HUC12)))
		}
		# Write the CSVs
		outputfilename_tot <- paste0(toupper(i), '_', ifelse(j == 'lake', 'lake_areas', 'river_lengths'), '.csv')
		outputfilename_mean <- paste0(toupper(i), '_', ifelse(j == 'lake', 'lake_areas', 'river_lengths'), '_means.csv')
		
		#write.csv(dat_totals, file.path(fpout, outputfilename_tot), row.names = FALSE)
		write.csv(dat_means, file.path(fpout, outputfilename_mean), row.names = FALSE)
	}
}
