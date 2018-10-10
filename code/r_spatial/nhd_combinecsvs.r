# Combine all NHD CSVs from individual HUC8 and HUC12 into single CSV for each HUC and waterbody type.
# QDR/aquaXterra/21 Aug 2018

library(purrr)
library(dplyr)
library(data.table)
library(tidyr)

fpin <- '/mnt/research/aquaxterra/CODE/R/nhd/csvs'
fpout <- '/mnt/research/aquaxterra/DATA/albers_projected_data/NHD'

options(scipen = 999) # Prevents HUC codes from writing as scientific notation.

for (i in c('huc8', 'huc12')) {
	for (j in c('lake', 'river')) {
		# List file names and read them into one data frame
		filenames <- list.files(fpin, pattern = paste(i, j, sep = '_'), full.names = TRUE)
		
		# get data
		dat <- map_dfr(filenames, read.csv, stringsAsFactors = FALSE)
		
		# Format the data frame including adding leading zeroes to HUC identifier
		names(dat)[1] <- toupper(i)
		names(dat)[5] <- ifelse(j == 'lake', 'area', 'length') # change size to area or length
		
		if (i == 'huc8') {
		  dat <- dat %>% mutate(HUC8 = as.character(HUC8),
											 HUC8 = if_else(nchar(HUC8) == 8, HUC8, paste0('0', HUC8)))
		}
		if (i == 'huc12') {
		  dat <- dat %>% mutate(HUC12 = as.character(HUC12),
											HUC12 = if_else(nchar(HUC12) == 12, HUC12, paste0('0', HUC12)))
		}
		# Write the CSVs
		outputfilename <- paste0(toupper(i), '_', ifelse(j == 'lake', 'lake_areas', 'river_lengths'), '.csv')

		write.csv(dat, file.path(fpout, outputfilename), row.names = FALSE)
	}
}


# combine with overall HUC summary data -----------------------------------

for (huc in c('HUC4', 'HUC8', 'HUC12')) {
  rdat <- read.csv(file.path(fpout, paste(huc, '_river_lengths.csv', sep = '')), header = TRUE)
  ldat <- read.csv(file.path(fpout, paste(huc, '_lake_areas.csv', sep = '')), header = TRUE)
  
  # change column to generic huc name for now
  names(rdat)[1] <- 'huc'
  names(ldat)[1] <- 'huc'
  
  # switch format to area by huc, with categories as columns
  ldat <- ldat %>%
    filter(!is.na(category)) %>%
    mutate(category = paste(category, subcategory, permanence, 'area', sep = '_')) %>%
    select(c(huc, category, area)) %>%
    group_by(huc, category) %>%
    summarize(area = sum(area, na.rm = TRUE)) %>%
    spread(category, area) %>%
    as.data.frame
  ldat$total_lake_area <- rowSums(ldat[, 2:ncol(ldat)], na.rm = TRUE)
  
  rdat <- rdat %>%
    mutate(category = paste(category, subcategory, permanence, 'length', sep = '_')) %>%
    select(c(huc, category, length)) %>%
    group_by(huc, category) %>%
    summarize(length = sum(length, na.rm = TRUE)) %>%
    spread(category, length) %>%
    as.data.frame
  last_col <- ncol(rdat)
  rdat$perc_eph_stream_length <- rowSums(as.data.frame(rdat[, grep('ephemeral', names(rdat))]), na.rm = TRUE) / rowSums(rdat[, 2:last_col], na.rm = TRUE)
  rdat$perc_int_stream_length <- rowSums(as.data.frame(rdat[, grep('intermittent', names(rdat))]), na.rm = TRUE) / rowSums(rdat[, 2:last_col], na.rm = TRUE)
  rdat$perc_per_stream_length <- rowSums(as.data.frame(rdat[, grep('perennial', names(rdat))]), na.rm = TRUE) / rowSums(rdat[, 2:last_col], na.rm = TRUE)
  rdat$perc_unk_stream_length <- rowSums(as.data.frame(rdat[, grep('unknown', names(rdat))]), na.rm = TRUE) / rowSums(rdat[, 2:last_col], na.rm = TRUE)
  
  # change column name back to proper huc
  names(ldat)[1] <- huc
  names(rdat)[1] <- huc
  
  # read in summary table
  summary <- read.csv(paste('/mnt/research/aquaxterra/DATA/HUCed_data/raster/', huc, 'summarized_100818.csv', sep = ''), header = TRUE)
  
  # join with primary summary table
  new <- summary %>% left_join(ldat, by = huc)
  new <- new %>% left_join(rdat, by = huc)
  
  # write out new summary version
  write.csv(new, paste('/mnt/research/aquaxterra/DATA/HUCed_data/raster/', huc, 'summarized_101018.csv', sep = ''))
  print(paste('finished adding data for', huc))
}
