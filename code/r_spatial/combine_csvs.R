### Combine HUC data
# Takes the HUC4-level raster summary files and combines them.
# This also joins those files with the master dataset.

# Written by AC Smith on October 8, 2018
# Lasted edited by AC Smith, October 8, 2018


# load packages -----------------------------------------------------------

library(purrr)
library(dplyr)
library(data.table)

# get environmental variables from job script -----------------------------

fp <- '/mnt/research/aquaxterra/DATA/processed_data/baseflow/'

# list files --------------------------------------------------------------

# grab all filenames first
all_files <- list.files(fp, pattern = 'baseflow.csv$', full.names = TRUE)

# function to combine csvs ------------------------------------------------

combine_csvs <- function(file_list, out_path) {
  data <- map_dfr(file_list, read.csv, header = TRUE)
  write.csv(data, out_path)
  
  return(data)
}

# combine and join combined data with summary data ------------------------------------

for (huc in c('huc4', 'huc8', 'huc12')) {
  # get files for huc level
  files <- all_files[grep(huc, all_files)]
  data <- data.table(combine_csvs(file_list = files, out_path = paste('/mnt/research/aquaxterra/DATA/processed_data/baseflow/', huc, '_summaries.csv', sep = '')))
  names(data)[1] <- toupper(huc)
  summary <- as.data.frame(fread(paste('/mnt/research/aquaxterra/DATA/HUCed_data/raster/', toupper(huc), 'summarized.csv', sep = ''), header = TRUE))
  summary[, grep('HUC', names(summary))] <- as.numeric(summary[, grep('HUC', names(summary))])
  new <- data.table(summary) %>% left_join(data, by = toupper(huc))
  write.csv(new, paste('/mnt/research/aquaxterra/DATA/HUCed_data/raster/', toupper(huc), 'summarized_100818.csv', sep = ''))
  print(paste('finished adding data for', huc))
}
