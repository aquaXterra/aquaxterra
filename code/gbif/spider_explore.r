# GBIF arachnids data visualization
# All Arachnida data pulled 10 May 2017

fp <- '/mnt/research/aquaxterra/DATA/raw_data/insects'
ara <- read.delim(file.path(fp, 'arachnida.csv'), quote = '', stringsAsFactors=FALSE) # Need to disable quotes or the file is cut off before the last line.

library(dplyr)

# Occurrences of spiders only
# Occurrences in aquaxterra region only. Use lat long bounding box for now.
# Occurrences by species

usaspiders <- ara %>%
	filter(order == 'Araneae') %>%
	filter(decimallatitude >= 25 & decimallatitude < 50 & decimallongitude >= -125 & decimallongitude <= -67)
	
spidertable <- usaspiders %>%
	group_by(scientificname) %>%
	summarize(no_occurrences = n())
	
spider_reduced <- usaspiders %>% 
	left_join(spidertable) %>%
	filter(no_occurrences >= 10)
	
spider_noloc <- usaspiders <- ara %>%
	filter(order == 'Araneae') %>%
	filter(countrycode == 'US' & (is.na(decimallatitude) | is.na(decimallongitude)))
# There are about 25K spider records for the USA that have locality but no lat and long. We can get approximate lat and long for these.	