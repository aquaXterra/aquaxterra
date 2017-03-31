library(dplyr)


# Load summary CSV files
huc4summ <- read.csv('/mnt/research/aquaxterra/CODE/python/RasterOverlay/HUC4summarized.csv', stringsAsFactors = FALSE)

# Add new climate variables (means and interannual CVs)
mns <- list()
cvs <- list()
for (i in 1:19) {
	n <- ifelse(i < 10, paste0('0', i), as.character(i)) # add zero before number.
	cols <- grep(paste0('bio', n), names(huc4summ))
	mns[[i]] <- apply(huc4summ[,cols], 1, mean, na.rm=TRUE)
	x <- huc4summ[,cols]
	if (i %in% c(1,5,6,8,9,10,11)) x <- x + 273.15 # Convert any absolute temperatures to Kelvin to get a true SD.
	cvs[[i]] <- apply(x, 1, sd, na.rm=TRUE)/apply(x, 1, mean, na.rm=TRUE)
}

mns <- as.data.frame(do.call('cbind', mns))
cvs <- as.data.frame(do.call('cbind', cvs))
names(mns) <- paste0('mean_allyears_bio',1:19)
names(cvs) <- paste0('cv_allyears_bio',1:19)

huc4summ <- cbind(huc4summ, mns, cvs)

# Add new NLCD variables (summed groups, and evenness across groups)

# First, replace missing NLCD with zeroes.
nlcdcols <- grep('nlcd', names(huc4summ))
huc4summ[,nlcdcols][is.na(huc4summ[,nlcdcols])] <- 0

huc4summ <- huc4summ %>% mutate(nlcd_forest = nlcd2011_43_perc + nlcd2011_41_perc + nlcd2011_42_perc,
							  nlcd_agriculture = nlcd2011_81_perc + nlcd2011_82_perc,
							  nlcd_developed = nlcd2011_21_perc + nlcd2011_22_perc + nlcd2011_23_perc + nlcd2011_24_perc,
							  nlcd_wetland = nlcd2011_90_perc + nlcd2011_95_perc,
							  nlcd_grassland = nlcd2011_71_perc,
							  nlcd_shrubland = nlcd2011_31_perc,
							  nlcd_ice = nlcd2011_12_perc,
							  nlcd_barren = nlcd2011_31_perc,
							  nlcd_water = nlcd2011_11_perc,
							  nlcd_diversity = vegan::diversity(cbind(nlcd_forest, nlcd_agriculture, nlcd_developed, nlcd_wetland, nlcd_grassland, nlcd_shrubland, nlcd_ice, nlcd_barren, nlcd_water)))							  
### HUC8 ###

huc8summ <- read.csv('/mnt/research/aquaxterra/CODE/python/RasterOverlay/HUC8summarized.csv', stringsAsFactors = FALSE)

# Add more derived variables (30 March)
mns <- list()
cvs <- list()
for (i in 1:19) {
	n <- ifelse(i < 10, paste0('0', i), as.character(i)) # add zero before number.
	cols <- grep(paste0('bio', n), names(huc8summ))
	mns[[i]] <- apply(huc8summ[,cols], 1, mean, na.rm=TRUE)
	x <- huc8summ[,cols]
	if (i %in% c(1,5,6,8,9,10,11)) x <- x + 273.15 # Convert any absolute temperatures to Kelvin to get a true SD.
	cvs[[i]] <- apply(x, 1, sd, na.rm=TRUE)/apply(x, 1, mean, na.rm=TRUE)
}

mns <- as.data.frame(do.call('cbind', mns))
cvs <- as.data.frame(do.call('cbind', cvs))
names(mns) <- paste0('mean_allyears_bio',1:19)
names(cvs) <- paste0('cv_allyears_bio',1:19)

huc8summ <- cbind(huc8summ, mns, cvs)

# Add new NLCD variables (summed groups, and evenness across groups)

# First, replace missing NLCD with zeroes.
nlcdcols <- grep('nlcd', names(huc8summ))
huc8summ[,nlcdcols][is.na(huc8summ[,nlcdcols])] <- 0

huc8summ <- huc8summ %>% mutate(nlcd_forest = nlcd2011_43_perc + nlcd2011_41_perc + nlcd2011_42_perc,
							  nlcd_agriculture = nlcd2011_81_perc + nlcd2011_82_perc,
							  nlcd_developed = nlcd2011_21_perc + nlcd2011_22_perc + nlcd2011_23_perc + nlcd2011_24_perc,
							  nlcd_wetland = nlcd2011_90_perc + nlcd2011_95_perc,
							  nlcd_grassland = nlcd2011_71_perc,
							  nlcd_shrubland = nlcd2011_31_perc,
							  nlcd_ice = nlcd2011_12_perc,
							  nlcd_barren = nlcd2011_31_perc,
							  nlcd_water = nlcd2011_11_perc,
							  nlcd_diversity = vegan::diversity(cbind(nlcd_forest, nlcd_agriculture, nlcd_developed, nlcd_wetland, nlcd_grassland, nlcd_shrubland, nlcd_ice, nlcd_barren, nlcd_water)))							  