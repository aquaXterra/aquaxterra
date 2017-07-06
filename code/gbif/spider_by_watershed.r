# Compile spider data from GBIF and try to break it up by watershed. (huc4)
# Include the following covariates: plant diversity, bird diversity, aquatic insect diversity, any climate covariates that we have.

# Export of huc data, in both spatial and tabular format, for watershed-level covariates.

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

cols_to_plot <- c('mean_altitude','std_altitude','mean_allyears_npp','mean_allyears_gpp','mean_allyears_lai','mean_allyears_fpar','mean_allyears_bio1','mean_allyears_bio4','cv_allyears_bio1','mean_allyears_bio12','mean_allyears_bio15','cv_allyears_bio12','nlcd_forest','nlcd_agriculture','nlcd_developed','nlcd_wetland','nlcd_grassland','nlcd_shrubland','nlcd_ice','nlcd_barren','nlcd_water','nlcd_diversity')

watershed_summary_data <- huc4summ[,c('HUC4', cols_to_plot)]

# For each of the spider GBIF occurrences that we have, put them into a HUC4 polygon, and into a US state or Canadian province if we can.

fp <- '/mnt/research/aquaxterra/DATA'
ara <- read.delim(file.path(fp, 'raw_data/insects/arachnida.csv'), quote = '', stringsAsFactors=FALSE) # Need to disable quotes or the file is cut off before the last line.

library(dplyr)

# Occurrences of spiders only
# Occurrences in aquaxterra region only. Use lat long bounding box for now.
# Occurrences by species

usaspiders <- ara %>%
  filter(order == 'Araneae') %>%
  filter(decimallatitude >= 25 & decimallatitude < 50 & decimallongitude >= -125 & decimallongitude <= -67)

# Load the HUC4 polygons.
library(sp)
library(maptools)
crsaea <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0'
crswgs <- '+proj=longlat +ellps=WGS84 +no_defs'
x <- readShapePoly(file.path(fp, 'reprojected_data/HUC/HU4_CONUS_Alb.shp'), proj4string = CRS(crsaea))
xp <- as(x, 'SpatialPolygons')

# sptransform coordinates of usaspiders to the Albers crs.
spider_spatial <- with(usaspiders, SpatialPoints(coords = cbind(decimallongitude,decimallatitude), proj4string = CRS(crswgs)))
spider_aea <- spTransform(spider_spatial, CRSobj = CRS(crsaea))

# For each polygon, get which spider points are in it.
ranges <- lapply(1:length(xp), function(i) !is.na(over(spider_aea, xp[i])))

# For each row, get which HUC it is in.
ranges_df <- do.call(cbind, ranges)
table(apply(ranges_df, 1, sum)) # check to see if we got unique polygon for each.
# Most are in a single polygon, some are in no polygon but that's okay.

which_huc <- unlist(apply(ranges_df, 1, function(x) {
  xw <- which(x)
  ifelse(length(xw) == 0, NA, xw)
}))

# Get the huc4 ID's
which_huc_id <- x@data$HUC4[which_huc]
usaspiders$watershed <- which_huc_id

library(dplyr)
spider_richness_by_watershed <- usaspiders %>%
  mutate(watershed = as.numeric(as.character(watershed))) %>%
  group_by(watershed) %>%
  summarize(richness = length(unique(scientificname)),
            nrecords = n(),
            nsamplingunits = length(unique(datasetkey)))
  
  
# Make a map to plot where the spiders are.
library(ggplot2)
ggplot(usaspiders, aes(x=decimallongitude, y=decimallatitude)) + geom_point() + coord_map() + borders('state')

# Get rid of the watersheds that have less than 100 records.
# Then do a rarefaction so that we can see how many "should" be in each watershed. (Does not work)

good_watersheds <- subset(spider_richness_by_watershed, nrecords >= 100)$watershed
good_watersheds <- na.omit(good_watersheds)

good_nsampling <- spider_richness_by_watershed$nsamplingunits[match(good_watersheds, spider_richness_by_watershed$watershed)]

isinwatershed <- function(x) good_watersheds %in% x$watershed

spiderinwatershed <- usaspiders %>%
  mutate(watershed = as.numeric(as.character(watershed))) %>%  
  filter(watershed %in% good_watersheds) %>%
  group_by(scientificname) %>%
  do(x = isinwatershed(.))

spider_mat <- do.call(rbind, spiderinwatershed$x)
dimnames(spider_mat) <- list(spiderinwatershed$scientificname, good_watersheds)
spider_mat <- spider_mat[rowSums(spider_mat) > 0, ] * 1

# library(iNEXT)
# 
# spider_datlist <- c(good_nsampling, as.list(data.frame(spider_mat)))
# 
# spider_corr <- iNEXT(x=spider_datlist, q=0, datatype='incidence_raw', size = c(5,10,50,100,500,1000,2000,3000,4000))
# ggiNEXT(spider_corr) + theme_bw() + scale_shape_manual(values=rep(19,22))
# 

#############################################
# Join bbs data: richness of all birds, and richness of different groups of birds.
# By route
load('/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_div_byroute_presence.r')
fgrichness <- read.csv('/mnt/research/aquaxterra/DATA/raw_data/BBS/fgrichness.csv', stringsAsFactors = FALSE)

# Subset the routes that are at least half in one HUC4, and get the 2001-2011 means
divmedians <- bbs_div_byroute %>%
  filter(nstops4 > 25, year >= 2001, year <= 2011) %>% # Use only the ones that are a majority in one huc4
  group_by(rteNo, HUC4) %>%
  summarize_at(vars(richness, FRic, FEve, FDis, PD, mpd.obs.z, mntd.obs.z), median)

# Subset the routes that are at least half in one HUC4, and get the 2001-2011 means
fgdivmedians <- bbs_div_byroute %>%
  left_join(fgrichness) %>%
  filter(nstops4 > 25, year >= 2001, year <= 2011) %>% # Use only the ones that are a majority in one huc4
  group_by(rteNo, HUC4) %>%
  summarize_at(vars(starts_with('total_richness')), median)

bird_div <- cbind(divmedians[,c('HUC4','richness')], fgdivmedians[,-(1:2)]) %>%
  group_by(HUC4) %>%
  summarize_all(.funs = 'median') %>%
  rename(total_richness_allbirds = richness)

watershed_summary_data <- left_join(watershed_summary_data, bird_div %>% mutate(HUC4 = as.numeric(HUC4)))

#############################################
# Join aquatic insect data: richness of all inverts, and richness of different taxa.

# Must load a very large CSV into memory.
inverts <- read.csv(file.path(fp, 'raw_data/insects/WQP_insect_w_ranks.csv'), stringsAsFactors = FALSE)

# Get rid of some of the rows that aren't too important
inverts <- inverts %>% select(HUCEightDigitCode, LatitudeMeasure, LongitudeMeasure, class, order, family, genus, species)

inverts$HUC4 <- as.numeric(substr(inverts$HUCEightDigitCode, 1, 4))
inverts$HUC4[nchar(inverts$HUCEightDigitCode) == 7] <- as.numeric(substr(inverts$HUC4[nchar(inverts$HUCEightDigitCode) == 7], 1, 3))
inverts$HUC4[is.na(inverts$HUCEightDigitCode)] <- NA

inverts_table <- inverts %>%
  mutate(binomial = paste(genus, species)) %>%
  group_by(HUC4, class, order) %>%
  summarize(richness = length(unique(binomial)),
            nrecords = n())

inverts_table_class <- inverts %>%
  mutate(binomial = paste(genus, species)) %>%
  group_by(HUC4, class) %>%
  summarize(richness = length(unique(binomial)),
            nrecords = n())

insect_table <- inverts %>%
  mutate(binomial = paste(genus, species)) %>%
  filter(class == 'Insecta') %>%
  group_by(HUC4, order) %>%
  summarize(richness = length(unique(binomial)),
            nrecords = n())

insect_allrec <- insect_table %>% group_by(HUC4) %>% summarize(allr = sum(nrecords)) %>% filter(allr >= 1000)
insect_table <- filter(insect_table, HUC4 %in% insect_allrec$HUC4)

library(reshape2)
insect_cast <- dcast(insect_table, HUC4 ~ order, value.var = 'richness')
insect_cast <- insect_cast[, !names(insect_cast) %in% 'NA']
insect_cast[is.na(insect_cast)] <- 0
names(insect_cast)[-1] <- paste('richness', names(insect_cast)[-1], sep = '_')
insect_cast$richness_all_aquatic_insects <- apply(insect_cast[,-1], 1, sum)

watershed_summary_data <- left_join(watershed_summary_data, insect_cast %>% mutate(HUC4 = as.numeric(HUC4)))

#####
# Join all
watershed_summary_data <- watershed_summary_data %>%
  rename(watershed = HUC4) %>%
  left_join(spider_richness_by_watershed %>%
              select(watershed, richness, nrecords) %>%
              rename(richness_spiders = richness, nsamples_spiders = nrecords) %>%
              filter(watershed %in% good_watersheds))

# Save data
write.csv(watershed_summary_data, file = file.path(fp, 'raw_data/insects/spider_summary_data.csv'), row.names = FALSE)
write.csv(spider_mat, file = file.path(fp, 'raw_data/insects/spider_species_by_watershed.csv'), row.names = TRUE)