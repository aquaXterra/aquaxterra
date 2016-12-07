# Read and manipulate bbs data
# Author: QDR
# Project: Aquaxterra
# Created: 02 Nov 2016
# Last modified: 07 Dec 2016

# Read species list. Fix problems.
bbsspp <- read.fwf('data/SpeciesList.txt', widths = c(7,5,50,50,50,50,50,50,50), stringsAsFactors=FALSE)
bbsnames <- bbsspp[1,]
bbsspp <- as.data.frame(bbsspp[-1,])

# The gsub removes white space from the strings.
names(bbsspp) <- gsub('^\\s+|\\s+$', '', bbsnames)

ii <- sapply(bbsspp, is.character)
bbsspp[ii] <- lapply(bbsspp[ii], function(x) gsub('^\\s+|\\s+$', '', x))
bbsspp$Seq <- as.numeric(bbsspp$Seq)
bbsspp$AOU <- as.numeric(bbsspp$AOU)
names(bbsspp)[5] <- 'Latin_Name'

# Export CSV file
write.csv(bbsspp, file = 'data/specieslist.csv', row.names = FALSE)

# Get correct species list from the AOU and write to a better species list.
library(XLConnect)
aouspp <- readWorksheetFromFile('C:/Users/Q/Dropbox/projects/aquaxterra/splist_aou.xlsx', sheet='allnames')
# Remove invalid names and add correct names
goodnames <- aouspp$allnames[!aouspp$allnames %in% aouspp$Invalid.name]
goodnames <- c(goodnames, aouspp$Correct.name[!is.na(aouspp$Correct.name)])
write.table(goodnames, file = 'data/correctspecieslist.csv', row.names = FALSE)

# 04 Nov: Yet another try to get the correct species list from the phylogeny. Match with the English names because the Latin ones don't necessarily match.
bbsspp <- read.csv('data/specieslist.csv', stringsAsFactors = FALSE)
birdtreespp <- read.csv('C:/Users/Q/Dropbox/projects/aquaxterra/BLIOCPhyloMasterTax.csv', stringsAsFactors = FALSE)

# Check how many of the English common names in bbs list are in the phylogenetic list
# Correct for cases
bbsmatch <- tolower(bbsspp$English_Common_Name) %in% tolower(birdtreespp$English)
badmatches <- bbsspp[!bbsmatch, c('English_Common_Name','Latin_Name')]
write.csv(badmatches, file = 'C:/Users/Q/Dropbox/projects/aquaxterra/badmatchsppnames.csv', row.names=F)

# Load corrected match names
goodmatches <- read.csv(file = 'C:/Users/Q/Dropbox/projects/aquaxterra/correctedmatchsppnames.csv', stringsAsFactors = FALSE)

finalnamelist <- bbsspp$English_Common_Name
finalnamelist[!bbsmatch] <- NA
finalnamelist[!bbsmatch] <- goodmatches$Correct_english

bbsmatch2 <- tolower(finalnamelist) %in% tolower(birdtreespp$English)
finalscinames <- birdtreespp$Scientific[tolower(birdtreespp$English) %in% tolower(finalnamelist)]

write.csv(finalscinames, file = 'data/phylospecieslist.csv', row.names=FALSE)
# Match AOUs with the final scientific names
finalenglish <- birdtreespp$English[tolower(birdtreespp$English) %in% tolower(finalnamelist)]

bbsspp$finalenglishname <- finalnamelist



# Load Phoebe's bbs data (stored locally on Q's machine, but is also available on HPCC)
#load('C:/Users/Q/Dropbox/projects/aquaxterra/bbs6712.RData')
load('DATA/raw_data/BBS/bbs_counts/bbs6712.RData')

# Do some very basic tallying of richness

library(dplyr)
library(reshape2)

#bbsdf <- left_join(bbs6712 %>% select(-remove), bbsspp[,2:3])
bbsdf <- bbs6712 %>% select(-remove)
rm(bbs6712)

# Convert to long format
bbslong <- melt(bbsdf, id.vars = grep('Stop', names(bbsdf), invert=TRUE), variable.name = 'Stop', value.name = 'n')
bbslong <- bbslong %>% filter(n>0) # Lots of zeroes, get rid.

# Get richness by stop and year
bbsrichness <- bbslong %>% group_by(year, rteNo, Stop) %>% summarize(abund = sum(n), richness = sum(n>0)) # Takes a while.

# Get richness by route and year
bbsrichness_byroute <- bbslong %>% group_by(year, rteNo) %>% summarize(abund = sum(n), richness = length(unique(AOU))) # Takes a while.

# Get richness, evenness, diversity.
library(vegan)
library(vegetarian)

# Convert to a species by site matrix (site is a route by stop by year combination)
sppids <- sort(unique(bbsdf$AOU))
get_spp <- function(dat, sppids) {
  ns <- dat$n
  idx <- match(dat$AOU, sppids)
  res <- rep(0, length(sppids))
  res[idx] <- ns
  #as.data.frame(t(res))
  res
}
bbsmat <- bbslong %>% group_by(year, rteNo, Stop) %>% do(s = get_spp(., sppids=sppids))
#load('C:/Users/Q/Dropbox/projects/aquaxterra/bbstemp.r')
bbsgrps <- select(bbsmat, year, rteNo, Stop)
bbsmat <- bbsmat$s
bbsmat <- do.call('rbind', bbsmat)

save(bbsgrps, bbsmat, sppids, file = 'DATA/raw_data/BBS/bbsmat.r')
#names(bbsmat)[-(1:3)] <- sppids

# Run some taxonomic diversity metrics on this dataset
#q0 <- d(bbsmat, lev = 'alpha', q = 0, boot = FALSE)
#bbsdiv <- sapply(1:3, function(x) d(bbsmat, lev = 'alpha', q = x, boot = FALSE))

# vegan pkg metrics
bbsmat_richness <- apply(bbsmat > 0, 1, sum)
bbsmat_shannon <- diversity(bbsmat, index = 'shannon')
bbsmat_simpson <- diversity(bbsmat, index = 'simpson')
bbsmat_shannonevenness <- bbsmat_shannon/log(bbsmat_richness)

# Load the consolidated matrix that has the subspecies, hybrids, and unidentifiable species cleaned up a little bit.
load('DATA/raw_data/BBS/bbsmatconsolidated.r')

# Calculate the metrics on this consolidated matrix.
bbsmat_richness <- apply(fixedbbsmat > 0, 1, sum)
bbsmat_shannon <- diversity(fixedbbsmat, index = 'shannon')
bbsmat_simpson <- diversity(fixedbbsmat, index = 'simpson')
bbsmat_shannonevenness <- bbsmat_shannon/log(bbsmat_richness)

bbstaxdiv <- data.frame(bbsgrps, richness=bbsmat_richness, shannon=bbsmat_shannon, simpson=bbsmat_simpson, shannonevenness=bbsmat_shannonevenness)
save(bbstaxdiv, file = 'DATA/raw_data/BBS/bbstaxdiv.r')

# Merge the richness data to the huc data if possible

load('DATA/raw_data/BBS/bbstaxdiv.r')
bbs_huc <- read.csv('CODE/python/BBSSpatialJoin/BBS_SpatialJoin_Final.csv', stringsAsFactors = FALSE)

ns <- strsplit(bbs_huc$rtestopNo, '-')
bbs_huc$rteNo <- sapply(ns, '[', 1)
bbs_huc$Stop <- sapply(ns, '[', 2)

bbstaxdiv <- bbstaxdiv %>% mutate(rteNo = as.numeric(as.character(rteNo)), Stop = as.numeric(gsub('[a-zA-Z]', '', as.character(Stop))))
bbs_huc <- bbs_huc %>% mutate(rteNo = as.numeric(as.character(rteNo)), Stop = as.numeric(as.character(Stop))) 

bbstaxdiv <- left_join(bbstaxdiv, bbs_huc  %>% select(rteNo, Stop, HUC4, HUC8, HUC12))

# Aggregate by HUC and year
mediantaxdiv_huc4 <- bbstaxdiv %>% group_by(year, HUC4) %>% summarize_at(vars(richness, shannon, simpson, shannonevenness), median)
mediantaxdiv_huc8 <- bbstaxdiv %>% group_by(year, HUC8) %>% summarize_at(vars(richness, shannon, simpson, shannonevenness), median)
mediantaxdiv_huc12 <- bbstaxdiv %>% group_by(year, HUC12) %>% summarize_at(vars(richness, shannon, simpson, shannonevenness), median)

##########################################################
# 07 Dec: Add TD, FD, and PD up to 2012 to the merged bbs and huc data. Hopefully this can be used to make some plots.

# First load all bbs diversity data we have so far. This is the "old" dataset since I am still working on updating the new one.
load('/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_div.r')
bbs_huc <- read.csv('/mnt/research/aquaxterra/CODE/python/BBSSpatialJoin/BBS_SpatialJoin_Final.csv', stringsAsFactors = FALSE)

ns <- strsplit(bbs_huc$rtestopNo, '-')
bbs_huc$rteNo <- sapply(ns, '[', 1)
bbs_huc$Stop <- sapply(ns, '[', 2)

library(dplyr)

bbs_div <- bbs_div %>% mutate(rteNo = as.numeric(as.character(rteNo)), Stop = as.numeric(gsub('[a-zA-Z]', '', as.character(Stop))))
bbs_huc <- bbs_huc %>% mutate(rteNo = as.numeric(as.character(rteNo)), Stop = as.numeric(as.character(Stop))) 

bbs_div <- left_join(bbs_div, bbs_huc  %>% select(rteNo, Stop, HUC4, HUC8, HUC12))

bbs_coords <- read.csv('/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_div_georeferenced.csv')
bbs_div <- cbind(bbs_div, bbs_coords[, c('POINT_X', 'POINT_Y')])

write.csv(bbs_div, file = '/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_div_georeferenced.csv', row.names = FALSE)

# Subset out one year to download and plot things locally.
write.csv(bbs_div %>% filter(year == 2011), file = '/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_div_2011.csv', row.names = FALSE)