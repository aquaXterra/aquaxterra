# Read and manipulate bbs data
# Author: QDR
# Project: Aquaxterra
# Created: 02 Nov 2016
# Last modified:

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
#names(bbsmat)[-(1:3)] <- sppids

# Run some taxonomic diversity metrics on this dataset
#q0 <- d(bbsmat, lev = 'alpha', q = 0, boot = FALSE)
#bbsdiv <- sapply(1:3, function(x) d(bbsmat, lev = 'alpha', q = x, boot = FALSE))

# vegan pkg metrics
bbsmat_richness <- apply(bbsmat > 0, 1, sum)
bbsmat_shannon <- diversity(bbsmat, index = 'shannon')
bbsmat_simpson <- diversity(bbsmat, index = 'simpson')
bbsmat_shannonevenness <- bbsmat_shannon/log(bbsmat_richness)

bbstaxdiv <- data.frame(bbsgrps, richness=bbsmat_richness, shannon=bbsmat_shannon, simpson=bbsmat_simpson, shannonevenness=bbsmat_shannonevenness)

# Merge the richness data to the huc data if possible

load('~/data/bbstaxdiv.r')
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