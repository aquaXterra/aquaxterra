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
load('C:/Users/Q/Dropbox/projects/aquaxterra/bbs6712.RData')

# Do some very basic tallying of richness

library(dplyr)
library(reshape2)

bbsdf <- left_join(bbs6712 %>% select(-remove), bbsspp[,2:3])
rm(bbs6712)

# Convert to long format
bbslong <- melt(bbsdf, id.vars = grep('Stop', names(bbsdf), invert=TRUE), variable.name = 'Stop', value.name = 'n')
#bbslong <- bbslong %>% filter(n>0) # Lots of zeroes, get rid.

# Get richness by stop and year
bbsrichness <- bbslong %>% group_by(year, rteNo, Stop) %>% summarize(abund = sum(n), richness = sum(n>0)) # Takes a while.

# Get richness by route and year
bbsrichness_byroute <- bbslong %>% group_by(year, rteNo) %>% summarize(abund = sum(n), richness = length(unique(AOU))) # Takes a while.
