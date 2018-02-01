# Complete BBS data processing pipeline
# AquaXTerra WaterCube project
# QDR 31 Jan 2018

# ----------------------------------
# LOAD BBS DATA DOWNLOADED FROM USGS
# ----------------------------------

library(dplyr)
library(reshape2)
library(stringr)
library(ape)
library(phytools)

fp <- '/mnt/research/aquaxterra/DATA/raw_data/BBS/DataFiles26may2017/50-StopData/1997ToPresent_SurveyWide'

bbsdf <- list()

for (i in 1:10) {
	bbsdf[[i]] <- read.csv(file.path(fp, paste0('fifty',i,'.csv')))
}

bbsdf <- do.call('rbind', bbsdf)

# -----------------------------
# CREATE SPECIES-BY-SITE MATRIX
# -----------------------------

# Use only surveys done with the normal protocol.
bbsdf <- subset(bbsdf, RPID == 101)

# Combine state number and route number into single rteNo
rteNo <- character(nrow(bbsdf))
for (i in 1:nrow(bbsdf)) {
	rteNo[i] <- with(bbsdf, paste(statenum[i], paste(rep('0', 3-nchar(as.character(Route[i]))), collapse=''), Route[i], sep = ''))
}

bbsdf$rteNo <- rteNo

# Convert to long format
bbslong <- melt(bbsdf, id.vars = grep('Stop', names(bbsdf), invert=TRUE), variable.name = 'Stop', value.name = 'n')
bbslong <- bbslong %>% filter(n>0) # Get rid of zero rows.

# Convert to a species by site matrix (site is a route by year combination)
sppids <- sort(unique(bbsdf$AOU))
get_spp <- function(dat, sppids) {
  ns <- dat$n
  idx <- match(dat$AOU, sppids)
  res <- rep(0, length(sppids))
  res[idx] <- ns
  res
}

bbsmat_byroute <- bbslong %>% group_by(year, rteNo) %>% do(s = get_spp(., sppids=sppids))
bbsgrps_byroute <- select(bbsmat_byroute, year, rteNo)
bbsmat_byroute <- do.call('rbind', bbsmat_byroute$s)

# ----------------------------------------------------------------------------
# LOAD CORRECTED SPECIES LIST AND USE IT TO CONSOLIDATE SPECIES-BY-SITE MATRIX
# ----------------------------------------------------------------------------

bbsspp <- read.csv('/mnt/research/aquaxterra/DATA/raw_data/bird_traits/specieslist.csv', stringsAsFactors = FALSE)

# First, assign all species not in phylogeny to a species in the phylogeny

AOU_lists <- lapply(str_extract_all(bbsspp$AOU_list, pattern = '[0-9]+'), as.numeric)
resample <- function(x, ...) x[sample.int(length(x), ...)]

toconsolidate <- bbsspp$AOU[sapply(AOU_lists,length)>0]
idups <- sppids %in% toconsolidate

# Aggregation by route.
fixedbbsmat_byroute <- list()
pb <- txtProgressBar(0, nrow(bbsmat_byroute), style=3)

for (j in 1:nrow(bbsmat_byroute)) {

setTxtProgressBar(pb,j)

x <- bbsmat_byroute[j,]

# Reassign species randomly (must be done every time)
set.seed(688)
AOU_final <- bbsspp$AOU
for (i in 1:length(AOU_lists)) {
  if (length(AOU_lists[[i]]) > 0) AOU_final[i] <- resample(AOU_lists[[i]], 1)
}

# Consolidate rows with multiple species ids

ndups <- x[idups]
aoudups <- sppids[idups]

for (i in 1:length(ndups)) {
	if (ndups[i] > 0) {
		addto <- AOU_final[bbsspp$AOU == aoudups[i]]
		x[which(sppids == addto)] <- x[which(sppids == addto)] + ndups[i]
	}
}

x[idups] <- 0
fixedbbsmat_byroute[[j]] <- x

}

close(pb)
fixedbbsmat_byroute <- do.call('rbind', fixedbbsmat_byroute)

# ---------------------------------------------------
# LOAD PHYLOGENETIC TREES AND GENERATE CONSENSUS TREE
# ---------------------------------------------------

# Load 1000 ericson trees
erictree <- read.nexus('C:/Users/Q/Dropbox/projects/nasabiodiv/ericson1000.tre')
eric_cons_edges <- consensus.edges(trees = erictree, method = 'least.squares')
eric_cons_tree <- reroot(eric_cons_edges, node.number = 1226, position = 0)