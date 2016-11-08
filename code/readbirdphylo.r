# Read bird phylogenies and match to the real species names that we have. Also add species identified only to genus to the phylogeny.
# Author: QDR
# Project: Aquaxterra
# Created: 04 Nov 2016
# Last modified: 08 Nov 2016

# Modified 08 Nov: Resolve AOUs at random for each tree to create a new tree.

library(ape)

# Read the randomly sampled 1000-tree subsets of the birdtree.org phylogenies (Ericson and Hackett versions)

erictree <- read.nexus('DATA/raw_data/bird_traits/bird_phylogeny/ericson1000.tre')
hacktree <- read.nexus('DATA/raw_data/bird_traits/bird_phylogeny/hackett1000.tre')

# Find which species on our species list are not given as tips of the tree.

t1 <- erictree[[1]]
tlabel1 <- t1$tip.label
tlabel1 <- gsub('_', ' ', tlabel1)

bbsspp <- read.csv('DATA/raw_data/bird_traits/specieslist.csv', stringsAsFactors = FALSE)
load('DATA/raw_data/BBS/bbsmat.r')

#phymatch <- bbsspp$Latin_Name_clean %in% tlabel1 | bbsspp$Latin_Name_synonym %in% tlabel1 | bbsspp$Latin_Name_synonym2 %in% tlabel1

# We must add some non-identified species to the tree.
# The protocol should be: if it is unknown between 2 or 3 species, assign the unknown individual randomly to one of those species
# If it is unknown in a genus, assign randomly to any species in that genus
# If it is unknown in a family, assign randomly to any species in that family
# If it is a hybrid, assign randomly to one of the parent species
# If it is a subspecies, it gets the same ID as the undifferentiated species.

#library(phytools)
#spp_not_in_tree <- bbsspp[!phymatch, ]
# Manually edit these names then reload
#write.csv(spp_not_in_tree, file='DATA/raw_data/bird_traits/sppnotinphylo.csv', row.names=FALSE)

# function to get pd out of each row

# First, assign all species not in phylogeny to a species in the phylogeny

library(stringr)
AOU_lists <- lapply(str_extract_all(bbsspp$AOU_list, pattern = '[0-9]+'), as.numeric)
resample <- function(x, ...) x[sample.int(length(x), ...)]

toconsolidate <- bbsspp$AOU[sapply(AOU_lists,length)>0]
idups <- sppids %in% toconsolidate

fixedbbsmat <- list()
pb <- txtProgressBar(0, nrow(bbsmat), style=3)

for (j in 1:nrow(bbsmat)) {

setTxtProgressBar(pb,j)

x <- bbsmat[j,]

# Reassign species randomly (must be done every time)
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
fixedbbsmat[[j]] <- x

}

close(pb)

save(fixedbbsmat, 'DATA/raw_data/bbsmatconsolidated.r')