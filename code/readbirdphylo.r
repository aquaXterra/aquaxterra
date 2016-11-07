# Read bird phylogenies and match to the real species names that we have. Also add species identified only to genus to the phylogeny.
# Author: QDR
# Project: Aquaxterra
# Created: 04 Nov 2016

library(ape)

# Read the randomly sampled 1000-tree subsets of the birdtree.org phylogenies (Ericson and Hackett versions)

erictree <- read.nexus('DATA/raw_data/bird_traits/bird_phylogeny/ericson1000.tre')
hacktree <- read.nexus('DATA/raw_data/bird_traits/bird_phylogeny/hackett1000.tre')

# Find which species on our species list are not given as tips of the tree.

t1 <- erictree[[1]]
tlabel1 <- t1$tip.label
tlabel1 <- gsub('_', ' ', tlabel1)

bbsspp <- read.csv('DATA/raw_data/bird_traits/specieslist.csv', stringsAsFactors = FALSE)

phymatch <- bbsspp$Latin_Name_clean %in% tlabel1 | bbsspp$Latin_Name_synonym %in% tlabel1 | bbsspp$Latin_Name_synonym2 %in% tlabel1

# We must add some non-identified species to the tree.
# The protocol should be: if it is unknown between 2 or 3 species, assign the unknown individual randomly to one of those species
# If it is unknown in a genus, assign randomly to any species in that genus
# If it is unknown in a family, assign randomly to any species in that family
# If it is a hybrid, assign randomly to one of the parent species
# If it is a subspecies, it gets the same ID as the undifferentiated species.

library(phytools)
spp_not_in_tree <- bbsspp[!phymatch, ]
# Manually edit these names then reload
write.csv(spp_not_in_tree, file='DATA/raw_data/bird_traits/sppnotinphylo.csv', row.names=FALSE)