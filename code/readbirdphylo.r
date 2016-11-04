# Read bird phylogenies and match to the real species names that we have. Also add species identified only to genus to the phylogeny.
# Author: QDR
# Project: Aquaxterra
# Created: 04 Nov 2016

library(ape)

# Read the randomly sampled 1000-tree subsets of the birdtree.org phylogenies (Ericson and Hackett versions)

erictree <- read.nexus('DATA/raw_data/bird_traits/bird_phylogeny/ericson1000.tre')
hacktree <- read.nexus('DATA/raw_data/bird_traits/bird_phylogeny/hackett1000.tre')

# Find which species on our species list are not given as tips of the tree.

