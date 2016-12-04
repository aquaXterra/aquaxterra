# Read bird phylogenies and match to the real species names that we have. Also add species identified only to genus to the phylogeny.
# Author: QDR
# Project: Aquaxterra
# Created: 04 Nov 2016
# Last modified: 03 Dec 2016

# Modified 03 Dec: average ten trees instead of using just one (follow Jarzyna); also get rid of nocturnal species.
# Modified 15 Nov: Get rid of years older than 1997 (when modern bbs data was established)
# Modified 10 Nov: Add PD calculations for one test tree
# Modified 08 Nov: Resolve AOUs at random for each tree to create a new tree.

library(ape)

# Read the randomly sampled 1000-tree subsets of the birdtree.org phylogenies (Ericson and Hackett versions)

erictree <- read.nexus('/mnt/research/aquaxterra/DATA/raw_data/bird_traits/bird_phylogeny/ericson1000.tre')
hacktree <- read.nexus('/mnt/research/aquaxterra/DATA/raw_data/bird_traits/bird_phylogeny/hackett1000.tre')

# Load ten randomly selected trees
set.seed(46545)
treeids <- sample(length(erictree), size = 10, replace = FALSE)
use_trees <- erictree[treeids]

# Find which species on our species list are not given as tips of the tree.

t1 <- use_trees[[1]]
tlabel1 <- t1$tip.label
tlabel1 <- gsub('_', ' ', tlabel1)

bbsspp <- read.csv('/mnt/research/aquaxterra/DATA/raw_data/bird_traits/specieslist.csv', stringsAsFactors = FALSE)
load('/mnt/research/aquaxterra/DATA/raw_data/BBS/bbsmat.r')

phymatch <- bbsspp$Latin_Name_clean %in% tlabel1 | bbsspp$Latin_Name_synonym %in% tlabel1 | bbsspp$Latin_Name_synonym2 %in% tlabel1

phymatchidx <- rep(NA,length(tlabel1))

for (i in 1:length(tlabel1)) {
	phymatchidx[i] <- c(which(bbsspp$Latin_Name_clean == tlabel1[i] | bbsspp$Latin_Name_synonym == tlabel1[i] | bbsspp$Latin_Name_synonym2 == tlabel1[i]), NA)[1]
}

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

load('/mnt/research/aquaxterra/DATA/raw_data/BBS/bbsmatconsolidated.r') # Load fixed bbsmat.

# Exclude nocturnal birds
bt <- read.csv('/mnt/research/aquaxterra/DATA/raw_data/bird_traits/birdtraitmerged.csv', stringsAsFactors=FALSE)
nocturnalbirds <- bt$AOU[bt$Nocturnal == 1]

#fixedbbsmat <- fixedbbsmat[bbsgrps$year >= 1997, ]

# Match the tip labels of ericson or hackett tree with the row names of the fixed bbs matrix.

tlabelaou <- bbsspp$AOU[phymatchidx]
ns <- colSums(fixedbbsmat)
aoustoadd <- sppids[!sppids %in% tlabelaou & ns > 0] # AOUs that need to be added

# Set dimnames of fixedbbsmat and tip labels of erictree to be the same.
dimnames_tlabel <- rep(NA, length(tlabelaou))
for (i in 1:length(tlabelaou)) {
	names_i <- bbsspp[bbsspp$AOU == tlabelaou[i], c('Latin_Name')]
	if (length(names_i)>0) dimnames_tlabel[i] <- names_i[1]
}
dimnames_matrix <- rep(NA, length(sppids))
for (i in 1:length(sppids)) {
	names_i <- bbsspp[bbsspp$AOU == sppids[i], c('Latin_Name')]
	if (length(names_i)>0) dimnames_matrix[i] <- names_i[1]
}

for (i in 1:10) use_trees[[i]]$tip.label <- dimnames_tlabel
dimnames(fixedbbsmat)[[2]] <- dimnames_matrix
fixedbbsmat_nonzero <- fixedbbsmat[, ns > 0]

fixedbbsmat_nonzero <- fixedbbsmat_nonzero[, !(dimnames(fixedbbsmat_nonzero)[[2]] %in% nocturnalbirds)]

# Actual calculation of pd, mpd, and mntd

library(picante)

ericsondist <- lapply(use_trees, cophenetic)
# Must sort the distance matrices so that they are all the same order.
roworder <- dimnames(ericsondist[[1]])[[1]]
ericsondist <- lapply(ericsondist, function(x) x[roworder, roworder])
# Mean branch lengths across the ten randomly sampled trees
ericsondistmean <- apply(simplify2array(ericsondist), 1:2, mean)

# Split this into smaller jobs that can be run in parallel.
task <- as.numeric(Sys.getenv('PBS_ARRAYID'))

xx <- round(seq(0, nrow(fixedbbsmat), length.out=11))
xxmat <- cbind((xx+1)[-11], xx[-1])
rowstouse <- (xxmat[task,1]:xxmat[task,2])

x <- fixedbbsmat_nonzero[rowstouse,]

# Mean PD across ten trees
allpds <- list()
for (i in 1:10) {
	allpds[[i]] <- pd(x, use_trees[[i]], include.root=TRUE)
}

pd_ericson <- apply(Reduce(cbind, lapply(allpds, function(x) x$PD)), 1, mean) # messed up way of getting average pd
#pd_ericson <- pd(x, t1, include.root = TRUE)
mpd_ericson <- ses.mpd(x, ericsondistmean, null.model = 'independentswap', abundance.weighted = TRUE, runs = 999, iterations = 1000)
mntd_ericson <- ses.mntd(x, ericsondistmean, null.model = 'independentswap', abundance.weighted = TRUE, runs = 999, iterations = 1000)

save(pd_ericson, mpd_ericson, mntd_ericson, file = paste0('/mnt/research/aquaxterra/DATA/raw_data/bird_traits/bird_phylogeny/pd_test',task,'.r'))