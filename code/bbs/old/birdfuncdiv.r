# Bird functional diversity calculations
# QDR/Aquaxterra/created 17Nov2016/modified 18Nov2016


fp <- '/mnt/research/aquaxterra/DATA/raw_data/bird_traits'
birdtrait <- read.csv(file.path(fp, 'birdtraitmerged.csv'), stringsAsFactors = FALSE)

birdtrait[birdtrait == -999] <- NA

# Run principal components analysis on the bird traits.

#birddietpca <- prcomp(~., data=birdtrait[,15:24], scale = TRUE, center = TRUE, na.action = na.omit)

# Select traits to use
# Ones that seem important and/or have a lot of records.
# Includes diet, foraging strategy, body size, and life history.
traitnames <- names(birdtrait)[c(15:24, 29:36, 40, 46:50, 53, 55:59)]


# Match bird traits with the site by species matrix.
# traitmatch <- sppids %in% birdtrait$AOU # 65 don't match :-(

# Instead, use the consolidated matrix that was used for the phylogenetic diversity calculations.
load('/mnt/research/aquaxterra/DATA/raw_data/BBS/bbsmat.r')
load('/mnt/research/aquaxterra/DATA/raw_data/BBS/bbsmatconsolidated.r') # Load fixed bbsmat.

ns <- colSums(fixedbbsmat)
fixedbbsmat_nonzero <- fixedbbsmat[, ns > 0]
sppids_nonzero <- sppids[ns > 0]

# Clean trait matrix and sort trait and sitexsp matrices so their dimensions all match.
dimnames(fixedbbsmat_nonzero)[[2]] <- sppids_nonzero # Already sorted by AOU
birdtraitclean <- birdtrait[match(sppids_nonzero, birdtrait$AOU), traitnames]
dimnames(birdtraitclean)[[1]] <- sppids_nonzero

# Make sure all columns are numerics, even the binary variables.
birdtraitclean <- transform(birdtraitclean, PelagicSpecialist = as.numeric(PelagicSpecialist), Nocturnal = as.numeric(Nocturnal))

# Remove rows where no birds at all were found, flagging them for later.
rs <- apply(fixedbbsmat_nonzero, 1, sum)
fixedbbsmat_nonzerorows <- fixedbbsmat_nonzero[rs > 0, ]

# Run "naive" functional diversity, throwing all the traits in together.
library(FD)

# Gower distance matrix.
# Mark pelagic specialist trait as a binary trait.
# The "Diet.*" and "ForStrat.*" traits are all percentages.
# The remaining traits are continuous traits with different units.
# Argument w can be added to weight traits differently, but currently not weighted.

# birdtrait_gowdist <- gowdis(x = birdtraitclean)

# Attempt to construct distance matrix without NAs by removing the three species with very high NA's (they are also taxonomically weird)
# This problem was fixed in another script.
#badIDs <- c('3151', '3410', '3460')
#birdtraitgoodids <- birdtraitclean[!dimnames(birdtraitclean)[[1]] %in% badIDs, ]
# This works so we need to get rid of those bad IDs for the functional diversity. 

# # Test FD with a small subset of the data.

# set.seed(27701)
# testmatrix <- fixedbbsmat_nonzero[sample(nrow(fixedbbsmat_nonzero), size=10), ]

# # Must get rid of all-zero columns in testmatrix.
# zerocols <- apply(testmatrix,2,sum) == 0

# # The below code throws an error that the default sqrt correction does not represent the distance matrix as Euclidean. Try another.
# dbFD(x = birdtraitclean[!zerocols,], a = testmatrix[,!zerocols], w.abun = TRUE, asym.bin = grep('Pelagic', names(birdtraitclean)))

# fdtest <- dbFD(x = birdtraitclean[!zerocols,], a = testmatrix[,!zerocols], w.abun = TRUE, asym.bin = grep('Pelagic', names(birdtraitclean)), corr = 'cailliez')

# Run for all the communities.
fd_all <- dbFD(x = birdtraitclean, a = fixedbbsmat_nonzerorows, w.abun = TRUE, corr = 'cailliez')
save(fd_all, file = file.path(fp, 'birdfuncdivobject.r'))

# Fill back in the stops with zero birds, with zero for the species richness and NA for all functional diversity values.
