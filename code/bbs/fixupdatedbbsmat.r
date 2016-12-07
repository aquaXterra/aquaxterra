##################################################

# Fix bbs matrix (consolidating hybrids and such)
bbsspp <- read.csv('/mnt/research/aquaxterra/DATA/raw_data/bird_traits/specieslist.csv', stringsAsFactors = FALSE)
load('/mnt/research/aquaxterra/DATA/raw_data/BBS/bbsmat2015.r')

# First, assign all species not in phylogeny to a species in the phylogeny

library(stringr)
AOU_lists <- lapply(str_extract_all(bbsspp$AOU_list, pattern = '[0-9]+'), as.numeric)
resample <- function(x, ...) x[sample.int(length(x), ...)]

toconsolidate <- bbsspp$AOU[sapply(AOU_lists,length)>0]
idups <- sppids %in% toconsolidate

###############################################
# Aggregation by stop

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
fixedbbsmat <- do.call('rbind', fixedbbsmat)

################################################
# Aggregation by route.
fixedbbsmat_byroute <- list()
pb <- txtProgressBar(0, nrow(bbsmat_byroute), style=3)

for (j in 1:nrow(bbsmat_byroute)) {

setTxtProgressBar(pb,j)

x <- bbsmat_byroute[j,]

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
fixedbbsmat_byroute[[j]] <- x

}

close(pb)
fixedbbsmat_byroute <- do.call('rbind', fixedbbsmat_byroute)

# Correct single typo in bbsmat which should get rid of one species that is in error (not sure why but it has AOU 81650 which isn't found anywhere)
# added 16 Nov.
# Not necessary for the updated bbsmat?
#fixedbbsmat[which(fixedbbsmat[,608]>0), 608] <- 0

save(fixedbbsmat, fixedbbsmat_byroute, bbsgrps, bbsgrps_byroute, sppids, file = '/mnt/research/aquaxterra/DATA/raw_data/BBS/bbsmatconsolidated2015.r')