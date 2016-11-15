bbsspp <- read.csv('DATA/raw_data/bird_traits/specieslist.csv', stringsAsFactors = FALSE)
load('DATA/raw_data/BBS/bbsmat.r')

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
fixedbbsmat <- do.call('rbind', fixedbbsmat)

save(fixedbbsmat, 'DATA/raw_data/bbsmatconsolidated.r')