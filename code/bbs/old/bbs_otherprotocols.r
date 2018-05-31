# BBS additional route protocols.
# Can help establish detection probability for spp.

fp <- '/mnt/research/aquaxterra/DATA/raw_data/BBS/DataFiles/50-StopData/1997ToPresent_SurveyWide'

bbsdf <- list()

for (i in 1:10) {
	bbsdf[[i]] <- read.csv(file.path(fp, paste0('fifty',i,'.csv')))
}

bbsdf <- do.call('rbind', bbsdf)

# Modification 30 Jan: find the routes and years that have multiple runs. Get the 101 for those runs as well.

bbsdf_otherprotocol <- subset(bbsdf, RPID != 101)
bbsdf101 <- subset(bbsdf, RPID == 101 & paste(statenum, Route, year) %in% with(bbsdf_otherprotocol, paste(statenum, Route, year)))
bbsdf_otherprotocol <- rbind(bbsdf_otherprotocol, bbsdf101)

# Get only the repeated sample protocol routes
bbsdf_repeated <- subset(bbsdf_otherprotocol, RPID %in% 101:104)
save(bbsdf_repeated, file = '/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_repeated.r')

library(dplyr)
library(reshape2)

# Combine state number and route number into single rteNo
rteNo <- character(nrow(bbsdf_otherprotocol))
for (i in 1:nrow(bbsdf_otherprotocol)) {
	rteNo[i] <- with(bbsdf_otherprotocol, paste(statenum[i], paste(rep('0', 3-nchar(as.character(Route[i]))), collapse=''), Route[i], sep = ''))
}

bbsdf_otherprotocol$rteNo <- rteNo

# Convert to long format
bbslong_other <- melt(bbsdf_otherprotocol, id.vars = grep('Stop', names(bbsdf_otherprotocol), invert=TRUE), variable.name = 'Stop', value.name = 'n')
bbslong_other <- bbslong_other %>% filter(n>0) # Lots of zeroes, get rid.

# Convert to a species by site matrix (site is a route by stop by year combination)
sppids_other <- sort(unique(bbsdf_otherprotocol$AOU))

get_spp <- function(dat, sppids) {
  ns <- dat$n
  idx <- match(dat$AOU, sppids)
  res <- rep(0, length(sppids))
  res[idx] <- ns
  #as.data.frame(t(res))
  res
}
bbsmat_other <- bbslong_other %>% group_by(year, RPID, rteNo, Stop) %>% do(s = get_spp(., sppids=sppids_other))
bbsgrps_other <- select(bbsmat_other, year, RPID, rteNo, Stop)
bbsmat_other <- bbsmat_other$s
bbsmat_other <- do.call('rbind', bbsmat_other)

bbsmat_byroute_other <- bbslong_other %>% group_by(year, RPID, rteNo) %>% do(s = get_spp(., sppids=sppids_other))
bbsgrps_byroute_other <- select(bbsmat_byroute_other, year, RPID, rteNo)
bbsmat_byroute_other <- bbsmat_byroute_other$s
bbsmat_byroute_other <- do.call('rbind', bbsmat_byroute_other)

save(bbsmat_other, bbsgrps_other, bbsmat_byroute_other, bbsgrps_byroute_other, sppids_other, file = '/mnt/research/aquaxterra/DATA/raw_data/BBS/bbsmat2015_otherprotocols.r')

##################################################

# Fix bbs matrix (consolidating hybrids and such)
bbsspp <- read.csv('/mnt/research/aquaxterra/DATA/raw_data/bird_traits/specieslist.csv', stringsAsFactors = FALSE)
load('/mnt/research/aquaxterra/DATA/raw_data/BBS/bbsmat2015_otherprotocols.r')

# First, assign all species not in phylogeny to a species in the phylogeny

library(stringr)
AOU_lists <- lapply(str_extract_all(bbsspp$AOU_list, pattern = '[0-9]+'), as.numeric)
resample <- function(x, ...) x[sample.int(length(x), ...)]

toconsolidate <- bbsspp$AOU[sapply(AOU_lists,length)>0]
idups <- sppids_other %in% toconsolidate

###############################################
# Aggregation by stop

fixedbbsmat_other <- list()

pb <- txtProgressBar(0, nrow(bbsmat_other), style=3)

for (j in 1:nrow(bbsmat_other)) {

setTxtProgressBar(pb,j)

x <- bbsmat_other[j,]

# Reassign species randomly (must be done every time)
AOU_final <- bbsspp$AOU
for (i in 1:length(AOU_lists)) {
  if (length(AOU_lists[[i]]) > 0) AOU_final[i] <- resample(AOU_lists[[i]], 1)
}

# Consolidate rows with multiple species ids

ndups <- x[idups]
aoudups <- sppids_other[idups]

for (i in 1:length(ndups)) {
	if (ndups[i] > 0) {
		addto <- AOU_final[bbsspp$AOU == aoudups[i]]
		x[which(sppids_other == addto)] <- x[which(sppids_other == addto)] + ndups[i]
	}
}

x[idups] <- 0
fixedbbsmat_other[[j]] <- x

}

close(pb)
fixedbbsmat_other <- do.call('rbind', fixedbbsmat_other)

################################################
# Aggregation by route.
fixedbbsmat_byroute_other <- list()
pb <- txtProgressBar(0, nrow(bbsmat_byroute_other), style=3)

for (j in 1:nrow(bbsmat_byroute_other)) {

setTxtProgressBar(pb,j)

x <- bbsmat_byroute_other[j,]

# Reassign species randomly (must be done every time)
AOU_final <- bbsspp$AOU
for (i in 1:length(AOU_lists)) {
  if (length(AOU_lists[[i]]) > 0) AOU_final[i] <- resample(AOU_lists[[i]], 1)
}

# Consolidate rows with multiple species ids

ndups <- x[idups]
aoudups <- sppids_other[idups]

for (i in 1:length(ndups)) {
	if (ndups[i] > 0) {
		addto <- AOU_final[bbsspp$AOU == aoudups[i]]
		x[which(sppids_other == addto)] <- x[which(sppids_other == addto)] + ndups[i]
	}
}

x[idups] <- 0
fixedbbsmat_byroute_other[[j]] <- x

}

close(pb)
fixedbbsmat_byroute_other <- do.call('rbind', fixedbbsmat_byroute_other)

# Correct single typo in bbsmat which should get rid of one species that is in error (not sure why but it has AOU 81650 which isn't found anywhere)
# added 16 Nov.
# Not necessary for the updated bbsmat?
#fixedbbsmat[which(fixedbbsmat[,608]>0), 608] <- 0

save(fixedbbsmat_other, fixedbbsmat_byroute_other, bbsgrps_other, bbsgrps_byroute_other, sppids_other, file = '/mnt/research/aquaxterra/DATA/raw_data/BBS/bbsmatconsolidated2015_otherprotocols.r')