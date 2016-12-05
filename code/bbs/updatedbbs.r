# combine most recent bbs data into a single csv. Use to generate new bbsmat.

fp <- '/mnt/research/aquaxterra/DATA/raw_data/BBS/DataFiles/50-StopData/1997ToPresent_SurveyWide'

bbsdf <- list()

for (i in 1:10) {
	bbsdf[[i]] <- read.csv(file.path(fp, paste0('fifty',i,'.csv')))
}

bbsdf <- do.call('rbind', bbsdf)
bbsdf <- subset(bbsdf, RPID == 101)

library(dplyr)
library(reshape2)

# Combine state number and route number into single rteNo
rteNo <- character(nrow(bbsdf))
for (i in 1:nrow(bbsdf)) {
	rteNo[i] <- with(bbsdf, paste(statenum[i], paste(rep('0', 3-nchar(as.character(Route[i]))), collapse=''), Route[i], sep = ''))
}

bbsdf$rteNo <- rteNo

# Convert to long format
bbslong <- melt(bbsdf, id.vars = grep('Stop', names(bbsdf), invert=TRUE), variable.name = 'Stop', value.name = 'n')
bbslong <- bbslong %>% filter(n>0) # Lots of zeroes, get rid.

# Convert to a species by site matrix (site is a route by stop by year combination)
sppids <- sort(unique(bbsdf$AOU))
get_spp <- function(dat, sppids) {
  ns <- dat$n
  idx <- match(dat$AOU, sppids)
  res <- rep(0, length(sppids))
  res[idx] <- ns
  #as.data.frame(t(res))
  res
}
bbsmat <- bbslong %>% group_by(year, rteNo, Stop) %>% do(s = get_spp(., sppids=sppids))
bbsgrps <- select(bbsmat, year, rteNo, Stop)
bbsmat <- bbsmat$s
bbsmat <- do.call('rbind', bbsmat)

bbsmat_byroute <- bbslong %>% group_by(year, rteNo) %>% do(s = get_spp(., sppids=sppids))
bbsgrps_byroute <- select(bbsmat_byroute, year, rteNo)
bbsmat_byroute <- bbsmat_byroute$s
bbsmat_byroute <- do.call('rbind', bbsmat_byroute)

save(bbsmat, bbsgrps, bbsmat_byroute, bbsgrps_byroute, sppids, file = '/mnt/research/aquaxterra/DATA/raw_data/BBS/bbsmat2015.r')

