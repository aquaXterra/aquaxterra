# Compile BBS taxonomic, functional, and phylogenetic diversity into a single data frame.
# AGGREGATED BY ROUTE!
# Georeference all these values.
# 09 Nov 2016: Aggregated by route, using slightly better species list (no nocturnal birds), 
# uses all available data through 2015, and uses mean of ten randomly selected phylogenetic trees for all the PDs.

fp <- '/mnt/research/aquaxterra/'

# Recalculate bbs taxonomic diversity, removing the nocturnal birds.

load(file.path(fp, 'DATA/raw_data/BBS/bbsmatconsolidated2015.r'))
birdtrait <- read.csv(file.path(fp, 'DATA/raw_data/bird_traits/birdtraitmerged.csv'), stringsAsFactors = FALSE)
birdtrait[birdtrait == -999] <- NA
nocturnalbirdAOUs <- birdtrait$AOU[birdtrait$Nocturnal == 1]

fixedbbsmat_byroute_diurnal <- fixedbbsmat_byroute[, !sppids %in% nocturnalbirdAOUs]

library(vegan)

bbsmat_richness <- apply(fixedbbsmat_byroute_diurnal > 0, 1, sum)
bbsmat_shannon <- diversity(fixedbbsmat_byroute_diurnal, index = 'shannon')
bbsmat_simpson <- diversity(fixedbbsmat_byroute_diurnal, index = 'simpson')
bbsmat_shannonevenness <- bbsmat_shannon/log(bbsmat_richness)


pd_all <- read.csv(file.path(fp,'DATA/raw_data/bird_traits/bird_phylogeny/pd_byroute_mean.csv'), stringsAsFactors = FALSE)
load(file.path(fp,'DATA/raw_data/bird_traits/birdfuncdivobjectbyrouteall.r'))

bbs_div_byroute <- cbind(bbsgrps_byroute, richness=bbsmat_richness, shannon=bbsmat_shannon, simpson=bbsmat_simpson, shannonevenness=bbsmat_shannonevenness, fd_all_byroute[, c('FRic','FEve', 'FDiv', 'FDis', 'RaoQ')], pd_all[, c(1, 3:14)])

# Save the diversity only, can be georeferenced later.
#save(bbs_div_byroute, file = 'DATA/raw_data/BBS/bbs_div_byroute.r')

# Merge with HUCs

bbs_huc <- read.csv(file.path(fp,'CODE/python/BBSSpatialJoin/BBS_SpatialJoin_Final.csv'), stringsAsFactors = FALSE)

ns <- strsplit(bbs_huc$rtestopNo, '-')
bbs_huc$rteNo <- sapply(ns, '[', 1)
bbs_huc$Stop <- sapply(ns, '[', 2)

# Get most common HUC from each rteNo
# Throw out ones that do not have at least a strict majority (>25) within each watershed. This barely throws out any for HUC4, and few for HUC8, but won't work for HUC12. For now, let's just not include HUC12.

huc48summary <- function (x) {
	t4 <- table(x$HUC4)
	t8 <- table(x$HUC8)
	h4 <- names(t4)[which.max(t4)[1]]
	h8 <- names(t8)[which.max(t8)[1]]
	n4 <- max(t4)
	n8 <- max(t8)
	return(data.frame(HUC4 = h4, HUC8 = h8, nstops4 = n4, nstops8 = n8))
}

huctable <- bbs_huc %>% group_by(rteNo) %>% do(huc48summary(.))

# merge bbs_div_byroute and huctable

bbs_div_byroute <- left_join(bbs_div_byroute, huctable)

# Load the bbs stop locations.

library(rgdal)
library(dplyr)
library(stringr)

x <- readOGR(dsn = file.path(fp,'DATA/raw_data/BBS/bbs_stops'), layer = 'bbsStopsPRISMproj')

# Take route-level coordinate means.
rtelatlongs <- x@data %>% group_by(rteNo) %>% summarize(latitude = mean(POINT_Y), longitude = mean(POINT_X))

bbs_div_byroute <- bbs_div_byroute %>% mutate(rteNo = as.numeric(rteNo)) %>% left_join(rtelatlongs)
bbs_div_byroute <- arrange(bbs_div_byroute, year, rteNo)

write.csv(bbs_div_byroute, file = file.path(fp,'DATA/raw_data/BBS/bbs_div_byroute.csv'), row.names = FALSE)