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