# Compile BBS taxonomic, functional, and phylogenetic diversity into a single data frame.
# Individual stop-level values.
# Georeference all these values.
# uses all available data through 2015, and uses mean of ten randomly selected phylogenetic trees for all the PDs.

fp <- '/mnt/research/aquaxterra/'

# Recalculate bbs taxonomic diversity, removing the nocturnal birds.

load(file.path(fp, 'DATA/raw_data/BBS/bbsmatconsolidated2015.r'))
birdtrait <- read.csv(file.path(fp, 'DATA/raw_data/bird_traits/birdtraitmerged.csv'), stringsAsFactors = FALSE)
birdtrait[birdtrait == -999] <- NA
nocturnalbirdAOUs <- birdtrait$AOU[birdtrait$Nocturnal == 1]

fixedbbsmat_diurnal <- fixedbbsmat[, !sppids %in% nocturnalbirdAOUs]

library(vegan)

bbsmat_richness <- apply(fixedbbsmat_diurnal > 0, 1, sum)
bbsmat_shannon <- diversity(fixedbbsmat_diurnal, index = 'shannon')
bbsmat_simpson <- diversity(fixedbbsmat_diurnal, index = 'simpson')
bbsmat_shannonevenness <- bbsmat_shannon/log(bbsmat_richness)

#bbs_taxdiv <- cbind(bbsgrps, richness=bbsmat_richness, shannon=bbsmat_shannon, simpson=bbsmat_simpson, shannonevenness=bbsmat_shannonevenness)
#save(bbs_taxdiv, file = file.path(fp, 'DATA/raw_data/BBS/bbs_taxdiv2015.r')

pd_all <- read.csv(file.path(fp,'DATA/raw_data/bird_traits/bird_phylogeny/pd_mean.csv'), stringsAsFactors = FALSE)
load(file.path(fp,'DATA/raw_data/bird_traits/birdfuncdivobjectall2015.r'))

bbs_div <- cbind(bbsgrps, richness=bbsmat_richness, shannon=bbsmat_shannon, simpson=bbsmat_simpson, shannonevenness=bbsmat_shannonevenness, fd_all[, c('FRic','FEve', 'FDiv', 'FDis', 'RaoQ')], pd_all[, c(1, 3:14)])

# Save the diversity only, can be georeferenced later.
#save(bbs_div_byroute, file = 'DATA/raw_data/BBS/bbs_div_byroute.r')

# Merge with HUCs

bbs_huc <- read.csv(file.path(fp,'CODE/python/BBSSpatialJoin/BBS_SpatialJoin_Final.csv'), stringsAsFactors = FALSE)

ns <- strsplit(bbs_huc$rtestopNo, '-')
bbs_huc$rteNo <- sapply(ns, '[', 1)
bbs_huc$Stop <- sapply(ns, '[', 2)

library(dplyr)

bbs_div <- bbs_div %>% mutate(rteNo = as.numeric(as.character(rteNo)), Stop = as.numeric(gsub('[a-zA-Z]', '', as.character(Stop))))
bbs_huc <- bbs_huc %>% mutate(rteNo = as.numeric(as.character(rteNo)), Stop = as.numeric(as.character(Stop))) 

bbs_div <- left_join(bbs_div, bbs_huc %>% select(rteNo, Stop, HUC4, HUC8, HUC12))

# Load the bbs stop locations.

library(rgdal)

x <- readOGR(dsn = file.path(fp,'DATA/raw_data/BBS/bbs_stops'), layer = 'bbsStopsPRISMproj')

# Take route-level coordinate means.
bbs_div <- bbs_div %>% left_join(x@data %>% select(rteNo, stopNo, POINT_X, POINT_Y) %>% rename(Stop = stopNo)) %>% arrange(year, rteNo, Stop)

write.csv(bbs_div, file = file.path(fp,'DATA/raw_data/BBS/bbs_div_georeferenced2015.csv'), row.names = FALSE)