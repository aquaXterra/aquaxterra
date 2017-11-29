# Compile all BBS presence/absence by-route alpha diversity metrics.
# PD, FD, and TD
# Do this for all birds, and for resident birds only.

# Modified 29 Nov 2017: added huc12 summaries; did both majority-rule summary and averaging any route that has any stops in each watershed.

### ALL BIRDS ###

# Average the PD metrics from the 10 different trees.
pd_byroute_all <- list()

for (i in 1:10) {
	load(file = paste0('/mnt/research/aquaxterra/DATA/raw_data/bird_traits/bird_phylogeny/pd_tree_presence_',i,'_byroute.r'))
	pd_byroute_all[[i]] <- list(pd_ericson, mpd_ericson, mntd_ericson)
}

# Combine each of the 10 list entries into a single data frame.
pd_byroute_all <- lapply(pd_byroute_all, function(x) cbind(x[[1]], x[[2]][, -c(1,8)], x[[3]][, -c(1,8)]))
# This list can be used for the uncertainty calculations later on. But to avoid opening that can of worms at the moment, I'll take the elementwise mean.

pd_byroute_mean <- Reduce('+', pd_byroute_all)/length(pd_byroute_all)

# Load FD
load('/mnt/research/aquaxterra/DATA/raw_data/bird_traits/birdfuncdivobjectbyroutepresence.r')

# Correct FD for zero rows

birdtrait <- read.csv('/mnt/research/aquaxterra/DATA/raw_data/bird_traits/birdtraitmerged.csv', stringsAsFactors = FALSE)

birdtrait[birdtrait == -999] <- NA
nocturnalbirdAOUs <- birdtrait$AOU[birdtrait$Nocturnal == 1]
birdtrait_diurnal <- subset(birdtrait, Nocturnal != 1 | is.na(Nocturnal))

# Select traits to use
# Ones that seem important and/or have a lot of records.
# Includes diet, foraging strategy, body size, and life history.
traitnames <- names(birdtrait)[c(15:24, 29:36, 46:50, 53, 55:59)]

# Use the consolidated matrix that was used for the phylogenetic diversity calculations.
load('/mnt/research/aquaxterra/DATA/raw_data/BBS/bbsmatconsolidated2015.r') # Load fixed bbsmat.

ns <- colSums(fixedbbsmat_byroute)
fixedbbsmat_byroute_nonzero <- fixedbbsmat_byroute[, ns > 0 & !(sppids %in% nocturnalbirdAOUs)]
sppids_nonzero <- sppids[ns > 0 & !(sppids %in% nocturnalbirdAOUs)]

# Clean trait matrix and sort trait and sitexsp matrices so their dimensions all match.
dimnames(fixedbbsmat_byroute_nonzero)[[2]] <- sppids_nonzero # Already sorted by AOU
birdtraitclean <- birdtrait_diurnal[match(sppids_nonzero, birdtrait_diurnal$AOU), traitnames]
dimnames(birdtraitclean)[[1]] <- sppids_nonzero

# Make sure all columns are numerics, even the binary variables. (nocturnal is no longer included here)
birdtraitclean <- transform(birdtraitclean, PelagicSpecialist = as.numeric(PelagicSpecialist))

# Remove rows where no birds at all were found, flagging them for later. Probably none for entire routes.
rs <- apply(fixedbbsmat_byroute_nonzero, 1, sum)
fixedbbsmat_byroute_nonzerorows <- fixedbbsmat_byroute_nonzero[rs > 0,]



bfd_df_byroute <- with(fd_all, data.frame(nbsp, sing.sp, FRic, FEve, FDiv, FDis, RaoQ))

fd_all_byroute <- data.frame(bbsgrps_byroute, nbsp=0, sing.sp=0, FRic=NA, FEve=NA, FDiv=NA, FDis=NA, RaoQ=NA)
fd_all_byroute[rs != 0, 3:9] <- bfd_df_byroute


# TD consists only of richness (already in the FD dataframe)
# Combine all 3 metrics into one data frame.

bbs_div_byroute <- cbind(bbsgrps_byroute, richness=fd_all_byroute$nbsp, fd_all_byroute[, c('FRic','FEve', 'FDiv', 'FDis', 'RaoQ')], pd_byroute_mean[, c(1, 3:14)])


### RESIDENT BIRDS ###

# Average the PD metrics from the 10 different trees.
pd_byroute_allr <- list()

for (i in 1:10) {
	load(file = paste0('/mnt/research/aquaxterra/DATA/raw_data/bird_traits/bird_phylogeny/pd_tree_residentpresence_',i,'_byroute.r'))
	pd_byroute_allr[[i]] <- list(pd_ericson, mpd_ericson, mntd_ericson)
}

# Combine each of the 10 list entries into a single data frame.
pd_byroute_allr <- lapply(pd_byroute_allr, function(x) cbind(x[[1]], x[[2]][, -c(1,8)], x[[3]][, -c(1,8)]))
# This list can be used for the uncertainty calculations later on. But to avoid opening that can of worms at the moment, I'll take the elementwise mean.

pd_byroute_meanr <- Reduce('+', pd_byroute_allr)/length(pd_byroute_allr)

# Load FD
load('/mnt/research/aquaxterra/DATA/raw_data/bird_traits/birdfuncdivobjectbyrouteresidentpresence.r')

# Correct FD for zero rows
migrantAOUs <- birdtrait$AOU[birdtrait$migrant_status == 1]
birdtrait_diurnal <- subset(birdtrait, (Nocturnal != 1 | is.na(Nocturnal)) & migrant_status == 0)

fixedbbsmat_byroute_nonzero <- fixedbbsmat_byroute[, ns > 0 & !(sppids %in% nocturnalbirdAOUs) & !(sppids %in% migrantAOUs)]
sppids_nonzero <- sppids[ns > 0 & !(sppids %in% nocturnalbirdAOUs) & !(sppids %in% migrantAOUs)]

# Clean trait matrix and sort trait and sitexsp matrices so their dimensions all match.
dimnames(fixedbbsmat_byroute_nonzero)[[2]] <- sppids_nonzero # Already sorted by AOU
birdtraitclean <- birdtrait_diurnal[match(sppids_nonzero, birdtrait_diurnal$AOU), traitnames]
dimnames(birdtraitclean)[[1]] <- sppids_nonzero

# Make sure all columns are numerics, even the binary variables. (nocturnal is no longer included here)
birdtraitclean <- transform(birdtraitclean, PelagicSpecialist = as.numeric(PelagicSpecialist))

# Remove rows where no birds at all were found, flagging them for later. Probably none for entire routes.
rs <- apply(fixedbbsmat_byroute_nonzero, 1, sum)
fixedbbsmat_byroute_nonzerorows <- fixedbbsmat_byroute_nonzero[rs > 0,]

bfd_df_byroute <- with(fd_all, data.frame(nbsp, sing.sp, FRic, FEve, FDiv, FDis, RaoQ))

fd_all_byrouter <- data.frame(bbsgrps_byroute, nbsp=0, sing.sp=0, FRic=NA, FEve=NA, FDiv=NA, FDis=NA, RaoQ=NA)
fd_all_byrouter[rs != 0, 3:9] <- bfd_df_byroute

bbs_div_byrouteres <- cbind(bbsgrps_byroute, richness=fd_all_byrouter$nbsp, fd_all_byrouter[, c('FRic','FEve', 'FDiv', 'FDis', 'RaoQ')], pd_byroute_meanr[, c(1, 3:14)])

### GEOREFERENCE ###

bbs_huc <- read.csv('/mnt/research/aquaxterra/CODE/python/BBSSpatialJoin/BBS_SpatialJoin_Final.csv', stringsAsFactors = FALSE)

ns <- strsplit(bbs_huc$rtestopNo, '-')
bbs_huc$rteNo <- sapply(ns, '[', 1)
bbs_huc$Stop <- sapply(ns, '[', 2)

# Get most common HUC from each rteNo
# Throw out ones that do not have at least a strict majority (>25) within each watershed. This barely throws out any for HUC4, and few for HUC8, but won't work for HUC12. For now, let's just not include HUC12.
# Modification 27 Nov: add huc12 too (quick and dirty)
# Modification 28 Nov: also add fn to make list of ALL hucs that each route enters (for mapping purposes)

huc4812summary <- function (x) {
	t4 <- table(x$HUC4)
	t8 <- table(x$HUC8)
	t12 <- table(x$HUC12)
	h4 <- names(t4)[which.max(t4)[1]]
	h8 <- names(t8)[which.max(t8)[1]]
	h12 <- names(t12)[which.max(t12)[1]]
	n4 <- max(t4)
	n8 <- max(t8)
	n12 <- max(t12)
	return(data.frame(HUC4 = h4, HUC8 = h8, HUC12 = h12, nstops4 = n4, nstops8 = n8, nstops12 = n12))
}

library(dplyr)

huctable <- bbs_huc %>% group_by(rteNo) %>% do(huc4812summary(.))
huclist <- bbs_huc %>% group_by(rteNo) %>% do(HUC4list = unique(.$HUC4), HUC8list = unique(.$HUC8), HUC12list = unique(.$HUC12))

# merge bbs_div_byroute and huctable

bbs_div_byroute <- left_join(bbs_div_byroute, huctable) %>% left_join(huclist)
bbs_div_byrouteres <- left_join(bbs_div_byrouteres, huctable) %>% left_join(huclist)

# Edit 29 Nov.: Try to join all the hucs so that the bbs data are replicated across rows.
bbs_div_allhucs <- left_join(bbs_huc, bbs_div_byroute)
bbs_div_allhucsres <- left_join(bbs_huc, bbs_div_byrouteres)

# Load the bbs stop locations.

library(rgdal)
library(stringr)

x <- readOGR(dsn = '/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_stops', layer = 'bbsStopsPRISMproj')

# Take route-level coordinate means.
rtelatlongs <- x@data %>% group_by(rteNo) %>% summarize(latitude = mean(POINT_Y), longitude = mean(POINT_X))

bbs_div_byroute <- bbs_div_byroute %>% mutate(rteNo = as.numeric(rteNo)) %>% left_join(rtelatlongs)
bbs_div_byroute <- arrange(bbs_div_byroute, year, rteNo)

bbs_div_byrouteres <- bbs_div_byrouteres %>% mutate(rteNo = as.numeric(rteNo)) %>% left_join(rtelatlongs)
bbs_div_byrouteres <- arrange(bbs_div_byrouteres, year, rteNo)


save(bbs_div_byroute, bbs_div_byrouteres, file = '/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_div_byroute_presence.r')

bbs_div_allhucs <- bbs_div_allhucs %>%
  mutate(rteNo = as.numeric(rteNo)) %>%
  left_join(rtelatlongs) %>%
  arrange(year, rteNo)
bbs_div_allhucsres <- bbs_div_allhucsres %>%
  mutate(rteNo = as.numeric(rteNo)) %>%
  left_join(rtelatlongs) %>%
  arrange(year, rteNo)

save(bbs_div_allhucs, bbs_div_allhucsres, file = '/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_div_byroute_presence_allhucs.r')
