# Complete BBS data processing pipeline
# AquaXTerra WaterCube project
# QDR

# This code was tested successfully with R version 3.2.1 on 1 February 2018.

# -----------------------------
# LIST OF NECESSARY INPUT FILES
# -----------------------------

# Locally, these files are in /mnt/research/aquaxterra/DATA_PAPER/BBS which should be set as the working directory.

# 1. BBS raw data files downloaded from USGS (fifty1.csv through fifty10.csv)
# 2. Manually corrected BBS species list linking AOUs to all possible scientific names for the species (specieslist.csv)
# 3. List of 1000 phylogenetic trees downloaded from birdtree.org (ericson1000.tre)
# 4. Functional traits from EltonTraits (BirdFuncDat.txt)
# 5. Functional traits from Amniote Life History Database (Amniote_Database_Aug_2015.csv)
# 6. List of species by migratory status from US Fish & Wildlife (migratorybirds.csv)
# 7. Coordinates of BBS route centroids in Albers equal-area projection (bbs_correct_route_centroids.csv)
# 8. Script with function that will calculate alpha diversity for each row of the site by species matrix (diversity_3ways.r)
# 9. Spatial join of all BBS stop locations with hydrologic units HUC4, HUC8, and HUC12 (BBS_SpatialJoin_Final.csv)

# ----------------------------------
# LOAD BBS DATA DOWNLOADED FROM USGS
# ----------------------------------

library(dplyr)
library(reshape2)
library(stringr)
library(ape)
library(phytools)
library(FD)
library(vegan)
library(picante)

fp <- './rawBBS'

bbsdf <- list()

for (i in 1:10) {
	bbsdf[[i]] <- read.csv(file.path(fp, paste0('fifty',i,'.csv')))
}

bbsdf <- do.call('rbind', bbsdf)

# -----------------------------
# CREATE SITE-BY-SPECIES MATRIX
# -----------------------------

# Use only surveys done with the normal protocol.
bbsdf <- subset(bbsdf, RPID == 101)

# Combine state number and route number into single rteNo
rteNo <- character(nrow(bbsdf))
for (i in 1:nrow(bbsdf)) {
	rteNo[i] <- with(bbsdf, paste(statenum[i], paste(rep('0', 3-nchar(as.character(Route[i]))), collapse=''), Route[i], sep = ''))
}

bbsdf$rteNo <- rteNo

# Convert to long format
bbslong <- melt(bbsdf, id.vars = grep('Stop', names(bbsdf), invert=TRUE), variable.name = 'Stop', value.name = 'n')
bbslong <- bbslong %>% filter(n>0) # Get rid of zero rows.

# Convert to a site by species matrix (site is a route by year combination)
sppids <- sort(unique(bbsdf$AOU))
get_spp <- function(dat, sppids) {
  ns <- dat$n
  idx <- match(dat$AOU, sppids)
  res <- rep(0, length(sppids))
  res[idx] <- ns
  res
}

bbsmat_byroute <- bbslong %>% group_by(year, rteNo) %>% do(s = get_spp(., sppids=sppids))
bbsgrps_byroute <- select(bbsmat_byroute, year, rteNo)
bbsmat_byroute <- do.call('rbind', bbsmat_byroute$s)

# ----------------------------------------------------------------------------
# LOAD CORRECTED SPECIES LIST AND USE IT TO CONSOLIDATE SITE-BY-SPECIES MATRIX
# ----------------------------------------------------------------------------

bbsspp <- read.csv('specieslist.csv', stringsAsFactors = FALSE)

# First, assign all species not in phylogeny to a species in the phylogeny

AOU_lists <- lapply(str_extract_all(bbsspp$AOU_list, pattern = '[0-9]+'), as.numeric)
resample <- function(x, ...) x[sample.int(length(x), ...)]

toconsolidate <- bbsspp$AOU[sapply(AOU_lists,length)>0]
idups <- sppids %in% toconsolidate

# Aggregation by route.
fixedbbsmat_byroute <- list()
pb <- txtProgressBar(0, nrow(bbsmat_byroute), style=3)

for (j in 1:nrow(bbsmat_byroute)) {

setTxtProgressBar(pb,j)

x <- bbsmat_byroute[j,]

# Reassign species randomly (must be done every time)
set.seed(688)
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

# Quick correction to fix two birds that aren't in the phylogeny. Just get rid of the eastern yellow wagtail since it's probably only in Alaska anyway.
fixedbbsmat_byroute[, which(sppids == 5739)] <- fixedbbsmat_byroute[, which(sppids == 5738)] + fixedbbsmat_byroute[, which(sppids == 5739)]
fixedbbsmat_byroute[, which(sppids == 5738)] <- 0
fixedbbsmat_byroute[, which(sppids == 6960)] <- 0

# ---------------------------------------------------
# LOAD PHYLOGENETIC TREES AND GENERATE CONSENSUS TREE
# ---------------------------------------------------

# Load 1000 ericson trees
erictree <- read.nexus('ericson1000.tre')
eric_cons_edges <- consensus.edges(trees = erictree, method = 'least.squares')
eric_cons_tree <- reroot(eric_cons_edges, node.number = 1226, position = 0)

# ------------------------------------------------
# LOAD FUNCTIONAL TRAIT INFORMATION AND COMBINE IT
# ------------------------------------------------

foraging <- read.delim('BirdFuncDat.txt', stringsAsFactors = FALSE)
lifehist <- read.csv('Amniote_Database_Aug_2015.csv', stringsAsFactors = FALSE)

# Assign AOU
AOU_lists <- lapply(str_extract_all(bbsspp$AOU_list, pattern = '[0-9]+'), as.numeric)

resample <- function(x, ...) x[sample.int(length(x), ...)]

AOU_final <- bbsspp$AOU
for (i in 1:length(AOU_lists)) {
  if (length(AOU_lists[[i]]) > 0) AOU_final[i] <- resample(AOU_lists[[i]], 1)
}

# Subset trait databases for our species
lifehist$Scientific <- with(lifehist, paste(genus, species))

lifehistmatch <- bbsspp$Latin_Name_clean %in% lifehist$Scientific | bbsspp$Latin_Name_synonym %in% lifehist$Scientific | bbsspp$Latin_Name_synonym2 %in% lifehist$Scientific
foragingmatch <- bbsspp$Latin_Name_clean %in% foraging$Scientific | bbsspp$Latin_Name_synonym %in% foraging$Scientific | bbsspp$Latin_Name_synonym2 %in% foraging$Scientific

bbsspp$Latin_Name[!lifehistmatch]
bbsspp$Latin_Name[!foragingmatch]

foraging$AOU <- NA
lifehist$AOU <- NA

for (i in 1:nrow(bbsspp)) {
	foraging$AOU[(bbsspp$Latin_Name_clean[i] == foraging$Scientific | bbsspp$Latin_Name_synonym[i] == foraging$Scientific | bbsspp$Latin_Name_synonym2[i] == foraging$Scientific)] <- AOU_final[i]
	lifehist$AOU[(bbsspp$Latin_Name_clean[i] == lifehist$Scientific | bbsspp$Latin_Name_synonym[i] == lifehist$Scientific | bbsspp$Latin_Name_synonym2[i] == lifehist$Scientific)] <- AOU_final[i]
}

# Match the AOUs
bbsspp$AOU <- AOU_final

bbsspp <- left_join(bbsspp, foraging[,-(1:9)], by='AOU')
bbsspp <- left_join(bbsspp, lifehist[,-(1:7)] %>% select(-Scientific), by='AOU')

# Use mean genus values for Streptopelia, the correct species for Buteo nitidus, and the only other Geranoaetus sp. for Geranoaetus.
streptopelia <- foraging[grep('Streptopelia', foraging$Scientific), ]
streptopeliamean <- sapply(streptopelia, function(x) if (is.numeric(x)) mean(x) else names(table(x))[table(x) == max(table(x))][1])
streptopeliamean <- as.data.frame(t(streptopeliamean))
buteo_nitidus <- foraging[grep('Buteo nitidus', foraging$Scientific), ]
geranoaetus <- foraging[grep('Geranoaetus', foraging$Scientific), ]

# This is hard-coded but it's the best way to fix this annoying problem for only three rows.
bbsspp[bbsspp$AOU == 3151, 15:45] <- streptopeliamean[10:40]
bbsspp[bbsspp$AOU == 3410, 15:45] <- geranoaetus[1, 10:40]
bbsspp[bbsspp$AOU == 3460, 15:45] <- buteo_nitidus[1, 10:40]

# Edited again on 07 Dec: Two other species don't have foraging traits. Replace them with the foraging traits of their congeners and save again.
# Motacilla tschutschensis and Artemisiospiza nevadensis.
motacilla <- foraging[grep('Motacilla', foraging$Scientific), ]
motacillamean <- lapply(motacilla, function(x) if (is.numeric(x)) mean(x) else names(table(x))[table(x) == max(table(x))][1])
motacillamean <- as.data.frame(motacillamean)

artemisiospiza <- foraging[grep('Amphispiza belli', foraging$Scientific), ]

bbsspp[bbsspp$AOU == 6960, 15:45] <- motacillamean[1, 10:40]
bbsspp[bbsspp$AOU == 5738, 15:45] <- artemisiospiza[1, 10:40]

# Fish and Wildlife's list of migratory birds
migrants <- read.csv('migratorybirds.csv', stringsAsFactors = F)

# Get rid of asterisks in the Latin name strings, and spaces at the end. 
migrants$Latin.name <- sub('\xa0', '', migrants$Latin.name)

migrants <- subset(migrants, Latin.name != '')
migrants$Latin.name %in% bbsspp$Latin_Name_clean
migrants$Latin.name[!migrants$Latin.name %in% bbsspp$Latin_Name_clean]
migrants$Latin.name[!migrants$Latin.name %in% bbsspp$Latin_Name_synonym]

bbsspp$Latin_Name_clean[bbsspp$Latin_Name_clean == ''] <- NA
bbsspp$Latin_Name_synonym[bbsspp$Latin_Name_synonym == ''] <- NA
bbsspp$Latin_Name_synonym2[bbsspp$Latin_Name_synonym2 == ''] <- NA


ismigrant <- bbsspp$Latin_Name_clean %in% migrants$Latin.name | bbsspp$Latin_Name_synonym %in% migrants$Latin.name | bbsspp$Latin_Name_synonym2 %in% migrants$Latin.name

bbsspp$migrant_status <- ismigrant
birdtrait <- bbsspp

# ------------------------------------------------------------------------------
# PREPARE SITE-BY-SPECIES MATRIX AND DISTANCE MATRICES FOR DIVERSITY CALCULATION
# ------------------------------------------------------------------------------

bbsalbers <- read.csv('bbs_correct_route_centroids.csv')

ericdist <- cophenetic(eric_cons_tree)

birdtrait[birdtrait == -999] <- NA
nocturnalbirdAOUs <- birdtrait$AOU[birdtrait$Nocturnal == 1]
birdtrait_diurnal <- subset(birdtrait, Nocturnal != 1 | is.na(Nocturnal))
traitnames <- names(birdtrait)[c(15:24, 29:36, 46:50, 53, 55:59)]

birdtraitdist <- as.matrix(gowdis(birdtrait_diurnal[, traitnames]))
dimnames(birdtraitdist)[[1]] <- dimnames(birdtraitdist)[[2]] <- birdtrait_diurnal$Latin_Name

bbsgrps_byroute <- bbsgrps_byroute %>% mutate(rteNo=as.numeric(rteNo)) %>% left_join(bbsalbers, by = 'rteNo')
has_coords <- !is.na(bbsgrps_byroute$lon)

# Set dimnames of community matrix, trait distance matrix, and phylogenetic distance matrix to match.

dimnames_matrix <- rep(NA, length(sppids))
for (i in 1:length(sppids)) {
	names_i <- bbsspp[bbsspp$AOU == sppids[i], c('Latin_Name')]
	if (length(names_i)>0) dimnames_matrix[i] <- names_i[1]
}

dimnames(fixedbbsmat_byroute)[[2]] <- dimnames_matrix

tlabel1 <- eric_cons_tree$tip.label
tlabel1 <- gsub('_', ' ', tlabel1)

phymatchidx <- rep(NA,length(tlabel1))

for (i in 1:length(tlabel1)) {
	phymatchidx[i] <- c(which(bbsspp$Latin_Name_clean == tlabel1[i] | bbsspp$Latin_Name_synonym == tlabel1[i] | bbsspp$Latin_Name_synonym2 == tlabel1[i]), NA)[1]
}
tlabelaou <- bbsspp$AOU[phymatchidx]

dimnames_tlabel <- rep(NA, length(tlabelaou))
for (i in 1:length(tlabelaou)) {
	names_i <- bbsspp[bbsspp$AOU == tlabelaou[i], c('Latin_Name')]
	if (length(names_i)>0) dimnames_tlabel[i] <- names_i[1]
}

dimnames(ericdist)[[1]] <- dimnames(ericdist)[[2]] <- dimnames_tlabel

# Remove nocturnal species, rows without coordinates, and rows and columns with zero sum.

ns <- colSums(fixedbbsmat_byroute)
rs <- rowSums(fixedbbsmat_byroute)
nocturnalbirds <- birdtrait$Latin_Name[birdtrait$Nocturnal == 1]
fixedbbsmat_byroute <- fixedbbsmat_byroute[has_coords & rs != 0, !(dimnames(fixedbbsmat_byroute)[[2]] %in% nocturnalbirds) & ns != 0]

bbsgrps_byroute <- bbsgrps_byroute[has_coords & rs != 0, ]

names(bbsgrps_byroute) <- c('year','rteNo','lon','lat','lon_aea','lat_aea')
bbscov <- bbsgrps_byroute

# Combine 2001-2011 into a single year.
consolidate_years <- function(x) {
	mat_x <- fixedbbsmat_byroute[x$rowidx, , drop = FALSE]
	as.numeric(apply(mat_x, 2, sum) > 0)
}

bbs_consol <- bbscov %>%
	mutate(rowidx = 1:nrow(bbscov)) %>%
	filter(year >= 2001 & year <= 2011) %>%
	group_by(rteNo, lon, lat, lon_aea, lat_aea) %>%
	do(x = consolidate_years(.))
	
bbsmat_byroute_oneyear <- do.call('rbind', bbs_consol$x)
dimnames(bbsmat_byroute_oneyear)[[2]] <- dimnames(fixedbbsmat_byroute)[[2]]

bbscov_oneyear <- bbs_consol %>% select(-x)
bbscovmat_oneyear <- as.matrix(bbscov_oneyear)

# Create another matrix with all the migratory birds removed.
migrantbirds <- birdtrait$Latin_Name[birdtrait$migrant_status == TRUE]
bbsmat_byroute_oneyear_residents <- bbsmat_byroute_oneyear[, !dimnames(bbsmat_byroute_oneyear)[[2]] %in% migrantbirds]

# -----------------------------------------------------------
# CALCULATE TAXONOMIC, FUNCTIONAL, AND PHYLOGENETIC DIVERSITY
# -----------------------------------------------------------

source('diversity_3ways.r')

nnull <- 99

alpha_div <- diversity_3ways(m = bbsmat_byroute_oneyear, flavor = 'alpha', 
                             dotd=T, dopd=T, dofd=T, abundance=F,
                             pddist = ericdist, fddist = birdtraitdist,
                             nnull = nnull,
                             phylo_spp = NULL, func_problem_spp = NULL, combine = FALSE)
alpha_div_residents <- diversity_3ways(m = bbsmat_byroute_oneyear_residents, flavor = 'alpha', 
                             dotd=T, dopd=T, dofd=T, abundance=F,
                             pddist = ericdist, fddist = birdtraitdist,
                             nnull = nnull,
                             phylo_spp = NULL, func_problem_spp = NULL, combine = FALSE)

# Get only the columns containing data.
cnames <- c('richness', 'MPD_pa_z', 'MNTD_pa_z', 'MPDfunc_pa_z', 'MNTDfunc_pa_z')
							 
bbs_alpha <- cbind(bbscov_oneyear, alpha_div[, cnames])
bbs_alpha_res <- cbind(bbscov_oneyear, alpha_div_residents[, cnames])

# -----------------------------------
# CALCULATE FUNCTIONAL GROUP RICHNESS
# -----------------------------------

# Identify nocturnal and resident.
nocturnalbirdnames <- birdtrait$Latin_Name[birdtrait$Nocturnal == 1]
migrantnames <- birdtrait$Latin_Name[birdtrait$migrant_status == 1]

# Select the five functional groups.
birdtrait$FG <- birdtrait$Diet.5Cat
birdtrait$FG[birdtrait$ForStrat.watbelowsurf > 0 | birdtrait$ForStrat.wataroundsurf > 0 | birdtrait$PelagicSpecialist == 1] <- 'Waterbird'

# Get rid of nocturnal and the few fruit and nectar-eaters (sorry, waxwings, owls and hummingbirds).
birdtrait$FG[birdtrait$Nocturnal == 1 | is.na(birdtrait$Nocturnal)] <- NA
birdtrait$FG[birdtrait$FG == 'FruiNect' | birdtrait$Diet.5Cat == '1'] <- NA

# Get species richness within each functional group for each route. Do this separately for all birds, and for all resident birds.
fgs <- unique(birdtrait$FG)
fgs <- fgs[!is.na(fgs)]

allrich <- resrich <- matrix(NA, nrow=nrow(bbsmat_byroute_oneyear), ncol = length(fgs))

for (i in 1:length(fgs)) {
	fgnames <- birdtrait$Latin_Name[birdtrait$FG == fgs[i]]
	fgresnames <- fgnames[!fgnames %in% migrantnames]
	allrich[,i] <- apply(bbsmat_byroute_oneyear[, dimnames(bbsmat_byroute_oneyear)[[2]] %in% fgnames] > 0, 1, sum, na.rm = TRUE)
	resrich[,i] <- apply(bbsmat_byroute_oneyear[, dimnames(bbsmat_byroute_oneyear)[[2]] %in% fgresnames] > 0, 1, sum, na.rm = TRUE)
}

dimnames(allrich)[[2]] <- paste('total_richness', fgs, sep = '_')
dimnames(resrich)[[2]] <- paste('resident_richness', fgs, sep = '_')

bbs_fgrich <- cbind(bbscov_oneyear, allrich, resrich)

# ----------------------------------------------------------
# SUMMARIZE DIVERSITY BY HYDROLOGIC UNIT (HUC4, HUC8, HUC12)
# ----------------------------------------------------------

names(bbs_alpha_res)[-(1:5)] <- paste('resident', names(bbs_alpha_res)[-(1:5)], sep = '_')

bbs_alpha <- bbs_alpha %>% left_join(bbs_alpha_res) %>% left_join(bbs_fgrich)

# Read BBS+HUC spatial join and combine with BBS diversity data.

bbs_huc <- read.csv('/mnt/research/aquaxterra/DATA/HUCed_data/BBS/BBS_SpatialJoin_Final.csv', stringsAsFactors = FALSE, colClasses = c(HUC4 = 'character', HUC8 = 'character', HUC12 = 'character', lagoslakeid = 'character'))

ns <- strsplit(bbs_huc$rtestopNo, '-')
bbs_huc$rteNo <- sapply(ns, '[', 1)
bbs_huc$Stop <- sapply(ns, '[', 2)

# Merge bbs diversity with HUC for all HUCs that a route passes through
bbs_alpha_allhucs <- bbs_huc %>%
	mutate(rteNo = as.numeric(rteNo)) %>%
	left_join(select(bbs_alpha, -lon, -lat, -lon_aea, -lat_aea))

# Get the median values for all the diversity metrics for each HUC.
# Aggregation groups are HUC4, HUC8, HUC12, and lagoslakeid.
# Also do means to compare. In each case, it's weighted by how many stops in the route are in the HUC.
# Export them as CSVs.

bbs_div_huc4 <- bbs_alpha_allhucs %>%
  select(-rtestopNo, -lon, -lat, -HUC8, -HUC12, -lagoslakeid, -Stop, -rteNo) %>%
  group_by(HUC4) %>%
  summarize_all(.funs = median, na.rm = TRUE) 

bbs_div_huc4_mean <- bbs_alpha_allhucs %>%
  select(-rtestopNo, -lon, -lat, -HUC8, -HUC12, -lagoslakeid, -Stop, -rteNo) %>%
  group_by(HUC4) %>%
  summarize_all(.funs = mean, na.rm = TRUE) 
  
bbs_div_huc8 <- bbs_alpha_allhucs %>%
  select(-rtestopNo, -lon, -lat, -HUC4, -HUC12, -lagoslakeid, -Stop, -rteNo) %>%
  group_by(HUC8) %>%
  summarize_all(.funs = median, na.rm = TRUE) 

bbs_div_huc8_mean <- bbs_alpha_allhucs %>%
  select(-rtestopNo, -lon, -lat, -HUC4, -HUC12, -lagoslakeid, -Stop, -rteNo) %>%
  group_by(HUC8) %>%
  summarize_all(.funs = mean, na.rm = TRUE) 
  
bbs_div_huc12 <- bbs_alpha_allhucs %>%
  select(-rtestopNo, -lon, -lat, -HUC4, -HUC8, -lagoslakeid, -Stop, -rteNo) %>%
  group_by(HUC12) %>%
  summarize_all(.funs = median, na.rm = TRUE) 

bbs_div_huc12_mean <- bbs_alpha_allhucs %>%
  select(-rtestopNo, -lon, -lat, -HUC4, -HUC8, -lagoslakeid, -Stop, -rteNo) %>%
  group_by(HUC12) %>%
  summarize_all(.funs = mean, na.rm = TRUE) 

bbs_div_lagos <- bbs_alpha_allhucs %>%
  select(-rtestopNo, -lon, -lat, -HUC4, -HUC8, -HUC12, -Stop, -rteNo) %>%
  group_by(lagoslakeid) %>%
  summarize_all(.funs = median, na.rm = TRUE) 

bbs_div_lagos_mean <- bbs_alpha_allhucs %>%
  select(-rtestopNo, -lon, -lat, -HUC4, -HUC8, -HUC12, -Stop, -rteNo) %>%
  group_by(lagoslakeid) %>%
  summarize_all(.funs = mean, na.rm = TRUE)   
  
fp <- '.'
write.csv(bbs_div_huc4, file.path(fp, 'bbs_div_huc4_median.csv'), row.names = FALSE)
write.csv(bbs_div_huc4_mean, file.path(fp, 'bbs_div_huc4_mean.csv'), row.names = FALSE)
write.csv(bbs_div_huc8, file.path(fp, 'bbs_div_huc8_median.csv'), row.names = FALSE)
write.csv(bbs_div_huc8_mean, file.path(fp, 'bbs_div_huc8_mean.csv'), row.names = FALSE)
write.csv(bbs_div_huc12, file.path(fp, 'bbs_div_huc12_median.csv'), row.names = FALSE)
write.csv(bbs_div_huc12_mean, file.path(fp, 'bbs_div_huc12_mean.csv'), row.names = FALSE)
write.csv(bbs_div_lagos, file.path(fp, 'bbs_div_lagoslakeid_median.csv'), row.names = FALSE)
write.csv(bbs_div_lagos_mean, file.path(fp, 'bbs_div_lagoslakeid_mean.csv'), row.names = FALSE)
