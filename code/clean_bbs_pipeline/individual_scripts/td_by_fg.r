# Bird taxonomic diversity, split into different functional groups
# By HUC variables
# Updated 31 Jan 2018: use newer BBS data and combine by year.

birdtrait <- read.csv('/mnt/research/aquaxterra/DATA/raw_data/bird_traits/birdtraitmerged.csv', stringsAsFactors = FALSE)

birdtrait[birdtrait == -999] <- NA

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
# Load workspace with data pooled by the 11 years.
load('/mnt/research/aquaxterra/DATA/raw_data/BBS/bbsworkspace_singleyear.r')

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

fgrichness <- cbind(bbscov_oneyear, allrich, resrich)

write.csv(fgrichness, file = '/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_fgrichness_11years.csv', row.names = FALSE)
