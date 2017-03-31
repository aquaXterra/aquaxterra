# Bird taxonomic diversity, split into different functional groups
# By HUC variables
# If this looks interesting, I will put more work into functional and phylogenetic diversity

birdtrait <- read.csv('/mnt/research/aquaxterra/DATA/raw_data/bird_traits/birdtraitmerged.csv', stringsAsFactors = FALSE)

birdtrait[birdtrait == -999] <- NA

# Identify nocturnal and resident.
nocturnalbirdAOUs <- birdtrait$AOU[birdtrait$Nocturnal == 1]
migrantAOUs <- birdtrait$AOU[birdtrait$migrant_status == 1]

# Select the five functional groups.
birdtrait$FG <- birdtrait$Diet.5Cat
birdtrait$FG[birdtrait$ForStrat.watbelowsurf > 0 | birdtrait$ForStrat.wataroundsurf > 0 | birdtrait$PelagicSpecialist == 1] <- 'Waterbird'

# Get rid of nocturnal and the few fruit and nectar-eaters (sorry, waxwings, owls and hummingbirds).
birdtrait$FG[birdtrait$Nocturnal == 1 | is.na(birdtrait$Nocturnal)] <- NA
birdtrait$FG[birdtrait$FG == 'FruiNect' | birdtrait$Diet.5Cat == '1'] <- NA

# Get species richness within each functional group for each route. Do this separately for all birds, and for all resident birds.
load('/mnt/research/aquaxterra/DATA/raw_data/BBS/bbsmatconsolidated2015.r') # Load fixed bbsmat.

fgs <- unique(birdtrait$FG)
fgs <- fgs[!is.na(fgs)]

allrich <- resrich <- matrix(NA, nrow=nrow(fixedbbsmat_byroute), ncol = length(fgs))

for (i in 1:length(fgs)) {
	fgAOUs <- birdtrait$AOU[birdtrait$FG == fgs[i]]
	fgresAOUs <- fgAOUs[!fgAOUs %in% migrantAOUs]
	allrich[,i] <- apply(fixedbbsmat_byroute[, sppids %in% fgAOUs] > 0, 1, sum, na.rm = TRUE)
	resrich[,i] <- apply(fixedbbsmat_byroute[, sppids %in% fgresAOUs] > 0, 1, sum, na.rm = TRUE)
}

dimnames(allrich)[[2]] <- paste('total_richness', fgs, sep = '_')
dimnames(resrich)[[2]] <- paste('resident_richness', fgs, sep = '_')

fgrichness <- cbind(bbsgrps_byroute, allrich, resrich)
library(dplyr)
fgrichness <- arrange(fgrichness, year, rteNo)

write.csv(fgrichness, file = '/mnt/research/aquaxterra/DATA/raw_data/BBS/fgrichness.csv', row.names = FALSE)
