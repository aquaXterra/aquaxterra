# Read bird trait files in and get only the birds we care about (the North American ones, obv)
# Author: QDR
# Project: Aquaxterra
# Created: 03 Nov 2016
# Last modified: 18 Nov 2016

# Modified 18 Nov: Fixed three bad species with hard-coded patch. This makes the distance matrix work! yay
# Modified 8 Nov: assign AOU and give some of the unresolved species a trait as well

foraging <- read.delim('DATA/raw_data/bird_traits/foragingtraitdb/BirdFuncDat.txt', stringsAsFactors = FALSE)
lifehist <- read.csv('DATA/raw_data/bird_traits/lifehistorydb/Amniote_Database_Aug_2015.csv', stringsAsFactors = FALSE)
bbsspp <- read.csv('DATA/raw_data/bird_traits/specieslist.csv', stringsAsFactors = FALSE)

# Assign AOU
library(stringr)
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

library(dplyr)

bbsspp <- left_join(bbsspp, foraging[,-(1:9)], by='AOU')
bbsspp <- left_join(bbsspp, lifehist[,-(1:7)] %>% select(-Scientific), by='AOU')

write.csv(bbsspp, file='DATA/raw_data/bird_traits/birdtraitmerged.csv', row.names=FALSE)

# Added 18 Nov: Add the three species that don't have foraging traits to this master dataset and save it again.
# Streptopelia, Buteo nitidus, and Geranoaetus

bbsspp <- read.csv('DATA/raw_data/bird_traits/birdtraitmerged.csv', stringsAsFactors=FALSE)

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

write.csv(bbsspp, file='DATA/raw_data/bird_traits/birdtraitmerged.csv', row.names=FALSE)

# Edited again on 07 Dec: Two other species don't have foraging traits. Replace them with the foraging traits of their congeners and save again.
# Motacilla tschutschensis and Artemisiospiza nevadensis.
motacilla <- foraging[grep('Motacilla', foraging$Scientific), ]
motacillamean <- lapply(motacilla, function(x) if (is.numeric(x)) mean(x) else names(table(x))[table(x) == max(table(x))][1])
motacillamean <- as.data.frame(motacillamean)

artemisiospiza <- foraging[grep('Amphispiza belli', foraging$Scientific), ]

bbsspp[bbsspp$AOU == 6960, 15:45] <- motacillamean[1, 10:40]
bbsspp[bbsspp$AOU == 5738, 15:45] <- artemisiospiza[1, 10:40]

write.csv(bbsspp, file='/mnt/research/aquaxterra/DATA/raw_data/bird_traits/birdtraitmerged.csv', row.names=FALSE)


# Modification 3 March 2017: add the migrant status column to the CORRECT bird trait data frame.

# Fish and Wildlife's list of migratory birds
migrants <- read.csv('DATA/raw_data/bird_traits/migratorybirds.csv', stringsAsFactors = F)

# Get rid of asterisks in the Latin name strings, and spaces at the end. 
# trim.trailing <- function (x) sub("\\s+$", "", x)
# migrants$Latin.name <- sub('\\*', '', migrants$Latin.name)
# migrants$Latin.name <- trim.trailing(migrants$Latin.name)

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
write.csv(bbsspp, '/mnt/research/aquaxterra/DATA/raw_data/bird_traits/birdtraitmerged.csv', row.names = FALSE)