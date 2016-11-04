# Read bird trait files in and get only the birds we care about (the North American ones, obv)
# Author: QDR
# Project: Aquaxterra
# Created: 03 Nov 2016
# Last modified: 04 Nov 2016

foraging <- read.delim('DATA/raw_data/bird_traits/foragingtraitdb/BirdFuncDat.txt', stringsAsFactors = FALSE)
lifehist <- read.csv('DATA/raw_data/bird_traits/lifehistorydb/Amniote_Database_Aug_2015.csv', stringsAsFactors = FALSE)
bbsspp <- read.csv('DATA/raw_data/bird_traits/specieslist.csv', stringsAsFactors = FALSE)

# Subset trait databases for our species
lifehist$Scientific <- with(lifehist, paste(genus, species))

lifehistmatch <- bbsspp$Latin_Name_clean %in% lifehist$Scientific | bbsspp$Latin_Name_synonym %in% lifehist$Scientific | bbsspp$Latin_Name_synonym2 %in% lifehist$Scientific
foragingmatch <- bbsspp$Latin_Name_clean %in% foraging$Scientific | bbsspp$Latin_Name_synonym %in% foraging$Scientific | bbsspp$Latin_Name_synonym2 %in% foraging$Scientific

bbsspp$Latin_Name[!lifehistmatch]
bbsspp$Latin_Name[!foragingmatch]

foraging$AOU <- NA
lifehist$AOU <- NA

for (i in 1:nrow(bbsspp)) {
	foraging$AOU[(bbsspp$Latin_Name_clean[i] == foraging$Scientific | bbsspp$Latin_Name_synonym[i] == foraging$Scientific | bbsspp$Latin_Name_synonym2[i] == foraging$Scientific)] <- bbsspp$AOU[i]
	lifehist$AOU[(bbsspp$Latin_Name_clean[i] == lifehist$Scientific | bbsspp$Latin_Name_synonym[i] == lifehist$Scientific | bbsspp$Latin_Name_synonym2[i] == lifehist$Scientific)] <- bbsspp$AOU[i]
}

library(dplyr)

bbsspp <- left_join(bbsspp, foraging[,-(1:9)], by='AOU')
bbsspp <- left_join(bbsspp, lifehist[,-(1:7)] %>% select(-Scientific), by='AOU')

write.csv(bbsspp, file='DATA/raw_data/bird_traits/birdtraitmerged.csv', row.names=FALSE)