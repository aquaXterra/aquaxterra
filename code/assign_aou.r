# Randomly assign AOUs to the different unidentified species, subspecies, and hybrids

bbsspp <- read.csv('data/specieslist.csv', stringsAsFactors = FALSE)

library(stringr)
AOU_lists <- lapply(str_extract_all(bbsspp$AOU_list, pattern = '[0-9]+'), as.numeric)

AOU_final <- bbsspp$AOU
for (i in 1:length(AOU_lists)) {
  if (length(AOU_lists[[i]]) > 0) AOU_final[i] <- sample(AOU_lists[[i]], 1)
}