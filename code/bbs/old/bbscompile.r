# Compile BBS taxonomic, functional, and phylogenetic diversity into a single data frame.
# Georeference all these values.
# 30 Nov 2016: only uses "naive" values for all those quantities

setwd('/mnt/research/aquaxterra/')

load('DATA/raw_data/bbstaxdiv.r')
load('DATA/raw_data/bird_traits/bird_phylogeny/pd_test_all.r')
load('DATA/raw_data/bird_traits/birdfuncdivobjectall.r')

bbs_div <- cbind(bbstaxdiv, fd_all[, c('FRic','FEve', 'FDiv', 'FDis', 'RaoQ')], pd_all[, c(4, 7:12, 14:19)])

# Save the diversity only, can be georeferenced later.
save(bbs_div, file = 'DATA/raw_data/BBS/bbs_div.r')

# Load the bbs stop locations.

library(rgdal)
library(dplyr)
library(stringr)

x <- readOGR(dsn = 'DATA/raw_data/BBS/bbs_stops', layer = 'bbsStopsPRISMproj')

bbs_div <- bbs_div %>% mutate(Stop = as.numeric(sapply(str_extract_all(Stop, '[0-9]'), paste, collapse = ''))) %>% rename(stopNo = Stop)
bbs_div <- bbs_div %>% mutate(rteNo = as.numeric(rteNo)) %>% left_join(x@data %>% select(rteNo,stopNo,POINT_X,POINT_Y))

write.csv(bbs_div, file = 'DATA/raw_data/BBS/bbs_div_georeferenced.csv', row.names = FALSE)