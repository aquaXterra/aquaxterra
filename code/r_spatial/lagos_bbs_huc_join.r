# Load LAGOS data and combine with other datasets
# Author: QDR
# Project: Aquaxterra
# Created: 09 Dec 2016

library(dplyr)

fp <- '/mnt/research/aquaxterra/DATA/raw_data/LAGOS/LAGOS103'

hu4_chag <- read.delim(file.path(fp, 'hu4_chag.txt'))
hu4_conn <- read.delim(file.path(fp, 'hu4_conn.txt'))
hu4_lulc <- read.delim(file.path(fp, 'hu4_lulc.txt'))
hu4_etc <- read.delim(file.path(fp, 'hu4.txt'))

hu4_all <- hu4_etc %>% full_join(hu4_chag) %>% full_join(hu4_conn) %>% full_join(hu4_lulc)

hu8_chag <- read.delim(file.path(fp, 'hu8_chag.txt'))
hu8_conn <- read.delim(file.path(fp, 'hu8_conn.txt'))
hu8_lulc <- read.delim(file.path(fp, 'hu8_lulc.txt'))
hu8_etc <- read.delim(file.path(fp, 'hu8.txt'))

hu8_all <- hu8_etc %>% full_join(hu8_chag) %>% full_join(hu8_conn) %>% full_join(hu8_lulc)

hu12_chag <- read.delim(file.path(fp, 'hu12_chag.txt'))
hu12_conn <- read.delim(file.path(fp, 'hu12_conn.txt'))
hu12_lulc <- read.delim(file.path(fp, 'hu12_lulc.txt'))
hu12_etc <- read.delim(file.path(fp, 'hu12.txt'))
hu12_states <- read.delim(file.path(fp, 'hu12_states.txt'))

hu12_all <- hu12_etc %>% full_join(hu12_chag) %>% full_join(hu12_conn) %>% full_join(hu12_lulc) %>% full_join(hu12_states)

# Join the HUC4, HUC8, and HUC12 with bbs diversity.
# Note: this is the "old" bbs dataset that only goes up to 2012.
bbs_div <- read.csv('/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_div_georeferenced.csv', stringsAsFactors = FALSE)

bbs_lagos_huc4 <- bbs_div %>% left_join(hu4_all %>% rename(HUC4 = hu4))
bbs_lagos_huc8 <- bbs_div %>% left_join(hu8_all %>% rename(HUC8 = hu8))
bbs_lagos_huc12 <- bbs_div %>% left_join(hu12_all %>% rename(HUC12 = hu12))

# These data frames are much too big and will probably crash things if used to analyze anything. The best thing is probably to take a single year or average over time, and use that. Here is the average from 2001-2011, first averaging by stops within route, then years, joined with the hucs.

bbs_routeavg <- bbs_div %>% group_by(year, rteNo) %>% select(-Stop) %>% summarize_all(.funs = mean, na.rm = TRUE)
bbs_routeavg_tenyears <- bbs_routeavg %>% filter(year >= 2001 & year <= 2011) %>% group_by(rteNo) %>% select(-year) %>% summarize_all(.funs = mean, na.rm = TRUE)