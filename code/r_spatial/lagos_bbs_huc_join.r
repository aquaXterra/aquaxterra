# Load LAGOS data and combine with other datasets
# Author: QDR
# Project: Aquaxterra
# Created: 09 Dec 2016
# Last modified: 12 Jan 2017

# Modified 12 Jan: code the LAGOS data by year so that we can match it with the yearly bbs data
# Modified 23 Dec: join with new bbs data

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

# Split the variables by year, loading the table I wrote that codes the variables by year and type
code48 <- read.csv('/mnt/research/aquaxterra/DATA/raw_data/LAGOS/lagos48.csv', stringsAsFactors = FALSE)
code12 <- read.csv('/mnt/research/aquaxterra/DATA/raw_data/LAGOS/lagos12.csv', stringsAsFactors = FALSE)

# All the variables are numeric except for the glaciation category so we just need to change it to numeric
# 1 glaciated, 2 unglaciated, 3 partially glaciated
hu4_all <- mutate(hu4_all, hu4_latewisconsinglaciation_glaciation = as.numeric(hu4_latewisconsinglaciation_glaciation))
hu8_all <- mutate(hu8_all, hu8_latewisconsinglaciation_glaciation = as.numeric(hu8_latewisconsinglaciation_glaciation))
hu12_all <- mutate(hu12_all, hu12_latewisconsinglaciation_glaciation = as.numeric(hu12_latewisconsinglaciation_glaciation))

# Ignore states for now, put into long-form and join with the metadata columns
library(reshape2)

hu4_long <- melt(hu4_all[, !names(hu4_all) %in% 'hu4_states'], id.vars = c('hu4', 'hu4_name', 'hu4_zoneid'))
code48$variable <- names(hu4_all)
hu4_long <- left_join(hu4_long, code48[,-1])

hu8_long <- melt(hu8_all[, !names(hu8_all) %in% 'hu8_states'], id.vars = c('hu8', 'hu8_name', 'hu8_zoneid'))
code48$variable <- names(hu8_all)
hu8_long <- left_join(hu8_long, code48[,-1])

hu12_long <- melt(hu12_all[, !names(hu12_all) %in% c('hu12_states', 'hu12_country')], id.vars = c('hu12', 'hu12_name', 'hu12_zoneid'))
code12$variable <- names(hu12_all)
hu12_long <- left_join(hu12_long, code12[,-1])

# Recast the long-form data into years.
hu4_cast <- plyr::dlply(hu4_long, 'category', function(x) {
	res <- dcast(x %>% select(hu4, year, type, stat_or_unit, value), hu4 + year ~ type + stat_or_unit)
	names(res) <- gsub(',', '_', names(res))
	names(res) <- gsub(' ', '_', names(res))
	res
})

hu8_cast <- plyr::dlply(hu8_long, 'category', function(x) {
	res <- dcast(x %>% select(hu8, year, type, stat_or_unit, value), hu8 + year ~ type + stat_or_unit)
	names(res) <- gsub(',', '_', names(res))
	names(res) <- gsub(' ', '_', names(res))
	res
})

# Must tweak this one since there are some duplicate named huc12s
hu12_cast <- plyr::dlply(hu12_long, 'category', function(x) {
	res <- dcast(x %>% select(hu12, year, type, stat_or_unit, value), hu12 + year ~ type + stat_or_unit, fun.aggregate = function(z) z[1])
	names(res) <- gsub(',', '_', names(res))
	names(res) <- gsub(' ', '_', names(res))
	res
}, .progress = 'text')

#################################################################################

# Join the HUC4, HUC8, and HUC12 with bbs diversity.
bbs_div <- read.csv('/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_div_reduced2015.csv', stringsAsFactors = FALSE)

bbs_lagos_huc4 <- bbs_div %>% left_join(hu4_all %>% rename(HUC4 = hu4))
bbs_lagos_huc8 <- bbs_div %>% left_join(hu8_all %>% rename(HUC8 = hu8))
bbs_lagos_huc12 <- bbs_div %>% left_join(hu12_all %>% rename(HUC12 = hu12))

# These data frames are much too big and will probably crash things if used to analyze anything. The best thing is probably to take a single year or average over time, and use that. Here is the average from 2001-2011, first averaging by stops within route, then years, joined with the hucs.

bbs_routeavg <- bbs_div %>% group_by(year, rteNo) %>% select(-Stop) %>% summarize_all(.funs = mean, na.rm = TRUE)
bbs_routeavg_tenyears <- bbs_routeavg %>% filter(year >= 2001 & year <= 2011) %>% group_by(rteNo) %>% select(-year) %>% summarize_all(.funs = mean, na.rm = TRUE)

# Load by-route BBS data and join
bbs_div_byroute <- read.csv('/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_div_byroute.csv', stringsAsFactors = FALSE)
bbs_byroute_lagos_huc4 <- bbs_div_byroute %>% left_join(hu4_all %>% rename(HUC4 = hu4))
bbs_byroute_lagos_huc8 <- bbs_div_byroute %>% left_join(hu8_all %>% rename(HUC8 = hu8))

# Save all joined data frames.
save(bbs_lagos_huc4, bbs_lagos_huc8, bbs_lagos_huc12, file  = '/mnt/research/aquaxterra/DATA/raw_data/BBS/BBS_LAGOS_bystop_join.r')
save(bbs_byroute_lagos_huc4, bbs_byroute_lagos_huc8, file = '/mnt/research/aquaxterra/DATA/raw_data/BBS/BBS_LAGOS_byroute_join.r')

#################################################################################

# Join the HUC4, HUC8, and HUC12 data, with years included, to the BBS data. Some won't have a year.
# NOTE: These are messed up. The data points without years are not working. However, that's OK because the other df with averages matches the across-year data.
bbs_div_byroute <- select(bbs_div_byroute, year, rteNo, richness, shannon, FRic, FEve, FDiv, FDis, PD, mpd.obs, mpd.obs.z, mntd.obs, mntd.obs.z, HUC4, HUC8, nstops4, nstops8, latitude, longitude)

bbs_lagos_route_huc4 <- lapply(hu4_cast, function(x) x %>% rename(HUC4 = hu4) %>% full_join(bbs_div_byroute))
bbs_lagos_stop_huc4 <- lapply(hu4_cast, function(x) x %>% rename(HUC4 = hu4) %>% full_join(bbs_div))

bbs_lagos_route_huc8 <- lapply(hu8_cast, function(x) x %>% rename(HUC8 = hu8) %>% full_join(bbs_div_byroute))
bbs_lagos_stop_huc8 <- lapply(hu8_cast, function(x) x %>% rename(HUC8 = hu8) %>% full_join(bbs_div))

bbs_lagos_stop_huc12 <- lapply(hu12_cast, function(x) x %>% rename(HUC12 = hu12) %>% full_join(bbs_div))

save(bbs_lagos_route_huc4, bbs_lagos_route_huc8, file = '/mnt/research/aquaxterra/DATA/raw_data/BBS/BBS_LAGOS_route_year_type.r')
save(bbs_lagos_stop_huc4, bbs_lagos_stop_huc8, bbs_lagos_stop_huc12, file = '/mnt/research/aquaxterra/DATA/raw_data/BBS/BBS_LAGOS_stop_year_type.r')