# Diversity maps using new presence-absence diversity metrics; also do one for all birds and one for residents only

# By route. Mean from 2001-2011

# Load diversity.
fp <- '/mnt/research/aquaxterra/FIGS/bbs_huc_maps_fg'
load('/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_div_byroute_presence.r')
fgrichness <- read.csv('/mnt/research/aquaxterra/DATA/raw_data/BBS/fgrichness.csv', stringsAsFactors = FALSE)

library(dplyr)
library(ggplot2)
library(reshape2)
library(maptools)
library(rgdal)

huc4 <- readOGR(dsn = '/mnt/research/aquaxterra/DATA/reprojected_data/HUC', layer = 'HU4_CONUS_Alb')

# Get rid of obvious lakes. (just Great Lakes for now)

# lakenames <- huc4@data$NAME[grep('Lake', huc4@data$NAME)]
# greatlakes <- lakenames[-c(1,3,4,5,15,18)]
# 
# huc4 <- subset(huc4, !NAME %in% greatlakes)

# Calculate the averages only for routes that are at least 50% in one HUC4, and for years 2001-2011

div_huc4_all <- bbs_div_byroute %>%
    mutate(HUC4 = as.numeric(HUC4)) %>%
	left_join(fgrichness) %>%
	filter(nstops4 > 25, year >= 2001, year <= 2011) %>% # Use only the ones that are a majority in one huc4
	group_by(HUC4) %>%
	summarize_at(vars(starts_with('total_richness')), median)
div_huc4_res <- bbs_div_byrouteres %>%
	mutate(HUC4 = as.numeric(HUC4)) %>%
	left_join(fgrichness) %>%
	filter(nstops4 > 25, year >= 2001, year <= 2011) %>% # Use only the ones that are a majority in one huc4
	group_by(HUC4) %>%
	summarize_at(vars(starts_with('resident_richness')), median)

	# Merge the bbs_div data with HUC4
huc4res <- huc4

huc4@data <- huc4@data %>% mutate(id = rownames(huc4@data), HUC4 = as.numeric(as.character(HUC4))) %>% left_join(div_huc4_all)
huc4_fort <- fortify(huc4, region = 'id') %>% left_join(huc4@data, by = 'id')

huc4res@data <- huc4res@data %>% mutate(id = rownames(huc4res@data), HUC4 = as.numeric(as.character(HUC4))) %>% left_join(div_huc4_res)
huc4res_fort <- fortify(huc4res, region = 'id') %>% left_join(huc4res@data, by = 'id')


rbcolors <- rev(RColorBrewer::brewer.pal(9, 'RdYlBu'))

vars_to_plot <- names(div_huc4_all)[-1]
vars_to_plot_res <- names(div_huc4_res)[-1]

states <- read.csv('~/states_albers.csv', stringsAsFactors = FALSE)

for (i in 1:length(vars_to_plot)) {
   map_i <- ggplot(huc4_fort) +
    geom_polygon(aes_string(x='long', y='lat', group='group', fill=vars_to_plot[i])) +
    geom_path(aes_string(x='long', y='lat', group='group'), color = 'white', size = 0.25) +
	geom_path(data = states, aes(x = long, y = lat, group = group), color = 'gray20') +
    scale_fill_gradientn(colours = rbcolors) +
    coord_equal() +
    theme_bw() + 
    theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'black'), panel.border = element_blank(), plot.background = element_rect(fill = 'black'), legend.position = c(0.13,0.1), legend.direction = 'horizontal', legend.title = element_blank())
  ggsave(filename = paste0('/mnt/research/aquaxterra/FIGS/bbs_huc_maps_fg/huc4_byroute_', vars_to_plot[i], '_map.png'), plot = map_i, height = 6, width = 9, dpi = 400)
  map_i <- ggplot(huc4res_fort) +
    geom_polygon(aes_string(x='long', y='lat', group='group', fill=vars_to_plot_res[i])) +
    geom_path(aes_string(x='long', y='lat', group='group'), color = 'white', size = 0.25) +
	geom_path(data = states, aes(x = long, y = lat, group = group), color = 'gray20') +
    scale_fill_gradientn(colours = rbcolors) +
    coord_equal() +
    theme_bw() + 
    theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'black'), panel.border = element_blank(), plot.background = element_rect(fill = 'black'), legend.position = c(0.13,0.1), legend.direction = 'horizontal', legend.title = element_blank())
  ggsave(filename = paste0('/mnt/research/aquaxterra/FIGS/bbs_huc_maps_fg/huc4_byroute_', vars_to_plot_res[i], '_map.png'), plot = map_i, height = 6, width = 9, dpi = 400)
  print(i)
}

#### HUC8 ####

huc8 <- readOGR(dsn = '/mnt/research/aquaxterra/DATA/reprojected_data/HUC', layer = 'HU8_CONUS_Alb')

# Calculate the averages only for routes that are at least 50% in one HUC8, and for years 2001-2011

div_huc8_all <- bbs_div_byroute %>%
    mutate(HUC8 = as.numeric(HUC8)) %>%
	left_join(fgrichness) %>%
	filter(nstops8 > 25, year >= 2001, year <= 2011) %>% # Use only the ones that are a majority in one huc8
	group_by(HUC8) %>%
	summarize_at(vars(starts_with('total_richness')), median)
div_huc8_res <- bbs_div_byrouteres %>%
	mutate(HUC8 = as.numeric(HUC8)) %>%
	left_join(fgrichness) %>%
	filter(nstops8 > 25, year >= 2001, year <= 2011) %>% # Use only the ones that are a majority in one huc8
	group_by(HUC8) %>%
	summarize_at(vars(starts_with('resident_richness')), median)

# Merge the bbs_div data with HUC8
huc8res <- huc8

huc8@data <- huc8@data %>% mutate(id = rownames(huc8@data), HUC8 = as.numeric(as.character(HUC8))) %>% left_join(div_huc8_all)
huc8_fort <- fortify(huc8, region = 'id') %>% left_join(huc8@data, by = 'id')

huc8res@data <- huc8res@data %>% mutate(id = rownames(huc8res@data), HUC8 = as.numeric(as.character(HUC8))) %>% left_join(div_huc8_res)
huc8res_fort <- fortify(huc8res, region = 'id') %>% left_join(huc8res@data, by = 'id')


for (i in 1:length(vars_to_plot)) {
   map_i <- ggplot(huc8_fort) +
    geom_polygon(aes_string(x='long', y='lat', group='group', fill=vars_to_plot[i])) +
    geom_path(aes_string(x='long', y='lat', group='group'), color = 'white', size = 0.25) +
	geom_path(data = states, aes(x = long, y = lat, group = group), color = 'gray20') +
    scale_fill_gradientn(colours = rbcolors) +
    coord_equal() +
    theme_bw() + 
    theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'black'), panel.border = element_blank(), plot.background = element_rect(fill = 'black'), legend.position = c(0.13,0.1), legend.direction = 'horizontal', legend.title = element_blank())
  ggsave(filename = paste0('/mnt/research/aquaxterra/FIGS/bbs_huc_maps_fg/huc8_byroute_', vars_to_plot[i], '_map.png'), plot = map_i, height = 6, width = 9, dpi = 400)
  map_i <- ggplot(huc8res_fort) +
    geom_polygon(aes_string(x='long', y='lat', group='group', fill=vars_to_plot_res[i])) +
    geom_path(aes_string(x='long', y='lat', group='group'), color = 'white', size = 0.25) +
	geom_path(data = states, aes(x = long, y = lat, group = group), color = 'gray20') +
    scale_fill_gradientn(colours = rbcolors) +
    coord_equal() +
    theme_bw() + 
    theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'black'), panel.border = element_blank(), plot.background = element_rect(fill = 'black'), legend.position = c(0.13,0.1), legend.direction = 'horizontal', legend.title = element_blank())
  ggsave(filename = paste0('/mnt/research/aquaxterra/FIGS/bbs_huc_maps_fg/huc8_byroute_', vars_to_plot_res[i], '_map.png'), plot = map_i, height = 6, width = 9, dpi = 400)
  print(i)
}
