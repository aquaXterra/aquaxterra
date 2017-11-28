# HUC12 maps with BBS diversity
# Run in parallel

i <- as.numeric(Sys.getenv('PBS_ARRAYID'))

library(maptools)
library(rgdal)
library(dplyr)
library(ggplot2)

# Modification 3 Mar 2017: add state boundaries
states <- read.csv('~/states_albers.csv', stringsAsFactors = FALSE)

# This is a good heat map color scheme but we will need to come up with better ones for individual variables.
rbcolors <- rev(RColorBrewer::brewer.pal(9, 'RdYlBu'))

# Load combined and fortified huc data (big)

load('/mnt/research/aquaxterra/DATA/huc12_fortified_withbbs.r')

vars_to_plot <- c('richness', 'FEve', 'FDis', 'mpd.obs.z', 'mntd.obs.z')

map_i <- ggplot(huc12_fort) +
    geom_polygon(aes_string(x='long', y='lat', group='group', fill=vars_to_plot[i])) +
    geom_path(aes_string(x='long', y='lat', group='group'), color = 'white', size = 0.05) +
	geom_path(data = states, aes(x = long, y = lat, group = group), color = 'gray20') +
    scale_fill_gradientn(colours = rbcolors) +
    coord_equal() +
    theme_bw() + 
    theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'black'), panel.border = element_blank(), plot.background = element_rect(fill = 'black'), legend.position = c(0.13,0.1), legend.direction = 'horizontal', legend.title = element_blank())
ggsave(filename = paste0('/mnt/research/aquaxterra/FIGS/bbs_huc_maps/huc12_byroute_allbirds_', vars_to_plot[i], '_map.png'), plot = map_i, height = 6, width = 9, dpi = 400)
map_i <- ggplot(huc12res_fort) +
    geom_polygon(aes_string(x='long', y='lat', group='group', fill=vars_to_plot[i])) +
    geom_path(aes_string(x='long', y='lat', group='group'), color = 'white', size = 0.05) +
	geom_path(data = states, aes(x = long, y = lat, group = group), color = 'gray20') +
    scale_fill_gradientn(colours = rbcolors) +
    coord_equal() +
    theme_bw() + 
    theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'black'), panel.border = element_blank(), plot.background = element_rect(fill = 'black'), legend.position = c(0.13,0.1), legend.direction = 'horizontal', legend.title = element_blank())
ggsave(filename = paste0('/mnt/research/aquaxterra/FIGS/bbs_huc_maps/huc12_byroute_residents_', vars_to_plot[i], '_map.png'), plot = map_i, height = 6, width = 9, dpi = 400)

