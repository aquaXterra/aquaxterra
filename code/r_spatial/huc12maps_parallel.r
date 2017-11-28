# HUC12 maps
# Run in parallel

i <- as.numeric(Sys.getenv('PBS_ARRAYID'))

library(maptools)
library(rgdal)
library(dplyr)
library(ggplot2)

# Modification 3 Mar 2017: add state boundaries
states <- read.csv('~/states_albers.csv', stringsAsFactors = FALSE)

# Select variables to plot.
cols_to_plot <- c('mean_altitude','std_altitude','mean_allyears_npp','mean_allyears_gpp','mean_allyears_lai','mean_allyears_fpar','mean_allyears_bio1','mean_allyears_bio4','cv_allyears_bio1','mean_allyears_bio12','mean_allyears_bio15','cv_allyears_bio12','nlcd_forest','nlcd_agriculture','nlcd_developed','nlcd_wetland','nlcd_grassland','nlcd_shrubland','nlcd_ice','nlcd_barren','nlcd_water','nlcd_diversity')

# This is a good heat map color scheme but we will need to come up with better ones for individual variables.
rbcolors <- rev(RColorBrewer::brewer.pal(9, 'RdYlBu'))
purple8colors <- c('#fcfbfd','#efedf5','#dadaeb','#bcbddc','#9e9ac8','#807dba','#6a51a3','#54278f','#3f007d')[-1]
elevcolors <- topo.colors(9)
yellowtogreencolors <- colorRampPalette(c('forestgreen','lightgoldenrod1'))(9)
browntogreencolors <- c('#8c510a','#bf812d','#dfc27d','#f6e8c3','#f5f5f5','#c7eae5','#80cdc1','#35978f','#01665e')
browntobluecolors <- colorRampPalette(c('tan','skyblue'))(9)

colors_list <- list(rbcolors, purple8colors, elevcolors, yellowtogreencolors, browntogreencolors, browntobluecolors)
color_schemes <- c(3, 2, rep(5,4), 1, 4, 4, 6, 4, 4, rep(2, 10))

# Load combined and fortified huc data (big)

load('/mnt/research/aquaxterra/DATA/huc12_fortified.r')


map_i <- ggplot(huc12_fort) +
    geom_polygon(aes_string(x='long', y='lat', group='group', fill=cols_to_plot[i])) +
    geom_path(aes_string(x='long', y='lat', group='group'), color = 'white', size = 0.05) + # Thinner for huc12
	geom_path(data = states, aes(x = long, y = lat, group = group), color = 'gray20') +
    scale_fill_gradientn(colours = colors_list[[color_schemes[i]]]) +
    coord_equal() +
    theme_bw() + 
    theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'black'), panel.border = element_blank(), plot.background = element_rect(fill = 'black'), legend.position = c(0.13,0.1), legend.direction = 'horizontal', legend.title = element_blank())
ggsave(filename = paste0('/mnt/research/aquaxterra/FIGS/huc12maps/huc12_summary_', cols_to_plot[i], '_map.png'), plot = map_i, height = 6, width = 9, dpi = 400)

