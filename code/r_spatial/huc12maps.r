# HUC12 diversity maps
# Must run on HPCC because my machine has too puny RAM to render the maps

bbs_div <- read.csv('/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_div_2011.csv', stringsAsFactors = FALSE)

library(dplyr)
library(ggplot2)
library(reshape2)
library(GGally)

# All variables that are important, use median instead of mean
div_huc12_all <- bbs_div %>%
  group_by(HUC12) %>%
  summarize_at(.cols = vars(richness, shannon, simpson, shannonevenness, FRic, FEve, FDiv, FDis, RaoQ, PD, mpd.obs, mpd.obs.z, mntd.obs, mntd.obs.z), .funs = median, na.rm = TRUE)

rbcolors <- rev(RColorBrewer::brewer.pal(9, 'RdYlBu'))

library(maptools)
library(rgdal)

huc12 <- readOGR(dsn = '/mnt/research/aquaxterra/DATA/reprojected_data/HUC', layer = 'HU12_CONUS_Alb')

# Merge the bbs_div data with HUC8
huc12@data <- huc8@data %>% mutate(HUC8 = as.numeric(as.character(HUC12)), id = rownames(huc12@data)) %>% left_join(div_huc12_all)
huc12_fort <- fortify(huc12, region = 'id') %>% left_join(huc12@data, by = 'id')

vars_to_plot <- names(div_huc12_all)[-1]

for (i in vars_to_plot) {
  map_i <- ggplot(huc8_fort, aes_string(x='long', y='lat', group='group', fill=i)) +
    geom_polygon() +
    geom_path(color = 'white', size = 0.5) +
    scale_fill_gradientn(colours = rbcolors) +
    coord_equal() +
    theme_bw() + 
    theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'black'), panel.border = element_blank(), plot.background = element_rect(fill = 'black'), legend.position = c(0.13,0.1), legend.direction = 'horizontal', legend.title = element_blank())
  ggsave(filename = paste0('/mnt/research/aquaxterra/FIGS/huc12maps/huc12_2011_', i, '_map.png'), plot = map_i, height = 6, width = 9, dpi = 400)
  print(i)
}
