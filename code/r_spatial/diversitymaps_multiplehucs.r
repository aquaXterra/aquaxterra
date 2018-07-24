# Diversity maps by HUC, using quick and dirty averages of all routes that are even a teeny bit in the huc.
# NOTE: The diversity values here are NOT safe for use in analyses. They are really only for mapping purposes!

for (package in c(#"date", 
  "ggplot2", "dplyr", "reshape2", "GGally", "maptools",
  "rgdal")) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}
# From mounted directory on desktop
#load('/Volumes/aquaxterra/DATA/raw_data/BBS/bbs_div_byroute_presence_allhucs.r')
# on HPCC
load('/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_div_byroute_presence_allhucs.r')

# Calculate summaries by huc and year, averaging together the route numbers which is conveniently pre-weighted by stop numbers.
# Again, this is a QUICK AND DIRTY average used for mapping purposes. Don't run stats on this!!!! Please, I beg you.
# Median across routes, mean across years.

bbs_div_huc4 <- bbs_div_allhucs %>%
  group_by(year, HUC4) %>%
  summarize_at(.vars = vars(richness, FEve, FDis, mpd.obs.z, mntd.obs.z), .funs = median, na.rm = TRUE) %>%
  ungroup %>%
  group_by(HUC4) %>%
  filter(year >= 2001, year <= 2011) %>%
  summarize_all(.funs = mean, na.rm = TRUE)

bbs_div_huc8 <- bbs_div_allhucs %>%
  group_by(year, HUC8) %>%
  summarize_at(.vars = vars(richness, FEve, FDis, mpd.obs.z, mntd.obs.z), .funs = median, na.rm = TRUE) %>%
  ungroup %>%
  group_by(HUC8) %>%
  filter(year >= 2001, year <= 2011) %>%
  summarize_all(.funs = mean, na.rm = TRUE)

bbs_div_huc12 <- bbs_div_allhucs %>%
  group_by(year, HUC12) %>%
  summarize_at(.vars = vars(richness, FEve, FDis, mpd.obs.z, mntd.obs.z), .funs = median, na.rm = TRUE) %>%
  ungroup %>%
  group_by(HUC12) %>%
  filter(year >= 2001, year <= 2011) %>%
  summarize_all(.funs = mean, na.rm = TRUE)

### HUC4 maps ###
# From mounted directory on desktop
#huc4 <- readOGR(dsn = '/Volumes/aquaxterra/DATA/reprojected_data/HUC', layer = 'HU4_CONUS_Alb')
# on HPCC
huc4 <- readOGR(dsn = '/mnt/research/aquaxterra/DATA/reprojected_data/HUC', layer = 'HU4_CONUS_Alb')

huc4@data <- huc4@data %>% mutate(id = rownames(huc4@data), HUC4 = as.numeric(as.character(HUC4))) %>% left_join(bbs_div_huc4)
huc4_fort <- fortify(huc4, region = 'id') %>% left_join(huc4@data, by = 'id')

rbcolors <- rev(RColorBrewer::brewer.pal(9, 'RdYlBu'))

vars_to_plot <- names(bbs_div_huc4)[-(1:2)]

states <- read.csv('~/states_albers.csv', stringsAsFactors = FALSE)
#states <- read.csv('/Volumes/aquaxterra/DATA/state_borders/states_albers.csv', stringsAsFactors = FALSE)
for (i in vars_to_plot) {
  map_i <- ggplot(huc4_fort) +
    geom_polygon(aes_string(x='long', y='lat', group='group', fill=i)) +
    geom_path(aes_string(x='long', y='lat', group='group'), color = 'white', size = 0.25) +
    geom_path(data = states, aes(x = long, y = lat, group = group), color = 'gray20') +
    scale_fill_gradientn(colours = rbcolors) +
    coord_equal() +
    theme_bw() + 
    theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'black'), panel.border = element_blank(), plot.background = element_rect(fill = 'black'), legend.position = c(0.13,0.1), legend.direction = 'horizontal', legend.title = element_text(colour="white", size=16)) + labs(title="Number of Bird Species")
# on HPCC
#    ggsave(filename = paste0('/mnt/research/aquaxterra/FIGS/bbs_huc_maps_29nov/huc4_byroute_allbirds_', i, '_map.pdf'), plot = map_i, height = 6, width = 9, dpi = 600)
# on mounted drive
     ggsave(filename = paste0('/Volumes/GoogleDrive/My Drive/Research/NASA_Biodiversity/NASABiodiversityWG/Figures/bbs_diversity_maps/huc4_byroute_allbirds_', i, '_map.pdf'), plot = map_i, height = 6, width = 9, dpi = 300)
  print(i)
}

### HUC8 maps ###

huc8 <- readOGR(dsn = '/mnt/research/aquaxterra/DATA/reprojected_data/HUC', layer = 'HU8_CONUS_Alb')

huc8@data <- huc8@data %>% mutate(id = rownames(huc8@data), HUC8 = as.numeric(as.character(HUC8))) %>% left_join(bbs_div_huc8)
huc8_fort <- fortify(huc8, region = 'id') %>% left_join(huc8@data, by = 'id')

for (i in vars_to_plot) {
  map_i <- ggplot(huc8_fort) +
    geom_polygon(aes_string(x='long', y='lat', group='group', fill=i)) +
    geom_path(aes_string(x='long', y='lat', group='group'), color = 'white', size = 0.25) +
    geom_path(data = states, aes(x = long, y = lat, group = group), color = 'gray20') +
    scale_fill_gradientn(colours = rbcolors) +
    coord_equal() +
    theme_bw() + 
    theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'black'), panel.border = element_blank(), plot.background = element_rect(fill = 'black'), legend.position = c(0.13,0.1), legend.direction = 'horizontal', legend.title = element_blank())
  ggsave(filename = paste0('/mnt/research/aquaxterra/FIGS/bbs_huc_maps_29nov/huc8_byroute_allbirds_', i, '_map.png'), plot = map_i, height = 6, width = 9, dpi = 400)
  print(i)
}

### HUC12 just fortify and save to create map in parallel elsewhere ###

huc12 <- readOGR(dsn = '/mnt/research/aquaxterra/DATA/reprojected_data/HUC', layer = 'HU12_CONUS_Alb')

huc12@data <- huc12@data %>% mutate(id = rownames(huc12@data), HUC12 = as.numeric(as.character(HUC12))) %>% left_join(bbs_div_huc12)
huc12_fort <- fortify(huc12, region = 'id') %>% left_join(huc12@data, by = 'id')

save(huc12_fort, file = '/mnt/research/aquaxterra/DATA/huc12_fortified_withbbs_multiple.r')
