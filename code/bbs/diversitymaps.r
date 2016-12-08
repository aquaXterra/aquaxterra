# Create some plots of bbs diversity, by HUC, for 2011 only.
# QDR/Aquaxterra/created 07Dec2016

bbs_div <- read.csv('C:/Users/Q/Dropbox/projects/aquaxterra/bbs_div_2011.csv', stringsAsFactors = FALSE)
summary(bbs_div)

library(dplyr)
library(ggplot2)
library(reshape2)
library(GGally)

# Comparison of different types of diversity, and their distributions.
png('figs/bbs_diversity_pairs.png', height=8, width=8, res=300, units='in')
ggpairs(bbs_div %>% select(richness, FRic, PD), # Looks like phylogenetic and taxonomic are much more highly correlated, while functional is different.
  diag=list(continuous=wrap('barDiag', bins=20)), columnLabels = c('TD','FD','PD')) + theme_bw()
dev.off()

# Calculate mean pd, fd, and td by HUC, and plot their distributions.

div_huc4 <- bbs_div %>% group_by(HUC4) %>%
  summarize(TD = mean(richness, na.rm=T), FD = mean(FRic, na.rm=T), PD = mean(PD, na.rm=T))

# All variables that are important, use median instead of mean
div_huc4_all <- bbs_div %>%
  group_by(HUC4) %>%
  summarize_at(.cols = vars(richness, shannon, simpson, shannonevenness, FRic, FEve, FDiv, FDis, RaoQ, PD, mpd.obs, mpd.obs.z, mntd.obs, mntd.obs.z), .funs = median, na.rm = TRUE)

ggplot(melt(div_huc4, id.vars = 'HUC4', variable.name = 'diversity'), aes(x = value)) +
  geom_histogram() +
  theme_bw() +
  scale_y_continuous(expand = c(0,0), limits = c(0,23)) +
  facet_wrap(~ diversity, nrow=2, scales = 'free_x')

div_huc8 <- bbs_div %>% group_by(HUC8) %>%
  summarize(TD = mean(richness, na.rm=T), FD = mean(FRic, na.rm=T), PD = mean(PD, na.rm=T))

ggplot(melt(div_huc8, id.vars = 'HUC8', variable.name = 'diversity'), aes(x = value)) +
  geom_histogram() +
  theme_bw() +
  scale_y_continuous(expand = c(0,0), limits = c(0, 250)) +
  facet_wrap(~ diversity, nrow=2, scales = 'free_x')

div_huc12 <- bbs_div %>% group_by(HUC12) %>%
  summarize(TD = mean(richness, na.rm=T), FD = mean(FRic, na.rm=T), PD = mean(PD, na.rm=T))

ggplot(melt(div_huc12, id.vars = 'HUC12', variable.name = 'diversity'), aes(x = value)) +
  geom_histogram() +
  theme_bw() +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1800)) +
  facet_wrap(~ diversity, nrow=2, scales = 'free_x')

# Plot bird diversity in 2011 on a map

rbcolors <- rev(RColorBrewer::brewer.pal(9, 'RdYlBu'))

ggplot(bbs_div, aes(x = POINT_X, y = POINT_Y, color = richness)) +
  borders('state', fill = 'gray75') +
  geom_point() +
  scale_color_gradientn(colours = rbcolors) +
  theme_bw() + coord_map()

ggplot(bbs_div, aes(x = POINT_X, y = POINT_Y, color = shannon)) +
  borders('state', fill = 'gray75') +
  geom_point() +
  scale_color_gradientn(colours = rbcolors) +
  theme_bw() + coord_map()

ggplot(bbs_div, aes(x = POINT_X, y = POINT_Y, color = PD)) +
  borders('state', fill = 'gray75') +
  geom_point() +
  scale_color_gradientn(colours = rbcolors) +
  theme_bw() + coord_map()

ggplot(bbs_div, aes(x = POINT_X, y = POINT_Y, color = FRic)) +
  borders('state', fill = 'gray75') +
  geom_point() +
  scale_color_gradientn(colours = rbcolors) +
  theme_bw() + coord_map()

ggplot(bbs_div, aes(x = POINT_X, y = POINT_Y, color = mpd.obs.z)) +
  borders('state', fill = 'gray75') +
  geom_point() +
  scale_color_gradientn(colours = rbcolors) +
  theme_bw() + coord_map()

ggplot(bbs_div, aes(x = POINT_X, y = POINT_Y, color = mntd.obs.z)) +
  borders('state', fill = 'gray75') +
  geom_point() +
  scale_color_gradientn(colours = rbcolors) +
  theme_bw() + coord_map()

# Load HUC4 and HUC8 polygons so that we can shade each polygon by bird diversity.

library(maptools)
library(rgdal)

# Define projection and read the HUC4 polygons in
#crsalbers <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")
#x <- readShapePoly('C:/Users/Q/Dropbox/projects/aquaxterra/hucshapefiles/HU4_CONUS_Alb.shp', proj4string = crsalbers)

# Try another way of doing this.
huc4 <- readOGR(dsn = 'C:/Users/Q/Dropbox/projects/aquaxterra/hucshapefiles', layer = 'HU4_CONUS_Alb')

# Merge the bbs_div data with HUC4
huc4@data <- huc4@data %>% mutate(HUC4 = as.numeric(as.character(HUC4)), id = rownames(huc4@data)) %>% left_join(div_huc4_all)
huc4_fort <- fortify(huc4, region = 'id') %>% left_join(huc4@data, by = 'id')

# Make a map of HUC4 route-level median bird species richness in 2011!
huc4_richness_2011_map <- ggplot(huc4_fort, aes(x=long, y=lat, group=group, fill=richness)) +
  geom_polygon() +
  geom_path(color = 'white') +
  scale_fill_gradientn(colours = rbcolors, name = 'Richness') +
  coord_equal() +
  theme_bw() + 
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'black'), panel.border = element_blank(), plot.background = element_rect(fill = 'black'), legend.position = c(0.13,0.1), legend.direction = 'horizontal')

ggsave('figs/huc4_richness_2011_map.png', huc4_richness_2011_map, height = 6, width = 9, dpi = 400)

# Since that works, loop through all the variables, making a map for each one.

vars_to_plot <- names(div_huc4_all)[-1]

for (i in vars_to_plot) {
  map_i <- ggplot(huc4_fort, aes_string(x='long', y='lat', group='group', fill=i)) +
    geom_polygon() +
    geom_path(color = 'white') +
    scale_fill_gradientn(colours = rbcolors) +
    coord_equal() +
    theme_bw() + 
    theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'black'), panel.border = element_blank(), plot.background = element_rect(fill = 'black'), legend.position = c(0.13,0.1), legend.direction = 'horizontal', legend.title = element_blank())
  ggsave(filename = paste0('figs/huc4maps/huc4_2011_', i, '_map.png'), plot = map_i, height = 6, width = 9, dpi = 400)
  print(i)
}

# Do this for HUC8 as well. Might be very slow since the data frame is a lot bigger. (10x as many HUCs)
huc8 <- readOGR(dsn = 'C:/Users/Q/Dropbox/projects/aquaxterra/hucshapefiles', layer = 'HU8_CONUS_Alb')

# Merge the bbs_div data with HUC4
huc8@data <- huc8@data %>% mutate(HUC8 = as.numeric(as.character(HUC8)), id = rownames(huc8@data)) %>% left_join(div_huc8)
huc8_fort <- fortify(huc8, region = 'id') %>% left_join(huc8@data, by = 'id')

# Make a map of HUC4 route-level mean bird species richness in 2011!
huc8_richness_2011_map <- ggplot(huc8_fort, aes(x=long, y=lat, group=group, fill=TD)) +
  geom_polygon() +
  geom_path(color = 'white', size = 0.5) + # Need to make line a bit thinner because there are so many polygons.
  scale_fill_gradientn(colours = rbcolors, name = 'Richness') +
  coord_equal() +
  theme_bw() + 
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = 'black'), plot.background = element_rect(fill = 'black'), legend.position = c(0.13,0.1), legend.direction = 'horizontal')

ggsave('figs/huc8_richness_2011_map.png', huc8_richness_2011_map, height = 6, width = 9, dpi = 400)
# Anything of HUC8 or greater will have to be sent to the HPCC, since my machine does not have enough RAM to handle it.
