# Script to make a map colored by HUC4, including state borders, once you already have the data aggregated by HUC4.
# State border data are on the HPCC.
# Everything is in Albers equal area projection.
# Requires R, GDAL, and GEOS modules.
# ACS 28 Sep 2018

library(maptools)
library(rgdal)
library(dplyr)
library(ggplot2)
library(rgeos)
library(tidyr)
library(broom)

# State boundaries
states <- read.csv('/home/annie/Documents/MSU_postdoc/aquaXterra_localFiles/data/state_borders/states_albers.csv', stringsAsFactors = FALSE)
load(file.path('/home/annie/Documents/MSU_postdoc/aquaXterra_localFiles/data/state_borders/', 'states_albers.RData'))

#washington <- states[states$region == 'washington' | states$region == 'idaho' | states$region == 'montana',]

# Load shape file of HUC4 boundaries
huc4 <- readOGR(dsn = '/home/annie/Documents/MSU_postdoc/aquaXterra_localFiles/data/HUC', layer = 'HU4_CONUS_Alb')

# Load data file
bbs_div4 <- read.csv('/home/annie/Documents/MSU_postdoc/aquaXterra_localFiles/data/BBS/BBS_Diversity/bbs_div_huc4_mean.csv', header = TRUE)

rast4 <- read.csv('/home/annie/Documents/MSU_postdoc/aquaXterra_localFiles/data/HUC4summarized.csv', header = TRUE)

# Clip the huc4 shape file to the borders of the continental USA, since the map looks nicer that way.
goodusabounds <- gUnaryUnion(states_albers)
#goodusabounds <- gUnaryUnion(states_albers[states_albers$state_name == 'Washington' | states_albers$state_name == 'Idaho' | states_albers$state_name == 'Montana',]) 
hucdat4 <- huc4@data
hucdat4_pts <- huc4@data
huc4 <- gIntersection(huc4, goodusabounds, byid = TRUE, id = row.names(huc4@data))
huc4 <- SpatialPolygonsDataFrame(huc4, hucdat4[which(row.names(hucdat4) %in% names(huc4)),])
huc4_pts <- gCentroid(huc4, byid = TRUE, id = row.names(huc4@data))
huc4_pts <- SpatialPointsDataFrame(huc4_pts, hucdat4_pts[which(row.names(hucdat4_pts) %in% row.names(huc4_pts)),])

# Merge the summary CSV data with HUC4. The HUC4 must be converted to numeric for both.
# Your data needs to have a HUC4 column converted to numeric.
huc4@data <- huc4@data %>%
  mutate(HUC4 = as.numeric(as.character(HUC4)), id = rownames(huc4@data)) %>%
  left_join(bbs_div4[, 1:2], by = 'HUC4') %>%
  left_join(rast4[, c(1, grep('^mean_npp\\_\\d+', names(rast4)))], by = 'HUC4')
huc4_pts@data <- huc4_pts@data %>%
  mutate(HUC4 = as.numeric(as.character(HUC4)), id = rownames(huc4_pts@data)) %>%
  left_join(bbs_div4[, 1:2], by = 'HUC4') %>%
  left_join(rast4[, c(1, grep('^mean_npp\\_\\d+', names(rast4)))], by = 'HUC4')

# calculate trend if you are interested in a time series variable (e.g., gpp, npp)
get_stats <- function(data, index) {
  newdat <- gather(data[i, grep('^mean_npp\\_\\d+', names(data))], key = year, value = mean_npp)
  newdat$year <- as.numeric(gsub('^mean_npp_', '', newdat$year))
  newdat$mean_npp <- as.numeric(newdat$mean_npp)/1000
  mean <- mean(newdat$mean_npp, na.rm = TRUE)
  # scale to get relative increase
  newdat$mean_npp <- scale(newdat$mean_npp)
  mod <- lm(newdat$mean_npp~newdat$year, data = newdat)
  slope <- summary(mod)$coefficients[[2]]
  sig <- summary(mod)$coefficients[[8]] <= 0.05
  return(c(slope, sig, mean))
}

huc4@data$npp_trend <- NA
huc4@data$npp_trend_dir <- NA
huc4@data$npp_mean <- NA
huc4@data$npp_sig <- NA
for (i in 1:nrow(huc4@data)) {
  stats <- get_stats(data = huc4@data, i)
  huc4@data$npp_trend[i] <- stats[1]
  if(stats[1] > 0) {
    huc4@data$npp_trend_dir[i] <- '\U2191'
  } 
  if(stats[1] < 0) {
    huc4@data$npp_trend_dir[i] <- '\U2193'
  } 
  else{
    huc4@data$npp_trend_dir[i] <- '\U0020'
  }
  huc4_pts@data$npp_sig[i] <- stats[2]
  huc4@data$npp_mean[i] <- stats[3]
}
huc4_pts@data$npp_trend <- NA
huc4_pts@data$npp_trend_dir <- NA
huc4_pts@data$npp_mean <- NA
huc4_pts@data$npp_sig <- NA
for (i in 1:nrow(huc4_pts@data)) {
  stats <- get_stats(data = huc4_pts@data, i)
  huc4_pts@data$npp_trend[i] <- stats[1]
  if(stats[1] > 0) {
    huc4_pts@data$npp_trend_dir[i] <- '\U2191'
  } 
  if(stats[1] < 0) {
    huc4_pts@data$npp_trend_dir[i] <- '\U2193'
  } 
  if(stats[1] == 0){
    huc4_pts@data$npp_trend_dir[i] <- '\U0020'
  }
  huc4_pts@data$npp_sig[i] <- stats[2]
  huc4_pts@data$npp_mean[i] <- stats[3]
}

# Convert spatial data into a data frame usable by ggplot2
huc4_fort <- tidy(huc4, region = 'id') %>%
  left_join(huc4@data, by = 'id')
huc4_pts_fort <- as_tibble(huc4_pts) %>%
  mutate(lat = huc4_pts@coords[, 2], long = huc4_pts@coords[, 1])

# Here are some example color schemes
rbcolors <- rev(RColorBrewer::brewer.pal(9, 'RdYlBu'))
purple8colors <- c('#fcfbfd', '#efedf5', '#dadaeb', '#bcbddc', '#9e9ac8', '#807dba', '#6a51a3', '#54278f', '#3f007d')[-1]
elevcolors <- topo.colors(9)
yellowtogreencolors <- colorRampPalette(c('forestgreen', 'lightgoldenrod1'))(9)
browntogreencolors <- c('#8c510a', '#bf812d', '#dfc27d', '#f6e8c3', '#f5f5f5', '#c7eae5', '#80cdc1', '#35978f', '#01665e')
browntobluecolors <- colorRampPalette(c('tan', 'skyblue'))(9)

# Draw the map (slow and uses quite a bit of memory)
the_map <- ggplot(huc4_fort) +
    geom_polygon(aes_string(x = 'long', y = 'lat', group = 'group', fill = 'npp_mean')) +
    geom_path(aes_string(x = 'long', y = 'lat', group = 'group'), color = 'white', size = 0.25) +
    geom_text(data = huc4_pts_fort, aes(x = long, y = lat, label = sprintf(npp_trend_dir), size = abs(npp_trend), color = as.factor(npp_sig))) +
    geom_path(data = states, aes(x = long, y = lat, group = group), color = 'gray20') +
    scale_fill_gradientn(colors = rbcolors, name = 'Mean NPP') +
    scale_size_continuous(name = 'NPP Change (stdev) / Year') +
    scale_color_manual(breaks = c('1', '0'), values = c('gray48', 'black'), labels = c('p <= 0.05', 'p > 0.05'), name = 'Significance') +
    coord_equal() +
    theme_bw() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_rect(color = 'black', fill = 'black'),
          panel.border = element_blank(),
          plot.background = element_rect(fill = 'black'),
          legend.position = 'bottom',
          legend.direction = 'horizontal',
          legend.title = element_text(color = 'white'),
          legend.title.align = 0.5,
          legend.text = element_text(color = 'white'),
          legend.background = element_rect(fill = 'black'),
          legend.box = 'horizontal') +
      guides(colour = guide_legend(title.position = "top", title.hjust = 0.5, order = 3),
          size = guide_legend(title.position = "top", title.hjust = 0.5, order = 2),
          fill = guide_colourbar(title.position = 'top', title.hjust = 0.5, order = 1))


ggsave(filename = file.path('/home/annie/Documents/MSU_postdoc/aquaXterra_localFiles/figs/', 'huc4_summary_npp_trend_map.png'), plot = the_map, height = 6, width = 9, dpi = 400)

