# Break up insect data by watershed and plot maps of insect richness. (huc4)

# Script to make a map colored by HUC4, including state borders, once you already have the data aggregated by HUC4.
# State border data are on the HPCC.
# Everything is in Albers equal area projection.
# Requires R, GDAL, and GEOS modules.
# QDR 11 May 2018

library(maptools)
library(rgdal)
library(dplyr)
library(ggplot2)
library(rgeos)

############################################## Break up Family richness by HUC4 ##############
# Must load a very large CSV into memory.
inverts <- read.csv('/mnt/research/aquaxterra/DATA/Insects/insects2001v3.csv', stringsAsFactors = FALSE)

# Get rid of some of the rows that aren't too important
inverts <- inverts %>% select(HUCEightDigitCode, LatitudeMeasure, LongitudeMeasure, MonitoringLocationIdentifier, class, order, family, genus, species)

inverts$HUC4 <- as.numeric(substr(inverts$HUCEightDigitCode, 1, 4))
inverts$HUC4[nchar(inverts$HUCEightDigitCode) == 7] <- as.numeric(substr(inverts$HUC4[nchar(inverts$HUCEightDigitCode) == 7], 1, 3))
inverts$HUC4[is.na(inverts$HUCEightDigitCode)] <- NA

#look at family richness values by HUC- need to drop NAs from consideration for calculating family 

insect_table <- inverts %>%
  filter(family != "NA") %>%
  group_by(HUC4) %>%
  summarize(richness = length(unique(family)),
            nrecords = length(unique(MonitoringLocationIdentifier)))

#merge HUC area with invert data
HUC4_area<-read.csv('/mnt/research/aquaxterra/DATA/Insects/huc4area.csv')
insect_table2<-left_join(insect_table, HUC4_area, by="HUC4")

#randomly resample records in proportion to HUC area
insect_table3<-insect_table2%>%
  mutate(area_prop=AreaSqKm/max(AreaSqKm, na.rm=TRUE)) #make new variable for proportional area- proportion of maximum HUC4 area
inverts2<-full_join(inverts, insect_table3, by="HUC4")

#resample from HUCs with 20 or more sampling locations, weight by inverse of area_prop
inverts3<-subset(inverts2, nrecords>=10 & area_prop !="NA")

insect_table4<-inverts3 %>%
  do(sample_n(.,size=nrow(inverts3)*3/4, weight=1/(inverts3$area_prop), replace=FALSE))%>%
  group_by(HUC4, order)%>%
  summarize(richness = length(unique(family)),
            nrecords = length(unique(MonitoringLocationIdentifier)))

#reshape, Order rows -> columns
library(reshape2)
insect_cast <- dcast(insect_table4, HUC4 ~ order, value.var = 'richness')
insect_cast <- insect_cast[, !names(insect_cast) %in% 'NA']
insect_cast[is.na(insect_cast)] <- 0 #assigns zero value to all NAs
names(insect_cast)[-1] <- paste('richness', names(insect_cast)[-1], sep = '_') #rename columns
insect_cast$richness_all_aquatic_insects <- apply(insect_cast[,-1], 1, sum) #sum across rows to get total richness for each HUC4

###########Load Map data###############################
# State boundaries
states <- read.csv('/mnt/research/aquaxterra/DATA/state_borders/states_albers.csv', stringsAsFactors = FALSE)
load(file.path('/mnt/research/aquaxterra/DATA/state_borders', 'states_albers.RData'))

# Load shape file of HUC4 boundaries
huc4 <- readOGR(dsn = '/mnt/research/aquaxterra/DATA/reprojected_data/HUC', layer = 'HU4_CONUS_Alb')

# Clip the huc4 shape file to the borders of the continental USA, since the map looks nicer that way.
goodusabounds <- gUnaryUnion(states_albers)
hucdat <- huc4@data
huc4 <- gIntersection(huc4, goodusabounds, byid = TRUE, id = row.names(huc4@data))
huc4 <- SpatialPolygonsDataFrame(huc4, hucdat)

# Merge the summary CSV data with HUC4. The HUC4 must be converted to numeric for both.
# Your data needs to have a HUC4 column converted to numeric.
huc4@data <- huc4@data %>% mutate(HUC4 = as.numeric(as.character(HUC4)), id = rownames(huc4@data)) %>% left_join(insect_cast)

# Convert spatial data into a data frame usable by ggplot2
huc4_fort <- fortify(huc4, region = 'id') %>% left_join(huc4@data, by = 'id')

# Here are some example color schemes
rbcolors <- rev(RColorBrewer::brewer.pal(9, 'RdYlBu'))
purple8colors <- c('#fcfbfd','#efedf5','#dadaeb','#bcbddc','#9e9ac8','#807dba','#6a51a3','#54278f','#3f007d')[-1]
elevcolors <- topo.colors(9)
yellowtogreencolors <- colorRampPalette(c('forestgreen','lightgoldenrod1'))(9)
browntogreencolors <- c('#8c510a','#bf812d','#dfc27d','#f6e8c3','#f5f5f5','#c7eae5','#80cdc1','#35978f','#01665e')
browntobluecolors <- colorRampPalette(c('tan','skyblue'))(9)

# Draw the map (slow and uses quite a bit of memory)
the_map <- ggplot(huc4_fort) +
  geom_polygon(aes_string(x='long', y='lat', group='group', fill=huc4_fort$richness_all_aquatic_insects)) +
  geom_path(aes_string(x='long', y='lat', group='group'), color = 'white', size = 0.25) +
  geom_path(data = states, aes(x = long, y = lat, group = group), color = 'gray20') +
  scale_fill_gradientn(colours = rbcolors, name = 'Family Richness') +
  coord_equal() +
  theme_bw() + 
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'black'), panel.border = element_blank(), plot.background = element_rect(fill = 'black'), legend.position = c(0.13,0.1), legend.direction = 'horizontal', legend.title = element_blank(), legend.text = element_text(color = 'white'), legend.background = element_rect(fill = 'black'))
ggsave(filename = "HUC4_Resampled10_Insect_family_richness.png", plot = the_map, height = 6, width = 9, dpi = 400)






