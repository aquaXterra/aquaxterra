
library(dplyr)
library(rgdal)
library(maptools)

# Proj4 strings for Albers and the unprojected latlong.
aea_crs <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0'
latlong_crs <- '+proj=longlat +ellps=WGS72 +no_defs'

# Load HUC boundaries
huc4 <- readOGR(dsn = 'C:/Users/Q/Dropbox/projects/aquaxterra/hucshapefiles', layer = 'HU4_CONUS_Alb')
huc8 <- readOGR(dsn = 'C:/Users/Q/Dropbox/projects/aquaxterra/hucshapefiles', layer = 'HU8_CONUS_Alb')

# Subset bbs and huc to Michigan. Region code is 49
huc4 <- subset(huc4, grepl('MI', huc4@data$STATES))
huc8 <- subset(huc8, grepl('MI', huc8@data$STATES))

library(ggplot2)
huc4@data <- huc4@data %>% mutate(HUC4 = as.numeric(as.character(HUC4)), id = rownames(huc4@data))
huc4fort <- fortify(huc4, region = 'id')

huc8@data <- huc8@data %>% mutate(HUC8 = as.numeric(as.character(HUC8)), id = rownames(huc8@data))
huc8fort <- fortify(huc8, region = 'id')

load('C:/Users/Q/Dropbox/projects/aquaxterra/hucshapefiles/huc12fort.r')

# Create map
#ggplot(huc8fort, aes(long,lat, group=group)) + geom_path() + coord_map(projection = 'albers', lat0=23, lat1=29.5)


##############################
# Map with nested lines

# Bounding box for the map.
minlong <- 804000
maxlong <- 954000
minlat <- 2199000
maxlat <- 2349000


mytheme <- theme_bw() + theme(panel.grid=element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank(), legend.position='none')


p1 <- ggplot() +
  #geom_polygon(data=huc12fort, aes(x=long, y=lat, group=group, fill=id), alpha = 0.5) +
  
  geom_path(data=huc12fort, aes(x=long, y=lat, group=group), color = 'white') + 
  geom_path(data=huc8fort, aes(x=long, y=lat, group=group), color = 'white', size = 2) +
  geom_path(data=huc4fort, aes(x=long, y=lat, group=group), color = 'white', size = 4) +
  #scale_fill_manual(values = gray.colors(length(unique(huc12fort$id)))) +
  coord_fixed(ratio = 1, xlim=c(minlong, maxlong), ylim=c(minlat, maxlat)) +
  mytheme + theme(panel.background = element_rect(fill='gray50'))

ggsave('C:/Users/Q/Google Drive/aquaxterra_watercube_all/Maps_and_figs/nestedmap.png', p1, height=6, width=6, dpi=300)

#############################################
# 1. add colored polygons to the plot by bird richness
# 2. add a scale bar
# 3. add a North arrow

# Load bbs diversity. For now use the old 2011 by stop diversity just for show.
bbsd <- read.csv('C:/Users/Q/Dropbox/projects/aquaxterra/bbs_div_2011.csv', stringsAsFactors = F)

load('C:/Users/Q/Dropbox/projects/aquaxterra/hucshapefiles/huc12mich.r')
bbsd_huc <- bbsd %>% 
  filter(year==2011) %>%
  group_by(HUC12) %>%
  summarize(richness=median(richness))
huc12mich@data <- huc12mich@data %>% mutate(HUC12 = as.numeric(as.character(HUC12)), id = rownames(huc12mich@data)) %>%
  left_join(bbsd_huc)

huc12fort <- left_join(huc12fort, huc12mich@data[,c('id','richness')])

library(ggsn)

mytheme2 <- theme_bw() + theme(panel.grid=element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank(), legend.position='bottom')

p1_colors <- ggplot() +
  geom_polygon(data=huc12fort, aes(x=long, y=lat, group=group, fill=richness)) +
  
  geom_path(data=huc12fort, aes(x=long, y=lat, group=group), color = 'white', size = 0.25) + 
  geom_path(data=huc8fort, aes(x=long, y=lat, group=group), color = 'white', size = 1) +
  geom_path(data=huc4fort, aes(x=long, y=lat, group=group), color = 'white', size = 2) +
  scale_fill_gradientn(name = 'Breeding bird richness', colours=RColorBrewer::brewer.pal(9,'YlOrRd')) +
  coord_fixed(ratio = 1, xlim=c(minlong, maxlong), ylim=c(minlat, maxlat)) +
  scalebar(data=NULL, dist=10, x.min=930000, x.max = 950000, y.min=2200000, y.max=2205000, height = 0.3, st.size = 2.3, st.dist = 0.3, st.bottom=F) +
  mytheme2 #+ theme(panel.background = element_rect(fill='gray50'))

ggsave('C:/Users/Q/Google Drive/aquaxterra_watercube_all/Maps_and_figs/nestedmap_colors.png', p1_colors, height=6, width=6, dpi=300)




###############################
# Map with nested lines and a satellite image to look cooler

aea_crs <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0'
ll84_crs <- '+proj=longlat +datum=WGS84'

library(ggmap)
bboxcoords <- data.frame(long=c(minlong, maxlong), lat=c(minlat, maxlat))
bboxlatlong <- spTransform(SpatialPoints(bboxcoords, proj4string = CRS(aea_crs)), CRSobj = CRS(ll84_crs))
bboxvec <- c(bboxlatlong@coords[1,], bboxlatlong@coords[2,])
names(bboxvec) <- c('left','bottom','right','top')

mimap <- get_map(maptype='satellite', source='google', location=bboxvec, zoom=11)

