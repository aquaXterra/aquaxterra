# Schematic figure with bbs and nasa.

library(dplyr)
library(rgdal)
library(maptools)

# Proj4 strings for Albers and the unprojected latlong.
aea_crs <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0'
latlong_crs <- '+proj=longlat +ellps=WGS72 +no_defs'


# Load BBS data
bbs_div <- read.csv('C:/Users/Q/Dropbox/projects/aquaxterra/bbs_div_2011.csv', stringsAsFactors = FALSE)
bbsstops <- bbs_div %>% select(rteNo, Stop, POINT_X, POINT_Y)
rm(bbs_div)

# Load HUC boundaries
# Use HUC8 because it should be a reasonable size.
huc8 <- readOGR(dsn = 'C:/Users/Q/Dropbox/projects/aquaxterra/hucshapefiles', layer = 'HU8_CONUS_Alb')

# Subset bbs and huc to Michigan. Region code is 49
huc8 <- subset(huc8, grepl('MI', huc8@data$STATES))
bbsstops_mich <- subset(bbsstops, substr(as.character(rteNo),1,2) == '49' & !is.na(POINT_X))

# Convert bbsstops to the Albers projection.
bbsmich_df <- with(bbsstops_mich, SpatialPointsDataFrame(coords = cbind(POINT_X,POINT_Y), data=data.frame(rteNo,Stop), proj4string = CRS(latlong_crs)))
bbsmich_df <- spTransform(bbsmich_df, CRSobj = CRS(aea_crs))
bbsstops_mich_aea <- data.frame(x = bbsmich_df@coords[,1], y = bbsmich_df@coords[,2], rteNo = bbsmich_df@data$rteNo, Stop = bbsmich_df@data$Stop)

# Quick plot to show.
plot(huc8)
points(x=bbsstops_mich_aea$x, y=bbsstops_mich_aea$y, cex=0.5, col='red')

# Do in ggplot2
library(ggplot2)
huc8@data <- huc8@data %>% mutate(HUC8 = as.numeric(as.character(HUC8)), id = rownames(huc8@data))
huc8fort <- fortify(huc8, region = 'id')

ggplot(huc8fort, aes(long,lat, group=group)) + geom_path() + coord_map(projection = 'albers', lat0=23, lat1=29.5)


# Load huc12 for only Michigan
# load('C:/Users/Q/Dropbox/projects/aquaxterra/hucshapefiles/huc12mich.r')
# 
# huc12mich@data <- huc12mich@data %>% mutate(HUC12 = as.numeric(as.character(HUC12)), id = rownames(huc12mich@data))
# huc12fort <- fortify(huc12mich, region = 'id')

# Load pre-fortified huc12 data for Michigan (very large)
load('C:/Users/Q/Dropbox/projects/aquaxterra/hucshapefiles/huc12fort.r')

# Subset to a single route.
# Monterey, MI.
bbsoneroute <- subset(bbsstops_mich_aea, rteNo == 49037)
names(bbsoneroute)[1:2] <- c('long','lat')

# Bounding box for the map.
minlong <- 814000
maxlong <- 827000
minlat <- 2209000
maxlat <- 2227000

# Display empty grids on a map.
# high res example: SRTM 30 m
# low res example: MODIS 1 km
highres <- 30
lowres <- 1000
lat01deg <- 111093/10
lon01deg <- 81541/10
buff <- 15000
xcoordshi <- seq(minlong-buff, maxlong+buff, by = highres)
ycoordshi <- seq(minlat-buff, maxlat+buff, by = highres)
xcoordslo <- seq(minlong-buff, maxlong+buff, by = lowres)
ycoordslo <- seq(minlat-buff, maxlat+buff, by = lowres)
xcoordslon <- seq(minlong-buff, maxlong+buff, by = lon01deg)
ycoordslat <- seq(minlat-buff, maxlat+buff, by = lat01deg)


xyhi <- expand.grid(long=xcoordshi, lat=ycoordshi)
xylo <- expand.grid(long=xcoordslo, lat=ycoordslo)
xylonlat <- expand.grid(long=xcoordslon, lat=ycoordslat)

michlong <- c(-89.9, -82.5)
michlat <- c(41.7, 46.9)
# This does not work.
#mich01grid <- expand.grid(long = seq(michlong[1], michlong[2], by=0.1), lat = seq(michlat[1], michlat[2], by=0.1))
#mich01gridsp <- SpatialPoints(coords=mich01grid, proj4string = CRS(latlong_crs))
#mich01gridaea <- spTransform(mich01gridsp, CRSobj = CRS(aea_crs))
#mich01gridaeadf <- data.frame(long=mich01gridaea@coords[,1], lat=mich01gridaea@coords[,2])

ggplot(huc8fort, aes(long,lat)) + 
  geom_path(aes(group=group)) + coord_map() + 
  geom_tile(data = xyhi, fill = 'transparent', color = 'black') +
  geom_tile(data = xylo, fill = 'transparent', color = 'red') +
  xlim(minlong, maxlong) + ylim(minlat, maxlat)

hs <- huc8fort$long>minlong & huc8fort$long<maxlong & huc8fort$lat>minlat & huc8fort$lat<maxlat
huc8fortsub <- subset(huc8fort, id %in% huc8fort[hs,]$id)
hs12 <- huc12fort$long>minlong & huc12fort$long<maxlong & huc12fort$lat>minlat & huc12fort$lat<maxlat
huc12fortsub <- subset(huc12fort, id %in% huc12fort[hs12,]$id)


ggplot(bbsoneroute, aes(long,lat)) +
  geom_tile(data=huc8fort, aes(group=group)) +
  geom_path(size=2) +
  geom_point(size=3) +
  #geom_tile(data = xylo, fill = 'transparent', color = 'red') +
  geom_tile(data = mich01gridaeadf, fill = 'transparent', color = 'black') +
  
  xlim(minlong-10000, maxlong+10000) + ylim(minlat-10000, maxlat+10000) +
  theme_void() + coord_fixed(ratio=1)


#ggplot(mich01gridaeadf) + geom_tile(aes(x=long,y=lat), fill='skyblue',color='black')

mytheme <- theme_bw() + theme(panel.grid=element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank(), legend.position='none')

ggplot(huc8fort, aes(long,lat)) +
  geom_path(aes(group=group)) + coord_fixed(ratio = 1, xlim=c(minlong-20000, maxlong+20000), ylim=c(minlat-20000, maxlat+20000)) +
  theme_bw() + theme(panel.grid = element_blank())

ggplot(huc8fort, aes(long,lat)) +
  geom_polygon(aes(group=group, fill=id), alpha = 0.75) +
  geom_path(aes(group=group)) + 
  scale_fill_manual(values = gray.colors(length(unique(huc8fort$id)))) +
  coord_fixed(ratio = 1, xlim=c(minlong-15000, maxlong+15000), ylim=c(minlat-15000, maxlat+15000)) +
  mytheme +
  geom_path(data = bbsoneroute, size = 1) +
  geom_point(data = bbsoneroute, size = 2)

mapbuff <- 3000
mapbuffx <- 3000
mapbuffy <- 1500

panel_abc <- ggplot(huc12fort, aes(long,lat)) +
  geom_polygon(aes(group=group, fill=id), alpha = 0.5) +
  geom_path(aes(group=group), color = 'white') + 
  scale_fill_manual(values = gray.colors(length(unique(huc12fort$id)))) +
  geom_path(data = bbsoneroute, size = 1) +
  geom_point(data = bbsoneroute, size = 2) +
  geom_tile(data = xylo, fill='transparent', color='black') +
  geom_tile(data = xylonlat, fill = 'transparent', color = 'slateblue3', lwd = 1) +
  coord_fixed(ratio = 1, xlim=c(minlong-mapbuffx, maxlong+mapbuffx), ylim=c(minlat-mapbuffy, maxlat+mapbuffy)) +
  mytheme

ggsave('C:/Users/Q/Dropbox/projects/aquaxterra/hucfigures/schematic1.png', panel_abc, height=5, width=4, dpi=300)

###############################

# Split into 3 maps

# Low res grid
panel_a <- ggplot(huc12fort, aes(long,lat)) +
  #geom_polygon(aes(group=group, fill=id), alpha = 0.5) +
  #geom_path(aes(group=group), color = 'white') + 
  #scale_fill_manual(values = gray.colors(length(unique(huc12fort$id)))) +
  geom_path(data = bbsoneroute, size = 1) +
  geom_point(data = bbsoneroute, size = 2) +
  #geom_tile(data = xylo, fill='transparent', color='black') +
  geom_tile(data = xylonlat, fill = 'transparent', color = 'slateblue3', lwd = 1) +
  coord_fixed(ratio = 1, xlim=c(minlong-mapbuffx, maxlong+mapbuffx), ylim=c(minlat-mapbuffy, maxlat+mapbuffy)) +
  mytheme

# High res grid
panel_b <- ggplot(huc12fort, aes(long,lat)) +
  #geom_polygon(aes(group=group, fill=id), alpha = 0.5) +
  #geom_path(aes(group=group), color = 'white') + 
  #scale_fill_manual(values = gray.colors(length(unique(huc12fort$id)))) +
  geom_path(data = bbsoneroute, size = 1) +
  geom_point(data = bbsoneroute, size = 2) +
  geom_tile(data = xylo, fill='transparent', color='black') +
  #geom_tile(data = xylonlat, fill = 'transparent', color = 'slateblue3', lwd = 1) +
  coord_fixed(ratio = 1, xlim=c(minlong-mapbuffx, maxlong+mapbuffx), ylim=c(minlat-mapbuffy, maxlat+mapbuffy)) +
  mytheme

# HUC12
panel_c <- ggplot(huc12fort, aes(long,lat)) +
  geom_polygon(aes(group=group, fill=id), alpha = 0.5) +
  geom_path(aes(group=group), color = 'white') + 
  scale_fill_manual(values = gray.colors(length(unique(huc12fort$id)))) +
  geom_path(data = bbsoneroute, size = 1) +
  geom_point(data = bbsoneroute, size = 2) +
  #geom_tile(data = xylo, fill='transparent', color='black') +
  #geom_tile(data = xylonlat, fill = 'transparent', color = 'slateblue3', lwd = 1) +
  coord_fixed(ratio = 1, xlim=c(minlong-mapbuffx, maxlong+mapbuffx), ylim=c(minlat-mapbuffy, maxlat+mapbuffy)) +
  mytheme

fp <- 'C:/Users/Q/Google Drive/NASABiodiversityWG/Figures/schematics/'
ggsave(file.path(fp,'schematic_abc.png'), panel_abc, height=5, width=4, dpi=400)
ggsave(file.path(fp,'schematic_a.png'), panel_a, height=5, width=4, dpi=400)
ggsave(file.path(fp,'schematic_b.png'), panel_b, height=5, width=4, dpi=400)
ggsave(file.path(fp,'schematic_c.png'), panel_c, height=5, width=4, dpi=400)


#####################################################################

# Try to overlap this with a ggmap satellite image
library(ggmap)
bboxcoords <- data.frame(long=c(minlong-mapbuffx, maxlong+mapbuffx), lat=c(minlat-mapbuffy, maxlat+mapbuffy))
bboxlatlong <- spTransform(SpatialPoints(bboxcoords, proj4string = CRS(aea_crs)), CRSobj = CRS(ll84_crs))
bboxvec <- c(bboxlatlong@coords[1,], bboxlatlong@coords[2,])
names(bboxvec) <- c('left','bottom','right','top')

mimap <- get_map(maptype='satellite', source='google', location=bboxvec, zoom=11)
mimap <- get_map(source='cloudmade', location=bboxvec, zoom=12)

ggmap(mimap, alpha = 0.5)
ggmap(mimap)


# Project everything into lat long.
ll84_crs <- '+proj=longlat +datum=WGS84'

huc8latlong <- spTransform(huc8, CRSobj = CRS(ll84_crs))
huc8latlongfort <- fortify(huc8latlong, region = 'id')

huc12latlong <- spTransform(SpatialPoints(huc12fort[,c('long','lat')], proj4string = CRS(aea_crs)), CRSobj = CRS(ll84_crs))
huc12latlongfort <- cbind(huc12latlong@coords, huc12fort[,3:7])

# xylosp <- SpatialPoints(coords=xylo, proj4string = CRS(aea_crs))
# xylosp <- spTransform(xylosp, CRSobj = CRS(ll84_crs))
# xylo2 <- as.data.frame(xylosp@coords)
# 
# xylonlatsp <- SpatialPoints(coords=xylonlat, proj4string = CRS(aea_crs))
# xylonlatsp <- spTransform(xylonlatsp, CRSobj = CRS(ll84_crs))
# xylonlat2 <- as.data.frame(xylonlatsp@coords)

highreslat <- 1000/111093
highreslon <- 1000/81541
lowres <- 0.1
buff <- 1
xcoordsllhi <- seq(bboxlatlong@coords[1,1]-buff, bboxlatlong@coords[2,1]+buff, by = highreslon)
ycoordsllhi <- seq(bboxlatlong@coords[1,2]-buff, bboxlatlong@coords[2,2]+buff, by = highreslat)
xcoordslllo <- seq(floor(bboxlatlong@coords[1,1]-buff), ceiling(bboxlatlong@coords[2,1]+buff), by = lowres)
ycoordslllo <-seq(floor(bboxlatlong@coords[1,2]-buff), ceiling(bboxlatlong@coords[2,2]+buff), by = lowres)

xyllhi <- expand.grid(long=xcoordsllhi, lat=ycoordsllhi)
xylllo <- expand.grid(long=xcoordslllo, lat=ycoordslllo)


bbsonesp <- SpatialPoints(coords=bbsoneroute[,1:2], proj4string = CRS(aea_crs))
bbsonesp <- spTransform(bbsonesp, CRSobj = CRS(ll84_crs))
bbsoneroute2 <- as.data.frame(bbsonesp@coords)

ggpanel_base <- ggmap(mimap, darken = c(0.1, 'white')) +
  mytheme +
  coord_map(projection='albers', parameters = c(lat0=23, lat1=29.5), xlim=bboxlatlong@coords[,1]+c(0.02,-0.01), ylim=bboxlatlong@coords[,2]+c(0,-0.01))

ggpanel_abc <- ggpanel_base +
  geom_path(data = huc12latlongfort, aes(x=long, y=lat, group=group), color = 'white') + 
  geom_path(data = bbsoneroute2, aes(x=long, y=lat), size = 1, color = 'white') +
  geom_point(data = bbsoneroute2, aes(x=long, y=lat), size = 2, color = 'white') +
  geom_tile(data = xyllhi, aes(x=long, y=lat), fill='transparent', color='gray85') +
  geom_tile(data = xylllo, aes(x=long, y=lat), fill = 'transparent', color = 'slateblue3', lwd = 1)

ggpanel_a <- ggpanel_base +
  geom_path(data = bbsoneroute2, aes(x=long, y=lat), size = 1, color = 'white') +
  geom_point(data = bbsoneroute2, aes(x=long, y=lat), size = 2, color = 'white') +
  geom_tile(data = xylllo, aes(x=long, y=lat), fill = 'transparent', color = 'slateblue3', lwd = 1)

ggpanel_b <- ggpanel_base +
  geom_path(data = bbsoneroute2, aes(x=long, y=lat), size = 1, color = 'white') +
  geom_point(data = bbsoneroute2, aes(x=long, y=lat), size = 2, color = 'white') +
  geom_tile(data = xyllhi, aes(x=long, y=lat), fill='transparent', color='gray85')

ggpanel_c <- ggpanel_base +
  geom_path(data = huc12latlongfort, aes(x=long, y=lat, group=group), color = 'white') + 
  geom_path(data = bbsoneroute2, aes(x=long, y=lat), size = 1, color = 'white') +
  geom_point(data = bbsoneroute2, aes(x=long, y=lat), size = 2, color = 'white') 


fp <- 'C:/Users/Q/Google Drive/NASABiodiversityWG/Figures/schematics/'
ggsave(file.path(fp,'schematic_satellite_abc.png'), ggpanel_abc, height=5, width=4, dpi=400)
ggsave(file.path(fp,'schematic_satellite_a.png'), ggpanel_a, height=5, width=4, dpi=400)
ggsave(file.path(fp,'schematic_satellite_b.png'), ggpanel_b, height=5, width=4, dpi=400)
ggsave(file.path(fp,'schematic_satellite_c.png'), ggpanel_c, height=5, width=4, dpi=400)

