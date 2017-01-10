sites <- read.csv('data/invertsite_by_huc.csv')

library(maptools)
library(rgdal)

# Load huc8 shapefile
huc8 <- readOGR(dsn = '/mnt/research/aquaxterra/DATA/reprojected_data/HUC', layer = 'HU8_CONUS_Alb')

# Project site points into Albers projection to match huc8

aea_crs <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0'
latlong_crs <- '+proj=longlat +datum=NAD83 +no_defs' # In the future need to fix because there are different ellipses for different coords

# Get rid of points outside the 48 states
sites <- subset(sites, LatitudeMeasure > 25 & LatitudeMeasure < 50 & LongitudeMeasure > -125 & LongitudeMeasure < -67)

sites_latlong <- SpatialPoints(coords = with(sites, data.frame(x = LongitudeMeasure, y = LatitudeMeasure)), proj4string = CRS(latlong_crs))
sites_aea <- spTransform(sites_latlong, CRSobj = CRS(aea_crs))

# Convert huc8 into a data frame that ggplot can use
library(ggplot2)
huc8@data$id <- rownames(huc8@data)
huc8_fort <- fortify(huc8, region = 'id')

# Also convert sites_aea back to a data frame
site_coords <- as.data.frame(sites_aea@coords)

huc8map <- ggplot() +
    geom_path(data = huc8_fort, aes(x=long, y=lat, group=group), color = 'black', size = 0.5) +
	geom_point(data = site_coords, aes(x = x, y = y), color = 'gray50', alpha = 0.5) +
    coord_equal() +
    theme_bw()

ggsave('~/invertmap.png', huc8map, height = 8, width = 12, dpi = 300)

# Plot a map with just the site points

sitemap <- ggplot(sites, aes(x=LongitudeMeasure, y=LatitudeMeasure)) +
	borders('state', fill = 'beige') +
	coord_map() +
	geom_point()
	
# Cut out a single HUC8 and plot it, with the accompanying points

# Pick the Grand River
onehuc <- subset(huc8, NAME == 'Lower Grand' & STATES == 'MI')

# Get out all the site coordinates that are inside the polygon using the over() function
where_overlap <- sp::over(x = sites_aea, y = as(onehuc, 'SpatialPolygons'))

onehucplot <- ggplot() + 
	geom_path(data = onehuc, aes(x=long,y=lat,group=group)) + 
	geom_point(data = site_coords[where_overlap == 1, ], aes(x=x, y=y), color = 'gray50', alpha = 0.5) +
	coord_equal() +
	theme_bw()
