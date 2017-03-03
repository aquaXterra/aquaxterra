# Reproject state boundaries into Albers projection for plotting BBS and FIA data.

aea_crs <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0'
wgs_crs <- '+proj=longlat +ellps=WGS84 +no_defs'

states <- map_data('state')
state_coords <- SpatialPoints(coords = with(states, data.frame(x = long, y = lat)), proj4string = CRS(wgs_crs))
state_albers <- spTransform(state_coords, CRSobj = CRS(aea_crs))
states$long <- state_albers@coords[,1]
states$lat <- state_albers@coords[,2]

# Test plot
ggplot(states, aes(x=long,y=lat,group=group)) + geom_path()

# Save shape
write.csv(states, file = '~/states_albers.csv', row.names = FALSE)