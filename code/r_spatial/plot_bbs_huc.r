# Plot bbs and huc together.

nbbs <- rep(0, nrow(huc12nolakes@data))
for (i in 1:nrow(huc12nolakes@data)) {
	nbbs[i] <- sum(as.numeric(as.character(huc12nolakes@data$HUC12[i])) == bbs_huc$HUC12)
}

huc12nolakes@data$nbbs <- nbbs

library(ggplot2)
library(maptools)

huc12fort <- fortify(huc12nolakes, region = 'HUC12')
huc12fort <- huc12fort %>% rename(HUC12=id) %>% left_join(huc12nolakes@data %>% select(HUC12, nbbs))
save(huc12fort, file = '/mnt/research/aquaxterra/huc12fort.r')

nbbsmap <- ggplot(huc12fort, aes(y = lat, x = long, group = group, fill = nbbs)) + 
	geom_polygon(lwd = 0) +
	coord_map() + theme_bw() +
	ggtitle('Number of BBS routes within each HUC12')
	
ggsave('/mnt/research/aquaxterra/FIGS/huc_by_nbbs.png', nbbsmap, height=9, width=12, dpi=300)