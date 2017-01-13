# Exploratory analysis of joined BBS and LAGOS datasets
# For now, use the by-route data since the by-stop data takes forever to work with.

load('/mnt/research/aquaxterra/DATA/raw_data/BBS/BBS_LAGOS_byroute_join.r')
load('/mnt/research/aquaxterra/DATA/raw_data/BBS/BBS_LAGOS_bystop_join.r')


# Ridiculous fishing expedition.
# Enumerate bird variables, environmental variables, and find all pairwise correlations.

bird_vars <- 3:24
huc_vars <- c(39:261, 263:501)

pairwise_cors <- matrix(nrow = length(bird_vars), ncol = length(huc_vars))
dimnames(pairwise_cors) <- list(names(bbs_byroute_lagos_huc4)[bird_vars], names(bbs_byroute_lagos_huc4)[huc_vars])

pb <- txtProgressBar(0, length(bird_vars)*length(huc_vars), style=3)

for (i in 1:length(bird_vars)) {
	for (j in 1:length(huc_vars)) {
		pairwise_cors[i, j] <- cor(bbs_byroute_lagos_huc4[, bird_vars[i]], bbs_byroute_lagos_huc4[, huc_vars[j]], use = 'complete.obs')
		setTxtProgressBar(pb, j + length(huc_vars)*(i-1))
	}
}

close(pb)

sum(abs(pairwise_cors) > 0.4, na.rm = T) # Only 8 of them are bigger than 0.4. Not great.

which(abs(pairwise_cors) > 0.4, arr.ind = T)

# Try again with huc8

pairwise_cors8 <- matrix(nrow = length(bird_vars), ncol = length(huc_vars))
dimnames(pairwise_cors8) <- list(names(bbs_byroute_lagos_huc8)[bird_vars], names(bbs_byroute_lagos_huc8)[huc_vars])

pb <- txtProgressBar(0, length(bird_vars)*length(huc_vars), style=3)

for (i in 1:length(bird_vars)) {
	for (j in 1:length(huc_vars)) {
		pairwise_cors8[i, j] <- cor(bbs_byroute_lagos_huc8[, bird_vars[i]], bbs_byroute_lagos_huc8[, huc_vars[j]], use = 'complete.obs')
		setTxtProgressBar(pb, j + length(huc_vars)*(i-1))
	}
}

close(pb)

which(abs(pairwise_cors8) > 0.4, arr.ind = T)

#################################################

# Try fishing expedition again, this time averaging the 2001-2011 data

library(dplyr)

bbslagos4 <- bbs_byroute_lagos_huc4 %>% 
	filter(year >= 2001, year <= 2011) %>%
	group_by(rteNo) %>%
	summarize_all(.funs = mean)

for (i in 1:length(bird_vars)) {
	for (j in 1:length(huc_vars)) {
		pairwise_cors[i, j] <- cor(bbslagos4[, bird_vars[i]], bbslagos4[, huc_vars[j]], use = 'complete.obs')
		}
}	

#################################################

# By year and by type joins.

load('/mnt/research/aquaxterra/DATA/raw_data/BBS/BBS_LAGOS_stop_year_type.r')

pairwise_bygroup <- function(dat, x, y) {
	pairwise_cors <- matrix(nrow = length(x), ncol = length(y))
	dimnames(pairwise_cors) <- list(names(dat)[x], names(dat)[y])

	pb <- txtProgressBar(0, length(x)*length(y), style=3)

	for (i in 1:length(x)) {
		for (j in 1:length(y)) {
			pairwise_cors[i, j] <- cor(dat[, x[i]], dat[, y[j]], use = 'complete.obs')
			setTxtProgressBar(pb, j + length(y)*(i-1))
		}
	}

	close(pb)
	return(pairwise_cors)
} 

pairwise_bygroup(bbs_lagos_route_huc4[[1]], x=3:14, y=16:26) # HUC4 chemistry (deposition)
pairwise_bygroup(bbs_lagos_route_huc4[[2]], x=3:18, y=20:30) # HUC4 climate
pairwise_bygroup(bbs_lagos_route_huc4[[4]], x=3:45, y=47:57) # HUC4 geology
pairwise_bygroup(bbs_lagos_route_huc4[[5]], x=3:172, y=174:184) # HUC4 hydrology
pairwise_bygroup(bbs_lagos_route_huc4[[6]], x=3:60, y=62:72) # HUC4 landcover
pairwise_bygroup(bbs_lagos_route_huc4[[7]], x=3:10, y=12:22) # HUC4 topography
	
#################################################

# Use by stop data to see whether there are any closer relationships given the more local scale.

# For by-stop, HUC4:
bird_vars <- 4:14
huc_vars <- c(28:250, 252:490)

huc4stoppairs <- pairwise_bygroup(bbs_lagos_huc4, bird_vars, huc_vars)
huc4stopcat <- cut(abs(huc4stoppairs), 10)
# None of them are greater than 0.25. Sad!

huc8stoppairs <- pairwise_bygroup(bbs_lagos_huc8, bird_vars, huc_vars)
huc8stopcat <- cut(abs(huc8stoppairs), 10)

# For by-stop, HUC8:
huc12stoppairs <- pairwise_bygroup(bbs_lagos_huc12, 4:14, c(31:172, 174:250, 252:486))
huc12stopcat <- cut(abs(huc12stoppairs), 10)

# Create heat map of correlations.
# For ggplot2 to make a heat map, we need to convert the correlation matrices to long format.
library(reshape2)
library(ggplot2)
library(dplyr)

huc4stoppairs_long <- melt(huc4stoppairs, varnames = c('bird_var','huc_var')) %>% mutate(huc_var = substr(as.character(huc_var), 1, 25))
huc8stoppairs_long <- melt(huc8stoppairs, varnames = c('bird_var','huc_var')) %>% mutate(huc_var = substr(as.character(huc_var), 1, 25))
huc12stoppairs_long <- melt(huc12stoppairs, varnames = c('bird_var','huc_var')) %>% mutate(huc_var = substr(as.character(huc_var), 1, 25))
# truncate huc var names too

phuc4 <- ggplot(huc4stoppairs_long, aes(x = huc_var, y = bird_var, fill = value)) +
	geom_tile(color = 'black') +
	scale_fill_gradient(low = 'white', high = 'steelblue') +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) + 
	theme(legend.position = "bottom",
		  panel.grid = element_blank(),
		  panel.background = element_rect(fill = 'black'),
		  axis.ticks = element_blank(), 
		  axis.text.x = element_text(size = 5, angle = 270, hjust = 0, colour = "grey50"))
		  
phuc8 <- ggplot(huc8stoppairs_long, aes(x = huc_var, y = bird_var, fill = value)) +
	geom_tile(color = 'black') +
	scale_fill_gradient(low = 'white', high = 'steelblue') +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) + 
	theme(legend.position = "bottom",
		  panel.grid = element_blank(),
		  panel.background = element_rect(fill = 'black'),
		  axis.ticks = element_blank(), 
		  axis.text.x = element_text(size = 5, angle = 270, hjust = 0, colour = "grey50"))	

phuc12 <- ggplot(huc12stoppairs_long, aes(x = huc_var, y = bird_var, fill = value)) +
	geom_tile(color = 'black') +
	scale_fill_gradient(low = 'white', high = 'steelblue') +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) + 
	theme(legend.position = "bottom",
		  panel.grid = element_blank(),
		  panel.background = element_rect(fill = 'black'),
		  axis.ticks = element_blank(), 
		  axis.text.x = element_text(size = 5, angle = 270, hjust = 0, colour = "grey50"))		  
		  
ggsave('/mnt/research/aquaxterra/FIGS/bbs_lagos_pairplots/bbs_bystop_lagos_huc4_heatmap.png', phuc4, height = 4, width = 16, dpi = 400)
ggsave('/mnt/research/aquaxterra/FIGS/bbs_lagos_pairplots/bbs_bystop_lagos_huc8_heatmap.png', phuc8, height = 4, width = 16, dpi = 400)
ggsave('/mnt/research/aquaxterra/FIGS/bbs_lagos_pairplots/bbs_bystop_lagos_huc12_heatmap.png', phuc12, height = 4, width = 16, dpi = 400)