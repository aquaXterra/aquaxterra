# Exploratory data analysis of BBS and MODIS data sets
# Start with by-route data, then switch to by-stop data (larger)

fp <- '/mnt/research/aquaxterra/DATA/raw_data/BBS'
bm <- read.csv(file.path(fp, 'bbs_allcovariates_byroute.csv'), stringsAsFactors = FALSE)
bd <- read.csv(file.path(fp, 'bbs_div_byroute.csv'), stringsAsFactors = FALSE)

# Join BBS diversity and MODIS/PRISM data by route number and year.

# Convert MODIS df to wide format, similar to the BBS df.

library(dplyr)
library(reshape2)

bmwide <- dcast(bm, rteNo + year ~ variable)

# This joined data frame contains all needed to run exploratory correlational analysis (incl. raw, spatial, and time-series)
bmd <- full_join(bmwide, bd)
bmd2 <- filter(bmd, year >= 2001, year <= 2011)

# For raw analysis, average out by year
bmdyear <- bmd2[,1:51] %>% group_by(rteNo) %>% summarize_all(.funs = mean, na.rm = TRUE) 

# Group variable column indices by type (split different diversity types into groups as well).
bioclim_idx <- 3:21
modisnlcd_idx <- 22:29
tdiv_idx <- 30:33
fdiv_idx <- 34:37
pdiv_idx <- c(39,40,44,46,50)

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

# Go fishing!
bc_td <- pairwise_bygroup(bmdyear, bioclim_idx, tdiv_idx)
bc_fd <- pairwise_bygroup(bmdyear, bioclim_idx, fdiv_idx)
bc_pd <- pairwise_bygroup(bmdyear, bioclim_idx, pdiv_idx)
mn_td <- pairwise_bygroup(bmdyear, modisnlcd_idx, tdiv_idx)
mn_fd <- pairwise_bygroup(bmdyear, modisnlcd_idx, fdiv_idx)
mn_pd <- pairwise_bygroup(bmdyear, modisnlcd_idx, pdiv_idx)

# Modified pairs plots
pairplot_bygroup <- function(dat, x, y, filename) {
	require(ggplot2)
	require(gridExtra)
	
	pb <- txtProgressBar(0, length(x)*length(y), style=3)
	plot_list <- list()
	
	for (i in 1:length(y)) {
		for (j in 1:length(x)) {
			th <- theme_bw()
			if (i != length(y)) th <- th + theme(axis.title.x = element_blank())
			if (j != 1) th <- th + theme(axis.title.y = element_blank())
			plot_list[[length(plot_list) + 1]] <- ggplot(dat, aes_string(x = names(dat)[x[j]], y = names(dat)[y[i]])) + geom_point(alpha = 0.3) + th
			setTxtProgressBar(pb, j + length(x)*(i-1))
		}
	}

	close(pb)
	
	png(file.path('~/figs', filename), height = length(y)*1.5, width = length(x)*1.5, units = 'in', res = 400)
	grid.arrange(grobs = plot_list, nrow = length(y))
	dev.off()

} 

pairplot_bygroup(bmdyear, bioclim_idx, tdiv_idx, 'bioclim_taxdiv_pairs.png')
pairplot_bygroup(bmdyear, bioclim_idx, fdiv_idx, 'bioclim_fundiv_pairs.png')
pairplot_bygroup(bmdyear, bioclim_idx, pdiv_idx, 'bioclim_phydiv_pairs.png')
pairplot_bygroup(bmdyear, modisnlcd_idx, tdiv_idx, 'modisnlcd_taxdiv_pairs.png')
pairplot_bygroup(bmdyear, modisnlcd_idx, fdiv_idx, 'modisnlcd_fundiv_pairs.png')
pairplot_bygroup(bmdyear, modisnlcd_idx, pdiv_idx, 'modisnlcd_phydiv_pairs.png')