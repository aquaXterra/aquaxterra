# Correlation plots between BBS diversity and HUC variables.
# Forked on 31 March to only do functional group richness, naturally.

# Modified 31 Mar 2017: change all diversity to presence/absence, and add a separate resident-only set of plots.
# Modified 30 Mar 2017: added average climate, interannual climate CV, NLCD variability to data; checked missing values.
# Modified 24 Mar 2017: got rid of some of the redundant diversity variables, tweaked plot
# Modified 14 Feb 2017: Changed from mean to median.

library(dplyr)
library(ggplot2)

### HUC4 ###

# Load summary CSV files
huc4summ <- read.csv('/mnt/research/aquaxterra/CODE/python/RasterOverlay/HUC4summarized.csv', stringsAsFactors = FALSE)

# Add new climate variables (means and interannual CVs)
mns <- list()
cvs <- list()
for (i in 1:19) {
	n <- ifelse(i < 10, paste0('0', i), as.character(i)) # add zero before number.
	cols <- grep(paste0('bio', n), names(huc4summ))
	mns[[i]] <- apply(huc4summ[,cols], 1, mean, na.rm=TRUE)
	x <- huc4summ[,cols]
	if (i %in% c(1,5,6,8,9,10,11)) x <- x + 273.15 # Convert any absolute temperatures to Kelvin to get a true SD.
	cvs[[i]] <- apply(x, 1, sd, na.rm=TRUE)/apply(x, 1, mean, na.rm=TRUE)
}

mns <- as.data.frame(do.call('cbind', mns))
cvs <- as.data.frame(do.call('cbind', cvs))
names(mns) <- paste0('mean_allyears_bio',1:19)
names(cvs) <- paste0('cv_allyears_bio',1:19)

huc4summ <- cbind(huc4summ, mns, cvs)

# Add new NLCD variables (summed groups, and evenness across groups)

# First, replace missing NLCD with zeroes.
nlcdcols <- grep('nlcd', names(huc4summ))
huc4summ[,nlcdcols][is.na(huc4summ[,nlcdcols])] <- 0

huc4summ <- huc4summ %>% mutate(nlcd_forest = nlcd2011_43_perc + nlcd2011_41_perc + nlcd2011_42_perc,
							  nlcd_agriculture = nlcd2011_81_perc + nlcd2011_82_perc,
							  nlcd_developed = nlcd2011_21_perc + nlcd2011_22_perc + nlcd2011_23_perc + nlcd2011_24_perc,
							  nlcd_wetland = nlcd2011_90_perc + nlcd2011_95_perc,
							  nlcd_grassland = nlcd2011_71_perc,
							  nlcd_shrubland = nlcd2011_31_perc,
							  nlcd_ice = nlcd2011_12_perc,
							  nlcd_barren = nlcd2011_31_perc,
							  nlcd_water = nlcd2011_11_perc,
							  nlcd_diversity = vegan::diversity(cbind(nlcd_forest, nlcd_agriculture, nlcd_developed, nlcd_wetland, nlcd_grassland, nlcd_shrubland, nlcd_ice, nlcd_barren, nlcd_water)))							  

### BBS ###

# By route
load('/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_div_byroute_presence.r')
fgrichness <- read.csv('/mnt/research/aquaxterra/DATA/raw_data/BBS/fgrichness.csv', stringsAsFactors = FALSE)

# Subset the routes that are at least half in one HUC4, and get the 2001-2011 means
divmedians <- bbs_div_byroute %>%
	left_join(fgrichness) %>%
	filter(nstops4 > 25, year >= 2001, year <= 2011) %>% # Use only the ones that are a majority in one huc4
	group_by(rteNo, HUC4) %>%
	summarize_at(vars(starts_with('total_richness')), median)
divmediansres <- bbs_div_byrouteres %>%
	left_join(fgrichness) %>%
	filter(nstops4 > 25, year >= 2001, year <= 2011) %>% # Use only the ones that are a majority in one huc4
	group_by(rteNo, HUC4) %>%
	summarize_at(vars(starts_with('resident_richness')), median)

	
# Select variables to plot.
#cols_to_plot <- names(huc4summ)[c(3,5,391:410,601,604,612,615,623:626)]

# Note that some nlcd classes are Alaska only and thus all NA. Get rid of them.
#cols_alaskaonly <- grep('72|73|74|51', names(huc4summ), value = TRUE)
#cols_to_plot <- cols_to_plot[!cols_to_plot %in% cols_alaskaonly]

# Modification 30 March. Use the real column names.
cols_to_plot <- c('mean_altitude','std_altitude','mean_allyears_npp','mean_allyears_gpp','mean_allyears_lai','mean_allyears_fpar','mean_allyears_bio1','mean_allyears_bio4','cv_allyears_bio1','mean_allyears_bio12','mean_allyears_bio15','cv_allyears_bio12','nlcd_forest','nlcd_agriculture','nlcd_developed','nlcd_wetland','nlcd_grassland','nlcd_shrubland','nlcd_ice','nlcd_barren','nlcd_water','nlcd_diversity')

# Merge bird diversity means to HUC4.
huc4corrdat <- left_join(divmedians, huc4summ[,c('HUC4',cols_to_plot)] %>% mutate(HUC4 = as.character(HUC4)))
huc4corrdatres <- left_join(divmediansres, huc4summ[,c('HUC4',cols_to_plot)] %>% mutate(HUC4 = as.character(HUC4)))

# Summarize this by averaging all the routes within HUC.
huc4mediandat <- huc4corrdat %>% ungroup %>%
	select(-rteNo) %>%
	group_by(HUC4) %>%
	summarize_all(.funs = 'median')
huc4mediandatres <- huc4corrdatres %>% ungroup %>%
	select(-rteNo) %>%
	group_by(HUC4) %>%
	summarize_all(.funs = 'median')
	
# Add the NLCD percentages that are in similar categories.
# huc4mediandat <- huc4mediandat %>% mutate(nlcd_forest = nlcd2011_43_perc + nlcd2011_41_perc + nlcd2011_42_perc,
									  # nlcd_agriculture = nlcd2011_81_perc + nlcd2011_82_perc,
									  # nlcd_developed = nlcd2011_21_perc + nlcd2011_22_perc + nlcd2011_23_perc + nlcd2011_24_perc,
									  # nlcd_wetland = nlcd2011_90_perc + nlcd2011_95_perc,
									  # nlcd_grassland = nlcd2011_71_perc)	

# Correlation plots
# Make correlation plots for a lot of variables.
# Modification 31 March: include only 5 diversity variables: richness, FEve, FDis, MPD_Z, MNTD_Z

d_vars <- 2:6
clim_vars_temp <- 13:15
clim_vars_prec <- 16:18
topo_vars <- 7:8
nlcd_vars <- c(19:23, 28)
prod_vars <- 9:12

# Variable name labels.
dlabels <- c('Waterbirds', 'Seed eaters', 'Generalists', 'Insect eaters', 'Meat eaters')
topolabels <- c('mean elevation', 'SD elevation')
templabels <- c('temperature', 'temp seasonality', 'temp interannual CV')
preciplabels <- c('precipitation', 'precip seasonality', 'precip interannual CV')
#nlcdlabels <- c('% mixed forest', '% grassland', '% deciduous', '% evergreen', '% open water', '% ice', '% pasture', '% crops', '% scrubland', '% developed open', '% developed low', '% developed medium', '% developed high', '% woody wetland', '% herb. wetland', '% barren')
nlcdlabels <- c('% forest', '% agricultural', '% developed', '% wetland', '% grassland', 'landcover diversity')
prodlabels <- c('NPP', 'GPP', 'LAI', 'fPAR')

# Modified pairs plots
pairplot_bygroup <- function(dat, x, y, xlabels, ylabels, dirname, filename) {
	require(ggplot2)
	require(gridExtra)
	
	pb <- txtProgressBar(0, length(x)*length(y), style=3)
	plot_list <- list()
	
	for (i in 1:length(y)) {
		for (j in 1:length(x)) {
			th <- theme_bw() + theme(panel.grid = element_blank())
			if (i != length(y)) th <- th + theme(axis.title.x = element_blank())
			if (j != 1) th <- th + theme(axis.title.y = element_blank())
			plot_list[[length(plot_list) + 1]] <- ggplot(dat, aes_string(x = names(dat)[x[j]], y = names(dat)[y[i]])) + geom_point(alpha = 0.3) + stat_smooth() + labs(x=xlabels[j], y=ylabels[i]) + th
			setTxtProgressBar(pb, j + length(x)*(i-1))
		}
	}

	close(pb)
	
	png(file.path(dirname, filename), height = length(y)*2.5, width = length(x)*2.5, units = 'in', res = 200)
	grid.arrange(grobs = plot_list, nrow = length(y))
	dev.off()

} 

dn <- '/mnt/research/aquaxterra/FIGS/bbs_huc_pairplots_fg'

pairplot_bygroup(huc4mediandat, clim_vars_temp, d_vars, templabels, dlabels, dn, 'bbs_vs_huc4temp_allbirds.png')
pairplot_bygroup(huc4mediandat, clim_vars_prec, d_vars, preciplabels, dlabels, dn, 'bbs_vs_huc4precip_allbirds.png')
pairplot_bygroup(huc4mediandat, topo_vars, d_vars, topolabels, dlabels, dn, 'bbs_vs_huc4topo_allbirds.png')
pairplot_bygroup(huc4mediandat, nlcd_vars, d_vars, nlcdlabels, dlabels, dn, 'bbs_vs_huc4nlcd_allbirds.png')
pairplot_bygroup(huc4mediandat, prod_vars, d_vars, prodlabels, dlabels, dn, 'bbs_vs_huc4productivity_allbirds.png')

pairplot_bygroup(huc4mediandatres, clim_vars_temp, d_vars, templabels, dlabels, dn, 'bbs_vs_huc4temp_residents.png')
pairplot_bygroup(huc4mediandatres, clim_vars_prec, d_vars, preciplabels, dlabels, dn, 'bbs_vs_huc4precip_residents.png')
pairplot_bygroup(huc4mediandatres, topo_vars, d_vars, topolabels, dlabels, dn, 'bbs_vs_huc4topo_residents.png')
pairplot_bygroup(huc4mediandatres, nlcd_vars, d_vars, nlcdlabels, dlabels, dn, 'bbs_vs_huc4nlcd_residents.png')
pairplot_bygroup(huc4mediandatres, prod_vars, d_vars, prodlabels, dlabels, dn, 'bbs_vs_huc4productivity_residents.png')

### HUC8 ###

huc8summ <- read.csv('/mnt/research/aquaxterra/CODE/python/RasterOverlay/HUC8summarized.csv', stringsAsFactors = FALSE)

# Add more derived variables (30 March)
mns <- list()
cvs <- list()
for (i in 1:19) {
	n <- ifelse(i < 10, paste0('0', i), as.character(i)) # add zero before number.
	cols <- grep(paste0('bio', n), names(huc8summ))
	mns[[i]] <- apply(huc8summ[,cols], 1, mean, na.rm=TRUE)
	x <- huc8summ[,cols]
	if (i %in% c(1,5,6,8,9,10,11)) x <- x + 273.15 # Convert any absolute temperatures to Kelvin to get a true SD.
	cvs[[i]] <- apply(x, 1, sd, na.rm=TRUE)/apply(x, 1, mean, na.rm=TRUE)
}

mns <- as.data.frame(do.call('cbind', mns))
cvs <- as.data.frame(do.call('cbind', cvs))
names(mns) <- paste0('mean_allyears_bio',1:19)
names(cvs) <- paste0('cv_allyears_bio',1:19)

huc8summ <- cbind(huc8summ, mns, cvs)

# Add new NLCD variables (summed groups, and evenness across groups)

# First, replace missing NLCD with zeroes.
nlcdcols <- grep('nlcd', names(huc8summ))
huc8summ[,nlcdcols][is.na(huc8summ[,nlcdcols])] <- 0

huc8summ <- huc8summ %>% mutate(nlcd_forest = nlcd2011_43_perc + nlcd2011_41_perc + nlcd2011_42_perc,
							  nlcd_agriculture = nlcd2011_81_perc + nlcd2011_82_perc,
							  nlcd_developed = nlcd2011_21_perc + nlcd2011_22_perc + nlcd2011_23_perc + nlcd2011_24_perc,
							  nlcd_wetland = nlcd2011_90_perc + nlcd2011_95_perc,
							  nlcd_grassland = nlcd2011_71_perc,
							  nlcd_shrubland = nlcd2011_31_perc,
							  nlcd_ice = nlcd2011_12_perc,
							  nlcd_barren = nlcd2011_31_perc,
							  nlcd_water = nlcd2011_11_perc,
							  nlcd_diversity = vegan::diversity(cbind(nlcd_forest, nlcd_agriculture, nlcd_developed, nlcd_wetland, nlcd_grassland, nlcd_shrubland, nlcd_ice, nlcd_barren, nlcd_water)))							  
# Subset the routes that are at least half in one HUC8 and get the 2001-2011 means
divmedians8 <- bbs_div_byroute %>%
    left_join(fgrichness) %>%
	filter(nstops8 > 25, year >= 2001, year <= 2011) %>% # Use only the ones that are a majority in one huc8
	group_by(rteNo, HUC8) %>%
	summarize_at(vars(richness, FRic, FEve, FDis, PD, mpd.obs.z, mntd.obs.z), median)
divmedians8res <- bbs_div_byrouteres %>%
	left_join(fgrichness) %>%
	filter(nstops8 > 25, year >= 2001, year <= 2011) %>% # Use only the ones that are a majority in one huc8
	group_by(rteNo, HUC8) %>%
	summarize_at(vars(richness, FRic, FEve, FDis, PD, mpd.obs.z, mntd.obs.z), median)


# Merge bird diversity means to HUC8.
huc8corrdat <- left_join(divmedians8, huc8summ[,c('HUC8',cols_to_plot)] %>% mutate(HUC8 = as.character(HUC8)))
huc8corrdatres <- left_join(divmedians8res, huc8summ[,c('HUC8',cols_to_plot)] %>% mutate(HUC8 = as.character(HUC8)))

# Summarize this by averaging all the routes within HUC.
huc8mediandat <- huc8corrdat %>% ungroup %>%
	select(-rteNo) %>%
	group_by(HUC8) %>%
	summarize_all(.funs = 'median')
huc8mediandatres <- huc8corrdatres %>% ungroup %>%
	select(-rteNo) %>%
	group_by(HUC8) %>%
	summarize_all(.funs = 'median')	
								  
pairplot_bygroup(huc8mediandat, clim_vars_temp, d_vars, templabels, dlabels, dn, 'bbs_vs_huc8temp_allbirds.png')
pairplot_bygroup(huc8mediandat, clim_vars_prec, d_vars, preciplabels, dlabels, dn, 'bbs_vs_huc8precip_allbirds.png')
pairplot_bygroup(huc8mediandat, topo_vars, d_vars, topolabels, dlabels, dn, 'bbs_vs_huc8topo_allbirds.png')
pairplot_bygroup(huc8mediandat, nlcd_vars, d_vars, nlcdlabels, dlabels, dn, 'bbs_vs_huc8nlcd_allbirds.png')
pairplot_bygroup(huc8mediandat, prod_vars, d_vars, prodlabels, dlabels, dn, 'bbs_vs_huc8productivity_allbirds.png')

pairplot_bygroup(huc8mediandatres, clim_vars_temp, d_vars, templabels, dlabels, dn, 'bbs_vs_huc8temp_residents.png')
pairplot_bygroup(huc8mediandatres, clim_vars_prec, d_vars, preciplabels, dlabels, dn, 'bbs_vs_huc8precip_residents.png')
pairplot_bygroup(huc8mediandatres, topo_vars, d_vars, topolabels, dlabels, dn, 'bbs_vs_huc8topo_residents.png')
pairplot_bygroup(huc8mediandatres, nlcd_vars, d_vars, nlcdlabels, dlabels, dn, 'bbs_vs_huc8nlcd_residents.png')
pairplot_bygroup(huc8mediandatres, prod_vars, d_vars, prodlabels, dlabels, dn, 'bbs_vs_huc8productivity_residents.png')

