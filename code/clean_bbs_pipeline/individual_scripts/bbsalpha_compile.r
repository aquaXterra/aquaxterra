# Join BBS alpha-diversity output (td, pd, fd, and functional group tallies) to HUCs
# Updated with all new BBS diversity values 31 Jan 2018
# Added lagoslakeid as an aggregation group 09 Oct 2018
# Updated file paths 09 Oct 2018
# Correct HUC ids so that they are characters and can have leading zeroes 09 Oct 2018
# QDR AquaXTerra

# Read BBS CSVs

library(dplyr)

fp <- '/mnt/research/aquaxterra/DATA/raw_data/BBS/diversity_CSVs'

bbs_alpha <- read.csv(file.path(fp, 'bbs_alpha_11years.csv'), stringsAsFactors = FALSE)
bbs_alpha_res <- read.csv(file.path(fp, 'bbs_alpha_11years_residents.csv'), stringsAsFactors = FALSE)
bbs_fgrich <- read.csv(file.path(fp, 'bbs_fgrichness_11years.csv'), stringsAsFactors = FALSE)
names(bbs_alpha_res)[-(1:5)] <- paste('resident', names(bbs_alpha_res)[-(1:5)], sep = '_')

bbs_alpha <- bbs_alpha %>% left_join(bbs_alpha_res) %>% left_join(bbs_fgrich)

# Read BBS+HUC spatial join and combine with BBS diversity data.

bbs_huc <- read.csv('/mnt/research/aquaxterra/DATA/HUCed_data/BBS/BBS_SpatialJoin_Final.csv', stringsAsFactors = FALSE, colClasses = c(HUC4 = 'character', HUC8 = 'character', HUC12 = 'character', lagoslakeid = 'character'))

ns <- strsplit(bbs_huc$rtestopNo, '-')
bbs_huc$rteNo <- sapply(ns, '[', 1)
bbs_huc$Stop <- sapply(ns, '[', 2)

# Get most common HUC and lagoslakeid from each rteNo and how many stops were contained in it.
huc4812summary <- function (x) {
	t4 <- table(x$HUC4)
	t8 <- table(x$HUC8)
	t12 <- table(x$HUC12)
	tlagos <- table(x$lagoslakeid)
	h4 <- names(t4)[which.max(t4)[1]]
	h8 <- names(t8)[which.max(t8)[1]]
	h12 <- names(t12)[which.max(t12)[1]]
	lagos <- if (length(tlagos) > 0) names(tlagos)[which.max(tlagos)[1]] else NA
	n4 <- max(t4)
	n8 <- max(t8)
	n12 <- max(t12)
	nlagos <- if (length(tlagos) > 0) max(tlagos) else NA
	return(data.frame(HUC4 = h4, HUC8 = h8, HUC12 = h12, lagoslakeid = lagos, nstops4 = n4, nstops8 = n8, nstops12 = n12, nstopslagos = nlagos))
}

huctable <- bbs_huc %>% 
	group_by(rteNo) %>% 
	do(huc4812summary(.)) %>% 
	ungroup %>%
	mutate(rteNo = as.numeric(rteNo))

# Get all HUCs of each level for each rteNo
huclist <- bbs_huc %>% 
	group_by(rteNo) %>% 
	do(HUC4list = unique(.$HUC4), HUC8list = unique(.$HUC8), HUC12list = unique(.$HUC12), lagoslakeidlist = unique(.$lagoslakeid)) %>%
	ungroup %>%
	mutate(rteNo = as.numeric(rteNo))

# Merge bbs diversity with HUC for majority rule
bbs_alpha_majorityhuc <- left_join(bbs_alpha, huctable)
# Merge bbs diversity with HUC for all HUCs that a route passes through
bbs_alpha_allhucs <- bbs_huc %>%
	mutate(rteNo = as.numeric(rteNo)) %>%
	left_join(select(bbs_alpha, -lon, -lat, -lon_aea, -lat_aea))

	
# If we are using all hucs that a route passes through, here is code to get the median values for all the diversity metrics for each HUC.
# Also do means to compare. In each case, it's weighted by how many stops in the route are in the HUC.
# Export them as CSVs.

bbs_div_huc4 <- bbs_alpha_allhucs %>%
  select(-rtestopNo, -lon, -lat, -HUC8, -HUC12, -lagoslakeid, -Stop, -rteNo) %>%
  group_by(HUC4) %>%
  summarize_all(.funs = median, na.rm = TRUE) 

bbs_div_huc4_mean <- bbs_alpha_allhucs %>%
  select(-rtestopNo, -lon, -lat, -HUC8, -HUC12, -lagoslakeid, -Stop, -rteNo) %>%
  group_by(HUC4) %>%
  summarize_all(.funs = mean, na.rm = TRUE) 
  
bbs_div_huc8 <- bbs_alpha_allhucs %>%
  select(-rtestopNo, -lon, -lat, -HUC4, -HUC12, -lagoslakeid, -Stop, -rteNo) %>%
  group_by(HUC8) %>%
  summarize_all(.funs = median, na.rm = TRUE) 

bbs_div_huc8_mean <- bbs_alpha_allhucs %>%
  select(-rtestopNo, -lon, -lat, -HUC4, -HUC12, -lagoslakeid, -Stop, -rteNo) %>%
  group_by(HUC8) %>%
  summarize_all(.funs = mean, na.rm = TRUE) 
  
bbs_div_huc12 <- bbs_alpha_allhucs %>%
  select(-rtestopNo, -lon, -lat, -HUC4, -HUC8, -lagoslakeid, -Stop, -rteNo) %>%
  group_by(HUC12) %>%
  summarize_all(.funs = median, na.rm = TRUE) 

bbs_div_huc12_mean <- bbs_alpha_allhucs %>%
  select(-rtestopNo, -lon, -lat, -HUC4, -HUC8, -lagoslakeid, -Stop, -rteNo) %>%
  group_by(HUC12) %>%
  summarize_all(.funs = mean, na.rm = TRUE) 
  
bbs_div_lagos <- bbs_alpha_allhucs %>%
  select(-rtestopNo, -lon, -lat, -HUC4, -HUC8, -HUC12, -Stop, -rteNo) %>%
  group_by(lagoslakeid) %>%
  summarize_all(.funs = median, na.rm = TRUE) 

bbs_div_lagos_mean <- bbs_alpha_allhucs %>%
  select(-rtestopNo, -lon, -lat, -HUC4, -HUC8, -HUC12, -Stop, -rteNo) %>%
  group_by(lagoslakeid) %>%
  summarize_all(.funs = mean, na.rm = TRUE)   
  
fp <- '/mnt/research/aquaxterra/DATA/albers_projected_data/BBS_Diversity'
write.csv(bbs_div_huc4, file.path(fp, 'bbs_div_huc4_median.csv'), row.names = FALSE)
write.csv(bbs_div_huc4_mean, file.path(fp, 'bbs_div_huc4_mean.csv'), row.names = FALSE)
write.csv(bbs_div_huc8, file.path(fp, 'bbs_div_huc8_median.csv'), row.names = FALSE)
write.csv(bbs_div_huc8_mean, file.path(fp, 'bbs_div_huc8_mean.csv'), row.names = FALSE)
write.csv(bbs_div_huc12, file.path(fp, 'bbs_div_huc12_median.csv'), row.names = FALSE)
write.csv(bbs_div_huc12_mean, file.path(fp, 'bbs_div_huc12_mean.csv'), row.names = FALSE)
write.csv(bbs_div_lagos, file.path(fp, 'bbs_div_lagoslakeid_median.csv'), row.names = FALSE)
write.csv(bbs_div_lagos_mean, file.path(fp, 'bbs_div_lagoslakeid_mean.csv'), row.names = FALSE)
