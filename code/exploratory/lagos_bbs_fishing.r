# Exploratory analysis of joined BBS and LAGOS datasets
# For now, use the by-route data since the by-stop data takes forever to work with.

load('/mnt/research/aquaxterra/DATA/raw_data/BBS/BBS_LAGOS_byroute_join.r')

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

# Use by stop data to see whether there are any closer relationships given the more local scale.

