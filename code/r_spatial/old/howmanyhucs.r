# Quantify the number of bbs routes in each polygon
# author: QDR
# Project: Aquaxterra
# Created: 02 Nov 2016

bbs_huc <- read.csv('data/BBS_SpatialJoin_Final.csv', stringsAsFactors = FALSE)
bbs_huc <- read.csv('/mnt/research/aquaxterra/CODE/python/BBSSpatialJoin/BBS_SpatialJoin_Final.csv', stringsAsFactors = FALSE)

ns <- strsplit(bbs_huc$rtestopNo, '-')
bbs_huc$route <- sapply(ns, '[', 1)
bbs_huc$stopno <- sapply(ns, '[', 2)

library(dplyr)

huc4n <- bbs_huc %>% group_by(HUC4) %>% summarize(nroutes = length(unique(route)), nstops = length(rtestopNo))
huc8n <- bbs_huc %>% group_by(HUC8) %>% summarize(nroutes = length(unique(route)), nstops = length(rtestopNo))
huc12n <- bbs_huc %>% group_by(HUC12) %>% summarize(nroutes = length(unique(route)), nstops = length(rtestopNo))

# How many routes are in multiple polygons of each level?
hucsperroute <- bbs_huc %>% group_by(route) %>% summarize(n_huc4 = length(unique(HUC4)), n_huc8 = length(unique(HUC8)), n_huc12 = length(unique(HUC12)))

table(hucsperroute$n_huc4)
table(hucsperroute$n_huc8)
table(hucsperroute$n_huc12)

# Load huc area
huc12area <- read.csv('data/huc12out.csv', stringsAsFactors = FALSE)
bbs_huc <- left_join(bbs_huc, huc12area)

huc4area <- read.csv('data/huc4area.csv')
huc8area <- read.csv('data/huc8area.csv')

library(ggplot2)
source('~/qutil.r')

pdf('figs/bbs_huc_histograms.pdf', height=6, width=6)

ggplot(huc4n, aes(x = nroutes)) + geom_bar(stat='count') + theme_bw() + scale_y_continuous(expand = c(0,0), limits = c(0,15)) + ggtitle('Number of BBS routes in each HUC4')
ggplot(huc8n, aes(x = nroutes)) + geom_bar(stat='count') + theme_bw() + scale_y_continuous(expand = c(0,0), limits = c(0,550)) + ggtitle('Number of BBS routes in each HUC8')
ggplot(huc12n, aes(x = nroutes)) + geom_bar(stat='count') + theme_bw() + scale_y_continuous(expand = c(0,0), limits = c(0,15000)) + ggtitle('Number of BBS routes in each HUC12')

ggplot(hucsperroute, aes(x = n_huc4)) + geom_bar(stat='count') + theme_bw() + scale_y_continuous(expand = c(0,0), limits = c(0,2500)) + ggtitle('Number of HUC4s that each BBS route crosses')
ggplot(hucsperroute, aes(x = n_huc8)) + geom_bar(stat='count') + theme_bw() + scale_y_continuous(expand = c(0,0), limits = c(0,1600)) + ggtitle('Number of HUC8s that each BBS route crosses')
ggplot(hucsperroute, aes(x = n_huc12)) + geom_bar(stat='count') + theme_bw() + scale_y_continuous(expand = c(0,0), limits = c(0,860)) + scale_x_continuous(breaks=0:12) + ggtitle('Number of HUC12s that each BBS route crosses')

ggplot(huc4area, aes(x = AreaSqKm)) + geom_histogram(bins = 20) + theme_bw() + scale_y_continuous(expand = c(0,0), limits = c(0,35)) + ggtitle('HUC4 sizes')
ggplot(huc8area, aes(x = AreaSqKm)) + geom_histogram(bins = 20) + theme_bw() + scale_y_continuous(expand = c(0,0), limits = c(0,600)) + scale_x_continuous(limits=c(0,20000)) + qSubtitle('HUC8 sizes', 'eight very large outliers not plotted')
ggplot(huc12area, aes(x = AreaSqKm)) + geom_histogram(bins = 20) + theme_bw() + scale_y_continuous(expand = c(0,0), limits = c(0,200)) + scale_x_continuous(limits=c(0,5000)) + qSubtitle('HUC12 sizes', 'thirty-two very large outliers not plotted')

dev.off()