# Create paired plots of BBS and HUC variables 
# HUC4, HUC8, and HUC12

load('/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_div_byroute_presence.r')

library(dplyr)
library(ggplot2)
library(reshape2)
library(GGally)

# Load HUC summary data
huc4summ <- read.csv('/mnt/research/aquaxterra/DATA/huc4summarized_reduced.csv')
huc8summ <- read.csv('/mnt/research/aquaxterra/DATA/huc8summarized_reduced.csv')
huc12summ <- read.csv('/mnt/research/aquaxterra/DATA/huc12summarized_reduced.csv')

# Generate BBS diversity summary and join with HUC data (just all-spp for now)
div_huc4_all <- bbs_div_byroute %>%
  mutate(HUC4 = as.numeric(HUC4)) %>%
  filter(nstops4 > 25, year >= 2001, year <= 2011) %>%
  group_by(HUC4) %>%
  summarize_at(.cols = vars(richness, FEve, FDis, mpd.obs.z, mntd.obs.z), .funs = median, na.rm = TRUE)
div_huc4_res <- bbs_div_byrouteres %>%
  mutate(HUC4 = as.numeric(HUC4)) %>%
  filter(nstops4 > 25, year >= 2001, year <= 2011) %>%
  group_by(HUC4) %>%
  summarize_at(.cols = vars(richness, FEve, FDis, mpd.obs.z, mntd.obs.z), .funs = median, na.rm = TRUE)

div_huc8_all <- bbs_div_byroute %>%
  mutate(HUC8 = as.numeric(HUC8)) %>%
  filter(nstops8 > 25, year >= 2001, year <= 2011) %>%
  group_by(HUC8) %>%
  summarize_at(.cols = vars(richness, FEve, FDis, mpd.obs.z, mntd.obs.z), .funs = median, na.rm = TRUE)
div_huc8_res <- bbs_div_byrouteres %>%
  mutate(HUC8 = as.numeric(HUC8)) %>%
  filter(nstops8 > 25, year >= 2001, year <= 2011) %>%
  group_by(HUC8) %>%
  summarize_at(.cols = vars(richness, FEve, FDis, mpd.obs.z, mntd.obs.z), .funs = median, na.rm = TRUE)

div_huc12_all <- bbs_div_byroute %>%
  mutate(HUC12 = as.numeric(HUC12)) %>%
  filter(nstops12 > 9, year >= 2001, year <= 2011) %>%
  group_by(HUC12) %>%
  summarize_at(.cols = vars(richness, FEve, FDis, mpd.obs.z, mntd.obs.z), .funs = median, na.rm = TRUE)
div_huc12_res <- bbs_div_byrouteres %>%
  mutate(HUC12 = as.numeric(HUC12)) %>%
  filter(nstops12 > 9, year >= 2001, year <= 2011) %>%
  group_by(HUC12) %>%
  summarize_at(.cols = vars(richness, FEve, FDis, mpd.obs.z, mntd.obs.z), .funs = median, na.rm = TRUE)

huc4summ <- left_join(huc4summ, div_huc4_all)
huc8summ <- left_join(huc8summ, div_huc8_all)
huc12summ <- left_join(huc12summ, div_huc12_all)

# Draw pair plots
# Later once GPP/NPP is corrected, we can add it.
vars_to_plot <- c('richness', 'mean_altitude', 'std_altitude', 'mean_allyears_bio12', 'mean_allyears_lai')
names_to_plot <- c('Breeding bird\nrichness', 'Elevation\nmean', 'Elevation\nstandard deviation', 'Mean annual\nprecipitation', 'Maximum annual\nleaf area index')

# Plot paired plots
my_hex <- function(data, mapping, ...) {
  p <- ggplot(data = data, mapping = mapping) + 
    geom_hex(...) + 
    scale_fill_continuous(low = 'gray90', high = 'black')
  p
}

my_pairs <- function(dat, vars, varnames, n_bins = 20, fpath, fname, img_h = 8, img_w = 8) {
  png(file.path(fpath, fname), height = img_h, width = img_w, res = 300, units = 'in')
  print(ggpairs(dat[, vars],
          columnLabels = varnames,
          diag = list(continuous = wrap('barDiag', bins = n_bins)),
          lower = list(continuous = my_hex)) + 
    theme_bw())
  dev.off()
}

fp <- '/mnt/research/aquaxterra/FIGS/bbs_huc_pairplots'

my_pairs(dat = huc4summ, vars = vars_to_plot, varnames = names_to_plot, fpath = fp, fname = 'huc4_pairs_forppt.png')
my_pairs(dat = huc8summ, vars = vars_to_plot, varnames = names_to_plot, fpath = fp, fname = 'huc8_pairs_forppt.png')
my_pairs(dat = huc12summ, vars = vars_to_plot, varnames = names_to_plot, fpath = fp, fname = 'huc12_pairs_forppt.png')
