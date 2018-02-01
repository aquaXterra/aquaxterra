# Calculate alpha diversity within different radii for BBS.
# in contrast to FIA, we can only use groups within the same year. 1997-present.

# Do as a single group because it goes pretty quickly.
# Use precalculated matrix.
# Loop through each BBS plot.
# Get alpha diversity (PD,TD,FD) for that plot.
# Repeat for only residents.

load('/mnt/research/aquaxterra/DATA/raw_data/BBS/bbsworkspace_singleyear.r')
source('~/code/fia/pairwise_beta_focal.r')
load('/mnt/research/nasabio/data/bbs/bbspdfddist.r') # Phy and Func distance matrices.

# Replace AOU codes in the trait matrix with actual species names.

library(sp)
library(vegan)

nnull <- 99

alpha_div <- diversity_3ways(m = bbsmat_byroute_oneyear, flavor = 'alpha', 
                             dotd=T, dopd=T, dofd=T, abundance=F,
                             pddist = ericdist, fddist = birdtraitdist,
                             nnull = nnull,
                             phylo_spp = NULL, func_problem_spp = NULL, combine = FALSE)
alpha_div_residents <- diversity_3ways(m = bbsmat_byroute_oneyear_residents, flavor = 'alpha', 
                             dotd=T, dopd=T, dofd=T, abundance=F,
                             pddist = ericdist, fddist = birdtraitdist,
                             nnull = nnull,
                             phylo_spp = NULL, func_problem_spp = NULL, combine = FALSE)

# Get only the columns containing data.
cnames <- c('richness', 'MPD_pa_z', 'MNTD_pa_z', 'MPDfunc_pa_z', 'MNTDfunc_pa_z')
							 
alpha_div <- cbind(bbscov_oneyear, alpha_div[, cnames])
alpha_div_residents <- cbind(bbscov_oneyear, alpha_div_residents[, cnames])

write.csv(alpha_div, file = '/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_alpha_11years.csv', row.names = FALSE)
write.csv(alpha_div_residents, file = '/mnt/research/aquaxterra/DATA/raw_data/BBS/bbs_alpha_11years_residents.csv', row.names = FALSE)

