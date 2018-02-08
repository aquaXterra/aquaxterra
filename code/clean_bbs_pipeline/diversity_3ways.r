# function to do all 3 flavors of diversity for alpha or gamma given an input matrix.
# pd and fd are done with the same two distance-based metrics: pairwise and nearest-neighbor.

diversity_3ways <- function(m, flavor = 'alpha', dotd=TRUE, dopd=TRUE, dofd=TRUE, abundance=TRUE, pddist = NULL, fddist = NULL, nnull = 99, phylo_spp = NULL, func_problem_spp = NULL, combine = TRUE) {
	
  require(vegan)
  require(picante)
  
	# Get rid of species that aren't in phylogenetic and functional diversity.
	if (!is.null(phylo_spp)) m <- m[, dimnames(m)[[2]] %in% phylo_spp, drop = FALSE]
    if (!is.null(func_problem_spp)) mfunc <- m[, !dimnames(m)[[2]] %in% func_problem_spp, drop = FALSE] else mfunc <- m
	
	# Alpha diversity is done for each row separately.
	# Gamma diversity is for the sum of all the species down each column.
	if (flavor == 'gamma') {
		m <- t(apply(m, 2, sum))
		mfunc <- t(apply(mfunc, 2, sum))
	}
	
	# Declare variables to hold the data
	richness <- shannon <- evenness <- rep(NA, nrow(m))
	MPD_z <- MNTD_z <- rep(NA, nrow(m))
	MPD_pa_z <- MNTD_pa_z <- rep(NA, nrow(m))
	MPDfunc_z <- MNTDfunc_z <- rep(NA, nrow(m))
	MPDfunc_pa_z <- MNTDfunc_pa_z <- rep(NA, nrow(m))
	
	# Calculate the different flavors of diversity.
	if (dotd) {
		richness <- apply(m > 0, 1, sum)
	}
	if (dotd & abundance) {
		shannon <- diversity(m, index = 'shannon')
		evenness <- shannon/log(richness)
	}
	if (dopd) {
		MPD_pa <- ses.mpd(m, pddist, null.model = 'taxa.labels', abundance.weighted = FALSE, runs = nnull)
		MNTD_pa <- ses.mntd(m, pddist, null.model = 'taxa.labels', abundance.weighted = FALSE, runs = nnull)
		
		if (flavor == 'alpha') {
		  MPD_pa_z <- MPD_pa$mpd.obs.z
		  MNTD_pa_z <- MNTD_pa$mntd.obs.z
		}
		if (flavor == 'gamma') {
		  MPD_pa_z <- (MPD_pa$mpd.obs[1] - mean(MPD_pa$mpd.rand.mean, na.rm=T))/sd(MPD_pa$mpd.rand.mean, na.rm=T)
		  MNTD_pa_z <- (MNTD_pa$mntd.obs[1] - mean(MNTD_pa$mntd.rand.mean, na.rm=T))/sd(MNTD_pa$mntd.rand.mean, na.rm=T)
		}

	}
	if (dopd & abundance) {
		MPD <- ses.mpd(m, pddist, null.model = 'taxa.labels', abundance.weighted = TRUE, runs = nnull)
		MNTD <- ses.mntd(m, pddist, null.model = 'taxa.labels', abundance.weighted = TRUE, runs = nnull)
		if (flavor == 'alpha') {
		  MPD_z <- MPD$mpd.obs.z
		  MNTD_z <- MNTD$mntd.obs.z
		}
		if (flavor == 'gamma') {
		  MPD_z <- (MPD$mpd.obs[1] - mean(MPD$mpd.rand.mean, na.rm=T))/sd(MPD$mpd.rand.mean, na.rm=T)
		  MNTD_z <- (MNTD$mntd.obs[1] - mean(MNTD$mntd.rand.mean, na.rm=T))/sd(MNTD$mntd.rand.mean, na.rm=T)
		}
	}
	if (dofd) {
	  MPDfunc_pa <- ses.mpd(mfunc, fddist, null.model = 'taxa.labels', abundance.weighted = FALSE, runs = nnull)
	  MNTDfunc_pa <- ses.mntd(mfunc, fddist, null.model = 'taxa.labels', abundance.weighted = FALSE, runs = nnull)
	  
	  if (flavor == 'alpha') {
	    MPDfunc_pa_z <- MPDfunc_pa$mpd.obs.z
	    MNTDfunc_pa_z <- MNTDfunc_pa$mntd.obs.z
	  }
	  if (flavor == 'gamma') {
	    MPDfunc_pa_z <- (MPDfunc_pa$mpd.obs[1] - mean(MPDfunc_pa$mpd.rand.mean, na.rm=T))/sd(MPDfunc_pa$mpd.rand.mean, na.rm=T)
	    mntdrand <- MNTDfunc_pa$mntd.rand.mean[is.finite(MNTDfunc_pa$mntd.rand.mean)]
	    MNTDfunc_pa_z <- (MNTDfunc_pa$mntd.obs[1] - mean(mntdrand))/sd(mntdrand)
	  }
	  
	}
	if (dofd & abundance) {
	  MPDfunc <- ses.mpd(mfunc, pddist, null.model = 'taxa.labels', abundance.weighted = TRUE, runs = nnull)
	  MNTDfunc <- ses.mntd(mfunc, pddist, null.model = 'taxa.labels', abundance.weighted = TRUE, runs = nnull)
	  if (flavor == 'alpha') {
	    MPDfunc_z <- MPDfunc$mpd.obs.z
	    MNTDfunc_z <- MNTDfunc$mntd.obs.z
	  }
	  if (flavor == 'gamma') {
	    MPDfunc_z <- (MPDfunc$mpd.obs[1] - mean(MPDfunc$mpd.rand.mean, na.rm=T))/sd(MPDfunc$mpd.rand.mean, na.rm=T)
	    mntdrand <- MNTDfunc$mntd.rand.mean[is.finite(MNTDfunc$mntd.rand.mean)]
	    MNTDfunc_z <- (MNTDfunc$mntd.obs[1] - mean(mntdrand))/sd(mntdrand)
	  }
	}
	
	
	# Concatenate into a vector and return.
	if(combine) return(c(richness = median(richness, na.rm=T), shannon = median(shannon, na.rm=T), evenness = median(evenness, na.rm=T),
	  MPD_pa_z = median(MPD_pa_z, na.rm=T), MNTD_pa_z = median(MNTD_pa_z, na.rm=T),
	  MPD_z = median(MPD_z, na.rm=T), MNTD_z = median(MNTD_z, na.rm=T),
	  MPDfunc_pa_z = median(MPDfunc_pa_z, na.rm=T), MNTDfunc_pa_z = median(MNTDfunc_pa_z, na.rm=T),
	  MPDfunc_z = median(MPDfunc_z, na.rm=T), MNTDfunc_z = median(MNTDfunc_z, na.rm=T)))
	if(!combine) return(data.frame(richness, shannon, evenness, MPD_pa_z, MNTD_pa_z, MPD_z, MNTD_z, MPDfunc_pa_z, MNTDfunc_pa_z, MPDfunc_z, MNTDfunc_z))
	

}