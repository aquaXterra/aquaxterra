# BBS Processing Pipeline

AquaXTerra and NASABioXGeo projects

As of 31 Jan 2018, all scripts needed to run this pipeline are located at `GitHub/aquaxterra/code/clean_bbs_pipeline` but many of the scripts have older hard-coded paths in them. Also, older locations are referenced below.

Created: QDR 31 Jan 2018
Last modified:

## Create matrix by year and route from raw data downloaded from USGS

- `GitHub/aquaxterra/code/bbs/updatedbbs.r`
	- Input: raw BBS files downloaded from USGS and including 2016
	- Output: `bbsmat2016.r`
- `GitHub/aquaxterra/code/bbs/fixupdatedbbsmat.r` 
	- Input: `bbsmat2016.r`
	- Input: `/mnt/research/aquaxterra/DATA/raw_data/bird_traits/specieslist.csv` which was manually created to do QC on the species names.
	- Output: `bbsmatconsolidated2016.r`

## Gather functional trait info needed for FD and PD calculations

- `GitHub/nasabio/trait_phylo_data_processing/bbs_consensustree.r` 
	- Input: `ericson1000.tre` (list of 1000 trees downloaded from birdtree.org)
	- Output: `ericson_cons.tre` (single consensus tree)
- `GitHub/aquaxterra/code/bbs/readbirdtraits.r`
	- Input: EltonTraits, Amniote Life History database traits, USFWS list of migratory status for all bird species
	- Output: `birdtraitmerged.csv`

## Prepare matrix for diversity calculations

This part also includes some wrangling that's needed for beta and gamma diversity at different radii, which is used for NASA stuff but not Aqua stuff. It pools years 2007-2016 into a single value but we probably want to do 2001-2011 instead. 

- `GitHub/nasabio/prep_diversity_files/bbsbeta_byroute_prep.r`
	- Input: `ericson_cons.tre` (Ericson consensus tree), `birdtraitmerged.csv`, `bbsmatconsolidated2016.r`
	- Output: the pooled matrix and covariate table `bbsworkspace_singleyear.r` as well as the phylogenetic distance matrix and functional distance matrix `bbspdfddist.r` (as of 31 Jan it is pooled for 2001-2011 and also has a separate matrix for all birds and for residents only!)

## Do diversity calculations

- `GitHub/nasabio/run_compile_diversity/bbs1year/bbs_allplotsalpha.r`
	- Input: `bbsworkspace_singleyear.r`, `bbspdfddist.r`, sources the function `pairwise_beta_focal.r`.
	- Output: One CSV file for all birds, one for residents only. `bbs_alpha_11years.csv` and `bbs_alpha_11years_residents.csv`.
	
- `GitHub/aquaxterra/code/bbs/td_by_fg.r`
	- Input: `bbsworkspace_singleyear.r`, `birdtraitmerged.csv`
	- Output: richness by functional group, for both all birds and for residents only `bbs_fgrichness_11years.csv`

## Process output of diversity calculations and summarize by HUC

- `clean_bbs_pipeline/bbsalpha_compile.r`
	- Input: 3 csvs produced in previous step, and `BBS_SpatialJoin.csv` file produced by Ed.
	- Output: 6 csvs, one for mean and median BBS diversity in each of the three HUC levels.

## Draw maps and make plots

