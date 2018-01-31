# BBS Processing Pipeline

AquaXTerra and NASABioXGeo projects

QDR 31 Jan 2018

## Create matrix by year and route from raw data downloaded from USGS

- `GitHub/aquaxterra/code/bbs/updatedbbs.r`
	- Input: raw BBS files downloaded from USGS and including 2016
	- Output: `bbsmat2016.r`
- `GitHub/aquaxterra/code/bbs/fixupdatedbbsmat.r` 
	- Input: `bbsmat2016.r`
	- Input: `/mnt/research/aquaxterra/DATA/raw_data/bird_traits/specieslist.csv` which was manually created to do QC on the species names.
	- Output: `bbsmatconsolidated2016.r`

## Gather functional trait info needed for FD and PD calculations

- `GitHub/nasabio/trait_phylo_data_processing/bbs_consensustree.r` (Sources a function written by Brian O'Meara called `consensusbranchlength.r`)
	- Input: Tree downloaded from birdtree.org
	- Output: 
- `GitHub/aquaxterra/code/bbs/readbirdtraits.r`
	- Input: EltonTraits, Amniote Life History database traits, USFWS list of migratory status for all bird species
	- Output: `birdtraitmerged.csv`

## Prepare matrix for diversity calculations

This part also includes some wrangling that's needed for beta and gamma diversity at different radii, which is used for NASA stuff but not Aqua stuff. It pools years 2007-2016 into a single value but we probably want to do 2001-2011 instead. **TO DO**: See which years we want to do.

- `GitHub/nasabio/prep_diversity_files/bbsbeta_byroute_prep.r`
	- Input: `ericson_cons.tre` (Ericson consensus tree), `birdtraitmerged.csv`, `bbsmatconsolidated2016.r`
	- Output: the pooled matrix as well as the phylogenetic distance matrix and functional distance matrix

## Do diversity calculations

- `GitHub/nasabio/run_compile_diversity/bbs1year/bbs_allplotsalpha.r`
	- Input:
	- Output: a lot of R objects
	- **TO DO**: Do this with and without migratory species. (i.e., all species and residents-only)
- `GitHub/aquaxterra/code/bbs/td_by_fg.r`
	- Input:
	- Output: richness by functional group
	- **TO DO**: Do this again with the new BBS data? Definitely do this pooled by year.

## Process output of diversity calculations

- Will need to write a new script for this.

## Summarize diversity values by HUC

## Draw maps and make plots

