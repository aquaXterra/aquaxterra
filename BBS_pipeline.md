# BBS Processing Pipeline

AquaXTerra project

As of 01 Feb 2018, the pipeline is completely contained in a single script: `BBS_pipeline.r`, located at `GitHub/aquaxterra/code/clean_bbs_pipeline`. All file paths have been removed from the script, and all raw data required for generating the final .CSV output is located at `/mnt/research/aquaxterra/DATA_PAPER/BBS`. The script can be reproducibly run out of that working directory. The pipeline does not include any map drawing or plots to visualize the results (the final step is generating CSV files with BBS summary statistics by HUC).

Created: QDR 31 Jan 2018
Last modified: 01 Feb 2018

## Create matrix by year and route from raw data downloaded from USGS

	- Input: raw BBS files downloaded from USGS and including 2016: `rawBBS/fifty*.csv` where * is 1 through 10.
	- Input: `specieslist.csv` which was manually created to do QC on the species names.
	- Output: a site by species matrix

## Gather functional trait info needed for FD and PD calculations

	- Input: `ericson1000.tre` (list of 1000 trees downloaded from birdtree.org)
	- Output: a single rooted consensus tree
	- Input: `BirdFuncDat.csv` EltonTraits, `Amniote_Database_Aug_2015.csv` Amniote Life History database traits, `migratorybirds.csv` USFWS list of migratory status for all bird species
	- Output: a corrected species by trait data frame

## Prepare site by species matrix for diversity calculations and create distance matrices

	- Input: Ericson consensus tree, site by species matrix, species by trait data frame, `bbs_correct_route_centroids.csv` coordinates of route centroids
	- Output: the pooled matrix and covariate table  as well as the phylogenetic distance matrix and functional distance matrix (as of 31 Jan it is pooled for 2001-2011 and also has a separate matrix for all birds and for residents only!)

## Do diversity calculations

	- Input: Outputs from previous step
	- Also requires sourcing the function `diversity_3ways.r`.
	- Output: Alpha diversity for all birds and for resident species only
	- Output: richness by functional group, for both all birds and for residents only 

## Process output of diversity calculations and summarize by HUC

	- Input: Outputs from previous step, and `BBS_SpatialJoin.csv` file produced by Ed.
	- Output: 6 csvs, one for mean and median BBS diversity in each of the three HUC levels.

## Draw maps and make plots
	
	- Not included in this pipeline.

