# BBS Processing Pipeline

AquaXTerra project

As of 01 Feb 2018, the pipeline is completely contained in a single script: `BBS_pipeline.r`, located at `GitHub/aquaxterra/code/clean_bbs_pipeline`. All file paths have been removed from the script, and all raw data required for generating the final .CSV output is located at `/mnt/research/aquaxterra/DATA_PAPER/BBS`. The script can be reproducibly run out of that working directory. The pipeline does not include any map drawing or plots to visualize the results (the final step is generating CSV files with BBS summary statistics by HUC).

Created: QDR 31 Jan 2018
Last modified: 07 Feb 2018

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
	- Output: the pooled matrix and covariate table as well as the phylogenetic distance matrix and functional distance matrix (as of 31 Jan it is pooled for 2001-2011 and also has a separate matrix for all birds and for residents only!)

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

# Methods written out in prose form

*need to add all the r packages and citations to this!*

## Breeding bird diversity aggregated by hydrologic unit 

### Overview

We used data from the U.S. Geological Survey's Breeding Bird Survey (BBS) to characterize bird diversity within each of the hydrologic units. BBS data consists of approximately 4000 routes in the continental USA that are surveyed once yearly at the peak of breeding season. During a survey, a qualified amateur observer conducts a point count every 0.5 miles (~800 m) along the 25-mile (~40 km) route (50 stops per route), recording the number of individuals seen or heard of each bird species. We calculated taxonomic diversity, functional diversity, and phylogenetic diversity (TD, FD, and PD, respectively) at the route level from the BBS survey data. 

### Data sources

The data sources we used are: the BBS survey data available from USGS, a list of United States bird species categorized by migratory status available from the US Fish and Wildlife Service, a database of bird diet and foraging traits, a database of bird life history traits, and a phylogenetic tree of all bird species. *cite these and/or refer to table*

### Data processing

First, we manually checked the species lists from the BBS survey data, the trait databases, and the phylogenetic tree and created a table of synonymous names so that all data sources could be joined. In some cases, this required lumping together records at the subspecies level. We selected the following functional traits: *add here*, standardized the trait values, and calculated pairwise Gower distance between the vector of trait values for each pair of bird species. We also computed the branch lengths of a single phylogenetic tree representing the consensus tree of 1000 trees randomly chosen from the posterior distribution of trees from the global bird phylogeny. We calculated the pairwise phylogenetic distance along the branches of the tree for each pair of bird species. Finally, we used the migratory status table to split the species list into two categories: migrants and residents. We calculated separate diversity metrics for all birds together and for year-round residents only.

We converted all bird abundance records to binary incidence records, then pooled all stop-level incidences to yield a single incidence value for each species for each route. Furthermore we pooled each year's incidences from 2001-2011 to yield a single incidence for the entire time period for each bird species. If a species was ever recorded at any stop in any year, it is coded as present at the route level. This conservative method of estimating occupancy is robust to imperfect detection (low rate of false negatives) at the cost of increasing the rate of false positives (incidental or transient species are counted as present).

### Calculating diversity

All of the following diversity calculations were done first for all birds at a route, then for only those birds categorized as year-round residents.

We calculated taxonomic diversity by totaling the species richness for each BBS route. We also calculated richness for each of the following functional groups or trophic guilds as defined by the database of bird diet and foraging traits: waterfowl, land-based herbivores, land-based omnivores, land-bassed insectivores, and land-based carnivores/scavengers. 

We calculated two metrics of functional diversity using the functional distance matrix described above: mean pairwise functional distance and mean nearest-neighbor phylogenetic distance. *describe* We generated a null distribution of functional diversity metrics by randomly permuting the species labels on the distance matrix 99 times per community and calculating the functional diversity metrics on each of the permuted communities. We report the standardized effect size, or z-score, for the two functional diversity metrics (the z-score of the observed functional diversity metric relative to the distribution, assumed normal, of the distribution of functional diversity metrics of the null communities). We calculated the same metrics and z-scores for phylogenetic diversity.

### Aggregating diversity by hydrologic unit

For each hydrologic unit, we calculated the weighted average of each route-level diversity metric within that region, weighted by the number of stops in each BBS route that lay within the hydrologic unit boundaries. We calculated the weighted mean and weighted median, and report both. All HUC4 units contained at least one stop on one or more BBS route, but around 10% of the HUC8 units, and around 50% of the HUC12 units, contained no BBS routes.