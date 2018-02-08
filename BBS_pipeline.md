# BBS Processing Pipeline

AquaXTerra Data Paper

This document contains a step-by-step list of the pipeline as well as a prose version that can be used in the methods section of the data paper, including citations.

As of 01 Feb 2018, the pipeline is completely contained in a single script: `BBS_pipeline.r`, located at `GitHub/aquaxterra/code/clean_bbs_pipeline`. All file paths have been removed from the script, and all raw data required for generating the final .CSV output is located at `/mnt/research/aquaxterra/DATA_PAPER/BBS`. The script can be reproducibly run out of that working directory. The pipeline does not include any map drawing or plots to visualize the results (the final step is generating CSV files with BBS summary statistics by HUC). The code was tested on the MSU high-performance computing cluster using R version 3.2.1 on 01 February 2018.

Created: QDR 31 Jan 2018  
Last modified: 08 Feb 2018

## List of R packages needed

### Packages used for data wrangling only, not needed to cite
- dplyr
- reshape2
- stringr
### Packages used for producing results, needed to cite
- ape
- phytools
- FD
- picante

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
- Also requires sourcing the function `diversity_3ways.r`, the code for which is found in the same directory as the master script.
- Output: Alpha diversity for all birds and for resident species only
- Output: richness by functional group, for both all birds and for residents only 

## Process output of diversity calculations and summarize by HUC

- Input: Outputs from previous step, and `BBS_SpatialJoin.csv` file produced by Ed.
- Output: 6 csvs, one for mean and median BBS diversity in each of the three HUC levels.

## Draw maps and make plots
	
- Not included in this pipeline.

# Methods written out in prose form

## Breeding bird diversity aggregated by hydrologic unit 

### Overview

We used data from the U.S. Geological Survey's Breeding Bird Survey (BBS) to characterize bird diversity within each of the hydrologic units. BBS data consists of approximately 4000 routes in the continental USA that are surveyed once yearly at the peak of breeding season. During a survey, a qualified amateur observer conducts a point count every 0.5 miles (~800 m) along the 25-mile (~40 km) route (50 stops per route), recording the number of individuals seen or heard of each bird species. We calculated taxonomic diversity, functional diversity, and phylogenetic diversity (TD, FD, and PD, respectively) at the route level from the BBS survey data. All data manipulation and processing was done in R version 3.2.1 (R Core Team 2015), with individual packages cited below where appropriate.

### Data sources

The data sources we used are: the BBS survey data available from USGS, a list of United States bird species categorized by migratory status available from the US Fish and Wildlife Service, a database of bird diet and foraging traits, a database of bird life history traits, and a phylogenetic tree of all bird species. See table xxx for details on these data sources, including citations.

### Data processing

First, we manually checked the species lists from the BBS survey data, the trait databases, and the phylogenetic tree and created a table of synonymous names so that all data sources could be joined. In some cases, this required lumping together records at the subspecies level. We selected the following functional traits: *add here* and calculated pairwise Gower distance between the vector of trait values for each pair of bird species using the *gowdis* function in R package *FD* (Lalibert&eacute; & Legendre 2010; Lalibert&eacute; et al. 2014). Gower distance (Gower 1971) is a distance metric that standardizes the variables by dividing by the range and which has been extended to accommodate both binary and continuous variables (Podani 1999). We also computed the branch lengths of a single phylogenetic tree representing the consensus tree of 1000 trees randomly chosen from the posterior distribution of trees from the global bird phylogeny. We calculated the pairwise phylogenetic distance along the branches of the tree for each pair of bird species. Phylogenetic analysis was done with R packages *ape* (Paradis et al. 2004) and *phytools* (Revell 2012). Finally, we used the migratory status table to split the species list into two categories: migrants and residents. We calculated separate diversity metrics for all birds together and for year-round residents only.

We converted all bird abundance records to binary incidence records, then pooled all stop-level incidences to yield a single incidence value for each species for each route. Furthermore, we pooled each year's incidences from 2001-2011 to yield a single incidence for the entire time period for each bird species. If a species was ever recorded at any stop in any year, it was coded as present at the route level. This conservative method of estimating occupancy is robust to imperfect detection (low rate of false negatives) at the cost of increasing the rate of false positives (incidental or transient species are counted as present).

### Calculating diversity

All of the following diversity calculations were done first for all birds at a route, then for only those birds categorized as year-round residents.

We calculated taxonomic diversity by totaling the species richness for each BBS route. We also calculated richness for each of the following functional groups or trophic guilds as defined by the database of bird diet and foraging traits: waterfowl, land-based herbivores, land-based omnivores, land-bassed insectivores, and land-based carnivores/scavengers. 

We calculated two metrics of functional diversity using the functional distance matrix described above: mean pairwise functional distance (MPD) and mean nearest-taxon phylogenetic distance (MNTD). MPD is the average Gower distance of all pairs of taxa in the community, while MNTD is the average of the Gower distances between each taxon and its closest neighbor (Webb et al. 2002). Broadly speaking, MPD characterizes the overall spread in functional space, while MNTD captures the clustering of taxa near one another in functional space. We generated a null distribution of functional diversity metrics by randomly permuting the species labels on the distance matrix 99 times per community and calculating the functional diversity metrics on each of the permuted communities. We report the standardized effect size, or z-score, for the two functional diversity metrics (the z-score of the observed functional diversity metric relative to the distribution, assumed normal, of the distribution of functional diversity metrics of the null communities). We calculated the same metrics and z-scores for phylogenetic diversity. The calculation of these metrics and effect sizes was implemented with the *ses.mpd* and *ses.mntd* functions in the R package *picante* (Kembel et al. 2010).

### Aggregating diversity by hydrologic unit

*Here, if needed, include more detailed methods for how the points along the BBS routes were spatially joined with the HUC polygons*.

For each hydrologic unit, we calculated the weighted average of each route-level diversity metric within that region, weighted by the number of stops in each BBS route that lay within the hydrologic unit boundaries. We calculated the weighted mean and weighted median, and report both. All HUC4 units contained at least one stop on one or more BBS route, but around 10% of the HUC8 units, and around 50% of the HUC12 units, contained no BBS routes.

## Works cited

### Data sources

See the "Data origins documentation" document.

### Primary literature (including peer-reviewed manuscripts describing R packages)

Gower, J. C. (1971) A general coefficient of similarity and some of its properties. Biometrics 27:857-871.

Kembel, S.W., P.D. Cowan, M.R. Helmus, W.K. Cornwell, H. Morlon, D.D. Ackerly, S.P. Blomberg, and C.O. Webb. (2010) Picante: R tools for integrating phylogenies and ecology. Bioinformatics 26:1463-1464.

Laliberté, E., and P. Legendre (2010) A distance-based framework for measuring functional diversity from multiple traits. Ecology 91:299-305.

Paradis E., Claude J. & Strimmer K. (2004) APE: analyses of phylogenetics and evolution in R language. Bioinformatics 20:289-290.

Podani, J. (1999) Extending Gower's general coefficient of similarity to ordinal characters. Taxon 48:331-340.

Revell, L. J. (2012) phytools: An R package for phylogenetic comparative biology (and other things). Methods Ecol. Evol. 3:217-223. doi:10.1111/j.2041-210X.2011.00169.x

Webb, C.O. et al. (2002) Phylogenies and community ecology. Ann. Rev. Ecol. Syst. 33:475–505.

### Software and packages (not peer-reviewed manuscripts)

Laliberté, E., Legendre, P., and B. Shipley. (2014) FD: measuring functional diversity from multiple traits, and other tools for functional ecology. R package version 1.0-12.

R Core Team (2015) R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL http://www.R-project.org/.



 