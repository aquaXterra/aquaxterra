# aquaxterra

Data analysis for the aquaXterra Watercube project

## Description

This repo contains R code for analyses that I (QDR) did for the aquaXterra Watercube project. The work I did was mostly related to processing Breeding Bird Survey (BBS) data, phylogenetic and trait information so that I could calculate PD and FD for the birds, and making maps showing different properties of BBS surveys grouped by hydrologic unit (HUC). I also made some plots and maps with the environmental data, summarized by HUC, that was created as part of the aquaXterra project. Lastly, I ran some very basic summary statistics and analysis on spider data from GBIF and on aquatic insect data from GBIF and NAWQA.

## Location of data

Most of the R scripts in this repo reference data located on MSU's server at `/mnt/research/aquaxterra`. Anyone who wants to run this code has to be a registered MSU user with access to the server. There are a few data files in the `data` directory on this repo, so that some code can be run locally, but this is only for a few limited uses.

## Summary of repo directory tree

- **code**: All R scripts are in this directory
	- **bbs**: R scripts to read in raw survey data from BBS, read phylogenies and functional trait data, run QC on all those datasets, and calculate a lot of different diversity metrics from them.
	- **clean_bbs_pipeline**: Contains only the "final" BBS data processing pipeline which will be archived with the aquaXterra datasets.
	- **exploratory**: Some preliminary code for data visualization.
	- **r_spatial**: Code for loading HUC shapefiles into R, joining them with the diversity data from BBS or the environmental data produced for aquaXterra, and creating plots and maps.
	- **spiders**: Loading and processing the spider GBIF data and the aquatic insect GBIF/NAWQA data and making a few exploratory maps and plots.
- **data**: Some CSV files for doing a few of the spatial analyses.
- **results**: Rmarkdown files with analyses and results.

*this document last modified by QDR, 31 May 2018.* 


