# Readme: Lake area and stream length from USGS NHD

QDR 15 Aug 2018  
Last modified: ACS 28 Sep 2018

## Location of input data

The data were downloaded to `/mnt/research/aquaxterra/DATA/raw_data/NHD/usgs_nhd/`. In that directory, there is a subdirectory for each HUC4 with the naming format `NHD_H_xxxx_Shape` where `xxxx` is the 4 digit code. Each HUC4 has a shapefile called `NHDFlowline.shp` with stream line features and one called `NHDWaterbody.shp` with lake polygon features. Because of this, it is very easy to sum up the areas and lengths by HUC4 since they are already separated into different shape files, but more difficult to sum up HUC8 and HUC12 because you have to do an intersection each time and pull out only the lakes and streams that are inside of the HUC area. 

## Location of code

The entire code pipeline is done in R with the `rgeos` package used so that `GEOS` can do the actual heavy lifting of calculating the intersections of HUC8 and HUC12, then the areas and lengths. This might not be computationally the fastest or least memory-intensive. Not all the HUCs were finished running when I stopped working on this project a few months ago, thinking that it would be superseded either by Lifeng's code or by Nicole's code. If neither of those work, we will have to use this one anyway. *UPDATE 21 August*: I finished running all the HUCs so the data product is complete, whether or not we choose to use it. 

The code is on GitHub at `github/qdread/aquaxterra/code/r_spatial`. It has to be run remotely on the HPCC for two reasons: it takes a really long time, and all the NHD shapefiles are stored there already. So there is a directory on the HPCC where I copied the R scripts and shell scripts: `/mnt/research/aquaxterra/CODE/R/nhd`. 

## Location of output files

All the temporary output CSVs are written to the `csvs` subdirectory in the `/mnt/research/aquaxterra/CODE/R/nhd` folder. Also in the folder on the HPCC are two lookup tables for the different waterbody and flowline codes used in the NHD to classify the lakes and streams: `nhd_waterbody_lookup.csv` and `nhd_flowline_lookup.csv`. We are using these so that we can split up the lake areas and stream lengths by category, subcategory, and permanence status.

The final CSVs are written to `/mnt/research/aquaxterra/DATA/albers_projected_data/NHD`. As of 17 August, only final HUC4 files are there. *UPDATE 28 Sep* All final HUC files, including HUC8/12, are now located in the folder.

## Description of workflow

### HUC4

For HUC4s, it is very simple and can be run with a single job (no need to parallelize). The R script is `nhd_huc4.r` which can be run by submitting the `qsub` script called `nhdh4.sh`. The R script is one big `for()` loop that goes through each of the HUC4s. For each iteration of the loop, we

- load the waterbody and flowline shapefiles for the HUC4
- project the shapefiles to Albers equal area so that we can calculate length and area
- use the GEOS functions `gArea()` and `gLength()` to calculate the area and length of the lakes and rivers, respectively
- join the lookup table with category codes and the results of the area and length calculation to the attribute table of the lake/stream shapefiles
- calculate the sum of lake area/stream length grouped by category, subcategory, and permanence
- save the lake and river summaries as a list element

At the end of the loop, we write one CSV for HUC4 lake areas and one CSV for HUC4 river lengths.

### HUC8 and HUC12

HUC8 and HUC12 are more complex because we have to load the HUC4 shapefile and then do an intersection so that we can clip only the segments of each line and polygon feature that are in the appropriate HUC. At one point, I checked to make sure there is no double counting of streams running along the boundaries of two HUCs, so that isn't a problem. The script used is `nhd_huc812_optimized.r`. The shell script is `nhdhuc.sh`. This shell script takes two arguments, `watertype` and `huclevel`. This means every HUC4 is done in parallel four times: once to sum the stream length by HUC8, once to sum the lake area by HUC8, once to sum the stream length by HUC12, and once to sum the lake area by HUC12. Each of these should produce a CSV. Most have been run already but a few have not yet been run. *UPDATE 28 Sep* All HUCs have now been run. The workflow is as follows:

- Submit four job arrays with 223 child tasks, one for each HUC4. The four job arrays are for HUC8 length, HUC8 area, HUC12 length, and HUC12 area.
- In each of the child tasks, load the NHD shapefile for that HUC4 (either waterbody or flowline).
- Load either the HUC8 boundary shapefile or the HUC12 boundary shapefile for that HUC4.
- *added 21 August*: Exclude HUCs that contain the entire area of one of the Great Lakes or one of the huge islands in the Canadian part of the Great Lakes. These are so big that they cause the individual jobs to time out. I believe this is because the Canadian islands in the Great Lakes are not really part of the HUC system so they were designated as HUC12s even though they are much bigger than HUC12s in the USA.
- Loop through the boundary shapefile and call `gIntersection()` on each of the smaller HUCs and the waterbody/flowline shapefiles to get the subset in each smaller HUC. Note that this loop is itself parallelized so a single job is split among 5 processors! *UPDATE 21 August*: I added some code to optimize this part so it runs pretty quickly even for the HUCs with the most lakes and rivers in them.
- Do a second loop through the intersected shapefiles, first projecting to Albers equal area then calling either `gArea()` or `gLength()`.
- Calculate the sum of lake area/stream length grouped by category, subcategory, and permanence.
- Do some wrangling to fix the IDs and join the lookup table and area/length results to the attribute table.
- Write the results to a single CSV for that particular HUC4 named `xxxx_HUCno_watertype.csv` where `xxxx` is the 4-digit code, `no` is either 8 or 12, and `watertype` is either `lake` or `river`.

*added 21 August*: In the script `nhd_combinecsvs.r`, the results are combined into four total CSVs and written to `/mnt/research/aquaxterra/DATA/albers_projected_data/NHD`.

## Additional Notes

### Named lake area and river length summaries

New files have been created as of Sep 28, 2018 that contain the mean, median, sd, and number of major (named) lakes and rivers within each HUC. The river and lake segments are joined by name and the total area or length is calculated. 

These files are created using the same process described above. However, all code files to create these data are appended with XXX_withMeans. Data files are appended with 'means.' The full code file equivalents are listed below. 

+ nhd_huc4.r --> nhd_huc4_withMeans.r
+ nhdh4.sh (no changes)
+ nhd_huc812_optimized.r --> nhd_huc812_optimized_withMeans.r
+ nhdhuc.sh --> nhdhuc_withMeans.sh
