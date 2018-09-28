#!/bin/sh -login
#PBS -l walltime=12:00:00
#PBS -l nodes=1:ppn=1
#PBS -l mem=32gb
#PBS -N nhdhuc4
#PBS -j oe
#PBS -m n

# load correct version of R, GDAL, and GEOS
module load R/3.2.0 GDAL GEOS

# import local R library for newer version of rgdal and dplyr
export R_LIBS_USER=~/R/library

# add code file to the temp directory
cp /mnt/research/aquaxterra/CODE/R/nhd/nhd_huc4.r $TMPDIR/rcode.r

cd $TMPDIR
now="$(date +'%d%h%Y%H%M')"

# execute the r code
R CMD BATCH --no-save --no-restore rcode.r /mnt/research/aquaxterra/CODE/R/nhd/routputfiles/huc4out.txt

