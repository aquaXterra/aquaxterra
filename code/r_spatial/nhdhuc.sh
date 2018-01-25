#!/bin/sh -login
#PBS -l walltime=4:00:00
#PBS -l nodes=1:ppn=1
#PBS -l mem=2gb
#PBS -N nhd
#PBS -j oe
#PBS -m n

module load R/3.2.0 GDAL GEOS
cp /mnt/research/aquaxterra/CODE/R/nhd/nhd_huc812.r $TMPDIR/rcode.r
cd $TMPDIR
now="$(date +'%d%h%Y%H%M')"

# Cobble together the R command using the environmental variables supplied.
cmd="R CMD BATCH --no-save --no-restore '--args watertype=\""$watertype"\" huclevel=\""$huclevel"\"' rcode.r /mnt/research/aquaxterra/CODE/R/nhd/routputfiles/"$watertype"_"$huclevel"_"$PBS_ARRAYID"_"$now".txt"
eval $cmd

