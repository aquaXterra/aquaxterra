#!/bin/sh -login
#SBATCH --time=04:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=5
#SBATCH --mem=40G
#SBATCH --job-name=nhd
#SBATCH --mail-type=FAIL
#SBATCH --array=1-223

module load R

cd /mnt/research/aquaxterra/CODE/R/nhd/

# Run script with external variables
Rscript nhd_huc812_optimized_withMeans.r $watertype $huclevel

scontrol show job $SLURM_JOB_ID

# When submitting this job, remember to add variables (e.g., --export=watertype=lake,huclevel=huc8)
