#!/bin/sh -login
#SBATCH --time=6:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=5
#SBATCH --mem=40G
#SBATCH --job-name=nhd_huc812
#SBATCH --mail-type=FAIL
#SBATCH --array=1-223

module load R

# Run script with external variables
Rscript nhd_huc812_optimized.r $watertype $huclevel

scontrol show job $SLURM_JOB_ID

# When submitting this job, remember to add variables (e.g., --export=watertype=lake,huclevel=huc8)