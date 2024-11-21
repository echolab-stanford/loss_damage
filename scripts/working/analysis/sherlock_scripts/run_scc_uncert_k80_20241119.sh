#!/bin/bash
#
#SBATCH --job-name=test_run
#
#SBATCH --partition=serc
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=38
#SBATCH --time=04:00:00
#SBATCH --mem-per-cpu=10G
#SBATCH --output=/home/users/mhzahid/run_scc_uncert_20241119.log
#SBATCH --mail-type=ALL

# load modules
ml R/4.0.2
ml physics gdal/2.2.1 udunits proj/4.9.3 geos

# execute script
Rscript /scratch/users/mhzahid/run_5lag_k80_scc_sherlock2.R
