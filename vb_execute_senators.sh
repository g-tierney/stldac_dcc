#!/bin/bash
#SBATCH --account=volfovskylab
#SBATCH -p volfovskylab,statdept-low,herringlab-low,common
#SBATCH -N1
#SBATCH -c1
#SBATCH --mem=25G
#SBATCH -o vb_log.txt

module load R
Rscript scripts/vb_senTweets_execute.R
