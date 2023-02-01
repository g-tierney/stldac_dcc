#!/bin/bash
#SBATCH --account=volfovskylab
#SBATCH -p volfovskylab,statdept-low,herringlab-low,common
#SBATCH -N1
#SBATCH -c1
#SBATCH --mem=50G
#SBATCH -o logs/log_vb_SenTweets_JanMay_%A.txt
#SBATCH --array=1-16
#SBATCH --mail-user=gt83@duke.edu
#SBATCH --mail-type=END,FAIL


module load R
Rscript scripts/vb_senTweets_execute_JanMay.R
