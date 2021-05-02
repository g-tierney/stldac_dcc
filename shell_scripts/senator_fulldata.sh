#!/bin/bash
#SBATCH --account=volfovskylab
#SBATCH -p volfovskylab,statdept-low,herringlab-low,common
#SBATCH -N1
#SBATCH -c1
#SBATCH --mem=120G
#SBATCH -o logs/log_full_senators_%A.txt
#SBATCH --array=1-5
#SBATCH --mail-user=gt83@duke.edu
#SBATCH --mail-type=END,FAIL

module load R/4.0.0
R CMD BATCH --no-restore --no-save scripts/vb_senTweets_execute.R logs/rlog_full_senators$SLURM_ARRAY_TASK_ID.Rout

