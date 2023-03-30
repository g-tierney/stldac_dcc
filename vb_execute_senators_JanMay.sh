#!/bin/bash
#SBATCH --account=volfovskylab
#SBATCH -p volfovskylab,statdept-low,herringlab-low,common
#SBATCH -N1
#SBATCH -c1
#SBATCH --mem=100G
#SBATCH -o logs/log_vb_SenTweets_JanMay_%A.txt
#SBATCH --array=1-12
#SBATCH --mail-user=gt83@duke.edu
#SBATCH --mail-type=END,FAIL


module load R
R CMD BATCH --no-restore --no-save scripts/vb_senTweets_execute_JanMay_testing.R logs/rlog_vb_SenTweets_JanMay_testing_$SLURM_ARRAY_TASK_ID.Rout

