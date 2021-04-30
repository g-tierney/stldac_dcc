#!/bin/bash
#SBATCH --account=volfovskylab
#SBATCH -p volfovskylab,statdept-low,herringlab-low,common
#SBATCH -N1
#SBATCH -c1
#SBATCH --mem=100G
#SBATCH -o logs/log_senator_choose_G_%A.txt
#SBATCH --array=1-5
#SBATCH --mail-user=gt83@duke.edu
#SBATCH --mail-type=END,FAIL

module load R/4.0.0
R CMD BATCH --no-restore --no-save scripts/vb_senators_choose_G.R logs/rlog_chooseG$SLURM_ARRAY_TASK_ID.Rout

