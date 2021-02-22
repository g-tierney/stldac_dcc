#!/bin/bash
#SBATCH --account=volfovskylab
#SBATCH -p volfovskylab,statdept-low,herringlab-low,common
#SBATCH -N1
#SBATCH -c1
#SBATCH --mem=50G
#SBATCH -o logs/log_simstudy_ex.txt
#SBATCH --array=1-80
#SBATCH --mail-user=gt83@duke.edu
#SBATCH --mail-type=END,FAIL

module load R/4.0.0
R CMD BATCH --no-restore --no-save scripts/vb_gibbs_simstudy.R logs/rlog_simstudy$SLURM_ARRAY_TASK_ID.Rout

