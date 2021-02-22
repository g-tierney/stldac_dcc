#!/bin/bash
#SBATCH --account=volfovskylab
#SBATCH -p volfovskylab,statdept-low,herringlab-low,common
#SBATCH -N1
#SBATCH -c1
#SBATCH --mem=50G
#SBATCH -o log_simstudy_ex.txt
#SBATCH --array=1-2
#SBATCH --mail-user=gt83@duke.edu
#SBATCH --mail-type=END,FAIL

module load R
R CMD BATCH --no-restore --no-save scripts/vb_gibbs_simstudy.R rlog_simstudy$SLURM_ARRAY_TASK_ID.Rout

