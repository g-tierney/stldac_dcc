#!/bin/bash
#SBATCH --account=volfovskylab
#SBATCH -p volfovskylab,statdept-low,herringlab-low,common
#SBATCH -N1
#SBATCH -c1
#SBATCH --mem=50G
#SBATCH -o cgibbs_fgibbs_comp_ex_log.txt

module load R
Rscript scripts/cgibbs_fgibbs_comp.R
