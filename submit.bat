#!/bin/bash
#SBATCH --qos=priority
#SBATCH --job-name=ECEMF_reports
#SBATCH --nodes=1
#SBATCH --tasks-per-node=1
#SBATCH --mail-type=END
#SBATCH --mem=32000

Rscript ECEMF_reports.R