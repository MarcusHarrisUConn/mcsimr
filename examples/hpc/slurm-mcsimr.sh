#!/usr/bin/env bash
#SBATCH --job-name=mcsimr-sem
#SBATCH --output=logs/mcsimr-sem-%j.out
#SBATCH --error=logs/mcsimr-sem-%j.err
#SBATCH --time=2-00:00:00
#SBATCH --cpus-per-task=8
#SBATCH --mem=16G

set -euo pipefail

mkdir -p logs results/checkpoints results/sem-targets

Rscript -e "install.packages(c('targets', 'tarchetypes'), repos = 'https://cloud.r-project.org')"
Rscript -e "targets::tar_make(callr_function = NULL)"
