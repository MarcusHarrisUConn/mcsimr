#!/usr/bin/env bash
#SBATCH --job-name=mcsimr-sem
#SBATCH --output=logs/mcsimr-sem-%j.out
#SBATCH --error=logs/mcsimr-sem-%j.err
#SBATCH --time=2-00:00:00
#SBATCH --cpus-per-task=8
#SBATCH --mem=16G
#SBATCH --array=1-8

set -euo pipefail

mkdir -p logs results/checkpoints results/sem-shards

export MCSIMR_SHARDS="${MCSIMR_SHARDS:-8}"
export MCSIMR_SHARD_ID="${SLURM_ARRAY_TASK_ID:-1}"
export MCSIMR_WORKERS="${SLURM_CPUS_PER_TASK:-1}"

Rscript examples/hpc/run-sem-shard.R
echo "Shard manifest: results/checkpoints/sem-shards/shard-$(printf '%03d' "${MCSIMR_SHARD_ID}")-of-$(printf '%03d' "${MCSIMR_SHARDS}")/run-manifest.csv"
