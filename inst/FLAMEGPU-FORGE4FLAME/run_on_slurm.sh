#!/bin/bash
#SBATCH --partition=gracehopper
#SBATCH -N 1
#SBATCH --gres gpu:1
time bash /beegfs/home/scontald/gitProjects/FLAMEGPU2-F4F/run.sh -expdir NoCountermeasures -e ON -prun 8