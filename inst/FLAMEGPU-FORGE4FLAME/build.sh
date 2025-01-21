#!/bin/bash

: '
  FLAMEGPU2 Agent-Based Model

  build.sh

  Build the ABM.

  Inputs:
      -cps or --checkpoint_simulation:  run the model in a simplified version with the aim to obtain a checkpoint
      -v   or --visualisation:          activate the visualisation
      -g   or --debug:                  execute the simulation with debug prints

  Authors: Daniele Baccega, Irene Terrone, Simone Pernice
'

# Default values for input parameters
CHECKPOINT_SIMULATION="OFF"
VISUALISATION="OFF"
DEBUG="OFF"

while [[ $# -gt 0 ]]; do
  case $1 in
    -cps|--checkpoint_simulation)
      CHECKPOINT_SIMULATION="$2"
      shift
      shift
      ;;
    -v|--visualisation)
      VISUALISATION="$2"
      shift
      shift
      ;;
     -g|--debug)
      DEBUG="$2"
      shift
      shift
      ;;
    -h|--help)
  	  printf "./build.sh - build the ABM\n\n"
  	  printf "Arguments:\n"
      printf "        -cps or --checkpoint_simulation:  run the model in a simplified version with the aim to obtain a checkpoint (default: OFF; possible values: ON, OFF)\n"
      printf "        -v   or --visualisation:          activate the visualisation (default: OFF; possible values: ON, OFF)\n"
      printf "        -g   or --debug:                  execute the simulation with debug prints (default: OFF; possible values: ON, OFF)\n"
      exit 1
      ;;
    -*|--*)
      echo "Unknown option $1"
      exit 1
      ;;
    *)
      POSITIONAL_ARGS+=("$1")   # Save positional arg
      shift                     # Past argument
      ;;
  esac
done

COMP_CAP=$(nvidia-smi --query-gpu=compute_cap --format=csv | sed -n 2p | sed -r 's/[.]//g')

set -- "${POSITIONAL_ARGS[@]}"  # Restore positional parameters

# Create the build directory and change into it
mkdir -p build && cd build

# Configure CMake from the command line passing configure-time options. 
cmake .. -DCMAKE_CUDA_COMPILER=/usr/local/cuda/bin/nvcc -DCMAKE_BUILD_TYPE=Release -DCMAKE_CUDA_ARCHITECTURES=$COMP_CAP -DFLAMEGPU_SEATBELTS=OFF -DCHECKPOINT=$CHECKPOINT_SIMULATION -DFLAMEGPU_VISUALISATION=$VISUALISATION -DDEBUG=$DEBUG

#  Build the required targets. In this case all targets
cmake --build . --target all -j8

cd ..
