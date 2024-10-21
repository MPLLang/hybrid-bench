#!/bin/bash

# Define the CPU ratios to test
cpu_ratios=(0.1 0.35 0.5)

# Define the matrix sizes (N, M, K)
matrix_sizes=(
    "8192 8192 8192"
    "10000 10000 10000"
)

# Define the number of GPUs to test
num_gpus=(1 2 3 4)

# Define the number of CPU threads to test
cpu_threads=(1 10 20 30 40 50 56)

# Path to the benchmark executable
benchmark_executable="./cublasxt_hybrid_benchmark"

# Loop over CPU ratios
for cpu_ratio in "${cpu_ratios[@]}"; do
    # Loop over matrix sizes
    for matrix_size in "${matrix_sizes[@]}"; do
        # Parse N, M, K from matrix_size
        read -r N M K <<< "$matrix_size"
        
        # Loop over number of GPUs
        for gpus in "${num_gpus[@]}"; do
            # Loop over number of CPU threads
            for threads in "${cpu_threads[@]}"; do
                # Run the benchmark with the current set of parameters
                echo "Running benchmark with cpuRatio=${cpu_ratio}, N=${N}, M=${M}, K=${K}, numGpus=${gpus}, cpuThreads=${threads}"
                ./mgpu-hybrid $gpus $N $M $K $cpu_ratio $threads > ./mgpu-hybrid-results/${N}-gpus${gpus}-threads${threads}-ratio${cpu_ratio}.out
                
                # Add a separator between runs
                echo "-----------------------------------"
            done
        done
    done
done
