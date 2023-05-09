#!/bin/bash

echo "cpu procs=[2,3,4,6,8,16,24,32]"
for p in 2 3 4 6 8 16 24 32; do
  ./main.mpl @mpl procs $p -- -warmup 3 -repeat 10 -impl cpu \
  | grep average \
  | tail -c+9 \
  | head -c 6;
  echo "";
done

echo "gpu"
./main.mpl @mpl procs $p -- -warmup 3 -repeat 10 -impl gpu \
| grep average \
| tail -c+9 \
| head -c 6;
echo "";

echo "hybrid K=0.5 procs=[2,3,4,6,8,16,24,32]"
for p in 2 3 4 6 8 16 24 32; do
  ./main.mpl @mpl procs $p -- -warmup 3 -repeat 10 -impl hybrid -sched-gpu-payout 0.5 \
  | grep average \
  | tail -c+9 \
  | head -c 6;
  echo "";
done

echo "hybrid K=1 procs=[2,3,4,6,8,16,24,32]"
for p in 2 3 4 6 8 16 24 32; do
  ./main.mpl @mpl procs $p -- -warmup 3 -repeat 10 -impl hybrid -sched-gpu-payout 1 \
  | grep average \
  | tail -c+9 \
  | head -c 6;
  echo "";
done

echo "hybrid K=2 procs=[2,3,4,6,8,16,24,32]"
for p in 2 3 4 6 8 16 24 32; do
  ./main.mpl @mpl procs $p -- -warmup 3 -repeat 10 -impl hybrid -sched-gpu-payout 2 \
  | grep average \
  | tail -c+9 \
  | head -c 6;
  echo "";
done

echo "hybrid K=4 procs=[2,3,4,6,8,16,24,32]"
for p in 2 3 4 6 8 16 24 32; do
  ./main.mpl @mpl procs $p -- -warmup 3 -repeat 10 -impl hybrid -sched-gpu-payout 4 \
  | grep average \
  | tail -c+9 \
  | head -c 6;
  echo "";
done

echo "hybrid K=8 procs=[2,3,4,6,8,16,24,32]"
for p in 2 3 4 6 8 16 24 32; do
  ./main.mpl @mpl procs $p -- -warmup 3 -repeat 10 -impl hybrid -sched-gpu-payout 8 \
  | grep average \
  | tail -c+9 \
  | head -c 6;
  echo "";
done