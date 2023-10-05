#!/bin/bash
echo "hybrid K=0.5 procs=[2,3,4,6,8,16,24,32]"
for p in 2 3 4 6 8 16 24 32; do
  ./main.mpl @mpl procs $p -- -n 10000000 -warmup 3 -repeat 10 -impl hybrid -sched-gpu-payout 0.5 \
  | grep average \
  | tail -c+9 \
  | head -c 6;
  echo "";
done
