#!/bin/bash
thresholds=(0.25 0.1 0.075 0.05 0.04 0.03 0.02 0.01 0.005 0.001 0.0005 0.0001)
for i in "${!thresholds[@]}"; do 
  tcurr=${thresholds[$i]}
  if ((i > 0)); then
    tprev=${thresholds[$((i-1))]}
    comm -12 ${1}/singletons-${tprev}_to_${tcurr}.tsv ${2}/singletons-${tprev}_to_${tcurr}.tsv \
      | sort | shuf | head -n10 | cut -f1
  fi
done
