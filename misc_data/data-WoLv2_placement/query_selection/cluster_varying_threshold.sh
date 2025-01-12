#!/bin/bash
thresholds=(0.25 0.1 0.075 0.05 0.04 0.03 0.02 0.01 0.005 0.001 0.0005 0.0001)
for i in "${!thresholds[@]}"; do 
  tcurr=${thresholds[$i]}
  echo "Clustering with threshold ${tcurr}..."
  # TreeCluster version 1.0.4
  TreeCluster.py -i backbone.nwk -t ${tcurr} > "tree_clustering-t${tcurr}.tsv"
  if ((i > 0)); then
    tprev=${thresholds[$((i-1))]}
    diff --new-line-format="" --unchanged-line-format="" \
      <(grep -- "-1" "tree_clustering-t${tcurr}.tsv" | sort) \
      <(grep -- "-1" "tree_clustering-t${tprev}.tsv" | sort) \
      | sort > singletons-${tprev}_to_${tcurr}.tsv
      # | shuf  --random-source="backbone.nwk" | head -n20 | sort > singletons-${tprev}_to_${tcurr}.tsv
  fi
done
# cat singletons*_to_*.tsv | cut -f1 > selected_queries.txt
