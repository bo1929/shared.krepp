#!/bin/bash
PAIR_IDX=$(($1))
sed "${PAIR_IDX}q;d" all_pairs.tsv \
  | xargs -l bash -c 'paste <(echo $0) <(echo $1) <(skani dist --min-af 0 ../../expt-WoLv2_16k/reference_genomes/$0.fna ../extra_queries_genomes/$1.fna | cut -f3 | tail -1)'

