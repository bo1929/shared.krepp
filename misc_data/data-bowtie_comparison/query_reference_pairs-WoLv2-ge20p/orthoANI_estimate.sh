#!/bin/bash
PAIR_IDX=$(($1))
sed "${PAIR_IDX}q;d" all_pairs.tsv \
  | xargs -l bash -c 'paste <(echo $0) <(echo $1) <(java -jar ~/.local/bin/OAT_cmd.jar -blastplus_dir ~/.local/bin/ -num_threads 32 -fasta1 ../../expt-WoLv2_16k/reference_genomes/$0.fna -fasta2 ../extra_queries_genomes/$1.fna | grep OrthoANI| cut -f3 -d" ")'
