#!/bin/bash
cat query_list.txt | xargs -I{} art_illumina -ss HS25 -i ../reference_genomes/{}.fna -l 150 -f 5 -na -s 10 -o simulated_reads/{}
cat query_list.txt | xargs -I{} bash -c "seqtk sample simulated_reads/{}.fq 10000 > simulated_reads/{}_sampreads.fq"
