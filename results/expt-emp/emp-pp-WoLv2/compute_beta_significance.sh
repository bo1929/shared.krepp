echo $0 $1 $2
qiime diversity beta-group-significance --i-distance-matrix  $2/weighted_unifrac_distance_matrix.qza --m-metadata-file ../metadata_emp-v2.tsv  --m-metadata-column "empo_$1"  --o-visualization $2/beta_significance-empo_$1 --p-permutations 1000
