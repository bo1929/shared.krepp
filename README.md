# shared.krepp
- Results used used in the paper and some scripts to analyze them. The preprint is available in [bioRxiv](https://doi.org/10.1101/2025.01.20.633730).
- Larger files, raw results, and sequence data can be found on [Dryad](https://doi.org/10.5061/dryad.63xsj3vd3).
- The source code and precompile binaries, together with a tutorial, are available in the [main repository](https://github.com/bo1929/krepp).
- A catalogue of indexes are [available](https://ter-trees.ucsd.edu/data/krepp/), more information is in the main repository.

## `misc_data`
* `misc_data/backbone-WoLv1.nwk`: WoL-v1 backbone tree.
* `misc_data/backbone-WoLv2.nwk`: WoL-v2 backbone tree.
* `misc_data/data-WoLv2_placement`: Information for the WoL-v2 reference database.
* `misc_data/data-WoLv1_placement`: Information for the WoL-v1 reference database, including 16S placement metrics.
* `misc_data/data-bowtie_comparison`: Information for alignment/distance benchmarking, and novelty values for queries.
* `misc_data/hmi_retained_samples.tsv`: Sample IDs retained for HMP analysis.
* `misc_data/random_tree-WoLv2.nwk`: Simulated random tree using a dual-birth model.
* `misc_data/ladderized_tree-WoLv2.nwk`: Ladder tree (WoL-v2) used to benchmark coloring algorithm.
* `misc_data/ladderized-WoLv2.nwk`: Ladder tree used to benchmark coloring algorithm.
* `misc_data/qiime2_hmi_metadata.tsv`: Human microbiome metadata.
* `misc_data/ladderized-WoLv1.nwk`: Ladder tree (WoL-v1) used to benchmark coloring algorithm.

## `results`
### `results/expt-hmi`
* `results/expt-hmi/hmi-woltka-WoLv2`: Human microbiome results for Woltka OGUs using WoL-v2 reference.
* `results/expt-hmi/hmi-bracken-WoLv2`: Human microbiome results for Bracken profiles using WoL-v2 reference.
* `results/expt-hmi/hmi-ogu-RefSeqCIIdup`: Human microbiome results for krepp OGUs using duplicated uDance tree as reference.
* `results/expt-hmi/hmi-pp-RefSeqCIIdup`: Human microbiome results for krepp placements using duplicated uDance tree as reference.
* `results/expt-hmi/hmi-ogu-WoLv2`: Human microbiome results for krepp OGUs using duplicated WoL-v2 reference.
* `results/expt-hmi/microbiome_metadata.tsv`: Metadata of samples.
* `results/expt-hmi/hmi-pp-RefSeqCII`: Human microbiome results for krepp placements using deduplicated uDance tree as reference.
* `results/expt-hmi/hmi_separation_summary.csv`: Summary for separation statistics (pseudo-F) across all methods/references.
* `results/expt-hmi/hmi-ogu-RefSeqCII`: Human microbiome results for krepp OGUs using deduplicated uDance tree as reference.
* `results/expt-hmi/hmi-pp-WoLv2`: Human microbiome results for krepp placements using duplicated WoL-v2 reference.

### `results/alignment_comparison`
* `results/alignment_comparison/count_summary-all-WoLv2-alignment_comparison.csv`: Number of matches per query read.
* `results/alignment_comparison/read_summary-WoLv2-alignment_comparison-1M.csv`: Distance benchmarking against alignment, metrics per read (1M subsample) for all queries.
* `results/alignment_comparison/read_summary-WoLv2-alignment_comparison.csv`: Distance benchmarking against alignment, metrics per read for all queries.
* `results/alignment_comparison/resource_benchmarking.tsv`: Running time and memory use (bowtie2 and krepp).
* `results/alignment_comparison/dist_to_closest-final.tsv`: Novelty values for queries, measured by Mash.
* `results/alignment_comparison/reference_summary-all-WoLv2-alignment_comparison.csv`: Distance benchmarking against alignment summarizing reads per query.

### `results/expt-appspam_comparison`
* `results/expt-appspam_comparison/expt-Bartonella_50`: App-SpaM comparison on *Bartonella*.
* `results/expt-appspam_comparison/expt-Mycobacterium_40`: App-SpaM comparison on *Mycobacterium*.
* `results/expt-appspam_comparison/appspam_comparison.tsv`: App-SpaM comparison combining all results.
* `results/expt-appspam_comparison/expt-Rhizobiaceae_50`: App-SpaM comparison on *Rhizobiaceae*.
* `results/expt-appspam_comparison/expt-Piscirickettsiaceae_40`: App-SpaM comparison on *Piscirickettsiaceae*.
* `results/expt-appspam_comparison/expt-Moraxella_40`: App-SpaM comparison on *Moraxella*.
* `results/expt-appspam_comparison/expt-Bacteroides_40`: App-SpaM comparison on *Bacteroides*.

### `results/expt-emp`
* `results/expt-emp/emp_separation_summary-v1.tsv`: Separation results across EMPO levels (v1).
* `results/expt-emp/emp_separation_summary-v2.tsv`: Separation results across EMPO levels (v2).
* `results/expt-emp/emp-pp-WoLv1`: krepp placement results and BIOM tables on Earth's microbiome.
* `results/expt-emp/emp-ogu-WoLv1`: krepp OGUs results and BIOM tables on Earth's microbiome.
* `results/expt-emp/metadata_emp-v1.tsv`: Sample labels across EMPO levels (v1).
* `results/expt-emp/metadata_emp-v2.tsv`: Sample labels across EMPO levels (v2).

### `results/algorithmic_evaluation`
* `results/algorithmic_evaluation/multitree_heights_info-WoLv2.tsv`: Heights of the nodes of the multitree.
* `results/algorithmic_evaluation/out_degrees.tsv`: Out-degrees of the nodes of the multitree.
* `results/algorithmic_evaluation/match_stats-krepp_dth4-all.tsv`: The number of matches per read at each HD threshold.
* `results/algorithmic_evaluation/all_simulations-results.tsv`: Evaluation metrics for distance benchmarking with simulated genomes.
* `results/algorithmic_evaluation/clade_versus_multitree_sizes.tsv`: Information for colors corresponding to tree nodes/clades.
* `results/algorithmic_evaluation/color_graph_stats.tsv`: Statistics for the color multitree (including degrees) and maximal clades.
* `results/algorithmic_evaluation/index_info-WoLv2.tsv`: Details of the color multitree (including the number of k-mers).
* `results/algorithmic_evaluation/postorder_sizes.tsv`: Size of the color multitree during the post-order traversal of the tree.

### `results/cami-ii`
See `results/cami-ii/amber_strain_madness_contigs-min_dist` and `results/cami-ii/amber_marine_contigs-min_dist` for results used in the paper (computed using AMBER).

### `results/placement_comparison`
* `results/placement_comparison/ppmetrics-heuristic_comparison.tsv`: Placement metrics (edge errors) on WoLv2 comparing krepp-closest, krepp-LCA, krepp and bowtie2-closest.
* `results/placement_comparison/ppmetrics-bowtie-closestpp.tsv`: Placement metrics (edge errors) only for bowtie2-closest.
* `results/placement_comparison/ppmetrics_queries16S-all.tsv`: Placement metrics (edge errors) for queries used in 16S marker placement benchmarking.
* `results/placement_comparison/ppmetrics_reads16S-all.tsv`: Placement metrics (edge errors) for 16S reads placed using krepp (not used in the paper).

## `scripts`
* `scripts/cami2_analysis.R`: CAMI-II figure (Figure 5).
* `scripts/color_graph_stats.R`: Evaluating color multitree (Figure S1).
* `scripts/method_empirical_eval.R`: Analyzing the size of the color multitree and the number of *k*-mer matches (Figure 1c and Figure S1).
* `scripts/placement_comparison.R`: Analyzing placement metrics (Figure 3)
* `scripts/compare_with_alignment.R`: Distance benchmarking with real genomes and alignment comparison (Figure 2c,d,e,f and Figure S3).
* `scripts/distance_eval_simulations.R`: Distance benchmarking with simulated genomes and alignment comparison (Figure 2, Figure S2).
* `scripts/earth_microbiome_analysis.R`: EMP analysis, metrics obtained using QIIME2 (Figure 4a,b).
* `scripts/human_microbiome_anaysis.R`: HMP analysis, metrics obtained using QIIME2 (Figure 4e,f,c).
* `scripts/resource_benchmarking.R`: Resource usage comparison (Figure 1e, Figure 2g, Figure S4)
* `scripts/hdisthist_likelihood.ipynb`: Figure 1d.

## `figures`
All figures created using scripts in `./scripts` using the results present in `./results'.
