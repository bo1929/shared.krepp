# Convert Bracken results into profiles.
# Usage: me.py bracken_dir id_list > output_dir

from sys import argv


with open(argv[2], 'r') as f:
    ids = f.read().splitlines()

# columns
# name, taxonomy_id, taxonomy_lvl, kraken_assigned_reads, added_reads,
# new_est_reads, fraction_total_reads

ranks = ['phylum', 'class', 'order', 'family', 'genus', 'species']
codes = list(map(str.upper, ranks))

for rank in ranks:
    code = rank[0].upper()
    data = {}
    taxa = set()
    for id in ids:
        data[id] = {}
        with open(f'{argv[1]}/{id}.{code}.tsv', 'r') as f:
            next(f)
            for line in f:
                row = line.rstrip().split('\t')
                data[id][row[1]] = row[5]
                taxa.add(row[1])

    with open(f'{rank}.tsv', 'w') as f:
        print('#SampleID', *ids, sep='\t', file=f)
        for taxon in sorted(taxa, key=int):
            row = [data[x][taxon] if taxon in data[x] else '0' for x in ids]
            print(taxon, *row, sep='\t', file=f)
