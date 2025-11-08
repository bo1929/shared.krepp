# Convert Kraken results into per-rank profiles.
# Compatible with Kraken1, Kraken2 and Centrifuge.
# Usage: me.py kraken_dir

from sys import argv
from os import listdir

indir = argv[1]

fext = '.report'
lext = len(fext)

ranks = ['phylum', 'class', 'order', 'family', 'genus', 'species']
code2rank = {x[0].upper(): x for x in ranks}

ids = []
taxa = {x: set() for x in ranks}
data = {}

for file in sorted(listdir(indir)):
    if not file.endswith(fext):
        continue
    id = file[:-lext]
    ids.append(id)

    data[id] = {}
    with open(f'{indir}/{file}', 'r') as f:
        for line in f:
            _, count, _, code, taxon, _ = line.strip().split('\t')
            if code in code2rank:
                rank = code2rank[code]
                taxa[rank].add(taxon)
                data[id][taxon] = count

for rank in ranks:
    with open(f'{rank}.tsv', 'w') as f:
        print('#SampleID', *ids, sep='\t', file=f)
        for taxon in sorted(taxa[rank], key=int):
            row = [data[x][taxon] if taxon in data[x] else '0' for x in ids]
            print(taxon, *row, sep='\t', file=f)
