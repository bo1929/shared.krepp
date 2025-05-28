# Convert MetaPhlAn2 results into per-rank profiles.
# Usage: me.py kraken_dir

from sys import argv
from os import listdir

indir = argv[1]

fext = '.txt'
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
            if line.startswith('#SampleID'):
                continue
            if line.startswith('unclassified'):
                continue
            taxon, percent = line.rstrip().split('\t')
            code = taxon.split('|')[-1][0].upper()
            if code in code2rank:
                rank = code2rank[code]
                taxa[rank].add(taxon)
                data[id][taxon] = percent

for rank in ranks:
    with open(f'{rank}.tsv', 'w') as f:
        print('#SampleID', *ids, sep='\t', file=f)
        for taxon in sorted(taxa[rank]):
            row = [data[x][taxon] if taxon in data[x] else '0.0' for x in ids]
            print(taxon, *row, sep='\t', file=f)
