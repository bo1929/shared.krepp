# Convert MetaPhlAn percentages to counts per million

import fileinput

for line in fileinput.input():
    line = line.rstrip()
    if line.startswith('#SampleID'):
        print(line)
        continue
    row = line.split('\t')
    row = [row[0]] + [int(float(x) * 10000) for x in row[1:]]
    print(*row, sep='\t')
