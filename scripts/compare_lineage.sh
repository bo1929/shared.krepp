#!/bin/bash
# $1: tree newick
# $2: node ID
# $3: node ID
paste <(./print_parents.py $1 $2 | sed 's/\t/\n/g') <(./print_parents.py $1 $3 | sed 's/\t/\n/g') | less
