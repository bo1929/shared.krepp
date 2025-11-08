#!/usr/bin/env python
# coding: utf-8
import pandas as pd
import sys

if __name__ == "__main__":
    dist = pd.read_csv(sys.argv[1], sep="\t", index_col=0)
    for i in dist.index:
        dist.loc[i, i] = float("inf")
    dist_novel_query = dist.min(axis=1)
    dist_novel_query.to_csv(sys.argv[2], sep="\t", header=False)
