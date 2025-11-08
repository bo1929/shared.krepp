import argparse
import json
import newick_extended
import collections
import pandas as pd
import treeswift as ts

parser = argparse.ArgumentParser(description='test')
parser.add_argument('--infile', type=str, help='input tree')
parser.add_argument('--jplace', type=str)
parser.add_argument('--outfile', type=str)
parser.add_argument('--prefix', type=str, help='prefix of the queries', default='query')
args = parser.parse_args()

tree = ts.read_tree_newick(args.infile)
dist = {}

with open(args.jplace, 'r') as f:
    tree_jplace = json.load(f)

original_tree = newick_extended.read_tree_newick(tree_jplace['tree'])
query_terminal = {p['n'][0].replace(args.prefix, ''): p['p'][0][-1] for p in tree_jplace['placements']}
query_s = set([i.split('_')[0] for i in query_terminal])
gt_terminal = {n.label: n.edge_length for n in original_tree.traverse_leaves() if n.label in query_s}
for n in tree.traverse_postorder():
    if n.is_leaf():
        if args.prefix in n.label:
            n.q = {n.label.replace(args.prefix, '').split('_')[0]: {n.label.replace(args.prefix, ''): -query_terminal[n.label.replace(args.prefix, '')]}}
            n.gt = {}
        else:
            if n.label in gt_terminal:
                n.gt = {n.label: -gt_terminal[n.label]}
            else:
                n.gt = {}
            n.q = {}
    else:
        q_dict = collections.defaultdict(dict)
        gt_dict = {}
        for c in n.child_nodes():
            if c.q:
                for q in c.q:
                    q_dict[q].update({q2: c.q[q][q2] + c.edge_length for q2 in c.q[q]})
            if c.gt:
                gt_dict.update({gt: c.gt[gt]+c.edge_length for gt in c.gt})
            for q in q_dict:
                if q in gt_dict:
                    for item in q_dict[q]:
                        dist[item] = q_dict[q][item] + gt_dict[q]
                    q_dict[q] = {}
        n.q = q_dict
        n.gt = gt_dict


dist = pd.DataFrame.from_dict(dist, orient='index', columns=['error'])
dist.to_csv(args.outfile, sep='\t')
