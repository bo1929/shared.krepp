import re
import json
import treeswift as ts
import pandas as pd
import numpy as np
import argparse
from collections import defaultdict



def make_newick(tree_str: str) -> str:
    subs_tree = re.sub("\{(\d+)\}", '', tree_str)
    return subs_tree

def map_branches(data) -> dict:
    """
    Build dictionary with edge/branch numbers as keys and 
    reference tree leaves as values
    """
    def get_id(s):
        return int(re.search("\{(\d+)\}", s).group(1))
    
    extnwk_str = data['tree']
    tree = ts.read_tree_newick(make_newick(extnwk_str))
    lbl_to_nd = tree.label_to_node(selection='all')

    branches = {
        get_id(extnwk_str[extnwk_str.find(lbl):]): lbl
        for lbl, nd in lbl_to_nd.items()
    }
    return branches


def main(args):
    query_label = args.query_label
    with open(args.jplace_file, 'r') as file:
        data = json.load(file)
    backbone = ts.read_tree_newick(args.tree)

    branch_to_lbl = map_branches(data)
    lbl_to_nd = backbone.label_to_node(selection='all')

    branch_pdict = defaultdict(list)
    blen_pdict = defaultdict(list)
    dist_pdict = defaultdict(list)
    lwr_pdict = defaultdict(list)
    summary = defaultdict(dict)

    for placement in data["placements"]:
        rid = placement["n"][0]
        for p in placement["p"]:
            edge_num, pendant_length, distal_length, likelihood, like_weight_ratio, distance = p
            if branch_to_lbl.get(edge_num, False):
                plbl = branch_to_lbl[edge_num]
                branch_pdict[rid].append(branch_to_lbl[edge_num])
                blen_pdict[rid].append(backbone.distance_between(lbl_to_nd[plbl], lbl_to_nd[query_label]))
                dist_pdict[rid].append(distance)
                lwr_pdict[rid].append(like_weight_ratio)
                if summary.get(plbl, False):
                    summary[plbl]["dr"] = (summary[plbl]["dr"]*summary[plbl]["c"]+distance)/(summary[plbl]["c"]+1)
                    summary[plbl]["c"] +=1
                else:
                    summary[plbl]["c"] = 1
                    summary[plbl]["dt"] = backbone.distance_between(lbl_to_nd[plbl], lbl_to_nd[query_label])
                    summary[plbl]["dr"] = distance

    # dfdict = defaultdict(list)
    # for p, val in summary.items():
    #     dfdict["p"].append(p)
    #     dfdict["r"].append(val["c"]/float(data["metadata"]["num_queries"]))
    #     dfdict["dt"].append(val["dt"])
    #     dfdict["dr"].append(val["dr"])
    # pd.DataFrame(dfdict).to_csv(args.output, sep="\t")

    dfdict = defaultdict(list)
    for rid, val in branch_pdict.items():
        max_ix = np.argmax(blen_pdict[rid])
        min_ix = np.argmin(blen_pdict[rid])
        dfdict["rid"].append(rid)
        dfdict["max_blen"].append(blen_pdict[rid][max_ix])
        dfdict["min_blen"].append(blen_pdict[rid][min_ix])
        dfdict["max_dist"].append(dist_pdict[rid][max_ix])
        dfdict["min_dist"].append(dist_pdict[rid][min_ix])
        dfdict["qid"].append(query_label)
    pd.DataFrame(dfdict).to_csv(args.output, sep="\t", index=False)

if __name__ == "__main__":
    argparser = argparse.ArgumentParser()
    argparser.add_argument("-q", "--query-label", type=str, required=True)
    argparser.add_argument("-t", "--tree", type=str, required=True)
    argparser.add_argument("-i", "--jplace-file", type=str, required=True)
    argparser.add_argument("-o", "--output", type=str, required=True)
    args = argparser.parse_args()
    main(args)