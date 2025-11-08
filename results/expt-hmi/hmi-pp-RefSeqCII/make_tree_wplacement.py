import re
import json
import random
import string
from collections import defaultdict, Counter

import pandas as pd
import treeswift as ts


def extend_tree(tree_inputpath, tree_outputpath):
    with open(tree_inputpath, "r") as f:
        tree_string = f.read()
    tree_pp = ts.read_tree_newick(re.sub(r"{[0-9]+}", "", tree_string).strip())
    ic = 0
    node_set = list(tree_pp.traverse_postorder(leaves=True, internal=True))
    avg_blen = {
        nd: tree_pp.extract_subtree(nd).avg_branch_length(internal=False)
        for nd in node_set
    }
    for nd in node_set:
        nd.set_label(f"N{ic}")
        nd_p = nd.get_parent()
        if nd_p is not None:
            nd_n = ts.Node(f"P{ic}", nd.edge_length - 1e-3)
            nd.set_edge_length(1e-3)
            # nd_n = ts.Node(f"P{ic}", nd.edge_length - 1e-3)
            # nd.set_edge_length(1e-3)
            # nd_n = ts.Node(f"P{ic}", abs(nd.edge_length) / 2 )
            # nd.set_edge_length(abs(nd.edge_length) / 2)
            nd_p.remove_child(nd)
            nd_p.add_child(nd_n)
            nd.set_parent(nd_n)
            nd_t = ts.Node(f"T{ic}", avg_blen[nd])
            # nd_t = ts.Node(f"T{ic}", 1e-3)
            nd_t.set_parent(nd_n)
            nd_n.add_child(nd_t)
            nd_n.add_child(nd)
        ic += 1
    tree_pp.write_tree_newick(tree_outputpath)
    return tree_pp


def generate_feature_table(samples_inputpath, placements_dir, features_outputpath):
    with open(samples_inputpath, "r") as f:
        samples = Counter(map(lambda x: x.strip(), f.readlines()))
    sample_profiles = defaultdict(lambda: defaultdict(float))
    for s in samples:
        with open(f"{placements_dir}/{s}.pplist", "r") as f:
            pp_counts = Counter(map(lambda x: x.strip(), f.readlines()))
        # sample_profiles[s] = dict(map(lambda x: (f"T{x[0]}", x[1]/sum(pp_counts.values())), pp_counts.items()))
        sample_profiles[s] = dict(map(lambda x: (f"T{x[0]}", x[1]), pp_counts.items()))
    df = pd.DataFrame(sample_profiles)
    df = df.fillna(0)
    df.to_csv(features_outputpath, sep="\t", index_label="#FeatureID")


if __name__ == "__main__":
    extend_tree("placement_tree.nwk", "./placement_tree_extended.nwk")
    # generate_feature_table("all_samples.txt", "hmi_placements/", "./placement_table.tsv")
