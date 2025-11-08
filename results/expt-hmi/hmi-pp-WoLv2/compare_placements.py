import numpy as np
from collections import defaultdict
import pandas as pd
import treeswift as ts
import json
import re
import random
import string
from collections import Counter
from joblib import Parallel, delayed


def compute_weighted_unifrac(tree_obj, true_labels, final_labels):
    true_abunds = {}
    final_abunds = {}

    u = 0
    D = 0
    for n in tree_obj.traverse_postorder():
        if n.is_root():
            break
        if n.is_leaf():
            true_abunds[n.label] = 0
            final_abunds[n.label] = 0
            if n.label in true_labels:
                true_abunds[n.label] = true_labels[n.label]
            if n.label in final_labels:
                final_abunds[n.label] = final_labels[n.label]
        else:
            true_abunds[n.label] = 0
            final_abunds[n.label] = 0
            for c in n.child_nodes():
                true_abunds[n.label] += true_abunds[c.label]
                final_abunds[n.label] += final_abunds[c.label]
        u += n.edge_length * np.fabs(true_abunds[n.label] - final_abunds[n.label])
        D += n.edge_length * (true_abunds[n.label] + final_abunds[n.label])
    # print(u)
    return u / D


def compute_unifrac(tree_obj, true_labels, final_labels):
    true_abunds = {}
    final_abunds = {}

    u = 0
    D = 0
    for n in tree_obj.traverse_postorder():
        if n.is_root():
            break
        if n.is_leaf():
            true_abunds[n.label] = False
            final_abunds[n.label] = False
            if n.label in true_labels:
                true_abunds[n.label] = True
            if n.label in final_labels:
                final_abunds[n.label] = True

        else:
            true_abunds[n.label] = False
            final_abunds[n.label] = False
            for c in n.child_nodes():
                if true_abunds[c.label]:
                    true_abunds[n.label] = True
                if final_abunds[c.label]:
                    final_abunds[n.label] = True

        if true_abunds[n.label] and not final_abunds[n.label]:
            u += n.edge_length
        if not true_abunds[n.label] and final_abunds[n.label]:
            u += n.edge_length
        D += n.edge_length
    return u / D


def distance_between(u, v):
    """Return the distance between nodes ``u`` and ``v`` in this ``Tree``
    Args:
    ``u`` (``Node``): Node ``u``
    ``v`` (``Node``): Node ``v``
    Returns:
    ``float``: The distance between nodes ``u`` and ``v``
    """
    # print(u.get_label(), v.get_label())
    if u == v:
        return 0.0
    elif u == v.parent:
        return v.edge_length
    elif v == u.parent:
        return u.edge_length
    u_dists = {u: 0.0}
    v_dists = {v: 0.0}
    c = u
    p = u.parent  # u traversal
    while p is not None:
        u_dists[p] = u_dists[c]
        if c.edge_length is not None:
            u_dists[p] += c.edge_length
        c = p
        p = p.parent
    if v in u_dists:
        return u_dists[v]
    c = v
    p = v.parent  # v traversal
    while p is not None:
        v_dists[p] = v_dists[c]
        if c.edge_length is not None:
            v_dists[p] += c.edge_length
        if p in u_dists:
            return u_dists[p] + v_dists[p]
        c = p
        p = p.parent


if __name__ == "__main__":
    with open(sys.argv[1], "r") as f:
        tree_string = f.read()
    tree_pp = ts.read_tree_newick(re.sub(r"{[0-9]+}", "", tree_string).strip())
    ic = 0
    for nd in tree_pp.traverse_postorder(leaves=True, internal=True):
        nd.set_label(f"N{ic}")
        ic += 1

    with open("all_samples.txt", "r") as f:
        samples = Counter(map(lambda x: x.strip(), f.readlines()))
    sample_profiles = defaultdict(lambda: defaultdict(float))
    for s in samples:
        with open(f"hmi_placements/{s}.pplist", "r") as f:
            pp_counts = Counter(map(lambda x: x.strip(), f.readlines()))
        total_count = sum(pp_counts.values())
        sample_profiles[s] = dict(
            map(lambda x: (f"N{x[0]}", x[1] / total_count), pp_counts.items())
        )

    for s in range(len(samples)):
        wunifrac_pairs[sys.argv[1]][s] = compute_weighted_unifrac(
            tree_pp, sample_profiles[sys.argv[1]], sample_profiles[s]
        )
    df_wunifrac = pd.DataFrame(wunifrac_pairs)
    df_wunifrac.to_csv(sep="\t")
