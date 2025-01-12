#!/usr/bin/env python
# coding: utf-8
import sys
import json
import treeswift

"""
sys.argv[1]: path to the jplace file.
sys.argv[2]: name of the query genome in the ground truth.
sys.argv[3]: path to the Newick tree with the query.
sys.argv[4]: mapping of read ids to query ids.
"""

NaN = float("nan")


def distance_between(u, v):
    """Return the distance between nodes ``u`` and ``v`` in this ``Tree``

    Args:
    ``u`` (``Node``): Node ``u``

    ``v`` (``Node``): Node ``v``

    Returns:
    ``float``: The distance between nodes ``u`` and ``v``
    """
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


def eval_jplace():
    with open(sys.argv[1]) as f:
        jd = json.load(f)
    gt_dict = {}
    try:
        with open(sys.argv[4]) as f:
            for line in f.readlines():
                gid, rid = line.strip().split()
                gt_dict[rid] = gid
    except IndexError:
        pass

    qtree_str = jd["tree"]
    rtree = treeswift.read_tree_newick(sys.argv[3])
    qnd_r = rtree.find_node(sys.argv[2], leaves=True, internal=True)
    pnd_r = qnd_r.get_parent()
    qtree = rtree.extract_tree_without(sys.argv[2], suppress_unifurcations=False)
    lbl_to_nd = qtree.label_to_node("all")
    pnd_q = lbl_to_nd[pnd_r.get_label()].child_nodes()[0]
    qtree.suppress_unifurcations()
    for nd in qtree.traverse_postorder():
        nd.set_edge_length(1)

    for placement in jd["placements"]:
        rid = placement["n"][0]
        # rid = placement["nm"][0][0]
        if gt_dict.get(rid, sys.argv[2]) != sys.argv[2] or rid == "NaN":
            continue
        if len(placement["p"]) == 0:
            print(
                f"{sys.argv[2]}\t{rid}\tNaN\tNaN\tNaN\tNaN"
            )
            continue
        ixe = qtree_str.find("{" + f"{placement['p'][0][0]}" + "}")
        ixs = max(
            (
                (qtree_str[:ixe]).rfind(","),
                (qtree_str[:ixe]).rfind("("),
                (qtree_str[:ixe]).rfind(")"),
            )
        )
        lbl_placement, blen = qtree_str[ixs + 1 : ixe].split(":")
        if (lbl_placement == sys.argv[2] or lbl_placement == pnd_r.get_label()):
            edge_error = 0
        else:
            edge_error = distance_between(pnd_q, lbl_to_nd[lbl_placement])
        # edge_error = distance_between(pnd_q, lbl_to_nd[lbl_placement])
        print(
            f"{sys.argv[2]}\t{rid}\t{lbl_placement}\t{placement['p'][0][1]}\t{placement['p'][0][2]}\t{edge_error}"
        )


if __name__ == "__main__":
    eval_jplace()
