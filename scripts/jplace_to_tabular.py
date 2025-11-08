import re
import argparse
import pathlib
import json
import treeswift as ts
from tqdm import tqdm


def make_newick(tree_str: str) -> str:
    subs_tree = re.sub("\{(\d+)\}", "", tree_str)
    return subs_tree


def map_branches(data) -> dict:
    """
    Build dictionary with edge/branch numbers as keys and
    reference tree leaves as values
    """

    def get_id(s):
        return int(re.search("\{(\d+)\}", s).group(1))

    extnwk_str = data["tree"]
    tree = ts.read_tree_newick(make_newick(extnwk_str))
    lbl_to_nd = tree.label_to_node(selection="all")
    branch_to_lbl = {
        get_id(extnwk_str[extnwk_str.find(lbl) :]): lbl for lbl, nd in lbl_to_nd.items()
    }
    return tree, branch_to_lbl


def main(args):
    with open(args.input_file, "r") as file:
        data = json.load(file)
    tree, branch_to_lbl = map_branches(data)
    ix = data["fields"].index("edge_num")

    with open(args.output_file, "w") as file:
        for placement in tqdm(data["placements"]):
            rid = placement["n"][0]
            nodes_l = []
            for p in placement["p"]:
                plbl = branch_to_lbl.get(p[ix], None)
                if plbl is None:
                    continue
                nodes_l.append(plbl)
            if nodes_l:
                file.write(f"{rid}\t{','.join(nodes_l)}\n")


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-i", "--input-file", type=pathlib.Path, required=True)
    parser.add_argument("-o", "--output-file", type=pathlib.Path, required=True)
    args = parser.parse_args()
    main(args)
