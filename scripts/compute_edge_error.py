#!/usr/bin/env python3
import sys
from treeswift import read_tree_newick


def main():
    node = sys.argv[2]
    filepath = sys.argv[3]
    tree = read_tree_newick(sys.argv[1])
    error_dict = {}
    with open(filepath, "r") as fileinput:
        for line in fileinput.readlines():
            err = 0
            vals = line.strip().split("\t")
            identifier = vals[0]
            placement = vals[1]
            if placement == "UP":
                err = -1
            else:
                err = error_dict.get(placement, -1)
                if err == -1:
                    nodes = tree.label_to_node(selection={node, placement})
                    lca = tree.mrca(nodes)
                    for nd in nodes.values():
                        parent = nd
                        while parent != lca:
                            err += 1
                            parent = parent.get_parent()
                    error_dict[placement] = err
            print(f"{identifier}\t{placement}\t{err}")


if __name__ == "__main__":
    main()
