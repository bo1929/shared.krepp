#!/usr/bin/env python3
import sys
from treeswift import read_tree_newick


def main():
    err = 0
    labels = sys.argv[2:]
    tree = read_tree_newick(sys.argv[1])
    nodes = tree.label_to_node(selection=set(labels))
    lca = tree.mrca(nodes)
    for nd in nodes.values():
        parent = nd
        while parent != lca:
            err += 1
            parent = parent.get_parent()
    print(err - 1)


if __name__ == "__main__":
    main()
