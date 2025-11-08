#!/usr/bin/env python3
import sys
from treeswift import read_tree_newick


def main():
    labels = sys.argv[2:]
    tree = read_tree_newick(sys.argv[1])
    nodes = tree.label_to_node(selection=set(labels))
    for nd in nodes.values():
        parent = nd
        parent_labels = parent.get_label()
        while not parent.is_root():
            parent = parent.get_parent()
            parent_labels = parent.get_label() + "\t" + parent_labels
        print(parent_labels)


if __name__ == "__main__":
    main()
