#!/usr/bin/env python3
import pysam
import sys


def main():
    qid_to_rid = {}
    qid_to_dist = {}
    with open(sys.argv[2], "r") as f:
        for line in f.readlines():
            qid, rid, dist = line.strip().split()
            qid_to_rid[qid] = rid
            qid_to_dist[qid] = dist

    samfile = pysam.AlignmentFile(sys.argv[1], "rb")
    for q in samfile.fetch():
        if q.has_tag("XM"):
            print(
                f"{sys.argv[3]}\t{q.query_name}\t{q.reference_name}\t{q.get_tag('XM')/q.query_length}\t{qid_to_dist[sys.argv[3]]}"
            )
        else:
            print(
                f"{sys.argv[3]}\t{q.query_name}\t{q.reference_name}\tNaN\t{qid_to_dist[sys.argv[3]]}"
            )


if __name__ == "__main__":
    main()
