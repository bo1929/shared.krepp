#!/usr/bin/env python3
import fileinput
import re


def main():
    for line in fileinput.input():
        if line.startswith("@"):
            continue
        fl = 0
        cadict = {}
        ls = line.strip().split("\t")
        matches = re.findall(r"(\d+)([^0-9]{1})", ls[5].strip())
        for m in matches:
            l = int(m[0])
            t = m[1]
            cadict[t] = cadict.get(t, 0) + l
            if t in ["=", "M", "X"]:
                fl += l
        if fl:
            print(f"{ls[0]}\t{1-cadict.get('=', 0)/fl}\t{fl}")
        else:
            print(f"{ls[0]}\t{1}\t{fl}")


if __name__ == "__main__":
    main()
