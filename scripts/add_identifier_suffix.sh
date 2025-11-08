#!/bin/bash
awk -v suffix=$2 '{ if (NR%4==1) {print $0"-"suffix;} else {print;}}' $1 > ${1%.fq}-as.fq
