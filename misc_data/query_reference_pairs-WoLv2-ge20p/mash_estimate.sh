#!/bin/bash
# Mash version 2.3
mash sketch -s 100000 -k 29 $1
mash sketch -s 100000 -k 29 $2
mash dist $1.msh $2.msh
