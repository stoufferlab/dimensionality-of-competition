#!/bin/bash

typeset -i r NRAND d MAX

# there are 25 random data sets and 7 possible dimensionalities
let NRAND=25
let MAX=7

# go through all random data sets r for all dimensions d
for((r=NRAND;r>=1;--r)); do
  for((d=MAX;d>=1;--d)); do
  		qsub -t 1:250 ./qsub-fake-goldberg.sh $r $d;
	done
done

