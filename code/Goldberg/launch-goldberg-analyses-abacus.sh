#!/bin/bash

typeset -i d MAX
let MAX=7
for((d=MAX;d>=1;--d)); do
  qsub -t 1:250 ./qsub-goldberg.sh $d;
done

