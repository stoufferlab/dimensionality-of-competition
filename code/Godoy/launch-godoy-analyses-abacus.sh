#!/bin/bash

typeset -i d MAX
let MAX=10
for((d=10;d>=1;--d)); do
  qsub -t 1:100 ./qsub-hampa-C.sh $d;
  qsub -t 1:100 ./qsub-hampa-T.sh $d;
done
