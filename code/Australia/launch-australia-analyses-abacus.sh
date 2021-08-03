#!/bin/bash

typeset -i d MAX
let MAX=8
for((d=MAX;d>=1;--d)); do
  qsub -t 1:250 ./qsub-australia-open.sh $d;
  qsub -t 1:250 ./qsub-australia-shade.sh $d;
done

