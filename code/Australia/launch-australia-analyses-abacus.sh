#!/bin/bash

typeset -i d MAX
let MAX=8
for((d=MAX;d>=1;--d)); do
  qsub -t 1:50 ./qsub-australia-open.sh $d;
  qsub -t 1:50 ./qsub-australia-shade.sh $d;
done

