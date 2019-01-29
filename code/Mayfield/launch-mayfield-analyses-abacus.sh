#!/bin/bash

typeset -i d MAX
let MAX=8
for((d=MAX;d>=1;--d)); do
  qsub -t 1:100 ./qsub-mayfield-open.sh $d;
  qsub -t 1:100 ./qsub-mayfield-shade.sh $d;
done

