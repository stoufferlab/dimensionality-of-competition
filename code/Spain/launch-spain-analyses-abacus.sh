#!/bin/bash

typeset -i d MAX
let MAX=10
for((d=10;d>=1;--d)); do
  qsub -t 1:50 ./qsub-spain-control.sh $d;
  qsub -t 1:50 ./qsub-spain-treatment.sh $d;
done
