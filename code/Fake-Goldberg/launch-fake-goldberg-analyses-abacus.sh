#!/bin/bash

typeset -i r NRAND d MAX
let NRAND=25
let MAX=7
for((r=MAX;r>=1;--r)); do
	for((d=MAX;d>=1;--d)); do
  		qsub -t 1:250 ./qsub-fake-goldberg.sh $r $d;
	done
done

