#!/bin/bash

O_DPATH=../../funwave-work
O_ENAME=funwave-central

N_DPATH=.
N_ENAME=exe_funwave

OUT_DPATH=./outputs

N_PROCS=4
O_INPUT=./inputs/beach_2d.txt
N_INPUT=./inputs/beach_2d.txt

O_EPATH=$O_DPATH/$O_ENAME
N_EPATH=$N_DPATH/$N_ENAME

#./exec_mpi.sh $N_PROCS $O_EPATH $O_INPUT $OUT_DPATH/old
./exec_mpi.sh $N_PROCS $N_EPATH $N_INPUT $OUT_DPATH/new
