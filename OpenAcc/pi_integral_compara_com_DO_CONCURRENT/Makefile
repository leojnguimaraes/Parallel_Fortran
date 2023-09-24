################################################################################
#
# Copyright (c) 2017, NVIDIA Corporation.  All rights reserved.
#
# Please refer to the NVIDIA end user license agreement (EULA) associated
# with this source code for terms and conditions that govern your use of
# this software. Any use, reproduction, disclosure, or distribution of
# this software and related documentation outside the terms of the EULA
# is strictly prohibited.
#
################################################################################

FC       = /opt/nvidia/hpc_sdk/Linux_x86_64/23.7/compilers/bin/nvfortran
OBJ=o
EXE=out
RUN=
UNAME := $(shell uname -a)
ifeq ($(findstring CYGWIN_NT, $(UNAME)), CYGWIN_NT)
   OBJ=obj
   EXE=exe
endif

FCFLAGS  = -fast
#ACCFLAGS = -Minfo -acc=gpu -gpu=cc75,cuda11.0 -stdpar=gpu $(OPT)  # -stdpar[=gpu] - Enable parallelization of DO CONCURRENT to host or GPU
ACCFLAGS = -Minfo -acc=multicore -stdpar=multicore $(OPT)  # -stdpar[=gpu] - Enable parallelization of DO CONCURRENT to host or CPU
# https://stackoverflow.com/questions/71641704/recommendation-for-nvfortran-compiler-useful-flags
# https://www.youtube.com/watch?v=Pq34HYI4Lsc

all: build run verify

build: pi_integral2.f90
	$(FC) $(FCFLAGS) $(ACCFLAGS) -o pi_integral2_nvidia.$(EXE) $<

run: pi_integral2_nvidia.$(EXE)
	$(RUN) ./pi_integral2_nvidia.$(EXE)

verify:


clean:
	@echo 'Cleaning up...'
	@rm -rf *.$(EXE) *.$(OBJ) *.mod *.dwf *.pdb prof

intel_oneAPI:
	ifx pi_integral2.f90 -O3 -o pi_integral2_intel.$(EXE) -liomp5 -lpthread -lm -qopenmp 

