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
ACCFLAGS = -Minfo -acc $(OPT)

all: build run verify

build: acc_f3a.f90
	$(FC) $(FCFLAGS) $(ACCFLAGS) -o acc_f3a.$(EXE) $<

run: acc_f3a.$(EXE)
	$(RUN) ./acc_f3a.$(EXE)

verify:


clean:
	@echo 'Cleaning up...'
	@rm -rf *.$(EXE) *.$(OBJ) *.mod *.dwf *.pdb prof
