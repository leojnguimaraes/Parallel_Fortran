FC       = /opt/nvidia/hpc_sdk/Linux_x86_64/23.7/compilers/bin/nvfortran
OBJ=o
EXE=out
RUN=
UNAME := $(shell uname -a)
ifeq ($(findstring CYGWIN_NT, $(UNAME)), CYGWIN_NT)
   OBJ=obj
   EXE=exe
endif

FC       = /opt/nvidia/hpc_sdk/Linux_x86_64/23.7/compilers/bin/nvfortran
ACCFLAGS = -Minfo -acc $(OPT)

all: build run verify

build: soma_acc.f90
	$(FC) $(FCFLAGS) $(ACCFLAGS) -o soma_acc.$(EXE) $<

run: soma_acc.$(EXE)
	$(RUN) ./soma_acc.$(EXE)

verify:


clean:
	@echo 'Cleaning up...'
	@rm -rf *.$(EXE) *.$(OBJ) *.dwf *.pdb prof
