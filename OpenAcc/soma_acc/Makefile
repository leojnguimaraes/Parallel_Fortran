FC       = nvfortran
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

build: soma_acc.f
	$(FC) $(FCFLAGS) $(ACCFLAGS) -o soma_acc.$(EXE) $<

run: soma_acc.$(EXE)
	$(RUN) ./soma_acc.$(EXE)

verify:


clean:
	@echo 'Cleaning up...'
	@rm -rf *.$(EXE) *.$(OBJ) *.dwf *.pdb prof
