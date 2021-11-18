all: build run

build:
	gfortran -o test_NodeND -I /mnt/d/fmetis/build/include -L /mnt/d/fmetis/build/lib test_NodeND.f90 -lfmetis -lmetis

run:
	./test_NodeND
