all: build run

build:
	gfortran -g -fbacktrace -Wall -o my_test -I /mnt/d/fmetis/build/include -L /mnt/d/fmetis/build/lib my_test.f90 -lfmetis -lmetis

run:
	./my_test
