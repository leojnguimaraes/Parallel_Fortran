all: build run

build:
	gfortran -o test_PartMeshNodal2 -I /mnt/d/fmetis/build/include -L /mnt/d/fmetis/build/lib test_PartMeshNodal2.f90 -lfmetis -lmetis

run:
	./test_PartMeshNodal2
