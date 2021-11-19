all: clean_all build clean

build:
	gfortran -o ./metis_partition.exe -I /mnt/d/fmetis/build/include -L /mnt/d/fmetis/build/lib ./module.F90 ./main.F90 ./input.F90 ./output.F90 ./system.F90 -lfmetis -lmetis

clean_all:
	rm -rf *.mod *.o *.exe

clean:
	rm -rf *.mod *.o
