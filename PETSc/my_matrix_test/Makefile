all: make run

make:
	/mnt/d/petsc/arch-linux-c-debug/bin/mpif90 -g -O0 -fbacktrace -Wall -ffree-line-length-0 -Wno-unused-dummy-argument -ffree-line-length-0 -Wno-unused-dummy-argument -g -O0   -I/mnt/d/petsc/include -I/mnt/d/petsc/arch-linux-c-debug/include     matrix_test.F90  -Wl,-rpath,/mnt/d/petsc/arch-linux-c-debug/lib -L/mnt/d/petsc/arch-linux-c-debug/lib -Wl,-rpath,/mnt/d/petsc/arch-linux-c-debug/lib -L/mnt/d/petsc/arch-linux-c-debug/lib -Wl,-rpath,/usr/lib/gcc/x86_64-linux-gnu/7 -L/usr/lib/gcc/x86_64-linux-gnu/7 -lpetsc -lflapack -lfblas -lpthread -lm -lX11 -lstdc++ -ldl -lmpifort -lmpi -lgfortran -lm -lgfortran -lm -lgcc_s -lquadmath -lstdc++ -ldl -o matrix_test

run:
	${PETSC_DIR}/arch-linux-c-debug/bin/mpirun -np 3 ./matrix_test
