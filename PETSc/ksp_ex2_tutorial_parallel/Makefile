all: make run1 run2

make:
	/mnt/d/petsc/arch-linux-c-debug/bin/mpif90 -Wall -ffree-line-length-0 -Wno-unused-dummy-argument -g -O0  -Wall -ffree-line-length-0 -Wno-unused-dummy-argument -g -O0   -I/mnt/d/petsc/include -I/mnt/d/petsc/arch-linux-c-debug/include     ex2f.F90  -Wl,-rpath,/mnt/d/petsc/arch-linux-c-debug/lib -L/mnt/d/petsc/arch-linux-c-debug/lib -Wl,-rpath,/mnt/d/petsc/arch-linux-c-debug/lib -L/mnt/d/petsc/arch-linux-c-debug/lib -Wl,-rpath,/usr/lib/gcc/x86_64-linux-gnu/7 -L/usr/lib/gcc/x86_64-linux-gnu/7 -lpetsc -lflapack -lfblas -lpthread -lm -lX11 -lstdc++ -ldl -lmpifort -lmpi -lgfortran -lm -lgfortran -lm -lgcc_s -lquadmath -lstdc++ -ldl -o ex2f

run1:
	${PETSC_DIR}/arch-linux-c-debug/bin/mpirun -np 3 ./ex2f -pc_type jacobi -ksp_monitor_short -ksp_gmres_cgs_refinement_type refine_always

run2:
	${PETSC_DIR}/arch-linux-c-debug/bin/mpirun -np 3 ./ex2f -pc_type jacobi -my_ksp_monitor -ksp_gmres_cgs_refinement_type refine_always
