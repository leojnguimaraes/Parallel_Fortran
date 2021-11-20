# Install MPI on Ubuntu:
# sudo apt-get install openmpi-bin openmpi-common openssh-client openssh-server libopenmpi2 libopenmpi-dev

all: gnu run_gnu

gnu: 
	${PETSC_DIR}/arch-linux-c-debug/bin/mpif90 -g -fbacktrace -Wall hello_world_mpi.f -o hello_world_mpi

run_gnu:
	${PETSC_DIR}/arch-linux-c-debug/bin/mpirun -np 4 ./hello_world_mpi
