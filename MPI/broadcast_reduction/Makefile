# Install MPI on Ubuntu:
# sudo apt-get install openmpi-bin openmpi-common openssh-client openssh-server libopenmpi2 libopenmpi-dev

all: gnu run_gnu

gnu: 
	${PETSC_DIR}/arch-linux-c-debug/bin/mpif90 -g -O0 -fbacktrace -Wall bcast_reduction.f -o bcast_reduction

run_gnu:
	${PETSC_DIR}/arch-linux-c-debug/bin/mpirun -np 4 ./bcast_reduction
