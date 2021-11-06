# Install MPI on Ubuntu:
# sudo apt-get install openmpi-bin openmpi-common openssh-client openssh-server libopenmpi2 libopenmpi-dev

all: gnu run_gnu

gnu: 
	mpif90 hello_world_mpi.f -o hello_world_mpi

run_gnu:
	mpirun -np 4 hello_world_mpi
