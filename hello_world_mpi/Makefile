# Install MPI on Ubuntu:
# sudo apt-get install openmpi-bin openmpi-common openssh-client openssh-server libopenmpi2 libopenmpi-dev

all: gnu run

gnu: 
	mpif90 hello_world_mpi.f -o hello_world_mpi

run:
	mpirun -np 4 hello_world_mpi
