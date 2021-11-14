intel:
	/opt/intel/bin/ifort hello_world_coarray.f -o hello_world_coarray -coarray -lintlc -Wl,-rpath,/opt/intel/lib/intel64 -L/opt/intel/lib/intel64 

gnu:
	mpif90 hello_world_coarray.f -o hello_world_coarray -lcaf_mpi
