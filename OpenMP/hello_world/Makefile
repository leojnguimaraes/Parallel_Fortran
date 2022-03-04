intel_oneAPI:
	ifort hello_world.f -o hello_world -liomp5 -lpthread -lm -qopenmp 

intel:
	/opt/intel/bin/ifort hello_world.f -o hello_world -Wl,-rpath,/opt/intel/lib/intel64 -L/opt/intel/lib/intel64 -liomp5 -lpthread -lm -qopenmp 

nvidia:
	pgfortran hello_world.f -o hello_world  -mp

gnu:
	gfortran hello_world.f -o hello_world -fopenmp 
