intel_oneAPI:
	ifort soma3.f -o soma3 -liomp5 -lpthread -lm -qopenmp 

intel:
	/opt/intel/bin/ifort soma3.f -o soma3 -Wl,-rpath,/opt/intel/lib/intel64 -L/opt/intel/lib/intel64 -liomp5 -lpthread -lm -qopenmp 

gnu:
	gfortran -g -fbacktrace -Wall soma3.f -o soma3 -fopenmp 

