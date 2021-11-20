intel:
	/opt/intel/bin/ifort soma2.f -o soma2 -Wl,-rpath,/opt/intel/lib/intel64 -L/opt/intel/lib/intel64 -liomp5 -lpthread -lm -qopenmp 

gnu:
	gfortran -g -fbacktrace -Wall soma2.f -o soma2 -fopenmp 

