intel_oneAPI:
	ifort pi_integral2.f -o pi_integral2 -liomp5 -lpthread -lm -qopenmp 

intel:
	/opt/intel/bin/ifort pi_integral2.f -o pi_integral2 -Wl,-rpath,/opt/intel/lib/intel64 -L/opt/intel/lib/intel64 -liomp5 -lpthread -lm -qopenmp 

gnu:
	gfortran -g -fbacktrace -Wall pi_integral2.f -o pi_integral2 -fopenmp 

