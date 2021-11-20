intel:
	/opt/intel/bin/ifort pi_mc.f -o pi_mc -Wl,-rpath,/opt/intel/lib/intel64 -L/opt/intel/lib/intel64 -liomp5 -lpthread -lm -qopenmp 

gnu:
	gfortran -g -fbacktrace -Wall pi_mc.f -o pi_mc -fopenmp 

