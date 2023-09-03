intel_oneAPI:
	ifort pi_integral2.f -o pi_integral2 -liomp5 -lpthread -lm -qopenmp 

nvidia:
	/opt/nvidia/hpc_sdk/Linux_x86_64/23.7/compilers/bin/nvfortran pi_integral2.f -o pi_integral2 -mp 

intel:
	/opt/intel/bin/ifort pi_integral2.f -o pi_integral2 -Wl,-rpath,/opt/intel/lib/intel64 -L/opt/intel/lib/intel64 -liomp5 -lpthread -lm -qopenmp 

gnu:
	gfortran -g -fbacktrace -Wall pi_integral2.f -o pi_integral2 -fopenmp 

