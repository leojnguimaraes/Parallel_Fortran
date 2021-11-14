intel:
	/opt/intel/bin/ifort pi_integral.f -o pi_integral -Wl,-rpath,/opt/intel/lib/intel64 -L/opt/intel/lib/intel64 -liomp5 -lpthread -lm -qopenmp 

gnu:
	gfortran pi_integral.f -o pi_integral -fopenmp 

