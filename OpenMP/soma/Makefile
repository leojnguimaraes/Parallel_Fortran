intel_oneAPI:
	ifort soma.f -o soma -liomp5 -lpthread -lm -qopenmp 

intel:
	/opt/intel/bin/ifort soma.f -o soma -Wl,-rpath,/opt/intel/lib/intel64 -L/opt/intel/lib/intel64 -liomp5 -lpthread -lm -qopenmp 

