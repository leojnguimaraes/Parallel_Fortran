LIBS_MKL=\
	-Wl,-rpath,/opt/intel/mkl/lib/intel64 -L/opt/intel/mkl/lib/intel64\
	-Wl,-rpath,/opt/intel/lib/intel64 -L/opt/intel/lib/intel64\
	-I/opt/intel/mkl/include\
	-Wl,--start-group /opt/intel/mkl/lib/intel64/libmkl_intel_lp64.a\
	/opt/intel/mkl/lib/intel64/libmkl_intel_thread.a\
	/opt/intel/mkl/lib/intel64/libmkl_core.a -Wl,--end-group -liomp5 -lpthread -lm -mkl 

intel_oneAPI:
	ifort inverse.f -o inverse  -coarray -lintlc -liomp5 -lpthread -lm -qmkl 

intel:
	/opt/intel/bin/ifort inverse.f -o inverse  $(LIBS_MKL) -coarray -lintlc -Wl,-rpath,/opt/intel/lib/intel64 -L/opt/intel/lib/intel64 
