LIBS_MKL=\
	-Wl,-rpath,/opt/intel/mkl/lib/intel64 -L/opt/intel/mkl/lib/intel64\
	-Wl,-rpath,/opt/intel/lib/intel64 -L/opt/intel/lib/intel64\
	-I/opt/intel/mkl/include\
	-Wl,--start-group /opt/intel/mkl/lib/intel64/libmkl_intel_lp64.a\
	/opt/intel/mkl/lib/intel64/libmkl_intel_thread.a\
	/opt/intel/mkl/lib/intel64/libmkl_core.a -Wl,--end-group -liomp5 -lpthread -lm -mkl 

intel:
	/opt/intel/bin/ifort matrix_multiply.f -o matrix_multiply -qopenmp $(LIBS_MKL)
