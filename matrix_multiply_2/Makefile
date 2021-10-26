all:
	/opt/intel/bin/ifort matrix_multiply_2.f -o matrix_multiply_2 -Wl,-rpath,/opt/intel/lib/intel64 -L/opt/intel/lib/intel64 -liomp5 -lpthread -lm -qopenmp 

gnu:
	gfortran matrix_multiply_2.f -o matrix_multiply_2 -fopenmp

