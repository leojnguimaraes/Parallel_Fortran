intel_oneAPI:
	ifort pi_approx_coarray.f -o pi_approx_coarray -coarray -lintlc 
	
intel:
	/opt/intel/bin/ifort pi_approx_coarray.f -o pi_approx_coarray -coarray -lintlc -Wl,-rpath,/opt/intel/lib/intel64 -L/opt/intel/lib/intel64 
	
