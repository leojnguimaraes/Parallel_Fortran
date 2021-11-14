	export OMP_NUM_THREADS=8
  export OMP_WAIT_POLICY=active # encourages idle threads to spin rather than sleep
  export OMP_DYNAMIC=false      # do not let the runtime deliver fewer threads to than you asked for
  export OMP_PROC_BIND=true     # prevents threads migrating between cores
	./pi_integral

