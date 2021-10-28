	export OMP_NUM_THREADS=8
  export MKL_NUM_THREADS=8

  timei=$(date +%s)
	./matrix_multiply
  timef=$(date +%s)

  runtime=$((timef-timei))

  echo " "
  echo " Tempo total de execução: $runtime"
