	export OMP_NUM_THREADS=8

  timei=$(date +%s)
	./matrix_multiply_2
  timef=$(date +%s)

  runtime=$((timef-timei))

  echo " "
  echo " Tempo de execução: $runtime"
