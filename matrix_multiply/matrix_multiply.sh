	export OMP_NUM_THREADS=1

  timei=$(date +%s)
	./matrix_multiply
  timef=$(date +%s)

  runtime=$((timef-timei))

  echo " "
  echo " Tempo de execução: $runtime"
