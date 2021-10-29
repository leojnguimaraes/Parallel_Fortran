      PROGRAM Soma
      USE OMP_LIB

      IMPLICIT NONE

      REAL*8 wtime, total_Sum, partial_Sum, Sum

      INTEGER :: nthreads, thread_id, block_size

      Sum = 100000000.0d0   ! incrementar até chegar nesse número

      total_Sum = 0.0d0

      wtime=omp_get_wtime()

      !$OMP PARALLEL PRIVATE(partial_Sum, thread_id, block_size)

      partial_Sum = 0.0d0

      nthreads = omp_get_num_threads() ! igual para todas threads
      thread_id = omp_get_thread_num() ! um para cada thread

      block_size = int(Sum/dfloat(nthreads))
      IF (thread_id.eq.0) block_size = int(Sum)-block_size*(nthreads-1)

      DO WHILE (1.EQ.1)
      IF (int(partial_Sum).ge.block_size) EXIT
        partial_Sum = partial_Sum + 1.0d0
      END DO

      PRINT *, 'Sum of process ', thread_id, ' = ', partial_Sum

      !$OMP CRITICAL
      total_Sum=total_Sum+partial_Sum
      !$OMP END CRITICAL

      !$OMP END PARALLEL

      wtime=omp_get_wtime()-wtime

      PRINT *, 'Elapsed time:',wtime
      PRINT *, 'Total Sum:', total_Sum
      PRINT *, 'Number of threads', nthreads ! sai do loop paralelo com
                                             ! valor da thread mais "lenta"
      END
