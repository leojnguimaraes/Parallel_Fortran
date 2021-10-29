      PROGRAM Soma
      USE OMP_LIB

      IMPLICIT NONE

      REAL*8 wtime, total_Sum, partial_Sum

      INTEGER :: i, j, nthreads, thread_id

      total_Sum = 0.0d0;

      wtime=omp_get_wtime()

      !$OMP PARALLEL PRIVATE(partial_Sum)

      partial_Sum = 0.0d0

      nthreads = omp_get_num_threads()

      DO i=1,100000,nthreads
        DO j=1,100000
          partial_Sum = partial_Sum + 1.0d0
        END DO
      END DO

      thread_id = omp_get_thread_num()
      PRINT *, 'Sum of process ', thread_id, ' = ', partial_Sum

      !$OMP CRITICAL
      total_Sum=total_Sum+partial_Sum
      !$OMP END CRITICAL

      !$OMP END PARALLEL

      wtime=omp_get_wtime()-wtime

      PRINT *, 'Elapsed time:',wtime
      PRINT *, 'Total Sum:', total_Sum
      PRINT *, 'Number of threads', nthreads

      END
