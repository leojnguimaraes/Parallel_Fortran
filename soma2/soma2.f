      PROGRAM Soma
      USE OMP_LIB

      IMPLICIT NONE

      REAL*8 wtime, total_Sum 

      INTEGER :: i, j, nthreads

      total_Sum = 0.0d0

      wtime=omp_get_wtime()

      !$OMP PARALLEL 

      nthreads = omp_get_num_threads()

      !$OMP DO REDUCTION(+:total_Sum) SCHEDULE(DYNAMIC)
              DO i=1,100000
                DO j=1,100000
                  total_Sum = total_Sum + 1.0d0
                END DO
              END DO
      !$OMP END DO

      !$OMP END PARALLEL

      wtime=omp_get_wtime()-wtime

      PRINT *, 'Elapsed time:', wtime
      PRINT *, 'Total Sum:', total_Sum
      PRINT *, 'Number of threads', nthreads 

      END
