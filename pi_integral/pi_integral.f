      PROGRAM PI_Integral
      USE OMP_LIB

      IMPLICIT NONE

      REAL*8 wtime, total_Sum, x, step, pi, pi_approx 

      INTEGER :: i, nstep, nthreads

      nstep = 1000000000

      total_Sum = 0.0d0

      step = 1.0d0/dfloat(nstep)

      wtime=omp_get_wtime()

      !$OMP PARALLEL 

      nthreads = omp_get_num_threads()

      !$OMP DO REDUCTION(+:total_Sum) PRIVATE(x)
              DO i=1,nstep
                x = (dfloat(i-1)+0.5d0)*step
                total_Sum = total_Sum + 4.0d0/(1.0d0+x*x)
              END DO
      !$OMP END DO

      !$OMP END PARALLEL

      pi = 4.0d0*datan(1.0d0)

      pi_approx = step * total_Sum

      wtime=omp_get_wtime()-wtime

      PRINT *, 'Elapsed time:', wtime
      PRINT *, 'Number of threads', nthreads 
      PRINT *, 'Integral PI approx:', pi_approx
      PRINT *, 'Exact value of PI: ', pi
      PRINT *, 'Error (%)', 100.0d0*dabs(pi_approx-pi)/pi

      END PROGRAM
