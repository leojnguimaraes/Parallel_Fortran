      PROGRAM PI_Integral
      USE OMP_LIB

      IMPLICIT NONE

      REAL*8 wtime, partial_Sum, total_Sum, x, step, pi, pi_approx 

      INTEGER :: i, nstep, nthreads

      nstep = 1000000000

      total_Sum = 0.0d0

      step = 1.0d0/dfloat(nstep)

      wtime=omp_get_wtime()

      !$OMP PARALLEL PRIVATE(x, partial_Sum)

      nthreads = omp_get_num_threads()

      partial_Sum = 0.0d0

      DO i=1,nstep,nthreads
        x = (dfloat(i)+0.5d0)*step
        partial_Sum = partial_Sum + 4.0d0/(1.0d0+x*x)
      END DO

      !$OMP CRITICAL
      total_Sum = total_Sum + partial_Sum
      !$OMP END CRITICAL

      !$OMP END PARALLEL

      pi = 4.0d0*datan(1.0d0)

      pi_approx = step * total_Sum

      wtime=omp_get_wtime()-wtime

      PRINT *, 'Elapsed time:', wtime
      PRINT *, 'Number of threads', nthreads 
      PRINT *, 'Integral PI approx:', pi_approx
      PRINT *, 'Error (%)', 100.0d0*dabs(pi_approx-pi)/pi

      END PROGRAM
