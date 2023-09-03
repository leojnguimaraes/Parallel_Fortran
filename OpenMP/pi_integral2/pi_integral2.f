      PROGRAM PI_Integral
      USE OMP_LIB

      IMPLICIT NONE

      DOUBLE PRECISION ::
     .wtime, partial_Sum, total_Sum, x, step, pi, pi_approx 
      
      DOUBLE PRECISION, ALLOCATABLE :: dc_step(:)

      INTEGER :: i, nstep, nthreads, thread_id, block_size, check

      pi = 4.0d0*datan(1.0d0)

      nstep = 1000000000
      step = 1.0d0/dfloat(nstep)

***** NÃO PARALELIZADO:

      total_Sum=0.0d0

      wtime=omp_get_wtime()

c     início do bloco + step/2:
      x = 0.5d0*step

      DO i=1,nstep
        total_Sum = total_Sum + 4.0d0/(1.0d0+x*x)
        x = x + step
      ENDDO

      pi_approx = step * total_Sum

      wtime=omp_get_wtime()-wtime

      PRINT *
      PRINT *, 'NÃO PARALELO:'
      PRINT *, 'Elapsed time:', wtime
      PRINT *, 'Integral PI approx:', pi_approx
      PRINT *, 'Exact value of PI: ', pi
      PRINT *, 'Error (%)', 100.0d0*dabs(pi_approx-pi)/pi

***** PARALELIZADO POR OMP:

      total_Sum = 0.0d0

      wtime=omp_get_wtime()

      !$OMP PARALLEL DEFAULT(NONE)
     .               PRIVATE(i, block_size, x, thread_id, partial_Sum)
     .               SHARED(nthreads, total_Sum, step, nstep)

      !$OMP SINGLE
      nthreads = omp_get_num_threads() ! igual para todas threads
      !$OMP END SINGLE

      thread_id = omp_get_thread_num() ! um para cada thread

      partial_Sum = 0.0d0

      block_size = nstep/nthreads ! cuidado: esta divisão é truncada

c     início do bloco + step/2:
      x = dfloat(thread_id*block_size)*step + 0.5d0*step

c     corrige tamanho do bloco para última thread:
      IF (thread_id.eq.nthreads-1) ! last thread
     .block_size = nstep-block_size*(nthreads-1)

      DO i=1,block_size
        partial_Sum = partial_Sum + 4.0d0/(1.0d0+x*x)
        x = x + step
      END DO

      !$OMP CRITICAL
      total_Sum = total_Sum + partial_Sum
      !$OMP END CRITICAL

      !$OMP END PARALLEL

      pi_approx = step * total_Sum

      wtime=omp_get_wtime()-wtime

      PRINT *
      PRINT *, 'PARALELO VIA OMP:'
      PRINT *, 'Elapsed time:', wtime
      PRINT *, 'Number of threads', nthreads 
      PRINT *, 'Integral PI approx:', pi_approx
      PRINT *, 'Exact value of PI: ', pi
      PRINT *, 'Error (%)', 100.0d0*dabs(pi_approx-pi)/pi

***** PARALELIZADO POR DO CONCURRENT:

c     OBS: para este problema em particular não compensou

      ALLOCATE(dc_step(nstep),source=0.0d0,stat=check)

      wtime=omp_get_wtime()

      DO CONCURRENT (i=1:nstep) LOCAL (x)  SHARED (dc_step,step)
c       fazer passos independentes (orden de execução não influi):
        x=dfloat(i-1)*step + 0.5d0*step
        dc_step(i) = 4.0d0/(1.0d0+x*x)
      ENDDO

      pi_approx = step * SUM(dc_step(:))

      wtime=omp_get_wtime()-wtime

      PRINT *
      PRINT *, 'PARALELO VIA DO CONCURRENT:'
      PRINT *, 'Elapsed time:', wtime
      PRINT *, 'Number of threads', nthreads 
      PRINT *, 'Integral PI approx:', pi_approx
      PRINT *, 'Exact value of PI: ', pi
      PRINT *, 'Error (%)', 100.0d0*dabs(pi_approx-pi)/pi

      END PROGRAM
