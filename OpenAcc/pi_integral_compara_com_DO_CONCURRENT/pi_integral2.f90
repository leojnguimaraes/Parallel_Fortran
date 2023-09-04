      PROGRAM PI_Integral

!      USE OMP_LIB  ! compilador intel
      USE OPENACC  ! compilador nvidia

      IMPLICIT NONE

      DOUBLE PRECISION :: x, step, pi, pi_approx 
      
      DOUBLE PRECISION, ALLOCATABLE :: dc_step(:,:)

      INTEGER :: wtime, c1, c2, i, j, nstep, check

      pi = 4.0d0*datan(1.0d0)

      nstep = 10000
      step = 1.0d0/dfloat(nstep)

      ALLOCATE(dc_step(nstep,nstep),source=0.0d0,stat=check)

!**** NÃO PARALELIZADO:

      call system_clock( count=c1 )

      DO i=1,nstep
        DO j=1,nstep
          x=dfloat(i-1)*step + 0.5d0*step
          dc_step(i,j) = 4.0d0/(1.0d0+x*x)
        ENDDO
      ENDDO

      call system_clock( count=c2 )
      wtime=c2-c1

      pi_approx = step * SUM(dc_step(:,1))

      PRINT *
      PRINT *, 'NÃO PARALELO:'
      PRINT *, 'Elapsed time (micro seconds):', wtime
      PRINT *, 'Integral PI approx:', pi_approx
      PRINT *, 'Exact value of PI: ', pi
      PRINT *, 'Error (%)', 100.0d0*dabs(pi_approx-pi)/pi

!**** PARALELIZADO POR Open ACC:

      call system_clock( count=c1 )

      !$omp parallel do collapse(2) default(shared) 
      !$acc parallel loop collapse(2) default(present)
      DO i=1,nstep
        DO j=1,nstep
          x=dfloat(i-1)*step + 0.5d0*step
          dc_step(i,j) = 4.0d0/(1.0d0+x*x)
        ENDDO
      ENDDO
      !$acc end parallel loop
      !$omp end parallel do

      call system_clock( count=c2 )
      wtime=c2-c1

      pi_approx = step * SUM(dc_step(:,1))

      PRINT *
      PRINT *, 'PARALELO VIA Open ACC (nvidia GPU) OR OMP (intel CPU):'
      PRINT *, 'Elapsed time (micro seconds):', wtime
      PRINT *, 'Integral PI approx:', pi_approx
      PRINT *, 'Exact value of PI: ', pi
      PRINT *, 'Error (%)', 100.0d0*dabs(pi_approx-pi)/pi

!**** PARALELIZADO POR DO CONCURRENT:

!     OBS: para este problema em particular não compensou

      call system_clock( count=c1 )

      DO CONCURRENT (i=1:nstep,j=1:nstep)
          x=dfloat(i-1)*step + 0.5d0*step
          dc_step(i,j) = 4.0d0/(1.0d0+x*x)
      ENDDO 

      call system_clock( count=c2 )
      wtime=c2-c1

      pi_approx = step * SUM(dc_step(:,1))

      PRINT *
      PRINT *, 'PARALELO VIA DO CONCURRENT:'
      PRINT *, 'Elapsed time (micro seconds):', wtime
      PRINT *, 'Integral PI approx:', pi_approx
      PRINT *, 'Exact value of PI: ', pi
      PRINT *, 'Error (%)', 100.0d0*dabs(pi_approx-pi)/pi

      END PROGRAM
