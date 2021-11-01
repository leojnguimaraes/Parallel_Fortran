      PROGRAM PI_Integral_Coarray

      IMPLICIT NONE

      REAL*8 total_Sum[*], x, step, pi, pi_approx, telaps 

      INTEGER :: i, nstep,rate,tic,toc,im

      nstep = 1000000000

      total_Sum = 0.0d0

      step = 1.0d0/dfloat(nstep)

      im = this_image()
      IF (im == 1) THEN
        CALL SYSTEM_CLOCK(tic)
        PRINT *, 'Number of Fortran coarray images:', num_images() 
      END IF

      DO i=im,nstep,num_images()
        x = (dfloat(i-1)+0.5d0)*step
        total_Sum = total_Sum + 4.0d0/(1.0d0+x*x)
      END DO

      SYNC ALL

      IF (im == 1) THEN
        pi = 4.0d0*datan(1.0d0)
        DO i=2,num_images()
          total_Sum=total_Sum+total_Sum[i]
        END DO
        pi_approx = step * total_Sum
        call system_clock(toc)
        call system_clock(count_rate=rate)
        telaps = dfloat(toc - tic)  / rate
        PRINT *, 'Elapsed time:', telaps
        PRINT *, 'Integral PI approx:', pi_approx
        PRINT *, 'Exact value of PI: ', pi
        PRINT *, 'Error (%)', 100.0d0*dabs(pi_approx-pi)/pi
      END IF

      END PROGRAM
