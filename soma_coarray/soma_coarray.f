      PROGRAM Soma_Coarray
      USE OMP_LIB

      IMPLICIT NONE

      REAL*8 total_Sum[*], telaps 

      INTEGER :: i, j, nstep,rate,tic,toc,im

      im = this_image()
      IF (im == 1) THEN
        CALL SYSTEM_CLOCK(tic)
        PRINT *, 'Number of Fortran coarray images:', num_images() 
      END IF

      total_Sum = 0.0d0

      DO i=im,100000,num_images()
        DO j=1,100000
          total_Sum = total_Sum + 1.0d0
        END DO
      END DO

      SYNC ALL

      IF (im == 1) THEN
        DO i=2,num_images()
          total_Sum=total_Sum+total_Sum[i]
        END DO
        CALL SYSTEM_CLOCK(toc)
        CALL SYSTEM_CLOCK(count_rate=rate)
        telaps = DFLOAT(toc - tic)  / rate
        PRINT *, 'Elapsed time:', telaps
        PRINT *, 'Total Sum', total_Sum
      END IF

      END PROGRAM
