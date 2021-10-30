      PROGRAM Hello_World
      USE OMP_LIB

      IMPLICIT NONE

      INTEGER :: i, thread_id

      !$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,thread_id)

      thread_id = OMP_GET_THREAD_NUM()

      DO i=0,OMP_GET_MAX_THREADS()
        IF (i == thread_id) THEN
            PRINT *, "Hello from process: ", thread_id
        END IF
        !$OMP BARRIER
      END DO

      !$OMP END PARALLEL


      END

c     Fonte: https://curc.readthedocs.io/en/latest/programming/OpenMP-Fortran.html
