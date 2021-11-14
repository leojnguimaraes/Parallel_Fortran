      PROGRAM Soma
      USE OMP_LIB

      IMPLICIT NONE

      INTEGER :: i, partial_Sum, total_Sum, thread_id

      total_Sum = 0

      !$OMP PARALLEL DEFAULT(NONE)
     .               PRIVATE(i,thread_id,partial_Sum) 
     .               SHARED(total_Sum)

          partial_Sum = 0

          !$OMP DO
          DO i=1,1000
              partial_Sum = partial_Sum + i
          END DO
          !$OMP END DO

          thread_id = OMP_GET_THREAD_NUM()
          PRINT *, 'Sum of process ', thread_id, ' = ', partial_Sum

          !$OMP CRITICAL
              total_Sum = total_Sum + partial_Sum
          !$OMP END CRITICAL

      !$OMP END PARALLEL

      PRINT *, 'Total Sum: ', total_Sum

      END

c     Fonte: https://curc.readthedocs.io/en/latest/programming/OpenMP-Fortran.html
