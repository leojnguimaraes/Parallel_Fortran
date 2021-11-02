      PROGRAM Soma

      USE OPENACC
          
      IMPLICIT NONE

      REAL*8 total_Sum 

      INTEGER i, j, c1, c2, c3, cgpu, chost

      !call acc_init( acc_device_nvidia )
      call system_clock( count=c1 )
      total_Sum = 0.0d0
      !$acc parallel loop reduction (+:total_Sum)
      DO i=1,100000
        DO j=1,100000
          total_Sum = total_Sum + 1.0d0
        END DO
      END DO
      call system_clock( count=c2 )
      cgpu = c2 - c1
      PRINT *, cgpu, ' microseconds on GPU'
      PRINT *, 'Total Sum:', total_Sum

      total_Sum = 0.0d0
      DO i=1,100000
        DO j=1,100000
          total_Sum = total_Sum + 1.0d0
        END DO
      END DO
      call system_clock( count=c3 )
      chost = c3 - c2
      PRINT *, chost, ' microseconds on host'
      PRINT *, 'Total Sum:', total_Sum

      END