      PROGRAM Soma

      USE OPENACC
          
      IMPLICIT NONE

      REAL*8 total_Sum 

      INTEGER i, c1, c2, cgpu, chost

      !call acc_init( acc_device_nvidia )
      call system_clock( count=c1 )
      total_Sum = 0.0d0
      !$acc parallel loop reduction (+:total_Sum)
      DO i=1,10000000
        total_Sum = total_Sum + 1.0d0
      END DO
      !$acc end parallel loop 
      call system_clock( count=c2 )
      cgpu = c2 - c1
      PRINT *, cgpu, ' microseconds on GPU'
      PRINT *, 'Total Sum:', total_Sum

      total_Sum = 0.0d0
      call system_clock( count=c1 )
      DO i=1,10000000
        total_Sum = total_Sum + 1.0d0
      END DO
      call system_clock( count=c2 )
      chost = c2 - c1
      PRINT *, chost, ' microseconds on host'
      PRINT *, 'Total Sum:', total_Sum

      END
