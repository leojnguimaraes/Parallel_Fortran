      PROGRAM hello_world_mpi

      use mpi
c     include 'mpif.h'

      implicit none
      
      integer process_Rank, size_Of_Cluster, ierror, i
      
      call MPI_INIT(ierror)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, size_Of_Cluster, ierror)
      call MPI_COMM_RANK(MPI_COMM_WORLD, process_Rank, ierror)
      
      DO i = 0,size_Of_Cluster
          IF(i == process_Rank) THEN
              print *, 'Hello World from process: '
     .               , process_Rank, 'of ', size_Of_Cluster
          END IF
          call MPI_BARRIER( MPI_COMM_WORLD, ierror)
      END DO
 
      call MPI_FINALIZE(ierror)

      END PROGRAM
