      ! Aloca dinamicamente vetor em cada rank com tamanhos diferentes
      
      program main
#include <petsc/finclude/petscsys.h>
      use petscmpi  ! or mpi or mpi_f08
      use petscsys
      
      implicit none

      PetscErrorCode                    :: i,j,ierr
      PetscMPIInt                       :: myRank,mySize
      character(len=PETSC_MAX_PATH_LEN) :: outputString
      PetscMPIInt, allocatable          :: count_vector(:)
      
      call PetscInitialize(PETSC_NULL_CHARACTER,ierr)
      if (ierr /= 0) then
        write(6,*) 'Unable to initialize PETSc'
        stop
      endif
      
      call MPI_Comm_size(MPI_COMM_WORLD,mySize,ierr)
      CHKERRA(ierr)
      call MPI_Comm_rank(MPI_COMM_WORLD,myRank,ierr)
      CHKERRA(ierr)

      allocate(count_vector(myRank+1),source=0)

      do i=1,myRank+1
        count_vector(i)=i
      enddo

      write(outputString,*)'\ncount_vector(:) in each rank:\n'
      call PetscPrintf(PETSC_COMM_WORLD,outputString,ierr)
      
      do i=1,mySize
        if (i==myRank+1) then
          write(outputString,*) (count_vector(j),j=1,myRank+1),'\n'
          call PetscPrintf(PETSC_COMM_SELF,outputString,ierr)
          CHKERRA(ierr)
        endif
        call MPI_Barrier(PETSC_COMM_WORLD,ierr)
        CHKERRA(ierr)
      enddo
      
      deallocate(count_vector)

      call PetscFinalize(ierr)
      CHKERRA(ierr)
      
      end program main
