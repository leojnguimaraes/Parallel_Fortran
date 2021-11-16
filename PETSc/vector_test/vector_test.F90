      program main
#include <petsc/finclude/petscvec.h>
      use petscvec
      implicit none

      PetscInt       :: i
      PetscInt       :: Istart,Iend,n,ione
      PetscErrorCode :: ierr
      PetscMPIInt    :: rank,size
      Vec            :: x
      PetscScalar    :: zero,one,value,sum
      character(len=PETSC_MAX_PATH_LEN) :: outputString

      n=10

      zero=0.0
      one=1.0
      ione=1

      call PetscInitialize(PETSC_NULL_CHARACTER,ierr)
      if (ierr .ne. 0) then
        print*,'Unable to initialize PETSc'
        stop
      endif

      call MPI_Comm_rank(PETSC_COMM_WORLD,rank,ierr);CHKERRA(ierr)
      call MPI_Comm_size(PETSC_COMM_WORLD,size,ierr);CHKERRA(ierr)

      call VecCreate(PETSC_COMM_WORLD,x,ierr);CHKERRA(ierr)
      call VecSetSizes(x,PETSC_DECIDE,n,ierr);CHKERRA(ierr)
      call VecSetFromOptions(x,ierr);CHKERRA(ierr)

      call VecSet(x,zero,ierr);CHKERRA(ierr)

      call VecGetSize(x,n,ierr);CHKERRA(ierr)

      call VecGetOwnershipRange(x,Istart,Iend,ierr);CHKERRA(ierr)

      do i=0,size 
        if (i==rank) then
          write(outputString,*)'Processador',rank,':',Istart,Iend-1,'\n'
          call PetscPrintf(PETSC_COMM_SELF,outputString,ierr)
          CHKERRA(ierr)
        endif
        call MPI_Barrier(PETSC_COMM_WORLD,ierr);CHKERRA(ierr)
      enddo

      do i=Istart,Iend-1
        value=float(i)*10.0
        call VecSetValues(x,ione,i,value,ADD_VALUES,ierr);CHKERRA(ierr)
      enddo

!     Sincronização para vetor: tem que chamar sempre que o vetor for modificado
      call VecAssemblyBegin(x,ierr);CHKERRA(ierr)
      call VecAssemblyEnd(x,ierr);CHKERRA(ierr)

      call VecSum(x,sum,ierr);CHKERRA(ierr)
      write(outputString,*)'\nVector Sum:',sum,'\n'
      call PetscPrintf(PETSC_COMM_WORLD,outputString,ierr);CHKERRA(ierr)
      call VecView(x,PETSC_VIEWER_STDOUT_WORLD,ierr);CHKERRA(ierr)

      if (rank==0) then
        do i=0,n-1
          value=-float(i)*10.0
          call VecSetValues(x,ione,i,value,ADD_VALUES,ierr)
          CHKERRA(ierr)
        enddo
      endif

!     Sincronização para vetor: tem que chamar sempre que o vetor for modificado
      call VecAssemblyBegin(x,ierr);CHKERRA(ierr)
      call VecAssemblyEnd(x,ierr);CHKERRA(ierr)

      call VecSum(x,sum,ierr);CHKERRA(ierr)
      write(outputString,*)'\nModified Vector (at rank 0) Sum:',sum,'\n'
      call PetscPrintf(PETSC_COMM_WORLD,outputString,ierr);CHKERRA(ierr)
      call VecView(x,PETSC_VIEWER_STDOUT_WORLD,ierr);CHKERRA(ierr)

      call VecDestroy(x,ierr);CHKERRA(ierr)

      call PetscFinalize(ierr)
      end
