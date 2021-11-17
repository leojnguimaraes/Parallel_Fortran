      program main
#include <petsc/finclude/petscmat.h>
      use petscmat
      implicit none

      PetscInt       :: i,j
      PetscInt       :: Istart,Iend,n,ione
      PetscErrorCode :: ierr
      PetscMPIInt    :: rank,size
      Mat            :: A
      PetscScalar    :: zero,one,value
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

      call MatCreate(PETSC_COMM_WORLD,A,ierr);CHKERRA(ierr)
      call MatSetSizes(A,PETSC_DECIDE,PETSC_DECIDE,n,n,ierr)
      CHKERRA(ierr)
      call MatSetFromOptions(A,ierr)
      call MatSetUp(A,ierr);CHKERRA(ierr)

      call MatGetOwnershipRange(A,Istart,Iend,ierr);CHKERRA(ierr)

      do i=0,size 
        if (i==rank) then
          write(outputString,*)'Processador',rank,':',Istart,Iend-1,'\n'
          call PetscPrintf(PETSC_COMM_SELF,outputString,ierr)
          CHKERRA(ierr)
        endif
        call MPI_Barrier(PETSC_COMM_WORLD,ierr);CHKERRA(ierr)
      enddo

!     cria matriz identidade:
      do i=Istart,Iend-1
        value=1.0
        call MatSetValues(A,ione,i,ione,i,value,INSERT_VALUES,ierr)
        CHKERRA(ierr)
      enddo

!     Sincronização para matriz: tem que chamar sempre que a matriz for modificada
      call MatAssemblyBegin(A,MAT_FINAL_ASSEMBLY,ierr);CHKERRA(ierr)
      call MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY,ierr);CHKERRA(ierr)

      call MatView(A,PETSC_VIEWER_STDOUT_WORLD,ierr);CHKERRA(ierr)

!     adiciona um termo a matriz:
      if (rank==0) then
        i=1
        j=3
        value=13.0
        call MatSetValues(A,ione,i,ione,j,value,INSERT_VALUES,ierr)
        CHKERRA(ierr)
      endif

!     Sincronização para matriz: tem que chamar sempre que a matriz for modificada
      call MatAssemblyBegin(A,MAT_FINAL_ASSEMBLY,ierr);CHKERRA(ierr)
      call MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY,ierr);CHKERRA(ierr)

      call MatView(A,PETSC_VIEWER_STDOUT_WORLD,ierr);CHKERRA(ierr)

      call MatDestroy(A,ierr);CHKERRA(ierr)

      call PetscFinalize(ierr)
      end
