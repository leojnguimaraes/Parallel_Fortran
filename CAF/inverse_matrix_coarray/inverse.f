      program inverse_test

      implicit none

      double precision, allocatable :: A(:,:), Ainv(:,:), Identity(:,:)

      integer :: n, i, j, k

      n=3

      allocate(A(1:n,1:n))
      allocate(Ainv(1:n,1:n))
      allocate(Identity(1:n,1:n))

      call RANDOM_SEED
      do j=1,n
        do i=1,n
          call RANDOM_NUMBER(A(i,j))
        enddo
      enddo

      Ainv=inv(A)

      do j=1,n
        do i=1,n
          Identity(i,j)=0.0d0
          do k=1,n
            Identity(i,j)=Identity(i,j)+A(i,k)*Ainv(k,j)
          enddo
        enddo
      enddo

      sync all

      do i=1,num_images()
        if (i==this_image()) then
          print *, 'Image:', this_image()
          print *, 'A:'
          print *, A
          print *, 'A*Ainv:'
          print *, Identity
        endif
        sync all
      enddo

      contains

*.......................................................................

      ! Fonte: http://fortranwiki.org/fortran/show/Matrix+inversion
      ! Returns the inverse of a matrix calculated by finding the LU
      ! decomposition.  Depends on LAPACK.
      function inv(A) result(Ainv)
        double precision, dimension(:,:), intent(in) :: A
        double precision, dimension(size(A,1),size(A,2)) :: Ainv
      
        double precision, dimension(size(A,1)) :: work  ! work array for LAPACK
        integer, dimension(size(A,1)) :: ipiv   ! pivot indices
        integer :: n, info
      
        ! External procedures defined in LAPACK
        external DGETRF
        external DGETRI
      
        ! Store A in Ainv to prevent it from being overwritten by LAPACK
        Ainv = A
        n = size(A,1)
      
        ! DGETRF computes an LU factorization of a general M-by-N matrix A
        ! using partial pivoting with row interchanges.
        call DGETRF(n, n, Ainv, n, ipiv, info)
      
        if (info /= 0) then
           stop 'Matrix is numerically singular!'
        end if
      
        ! DGETRI computes the inverse of a matrix using the LU factorization
        ! computed by DGETRF.
        call DGETRI(n, Ainv, n, ipiv, work, n, info)
      
        if (info /= 0) then
           stop 'Matrix inversion failed!'
        end if
      end function inv

      end program inverse_test
