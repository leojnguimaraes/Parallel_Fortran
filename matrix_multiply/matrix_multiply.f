      program Matrix_Multiply
      use omp_lib

      implicit none

      integer :: i,j,k,n
      real*8 :: angle,s,pi,wtime
      real*8,dimension(:,:),allocatable :: a,b,c

      pi=4.0d0*datan(1.0d0)

      n=650
      n=3000
      
      write(*,*) 'Matrix order:', n

      write(*,*) 'Number of processors availabe:', omp_get_num_procs()
      write(*,*) 'Number of threads availabe:', omp_get_max_threads()

      allocate(a(1:n,1:n))
      allocate(b(1:n,1:n))
      allocate(c(1:n,1:n))

      s=1.0d0/dsqrt(dfloat(n))

      wtime=omp_get_wtime()

      !$omp parallel shared(a,b,c,n,s) private(angle,i,j,k)

      !$omp do
      do i=1,n
        do j=1,n
          angle=2.0d0*pi*dfloat(i-1)*dfloat(j-1)/dfloat(n)
          a(i,j)=s*(dsin(angle)+dcos(angle))
        end do
      end do
      !$omp end do

      !$omp do
      do i=1,n
        do j=1,n
          b(i,j)=a(i,j)
        end do
      end do
      !$omp end do

      !$omp do
      do i=1,n
        do j=1,n
          c(i,j)=0.0d0
          do k=1,n
            c(i,j)=c(i,j)+a(i,k)*b(k,j)
          end do
        end do
      end do
      !$omp end do

      !$omp end parallel

      wtime=omp_get_wtime()-wtime

      write(*,*) 'Elapsed time:',wtime
      write(*,*) 'c(100,100)=',c(100,100)

      end program
