      program Matrix_Multiply
      use omp_lib

c     Fonte: https://youtu.be/2YhG_zl_lHU

      implicit none

      integer :: i,j,k,n
      real*8 :: angle,s,pi,wtime,aux,ALPHA,BETA
      real*8,dimension(:,:),allocatable :: a,b,c_omp,c_mkl

      pi=4.0d0*datan(1.0d0)

      n=650
      n=3000
      
      write(*,*)
      write(*,*) 'Matrix order:', n
      write(*,*) 'OpenMP processors availabe:', omp_get_num_procs()
      write(*,*) 'Number of threads availabe:', omp_get_max_threads()

      allocate(a(1:n,1:n))
      allocate(b(1:n,1:n))
      allocate(c_omp(1:n,1:n))
      allocate(c_mkl(1:n,1:n))

      s=1.0d0/dsqrt(dfloat(n))

      !$omp parallel private(angle,i,j,k)

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
              c_omp(i,j)=0.0d0
              c_mkl(i,j)=0.0d0
            end do
          end do
          !$omp end do

      !$omp end parallel

      wtime=omp_get_wtime()

      !$omp parallel private(aux,i,j,k)

        !$omp do 
        do i=1,n
          do j=1,n
            aux=0.0d0
            do k=1,n
              aux=aux+a(i,k)*b(k,j)
            end do
            c_omp(i,j)=aux ! esta linha consome todo tempo de CPU
          end do
        end do
        !$omp end do

      !$omp end parallel

      wtime=omp_get_wtime()-wtime

      write(*,*)
      write(*,*) 'OpenMP:'
      write(*,*) 'Elapsed time:',wtime
      write(*,*) 'c(100,100)=',c_omp(100,100)

      wtime=omp_get_wtime()

      ALPHA = 1.0d0
      BETA  = 0.0d0
      CALL DGEMM('N','N',n,n,n,ALPHA,a,n,b,n,BETA,c_mkl,n)

      wtime=omp_get_wtime()-wtime

      write(*,*)
      write(*,*) 'MKL:'
      write(*,*) 'Elapsed time:',wtime
      write(*,*) 'c(100,100)=',c_mkl(100,100)

      end program
