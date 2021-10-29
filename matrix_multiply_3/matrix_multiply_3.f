      ! benchmark.f90
      program benchmark

      ! Fonte: https://cyber.dabamos.de/programming/modernfortran/openmp.html

      use omp_lib

      implicit none

      integer, parameter :: dp       = kind(0.0d0)
      integer, parameter :: n        = 2000
      integer, parameter :: nthreads = 8
      
      integer       :: i, j, k
      real*8 :: t1, t2
      real*8,dimension(:,:),allocatable :: a, b, c

      allocate(a(1:n,1:n))
      allocate(b(1:n,1:n))
      allocate(c(1:n,1:n))
      
      ! Set number of threads to use.
      call omp_set_num_threads(nthreads)

      write(*,*)
      write(*,*) 'Matrix order:', n
      
      ! Initialise the PRNG and fill matrices A and B with random numbers.
      call random_seed()
      call random_number(a)
      call random_number(b)
      
      c = 0.0_dp
      t1 = omp_get_wtime()
      
      ! Calculate C = AB sequentially.
      do j = 1, n
          do k = 1, n
              do i = 1, n
                  c(i, j) = c(i, j) + a(i, k) * b(k, j)
              end do
          end do
      end do
      
      t2 = omp_get_wtime()
      write(*,*)
      print '(a, f7.3, a)', ' single: ', t2 - t1, ' s'
      
      c = 0.0_dp
      t1 = omp_get_wtime()
      
      ! Calculate C = AB in parallel with OpenMP, using static scheduling.
      !$omp parallel shared(a, b, c) private(i, j, k)

      !$omp single
      write(*,*)
      write(*,*) 'Number of processors availabe:', 
     .            omp_get_num_procs()
      write(*,*) 'Number of threads availabe:', 
     .            omp_get_max_threads()
      write(*,*)
      !$omp end single

      !$omp do schedule(static)
          do j = 1, n
              do k = 1, n
                  do i = 1, n
                      c(i, j) = c(i, j) + a(i, k) * b(k, j)
                  end do
              end do
          end do
      !$omp end do

      !$omp end parallel
      
      t2 = omp_get_wtime()
      print '(a, f7.3, a)', ' OpenMP: ', t2 - t1, ' s'

      end program benchmark
