      program test_pi
      !! implements calculation:
      !! $$ \pi = \int^1_{-1} \frac{dx}{\sqrt{1-x^2}}

      ! Fonte: https://github.com/scivision/fortran-coarray-mpi-examples/blob/main/coarray/pi.f90
      
      implicit none 
      
      real*8, parameter :: x0 = -1.0d0, x1 = 1.0d0
      real*8, parameter :: pi = 4.d0*datan(1.0d0)
      real*8 :: psum[*]  ! this is a scalar coarray
      integer :: rate,tic,toc
      real*8 :: f,x,telaps, dx
      integer :: i, stat, Ni, ierr
      
      psum = 0.0d0
      
      Ni=1000000000

      dx=(x1-x0)/dfloat(Ni)

      !---------------------------------
      if (this_image() == 1) then
        call system_clock(tic)
        print 
     .  '(A,I3)', 'number of Fortran coarray images:', num_images()
        print *,'approximating pi in ',Ni,' steps.'
      end if
      !---------------------------------
      
      do i = this_image(), Ni-1, num_images() ! Each image works on a subset of the problem
        x = x0 + dfloat(i)*dx
        f = dx / dsqrt(1.0d0 - x*x)
        psum = psum + f
      end do
      
      sync all 
      
      if (this_image() == 1)  then
        do i=2,num_images()
            psum = psum + psum[i]
        end do
        print *,'pi:',pi,'  iterated pi: ',psum
        print '(A,E10.3)', 'pi error', pi - psum
      endif
      
      if (this_image() == 1) then
        call system_clock(toc)
        call system_clock(count_rate=rate)
        telaps = real((toc - tic))  / rate
        print '(A,E10.3,A,I3,A)', 'Elapsed wall clock time ', 
     .  telaps, ' seconds, using',num_images(),' images.'
      end if
      
      end program
