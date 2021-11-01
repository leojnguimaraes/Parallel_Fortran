      program pi_mc_coarray

      ! Fonte: https://www.intel.com/content/www/us/en/develop/documentation/fortran-compiler-coarray-tutorial/top/calculating-value-of-pi-using-monte-carlo-method.html

      implicit none

      integer :: i,num_trials,clock_start,clock_end,clock_rate
      real :: x,y,computed_pi,actual_pi

      real, dimension(100), codimension[*] :: A
      integer :: B[3,*]

      ! Declare scalar coarray that will exist on each image
      integer :: total[*] ! Per-image subtotal

      num_trials=600000000

      actual_pi=4.0*atan(1.0)

      ! Image 1 initialization
      if (THIS_IMAGE() == 1) then
      ! Make sure that num_trials is divisible by the number of images
      if (MOD(num_trials,INT(NUM_IMAGES())) /= 0) 
     .error stop "num_trials not evenly divisible by number of images!"
      print '(A,I0,A,I0,A)', "Computing pi using ",num_trials,
     .                       " trials across ",NUM_IMAGES()," images"
      call SYSTEM_CLOCK(clock_start)
      end if

      ! Run the trials. Get a random X and Y and see if the position
      ! is within a circle of radius 1. If it is, add one to the subtotal
      do i=1,num_trials/int(NUM_IMAGES())
          call RANDOM_NUMBER(x)
          call RANDOM_NUMBER(y)
          if ((x*x)+(y*y) <= 1.0) total = total + 1
      end do

      ! Wait for everyone
      sync all

      ! Image 1 end processing
      if (this_image() == 1) then
        ! Sum all of the images' subtotals
        do i=2,num_images()
            total = total + total[i]
        end do
        ! total/num_trials is an approximation of pi/4
        computed_pi = 4.0* (REAL(total)/REAL(num_trials))
        print '(A,G0.8,A,G0.3)', "Computed value of pi is ",computed_pi,
     .      ", Relative Error: ",ABS((computed_pi-actual_pi)/actual_pi)
        ! Show elapsed time
        call SYSTEM_CLOCK(clock_end,clock_rate)
        print '(A,G0.3,A)', "Elapsed time is ",  
     .      REAL(clock_end-clock_start)/REAL(clock_rate)," seconds"
      end if

      end program pi_mc_coarray
