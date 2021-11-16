      program integrate_mpi

c     This program integrates sin(x) between 0 and pi by computing
c     the area of a number of rectangles chosen so as to approximate
c     the shape under the curve of the function using MPI.
c
c     The root process acts as a master to a group of child process
c     that act as slaves.  The master prompts for the number of 
c     interpolations and broadcasts that value to each slave.
c
c     There are num_procs processes all together, and a process 
c     computes the area defined by every num_procs-th interval,
c     collects a partial sum of those areas, and sends its partial 
c     sum to the root process.

      use mpi
!     include '/usr/include/mpif.h'

      parameter (pi=3.141592654)
      integer my_id, root_process, num_procs, ierr
      double precision rect_width, area, sum, x_middle, partial_sum
      integer status(MPI_STATUS_SIZE)

c     Let process 0 be the root process.

      root_process = 0

c     Now replicate this process to create parallel processes.

      call MPI_INIT (ierr)

c     Find out MY process ID, and how many processes were started.

      call MPI_COMM_RANK (MPI_COMM_WORLD, my_id, ierr)
      call MPI_COMM_SIZE (MPI_COMM_WORLD, num_procs, ierr)

      if (my_id .eq. root_process) then
c
c        I must be the root process, so I will query the user
c        to determine how many interpolation intervals to use.

         print *, "please enter the number of intervals to interpolate:"
         read *, num_intervals
      end if

c     Then...no matter which process I am:
c
c     I engage in a broadcast so that the number of intervals is 
c     sent from the root process to the other processes, and ...
 
      call MPI_BCAST (num_intervals, 1, MPI_INTEGER, root_process, 
     &        MPI_COMM_WORLD, ierr)

c     calculate the width of a rectangle, and

      rect_width = pi / num_intervals

c     then calculate the sum of the areas of the rectangles for
c     which I am responsible.  Start with the (my_id +1)th
c     interval and process every num_procs-th interval thereafter.

      partial_sum = 0.0
      do i = (my_id + 1), num_intervals, num_procs
c        Find the middle of the interval on the X-axis. 
         x_middle = (i - 0.5) * rect_width
         area =  dsin(x_middle) * rect_width 
         partial_sum = partial_sum + area
      end do
      print *,"proc", my_id, "computes:", partial_sum

c     and finally, engage in a reduction in which all partial sums 
c     are combined, and the grand sum appears in variable "sum" in
c     the root process,

      call MPI_REDUCE (partial_sum, sum, 1, MPI_DOUBLE_PRECISION,
     &       MPI_SUM, root_process, MPI_COMM_WORLD, ierr)

c     and, if I am the root process, print the result.

      if (my_id .eq. root_process) then 
         print *,'The integral is ', sum
c         (yes, we could have summed just the heights, and
c          postponed the multiplication by rect_width til now.)
      end if 

c     Close down this processes.

      call MPI_FINALIZE (ierr)
      stop
      end
