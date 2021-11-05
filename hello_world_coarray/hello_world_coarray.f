      program hello_world
      
      implicit none

      integer :: i

      do i=1,num_images()
        if (i==this_image())
     .  write(*,*) 'Hello world from ',this_image(),'of',num_images()
      enddo
      
      end program hello_world
