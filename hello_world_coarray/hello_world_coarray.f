      program hello_world
      use iso_fortran_env, only: output_unit
      implicit none
        write(*,*) 'Hello world from ', 
     .   this_image() , 'of', num_images()
      end program hello_world
