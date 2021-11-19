      subroutine open_file(file_unit,file_name,file_status,file_out)

      implicit none

      logical          fileex
      integer          file_unit, file_out
      character*3      file_status
      character*55     error_message
      character(len=*) file_name

      if (file_status.eq.'new') then

          if (fileex(file_name))                                       &
     &    call remove_file(file_name)                                  
          open(file_unit,                                              &
     &         file=file_name,                                         &
     &         status=file_status,                                     &
     &         err=10)
          return
10        error_message=                                               &
     &    'Could not open file with status NEW.       '

      elseif (file_status.eq.'old') then

          open(file_unit,                                              &
     &         file=file_name,                                         &
     &         status=file_status,                                     &
     &         err=20)
          return
20        error_message=                                               &
     &    'Could not open file with status OLD.       '

      else

          error_message=                                               &
     &    'Could not identify file status: NEW or OLD.'

      endif

      write (file_out,*)
      write (file_out,*) 'Error in subroutine open_file:'
      write (file_out,*) 'File name:   ',trim(file_name)
      write (file_out,*) 'File unit:   ',file_unit
      write (file_out,*) 'File status: ',file_status
      write (file_out,*) trim(error_message)
      write (file_out,*) 'Stop all images.'
      write (file_out,*) ' '
      stop

      end

!.......................................................................

      subroutine remove_file(file_name)

      implicit none

      character(len=*) file_name

      call system('rm '//file_name)

      return
      end

!.......................................................................

      logical function fileex  (file_name)

      implicit none

      character(len=*) file_name
      logical ex

      inquire ( file= file_name, exist= ex )
      fileex= ex

      return
      end

!.......................................................................
