      subroutine read_mesh

      use fem_mesh

      implicit none

      integer :: i, j, k, idim, check, total_check

      read(file_gen_dat,*)
      read(file_gen_dat,*) numnp
      read(file_gen_dat,*)
      read(file_gen_dat,*) numel
      read(file_gen_dat,*)
      read(file_gen_dat,*) ndim
      read(file_gen_dat,*)
      read(file_gen_dat,*) nummat
      read(file_gen_dat,*)
      read(file_gen_dat,*) numpar

      if (ndim==2) then
        mnnel=3  ! only linear triangles (at moment...) 
      elseif (ndim==3) then
        mnnel=4  ! only linear tetrahedrons (at moment...) 
      endif
      ndf=1    ! only monophasic flow problem (at moment...)

      write(file_gen_out,*)
      write(file_gen_out,*)'Mesh'
      write(file_gen_out,*)'===='
      write(file_gen_out,*)
      write(file_gen_out,*) 'Number of nodes:              ', numnp
      write(file_gen_out,*) 'Number of elements:           ', numel
      write(file_gen_out,*) 'Problem dimension:            ', ndim
      write(file_gen_out,*) 'Number of materials:          ', nummat
      write(file_gen_out,*) 'Degrees of freedom per node:  ', ndf
      write(file_gen_out,*) 'Nodes per element (maximum):  ', mnnel
      write(file_gen_out,*) 'Number of mesh partitions:    ', numpar

      if (ndim<2 .or. ndim>3) then
        write(file_gen_out,*)
        write(file_gen_out,*) 'ndim must be equal to 2 or 3'
        write(file_gen_out,*) 'Stop.'
        stop
      endif

      total_check=0
      allocate(mesh%kxx(mnnel,numel),source=0,stat=check)
      total_check=total_check+check
      allocate(mesh%ifluxtype(numnp),source=0,stat=check)
      total_check=total_check+check
      allocate(mesh%ifordisp(ndim,numnp),source=0,stat=check)
      total_check=total_check+check
      allocate(mesh%lnodes(numel),source=0,stat=check)
      total_check=total_check+check
      allocate(mesh%lnval(numel),source=0,stat=check)
      total_check=total_check+check
      allocate(mesh%ltype(numel),source=0,stat=check)
      total_check=total_check+check
      allocate(mesh%mtype(numel),source=0,stat=check)
      total_check=total_check+check
      allocate(mesh%partition_element(numel),source=0,stat=check)
      total_check=total_check+check
      allocate(mesh%partition_node(numnp),source=0,stat=check)
      total_check=total_check+check
      allocate(mesh%coord(ndim,numnp),source=0.0d0,stat=check)
      total_check=total_check+check
      allocate(mesh_new%kxx(mnnel,numel),source=0,stat=check)
      total_check=total_check+check
      allocate(mesh_new%ifluxtype(numnp),source=0,stat=check)
      total_check=total_check+check
      allocate(mesh_new%ifordisp(ndim,numnp),source=0,stat=check)
      total_check=total_check+check
      allocate(mesh_new%lnodes(numel),source=0,stat=check)
      total_check=total_check+check
      allocate(mesh_new%lnval(numel),source=0,stat=check)
      total_check=total_check+check
      allocate(mesh_new%ltype(numel),source=0,stat=check)
      total_check=total_check+check
      allocate(mesh_new%mtype(numel),source=0,stat=check)
      total_check=total_check+check
      allocate(mesh_new%partition_element(numel),source=0,stat=check)
      total_check=total_check+check
      allocate(mesh_new%partition_node(numnp),source=0,stat=check)
      total_check=total_check+check
      allocate(mesh_new%coord(ndim,numnp),source=0.0d0,stat=check)
      total_check=total_check+check
      if (total_check/=0) then
       write(file_gen_out,*)
       write(file_gen_out,*)'subroutine read_mesh'
       write(file_gen_out,*)'Problems with dynamic memory allocation.'
       write(file_gen_out,*)'Stop.'
       stop
      endif

      do i=1, numnp
         read(file_gri_dat,*) j, (mesh%coord(idim,i),idim=1,ndim),      &
     &                           (mesh%ifordisp(idim,i),idim=1,ndim),   &
     &                            mesh%ifluxtype(i)
         if (i/=j) then
           write(file_gen_out,*)
           write(file_gen_out,*)'Error reading coordinates at node', i
           write(file_gen_out,*)'Stop.'
           stop
         endif
      enddo

      do i=1, numel
         read(file_gri_dat,*) j, mesh%mtype(i), mesh%ltype(i),          &
     &                           (mesh%kxx(k,i),k=1,mnnel)
         if (ndim==2) then
           mesh%lnodes(i)=3  ! only linear triangles (at moment...)
         elseif (ndim==3) then
           mesh%lnodes(i)=4  ! only linear tetrahedrons (at moment...)
         endif
         if (i/=j              .or.                                     &            
     &       mesh%mtype(i)>nummat .or.                                  &           
     &       mesh%ltype(i)/=1                                           & ! only linear triangles or tetrahedrons (at moment...)
     &                              ) then
           write(file_gen_out,*)
           write(file_gen_out,*)'Error reading element', i
           write(file_gen_out,*)'Stop.'
           stop
         endif
      enddo

      return

      end subroutine read_mesh
