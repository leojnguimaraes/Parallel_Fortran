      program metis_partition

      use fem_mesh

      implicit none

      integer i, j, k, iel, nnel, check, total_check, idim, prev, n

      integer, allocatable :: eptr(:), nodes(:), epart(:), npart(:),    &
     &                        newnode(:)
      integer, pointer     :: vwgt=>null(), vsize=>null(), mopts=>null()
      real(8), pointer     :: tpwgts=>null()

      call open_file(file_gri_dat,'gri.dat','old',file_gen_out)
      call open_file(file_gen_dat,'gen.dat','old',file_gen_out)
      call open_file(file_gri_dat_NEW,'metis_gri.dat','new',file_gen_out)
      call open_file(file_gen_out,'metis_gen.out','new',file_gen_out)
      call open_file(file_gid_msh,'metis_gid.msh','new',file_gen_out)
      call open_file(file_gid_res,'metis_gid.res','new',file_gen_out)

      call read_mesh

      if (numpar>1) then

        total_check=0
        allocate(eptr(numel+1),source=0,stat=check)
        total_check=total_check+check
        allocate(nodes(numel*mnnel),source=0,stat=check)
        total_check=total_check+check
        if (total_check/=0) then
         write(file_gen_out,*)
         write(file_gen_out,*)'program metis_partition'
         write(file_gen_out,*)'Problems with dynamic memory allocation.'
         write(file_gen_out,*)'Stop.'
         stop
        endif

        i=0
        k=1
        eptr(1)=0
        do iel=1,numel
           nnel=mesh%lnodes(iel)
           do j=1,nnel
             i=i+1
             nodes(i)=mesh%kxx(j,iel)-1
           enddo
           k=k+1
           eptr(k)=eptr(k-1)+nnel
        enddo

        call METIS_PartMeshNodal(numel,numnp,eptr,nodes,vwgt,           &
     &                           vsize,numpar,tpwgts,mopts,n,           &
     &                           mesh%partition_element,                &
     &                           mesh%partition_node        ) 

        total_check=0
        allocate(newnode(numnp),source=0,stat=check)
        total_check=total_check+check
        if (total_check/=0) then
         write(file_gen_out,*)
         write(file_gen_out,*)'program metis_partition'
         write(file_gen_out,*)'Problems with dynamic memory allocation.'
         write(file_gen_out,*)'Stop.'
         stop
        endif

        k=0
        do i=0,numpar-1
          do j=1,numnp
            if (mesh%partition_node(j)==i) then
              k=k+1
              mesh_new%partition_node(k)=i
              mesh_new%ifluxtype(k)=mesh%ifluxtype(j)
              do idim=1,ndim
                mesh_new%coord(idim,k)=mesh%coord(idim,j)
                mesh_new%ifordisp(idim,k)=mesh%ifordisp(idim,j)
              enddo
              newnode(j)=k
            endif
          enddo
        enddo

        k=0
        do i=0,numpar-1
          do iel=1,numel
            if (mesh%partition_element(iel)==i) then
              k=k+1
              mesh_new%partition_element(k)=i
              mesh_new%lnodes(k)=mesh%lnodes(iel)
              mesh_new%lnval(k)=mesh%lnval(iel)
              mesh_new%ltype(k)=mesh%ltype(iel)
              mesh_new%mtype(k)=mesh%mtype(iel)
              do j=1,mesh%lnodes(iel)
                mesh_new%kxx(j,k)=newnode(mesh%kxx(j,iel))
              enddo
            endif
          enddo
        enddo

      else

        mesh_new=mesh

      endif

      call write_gid_mesh

      call write_gid_results

      write(file_gri_dat_NEW,*)'Nodal point partition:'
      do i=0,numpar-1
        prev=-1
        write(file_gri_dat_NEW,*)'  Rank:',i
        do j=1,numnp-1
          if (mesh_new%partition_node(j)==i) then
            if (mesh_new%partition_node(j)/=prev)                       &
     &      write(file_gri_dat_NEW,*)'    Lower: ',j
            if (mesh_new%partition_node(j)/=mesh_new%partition_node(j+1)) &
     &      write(file_gri_dat_NEW,*)'    Higher:',j
            prev=mesh_new%partition_node(j)
          endif
        enddo
      enddo
      write(file_gri_dat_NEW,*)'    Higher:',numnp
      write(file_gri_dat_NEW,*)'Nodal point list:'
      do i=1,numnp
       write(file_gri_dat_NEW,*) i,                                     &
     &                          (mesh_new%coord(idim,i),idim=1,ndim),   &
     &                          (mesh_new%ifordisp(idim,i),idim=1,ndim),&
     &                           mesh_new%ifluxtype(i)
      enddo

      write(file_gri_dat_NEW,*)'Element partition:'
      do i=0,numpar-1
        prev=-1
        write(file_gri_dat_NEW,*)'  Rank:',i
        do j=1,numel-1
          if (mesh_new%partition_element(j)==i) then
            if (mesh_new%partition_element(j)/=prev)                    &
     &      write(file_gri_dat_NEW,*)'    Lower: ',j
            if (mesh_new%partition_element(j)/=mesh_new%partition_element(j+1)) &
     &      write(file_gri_dat_NEW,*)'    Higher:',j
            prev=mesh_new%partition_element(j)
          endif
        enddo
      enddo
      write(file_gri_dat_NEW,*)'    Higher:',numel
      write(file_gri_dat_NEW,*)'Element list:'
      do i=1,numel
       write(file_gri_dat_NEW,*) i,                                     &
     &                           mesh_new%mtype(i),                     & 
     &                           mesh_new%ltype(i),                     &
     &                          (mesh_new%kxx(k,i),k=1,mnnel)
      enddo

      close(file_gri_dat)
      close(file_gri_dat_NEW)
      close(file_gen_dat)
      close(file_gen_out)
      close(file_gid_msh)
      close(file_gid_res)

      end program metis_partition

