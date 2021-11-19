      program metis_partition

      use fem_mesh

      implicit none

      integer i, j, k, iel, nnel, check, total_check, n

      integer, parameter   :: nels=3180, nnds=1711, npel=3
      integer, allocatable :: eptr(:), nodes(:), epart(:), npart(:)
      integer, pointer     :: vwgt=>null(), vsize=>null(), mopts=>null()
      real(8), pointer     :: tpwgts=>null()

      call open_file(file_gri_dat,'metis_gri.dat','old',file_gen_out)
      call open_file(file_gen_dat,'gen.dat','old',file_gen_out)
      call open_file(file_gri_dat_NEW,'gri.dat','new',file_gen_out)
      call open_file(file_gen_out,'metis_gen.out','new',file_gen_out)
      call open_file(file_gid_msh,'metis_gid.msh','new',file_gen_out)
      call open_file(file_gid_res,'metis_gid.res','new',file_gen_out)

      call read_mesh

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

      call METIS_PartMeshNodal(numel,numnp,eptr,nodes,vwgt,             &
     &                         vsize,numpar,tpwgts,mopts,n,             &
     &                         mesh%partition_element,                  &
     &                         mesh%partition_node        ) 

      mesh_new=mesh

      call write_gid_mesh

      call write_gid_results

      close(file_gri_dat)
      close(file_gri_dat_NEW)
      close(file_gen_dat)
      close(file_gen_out)
      close(file_gid_msh)
      close(file_gid_res)

      end program metis_partition

