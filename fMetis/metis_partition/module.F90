      module fem_mesh

        implicit none

        integer :: file_gen_dat=10
        integer :: file_gri_dat=20
        integer :: file_gen_out=30
        integer :: file_gid_msh=40
        integer :: file_gid_res=50
        integer :: file_gri_dat_NEW=60

        integer :: numnp              ! number of nodes
        integer :: numel              ! number of elements
        integer :: mnnel              ! max number of nodes per element
        integer :: ndim               ! problem dimension
        integer :: nummat             ! number of materials
        integer :: numpar             ! number of partitions
        integer :: ndf                ! number of degrees of freedom

        type mesh_data
          integer, allocatable :: kxx(:,:)              ! size=mnnel,numel
          integer, allocatable :: ifluxtype(:)          ! size=numnp
          integer, allocatable :: ifordisp(:,:)         ! size=ndim,numnp
          integer, allocatable :: lnodes(:)             ! size=numel
          integer, allocatable :: lnval(:)              ! size=numel
          integer, allocatable :: ltype(:)              ! size=numel
          integer, allocatable :: mtype(:)              ! size=numel
          integer, allocatable :: partition_element(:)  ! size=numel
          integer, allocatable :: partition_node(:)     ! size=numnp
          double precision, allocatable :: coord(:,:)   ! size=ndim,numnp
        end type mesh_data

      type(mesh_data) :: mesh
      type(mesh_data) :: mesh_new

      end module fem_mesh

