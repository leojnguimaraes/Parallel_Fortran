      subroutine write_gid_mesh

      use fem_mesh

      implicit none

      integer :: i, j, idim, load_type, ndat_loc, ndescr
      double precision :: step_val

      if (ndim==2) write(file_gid_msh,5)
  5   format('MESH "CBmsh" dimension 2 ElemType Triangle Nnode 3'/)

      if (ndim==3) write(file_gid_msh,6)
  6   format('MESH "CBmsh" dimension 3 ElemType Tetrahedra Nnode 4'/)


      write(file_gid_msh,11)
 11   format('Coordinates')
      do i=1,numnp
         write(file_gid_msh,12)i,(mesh_new%coord(idim,i),idim=1,ndim)
      end do
 12   format(i10,' ',3(1x,f30.20))

      write(file_gid_msh,13)
 13   format('end coordinates'//'Elements')
      do i=1,numel
        write(file_gid_msh,14)i,(mesh_new%kxx(j,i),                     &
     &                        j=1,mesh_new%lnodes(i)),mesh_new%mtype(i)
      enddo
 14   format (40(1x,i9))
      write(file_gid_msh,15)
 15   format('end elements')

      if (ndim==2) write(file_gid_res,18)
 18   format(/                                                          &
     &'  GaussPoints "PoroFlux" ElemType Triangle "CBmsh"'/             &
     &'  Number Of Gauss Points: 1         '/                           &
     &'  Natural Coordinates: Internal     '/                           &
     &'  End gausspoints'//                                             &
     &'  GaussPoints "Stress" ElemType Triangle "CBmsh"'/               &
     &'  Number Of Gauss Points: 1         '/                           &
     &'  Natural Coordinates: Internal     '/                           &
     &'  End gausspoints'//                                             &
     &'  GaussPoints "LiqSat" ElemType Triangle "CBmsh"'/               &
     &'  Number Of Gauss Points: 3         '/                           &
     &'  Natural Coordinates: Given        '/                           &
     &'     0  0        '/                                              &
     &'     1  0        '/                                              &
     &'     0  1        '/                                              &
     &'  End gausspoints'/)

      if (ndim==3) write(file_gid_res,19)
 19   format(/                                                          &
     &'  GaussPoints "PoroFlux" ElemType Tetrahedra "CBmsh"'/           &
     &'  Number Of Gauss Points: 1         '/                           &
     &'  Natural Coordinates: Internal     '/                           &
     &'  End gausspoints'//                                             &
     &'  GaussPoints "Stress" ElemType Tetrahedra "CBmsh"'/             &
     &'  Number Of Gauss Points: 1         '/                           &
     &'  Natural Coordinates: Internal     '/                           &
     &'  End gausspoints'//                                             &
     &'  GaussPoints "LiqSat" ElemType Tetrahedra "CBmsh"'/             &
     &'  Number Of Gauss Points: 4        '/                            &
     &'  Natural Coordinates: Given        '/                           &
     &'     0  0  0     '/                                              &
     &'     1  0  0     '/                                              &
     &'     0  1  0     '/                                              &
     &'     0  0  1     '/                                              &
     &'  End gausspoints'/)

!**** Nodes:

      load_type=1    ! kind of analysis (GiD) ->1= time-step
      step_val=0.0d0 ! time
      ndat_loc=1     ! position of data (GiD), 1 -> on the nodes
      ndescr=0       ! description of each component (GiD), 0 -> no descr

      do j=1,ndim
         write(file_gid_res,20)j,load_type,step_val,1,ndat_loc,ndescr
  20     format(1x,'  BCOND_Mech_',i1,i5,e15.6,3i5)
         do i=1,numnp
            write(file_gid_res,30)i,mesh_new%ifordisp(j,i)
         end do
  30     format (   i10  ,(1x,i5))
      enddo

      write(file_gid_res,40)load_type,step_val,1,ndat_loc,ndescr
  40  format(1x,'  BCOND_Flux',i5,e15.6,3i5)
      do i=1,numnp
         write(file_gid_res,30)i,mesh_new%ifluxtype(i)
      end do

      write(file_gid_res,50)load_type,step_val,1,ndat_loc,ndescr
  50  format(1x,'PARTITION_NODE ',i5,e15.6,3i5)
      do i=1,numnp
         write(file_gid_res,30)i,mesh_new%partition_node(i)
      end do

!**** Elements:

      load_type=1
      step_val=0.0d0
      ndat_loc=2 
      ndescr=0   

      write(file_gid_res,60)load_type,step_val,1,ndat_loc,ndescr
 60   format(1x,' PARTITION_EL ',i5,e15.6,3i5,' "PoroFlux"')
      do i=1,numel
        write(file_gid_res,30)i,mesh_new%partition_element(i)
      end do


      return

      end subroutine write_gid_mesh

!.......................................................................

      subroutine write_gid_results

      use fem_mesh

      implicit none

      return

      end subroutine write_gid_results
