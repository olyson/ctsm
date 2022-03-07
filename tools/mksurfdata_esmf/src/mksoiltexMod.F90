module mksoiltexMod

  !-----------------------------------------------------------------------
  ! Make soil data (texture)
  !-----------------------------------------------------------------------

  use ESMF
  use pio
  use shr_kind_mod     , only : r8 => shr_kind_r8, r4=>shr_kind_r4
  use shr_sys_mod      , only : shr_sys_abort
  use mkpioMod         , only : mkpio_get_rawdata, mkpio_get_dimlengths
  use mkpioMod         , only : pio_iotype, pio_iosystem
  use mkesmfMod        , only : regrid_rawdata, create_routehandle_r8
  use mkutilsMod       , only : chkerr
  use mkfileMod        , only : mkfile_output  
  use mkvarctl         , only : root_task, ndiag, spval
  use mkvarpar         , only : nlevsoi

  implicit none
  private ! By default make data private

  public :: mksoiltex      ! Set soil texture

  integer                :: num_soil_textures

  character(len=*) , parameter :: u_FILE_u = &
       __FILE__

!=================================================================================
contains
!=================================================================================

  subroutine mksoiltex(file_mesh_i, file_data_i, mesh_o, pioid_o, pctlnd_pft_o, rc)
    !
    ! make %sand and %clay from SoilGrids soil data
    !
    ! input/output variables
    character(len=*)  , intent(in)    :: file_mesh_i ! input mesh file name
    character(len=*)  , intent(in)    :: file_data_i ! input data file name
    type(ESMF_Mesh)   , intent(in)    :: mesh_o      ! output mesh
    type(file_desc_t) , intent(inout) :: pioid_o
    real(r8)          , intent(in)    :: pctlnd_pft_o(:) ! PFT data: % of gridcell for PFTs
    integer           , intent(out)   :: rc

    ! local variables
    type(ESMF_RouteHandle) :: routehandle
    type(ESMF_Mesh)        :: mesh_i
    type(file_desc_t)      :: pioid_i
    integer                :: ni,no
    integer                :: ns_i, ns_o
    integer                :: k,l,m,n
    integer                :: nlay         ! number of soil layers
    integer                :: ndims
    integer , allocatable  :: dimlengths(:)
    integer , allocatable  :: mask_i(:)
    real(r4), allocatable  :: rmask_i(:)
    real(r8), allocatable  :: frac_o(:)
    real(r8), allocatable  :: data_o1(:,:) ! temporary for sand_o
    real(r8), allocatable  :: data_o2(:,:) ! temporary for clay_o
    real(r8), allocatable  :: sand_i(:,:)  ! input grid: percent sand
    real(r8), allocatable  :: clay_i(:,:)  ! input grid: percent clay
    real(r8), allocatable  :: sand_o(:,:)  ! % sand (output grid)
    real(r8), allocatable  :: clay_o(:,:)  ! % clay (output grid)
    integer                :: rcode, ier   ! error status
    character(len=*), parameter :: subname = 'mksoiltex'
    !-----------------------------------------------------------------------

    rc = ESMF_SUCCESS

    if (root_task) then
       write(ndiag,*)
       write(ndiag,'(1x,80a1)') ('=',k=1,80)
       write(ndiag,*)
       write(ndiag,'(a)') 'Attempting to make %sand and %clay .....'
       write(ndiag,'(a)') ' Input file is '//trim(file_data_i)
       write(ndiag,'(a)') ' Input mesh file is '//trim(file_mesh_i)
    end if

    ! Determine ns_o and allocate output data
    call ESMF_MeshGet(mesh_o, numOwnedElements=ns_o, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    allocate(sand_o(ns_o,nlevsoi)) ; sand_o(:,:) = spval
    allocate(clay_o(ns_o,nlevsoi)) ; clay_o(:,:) = spval

    ! Open input data file
    rcode = pio_openfile(pio_iosystem, pioid_i, pio_iotype, trim(file_data_i), pio_nowrite)
    call ESMF_VMLogMemInfo("After pio_openfile for "//trim(file_data_i))

    ! Get dimensions of raw data.
    !  - raw sand and clay data have dimensions (lon,lat,lev)
    !  - just read sand and assume clay has same dimensions
    !  - input read from pio has dimensions(n,lev)
    allocate(dimlengths(3))
    call mkpio_get_dimlengths(pioid_i, 'sand_10L', ndims, dimlengths)
    nlay = dimlengths(3)
    deallocate(dimlengths)

    ! Read in input mesh
    call ESMF_VMLogMemInfo("Before create mesh_i in "//trim(subname))
    mesh_i = ESMF_MeshCreate(filename=trim(file_mesh_i), fileformat=ESMF_FILEFORMAT_ESMFMESH, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_VMLogMemInfo("After create mesh_i in "//trim(subname))

    ! Determine ns_i
    call ESMF_MeshGet(mesh_i, numOwnedElements=ns_i, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return

    ! Get the landmask from the file and reset the mesh mask based on that
    allocate(rmask_i(ns_i), stat=ier)
    if (ier/=0) call shr_sys_abort()
    allocate(mask_i(ns_i), stat=ier)
    if (ier/=0) call shr_sys_abort()
    call mkpio_get_rawdata(pioid_i, 'LANDMASK', mesh_i, rmask_i, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    do ni = 1,ns_i
       if (rmask_i(ni) > 0._r4) then
          mask_i(ni) = 1
       else
          mask_i(ni) = 0
       end if
    end do
    call ESMF_MeshSet(mesh_i, elementMask=mask_i, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    ! Create a route handle between the input and output mesh and get frac_o
    allocate(frac_o(ns_o),stat=ier)
    if (ier/=0) call shr_sys_abort()
    call create_routehandle_r8(mesh_i, mesh_o, routehandle, frac_o=frac_o, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_VMLogMemInfo("After create routehandle in "//trim(subname))
    do n = 1, ns_o
       if ((frac_o(n) < 0.0) .or. (frac_o(n) > 1.0001)) then
          write(6,*) "ERROR:: frac_o out of range: ", frac_o(n),n
       end if
    end do

    ! Read in input data
    ! - levels are the innermost dimension for esmf fields
    ! - levels are the outermost dimension in pio reads
    ! Input data is read into (ns_i,nlay) array and then transferred to sand_i(nlay,ns_i)
    ! and clay_i(nlay,ns_i)
    allocate(sand_i(nlay,ns_i),stat=ier)
    if (ier/=0) call shr_sys_abort()
    allocate(clay_i(nlay,ns_i),stat=ier)
    if (ier/=0) call shr_sys_abort()
    call mkpio_get_rawdata(pioid_i, 'sand_10L', mesh_i, sand_i, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call mkpio_get_rawdata(pioid_i, 'clay_10L', mesh_i, clay_i, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return
    call ESMF_VMLogMemInfo("After mkpio_getrawdata in "//trim(subname))

    ! Regrid sand_i to data_o1
    allocate(data_o1(nlay,ns_o),stat=ier)
    if (ier/=0) call shr_sys_abort()
    call regrid_rawdata(mesh_i, mesh_o, routehandle, sand_i, data_o1, 1, nlay, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_VMLogMemInfo("After regrid sand_i in  "//trim(subname))

    ! Regrid clay_i to data_o2
    allocate(data_o2(nlay,ns_o),stat=ier)
    if (ier/=0) call shr_sys_abort()
    call regrid_rawdata(mesh_i, mesh_o, routehandle, clay_i, data_o2, 1, nlay, rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    call ESMF_VMLogMemInfo("After regrid clay_i in  "//trim(subname))

    do l = 1,nlevsoi
       do no = 1,ns_o
          sand_o(no,l) = data_o1(l,no)
          clay_o(no,l) = data_o2(l,no)
       end do
    end do

    ! Write out fields
    if (root_task)  write(ndiag, '(a)') trim(subname)//" writing out soil percent sand"
    call mkfile_output(pioid_o,  mesh_o,  'PCT_SAND', sand_o, lev1name='nlevsoi', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) call shr_sys_abort('error in calling mkfile_output')

    if (root_task)  write(ndiag, '(a)') trim(subname)//" writing out soil percent clay"
    call mkfile_output(pioid_o,  mesh_o,  'PCT_CLAY', clay_o, lev1name='nlevsoi', rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) call shr_sys_abort('error in calling mkfile_output')
    call pio_syncfile(pioid_o)

    ! Close the file
    call pio_closefile(pioid_i)
    call ESMF_VMLogMemInfo("After pio_closefile in "//trim(subname))

    ! Release memory
    call ESMF_RouteHandleDestroy(routehandle, nogarbage = .true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) call shr_sys_abort()
    call ESMF_MeshDestroy(mesh_i, nogarbage = .true., rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) call shr_sys_abort()
    call ESMF_VMLogMemInfo("After destroy operations in "//trim(subname))

    if (root_task) then
       write (ndiag,'(a)') 'Successfully made %sand and %clay'
    end if

  end subroutine mksoiltex

end module mksoiltexMod
