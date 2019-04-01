module UrbanParamsType

  !------------------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Urban Constants
  !
  ! !USES:
  use shr_kind_mod , only : r8 => shr_kind_r8
  use shr_log_mod  , only : errMsg => shr_log_errMsg
  use abortutils   , only : endrun
  use decompMod    , only : bounds_type
  use clm_varctl   , only : iulog, urban_properties, fsurdat
  use clm_varcon   , only : namel, grlnd, spval
  use LandunitType , only : lun                
  use shr_sys_mod  , only : shr_sys_flush
  use spmdMod      , only : masterproc
  !
  implicit none
  save
  private
  !
  ! !PUBLIC MEMBER FUNCTIONS:
  public  :: UrbanReadNML      ! Read in the urban namelist items
  public  :: UrbanInput        ! Read in urban input data
  public  :: CheckUrban        ! Check validity of urban points
  public  :: IsSimpleBuildTemp ! If using the simple building temperature method
  public  :: IsProgBuildTemp   ! If using the prognostic building temperature method
  public  :: mkurbanpar        ! Make urban parameter data
  !
  ! !PRIVATE TYPE
  type urbinp_type
     real(r8), pointer :: canyon_hwr      (:,:)  
     real(r8), pointer :: wtlunit_roof    (:,:)  
     real(r8), pointer :: wtroad_perv     (:,:)  
     real(r8), pointer :: em_roof         (:,:)   
     real(r8), pointer :: em_improad      (:,:)  
     real(r8), pointer :: em_perroad      (:,:)  
     real(r8), pointer :: em_wall         (:,:)  
     real(r8), pointer :: alb_roof_dir    (:,:,:)  
     real(r8), pointer :: alb_roof_dif    (:,:,:)  
     real(r8), pointer :: alb_improad_dir (:,:,:)  
     real(r8), pointer :: alb_improad_dif (:,:,:)  
     real(r8), pointer :: alb_perroad_dir (:,:,:)  
     real(r8), pointer :: alb_perroad_dif (:,:,:)  
     real(r8), pointer :: alb_wall_dir    (:,:,:)  
     real(r8), pointer :: alb_wall_dif    (:,:,:)  
     real(r8), pointer :: ht_roof         (:,:)
     real(r8), pointer :: wind_hgt_canyon (:,:)
     real(r8), pointer :: tk_wall         (:,:,:)
     real(r8), pointer :: tk_roof         (:,:,:)
     real(r8), pointer :: tk_improad      (:,:,:)
     real(r8), pointer :: cv_wall         (:,:,:)
     real(r8), pointer :: cv_roof         (:,:,:)
     real(r8), pointer :: cv_improad      (:,:,:)
     real(r8), pointer :: thick_wall      (:,:)
     real(r8), pointer :: thick_roof      (:,:)
     integer,  pointer :: nlev_improad    (:,:)
     real(r8), pointer :: t_building_min  (:,:)
  end type urbinp_type
  type (urbinp_type), public :: urbinp   ! urban input derived type

  ! !PUBLIC TYPE
  type, public :: urbanparams_type
     real(r8), allocatable :: wind_hgt_canyon     (:)   ! lun height above road at which wind in canyon is to be computed (m)
     real(r8), allocatable :: em_roof             (:)   ! lun roof emissivity
     real(r8), allocatable :: em_improad          (:)   ! lun impervious road emissivity
     real(r8), allocatable :: em_perroad          (:)   ! lun pervious road emissivity
     real(r8), allocatable :: em_wall             (:)   ! lun wall emissivity
     real(r8), allocatable :: alb_roof_dir        (:,:) ! lun direct  roof albedo
     real(r8), allocatable :: alb_roof_dif        (:,:) ! lun diffuse roof albedo
     real(r8), allocatable :: alb_improad_dir     (:,:) ! lun direct  impervious road albedo
     real(r8), allocatable :: alb_improad_dif     (:,:) ! lun diffuse impervious road albedo
     real(r8), allocatable :: alb_perroad_dir     (:,:) ! lun direct  pervious road albedo
     real(r8), allocatable :: alb_perroad_dif     (:,:) ! lun diffuse pervious road albedo
     real(r8), allocatable :: alb_wall_dir        (:,:) ! lun direct  wall albedo
     real(r8), allocatable :: alb_wall_dif        (:,:) ! lun diffuse wall albedo

     integer , pointer     :: nlev_improad        (:)   ! lun number of impervious road layers (-)
     real(r8), pointer     :: tk_wall             (:,:) ! lun thermal conductivity of urban wall (W/m/K)
     real(r8), pointer     :: tk_roof             (:,:) ! lun thermal conductivity of urban roof (W/m/K)
     real(r8), pointer     :: tk_improad          (:,:) ! lun thermal conductivity of urban impervious road (W/m/K)
     real(r8), pointer     :: cv_wall             (:,:) ! lun heat capacity of urban wall (J/m^3/K)
     real(r8), pointer     :: cv_roof             (:,:) ! lun heat capacity of urban roof (J/m^3/K)
     real(r8), pointer     :: cv_improad          (:,:) ! lun heat capacity of urban impervious road (J/m^3/K)
     real(r8), pointer     :: thick_wall          (:)   ! lun total thickness of urban wall (m)
     real(r8), pointer     :: thick_roof          (:)   ! lun total thickness of urban roof (m)

     real(r8), pointer     :: vf_sr               (:)   ! lun view factor of sky for road
     real(r8), pointer     :: vf_wr               (:)   ! lun view factor of one wall for road
     real(r8), pointer     :: vf_sw               (:)   ! lun view factor of sky for one wall
     real(r8), pointer     :: vf_rw               (:)   ! lun view factor of road for one wall
     real(r8), pointer     :: vf_ww               (:)   ! lun view factor of opposing wall for one wall

     real(r8), pointer     :: t_building_min      (:)   ! lun minimum internal building air temperature (K)
     real(r8), pointer     :: eflx_traffic_factor (:)   ! lun multiplicative traffic factor for sensible heat flux from urban traffic (-)
   contains

     procedure, public :: Init 
     
  end type urbanparams_type
  !
  ! !Urban control variables
  character(len= *), parameter, public :: urban_hac_off = 'OFF'                
  character(len= *), parameter, public :: urban_hac_on =  'ON'                 
  character(len= *), parameter, public :: urban_wasteheat_on = 'ON_WASTEHEAT'  
  character(len= 16), public           :: urban_hac = urban_hac_off
  logical, public                      :: urban_traffic = .false.     ! urban traffic fluxes

  ! !PRIVATE MEMBER DATA:
  logical, private    :: ReadNamelist = .false.     ! If namelist was read yet or not
  integer, parameter, private :: BUILDING_TEMP_METHOD_SIMPLE = 0       ! Simple method introduced in CLM4.5
  integer, parameter, private :: BUILDING_TEMP_METHOD_PROG   = 1       ! Prognostic method introduced in CLM5.0
  integer, private :: building_temp_method = BUILDING_TEMP_METHOD_PROG ! Method to calculate the building temperature

  ! flag to indicate nodata for index variables in output file:
  integer, parameter :: index_nodata = 0
  character(len=*), parameter :: modname = 'UrbanParamsType'

  private :: index_nodata
  private :: modname

  character(len=*), parameter, private :: sourcefile = &
       __FILE__
  !----------------------------------------------------------------------- 

contains

  !-----------------------------------------------------------------------
  subroutine Init(this, bounds)
    !
    ! Allocate module variables and data structures
    !
    ! !USES:
    use shr_infnan_mod  , only : nan => shr_infnan_nan, assignment(=)
    use clm_varpar      , only : nlevcan, nlevcan, numrad, nlevgrnd, nlevurb
    use clm_varpar      , only : nlevsoi, nlevgrnd
    use clm_varctl      , only : use_vancouver, use_mexicocity
    use clm_varcon      , only : vkc
    use column_varcon   , only : icol_roof, icol_sunwall, icol_shadewall
    use column_varcon   , only : icol_road_perv, icol_road_imperv, icol_road_perv
    use landunit_varcon , only : isturb_MIN
    !
    ! !ARGUMENTS:
    class(urbanparams_type) :: this
    type(bounds_type)      , intent(in)    :: bounds  
    !
    ! !LOCAL VARIABLES:
    integer             :: j,l,c,p,g       ! indices
    integer             :: nc,fl,ib        ! indices 
    integer             :: dindx           ! urban density type index
    integer             :: ier             ! error status
    real(r8)            :: sumvf           ! sum of view factors for wall or road
    real(r8), parameter :: alpha = 4.43_r8 ! coefficient used to calculate z_d_town
    real(r8), parameter :: beta = 1.0_r8   ! coefficient used to calculate z_d_town
    real(r8), parameter :: C_d = 1.2_r8    ! drag coefficient as used in Grimmond and Oke (1999)
    real(r8)            :: plan_ai         ! plan area index - ratio building area to plan area (-)
    real(r8)            :: frontal_ai      ! frontal area index of buildings (-)
    real(r8)            :: build_lw_ratio  ! building short/long side ratio (-)
    integer		:: begl, endl
    integer		:: begc, endc
    integer		:: begp, endp
    integer             :: begg, endg
    !---------------------------------------------------------------------

    begp = bounds%begp; endp = bounds%endp
    begc = bounds%begc; endc = bounds%endc
    begl = bounds%begl; endl = bounds%endl
    begg = bounds%begg; endg = bounds%endg

    ! Allocate urbanparams data structure

    if ( nlevurb > 0 )then
       allocate(this%tk_wall          (begl:endl,nlevurb))  ; this%tk_wall             (:,:) = nan
       allocate(this%tk_roof          (begl:endl,nlevurb))  ; this%tk_roof             (:,:) = nan
       allocate(this%cv_wall          (begl:endl,nlevurb))  ; this%cv_wall             (:,:) = nan
       allocate(this%cv_roof          (begl:endl,nlevurb))  ; this%cv_roof             (:,:) = nan
    end if
    allocate(this%t_building_min      (begl:endl))          ; this%t_building_min      (:)   = nan
    allocate(this%tk_improad          (begl:endl,nlevurb))  ; this%tk_improad          (:,:) = nan
    allocate(this%cv_improad          (begl:endl,nlevurb))  ; this%cv_improad          (:,:) = nan
    allocate(this%thick_wall          (begl:endl))          ; this%thick_wall          (:)   = nan
    allocate(this%thick_roof          (begl:endl))          ; this%thick_roof          (:)   = nan
    allocate(this%nlev_improad        (begl:endl))          ; this%nlev_improad        (:)   = huge(1)
    allocate(this%vf_sr               (begl:endl))          ; this%vf_sr               (:)   = nan
    allocate(this%vf_wr               (begl:endl))          ; this%vf_wr               (:)   = nan
    allocate(this%vf_sw               (begl:endl))          ; this%vf_sw               (:)   = nan
    allocate(this%vf_rw               (begl:endl))          ; this%vf_rw               (:)   = nan
    allocate(this%vf_ww               (begl:endl))          ; this%vf_ww               (:)   = nan
    allocate(this%wind_hgt_canyon     (begl:endl))          ; this%wind_hgt_canyon     (:)   = nan
    allocate(this%em_roof             (begl:endl))          ; this%em_roof             (:)   = nan
    allocate(this%em_improad          (begl:endl))          ; this%em_improad          (:)   = nan
    allocate(this%em_perroad          (begl:endl))          ; this%em_perroad          (:)   = nan
    allocate(this%em_wall             (begl:endl))          ; this%em_wall             (:)   = nan
    allocate(this%alb_roof_dir        (begl:endl,numrad))   ; this%alb_roof_dir        (:,:) = nan
    allocate(this%alb_roof_dif        (begl:endl,numrad))   ; this%alb_roof_dif        (:,:) = nan    
    allocate(this%alb_improad_dir     (begl:endl,numrad))   ; this%alb_improad_dir     (:,:) = nan       
    allocate(this%alb_perroad_dir     (begl:endl,numrad))   ; this%alb_perroad_dir     (:,:) = nan       
    allocate(this%alb_improad_dif     (begl:endl,numrad))   ; this%alb_improad_dif     (:,:) = nan       
    allocate(this%alb_perroad_dif     (begl:endl,numrad))   ; this%alb_perroad_dif     (:,:) = nan       
    allocate(this%alb_wall_dir        (begl:endl,numrad))   ; this%alb_wall_dir        (:,:) = nan    
    allocate(this%alb_wall_dif        (begl:endl,numrad))   ; this%alb_wall_dif        (:,:) = nan
    allocate(this%eflx_traffic_factor (begl:endl))          ; this%eflx_traffic_factor (:)   = nan

    ! Initialize time constant urban variables

    do l = bounds%begl,bounds%endl

       ! "0" refers to urban wall/roof surface and "nlevsoi" refers to urban wall/roof bottom
       if (lun%urbpoi(l)) then

          g = lun%gridcell(l)
          dindx = lun%itype(l) - isturb_MIN + 1

          this%wind_hgt_canyon(l) = urbinp%wind_hgt_canyon(g,dindx)
          do ib = 1,numrad
             this%alb_roof_dir   (l,ib) = urbinp%alb_roof_dir   (g,dindx,ib)
             this%alb_roof_dif   (l,ib) = urbinp%alb_roof_dif   (g,dindx,ib)
             this%alb_improad_dir(l,ib) = urbinp%alb_improad_dir(g,dindx,ib)
             this%alb_perroad_dir(l,ib) = urbinp%alb_perroad_dir(g,dindx,ib)
             this%alb_improad_dif(l,ib) = urbinp%alb_improad_dif(g,dindx,ib)
             this%alb_perroad_dif(l,ib) = urbinp%alb_perroad_dif(g,dindx,ib)
             this%alb_wall_dir   (l,ib) = urbinp%alb_wall_dir   (g,dindx,ib)
             this%alb_wall_dif   (l,ib) = urbinp%alb_wall_dif   (g,dindx,ib)
          end do
          this%em_roof   (l) = urbinp%em_roof   (g,dindx)
          this%em_improad(l) = urbinp%em_improad(g,dindx)
          this%em_perroad(l) = urbinp%em_perroad(g,dindx)
          this%em_wall   (l) = urbinp%em_wall   (g,dindx)

          ! Landunit level initialization for urban wall and roof layers and interfaces

          lun%canyon_hwr(l)   = urbinp%canyon_hwr(g,dindx)
          lun%wtroad_perv(l)  = urbinp%wtroad_perv(g,dindx)
          lun%ht_roof(l)      = urbinp%ht_roof(g,dindx)
          lun%wtlunit_roof(l) = urbinp%wtlunit_roof(g,dindx)

          this%tk_wall(l,:)      = urbinp%tk_wall(g,dindx,:)
          this%tk_roof(l,:)      = urbinp%tk_roof(g,dindx,:)
          this%tk_improad(l,:)   = urbinp%tk_improad(g,dindx,:)
          this%cv_wall(l,:)      = urbinp%cv_wall(g,dindx,:)
          this%cv_roof(l,:)      = urbinp%cv_roof(g,dindx,:)
          this%cv_improad(l,:)   = urbinp%cv_improad(g,dindx,:)
          this%thick_wall(l)     = urbinp%thick_wall(g,dindx)
          this%thick_roof(l)     = urbinp%thick_roof(g,dindx)
          this%nlev_improad(l)   = urbinp%nlev_improad(g,dindx)
          this%t_building_min(l) = urbinp%t_building_min(g,dindx)

          ! Inferred from Sailor and Lu 2004
          if (urban_traffic) then
             this%eflx_traffic_factor(l) = 3.6_r8 * (lun%canyon_hwr(l)-0.5_r8) + 1.0_r8
          else
             this%eflx_traffic_factor(l) = 0.0_r8
          end if

          if (use_vancouver .or. use_mexicocity) then
             ! Freely evolving
             this%t_building_min(l) = 200.00_r8
          else
             if (urban_hac == urban_hac_off) then
                ! Overwrite values read in from urbinp by freely evolving values
                this%t_building_min(l) = 200.00_r8
             end if
          end if

          !----------------------------------------------------------------------------------
          ! View factors for road and one wall in urban canyon (depends only on canyon_hwr)
          ! ---------------------------------------------------------------------------------------
          !                                                        WALL    |
          !                  ROAD                                          |
          !                                                         wall   |
          !          -----\          /-----   -             -  |\----------/
          !              | \  vsr   / |       |         r   |  | \  vww   /   s
          !              |  \      /  |       h         o   w  |  \      /    k
          !        wall  |   \    /   | wall  |         a   |  |   \    /     y
          !              |vwr \  / vwr|       |         d   |  |vrw \  / vsw 
          !              ------\/------       -             -  |-----\/-----
          !                   road                                  wall   |
          !              <----- w ---->                                    |
          !                                                    <---- h --->|
          !
          !    vsr = view factor of sky for road          vrw = view factor of road for wall
          !    vwr = view factor of one wall for road     vww = view factor of opposing wall for wall
          !                                               vsw = view factor of sky for wall
          !    vsr + vwr + vwr = 1                        vrw + vww + vsw = 1
          !
          ! Source: Masson, V. (2000) A physically-based scheme for the urban energy budget in 
          ! atmospheric models. Boundary-Layer Meteorology 94:357-397
          !
          ! - Calculate urban land unit aerodynamic constants using Macdonald (1998) as used in
          ! Grimmond and Oke (1999)
          ! ---------------------------------------------------------------------------------------
          
          ! road -- sky view factor -> 1 as building height -> 0 
          ! and -> 0 as building height -> infinity

          this%vf_sr(l) = sqrt(lun%canyon_hwr(l)**2 + 1._r8) - lun%canyon_hwr(l)
          this%vf_wr(l) = 0.5_r8 * (1._r8 - this%vf_sr(l))

          ! one wall -- sky view factor -> 0.5 as building height -> 0 
          ! and -> 0 as building height -> infinity

          this%vf_sw(l) = 0.5_r8 * (lun%canyon_hwr(l) + 1._r8 - sqrt(lun%canyon_hwr(l)**2+1._r8)) / lun%canyon_hwr(l)
          this%vf_rw(l) = this%vf_sw(l)
          this%vf_ww(l) = 1._r8 - this%vf_sw(l) - this%vf_rw(l)

          ! error check -- make sure view factor sums to one for road and wall
          sumvf = this%vf_sr(l) + 2._r8*this%vf_wr(l)
          if (abs(sumvf-1._r8) > 1.e-06_r8 ) then
             write (iulog,*) 'urban road view factor error',sumvf
             write (iulog,*) 'clm model is stopping'
             call endrun(decomp_index=l, clmlevel=namel, msg=errmsg(sourcefile, __LINE__))
          endif
          sumvf = this%vf_sw(l) + this%vf_rw(l) + this%vf_ww(l)
          if (abs(sumvf-1._r8) > 1.e-06_r8 ) then
             write (iulog,*) 'urban wall view factor error',sumvf
             write (iulog,*) 'clm model is stopping'
             call endrun(decomp_index=l, clmlevel=namel, msg=errmsg(sourcefile, __LINE__))
          endif

          !----------------------------------------------------------------------------------
          ! Calculate urban land unit aerodynamic constants using Macdonald (1998) as used in
          ! Grimmond and Oke (1999)
          !----------------------------------------------------------------------------------

          ! Calculate plan area index 
          plan_ai = lun%canyon_hwr(l)/(lun%canyon_hwr(l) + 1._r8)

          ! Building shape shortside/longside ratio (e.g. 1 = square )
          ! This assumes the building occupies the entire canyon length
          build_lw_ratio = plan_ai

          ! Calculate frontal area index
          frontal_ai = (1._r8 - plan_ai) * lun%canyon_hwr(l)

          ! Adjust frontal area index for different building configuration
          frontal_ai = frontal_ai * sqrt(1/build_lw_ratio) * sqrt(plan_ai)

          ! Calculate displacement height
          if (use_vancouver) then
             lun%z_d_town(l) = 3.5_r8
          else if (use_mexicocity) then
             lun%z_d_town(l) = 10.9_r8
          else
             lun%z_d_town(l) = (1._r8 + alpha**(-plan_ai) * (plan_ai - 1._r8)) * lun%ht_roof(l)
          end if

          ! Calculate the roughness length
          if (use_vancouver) then
             lun%z_0_town(l) = 0.35_r8
          else if (use_mexicocity) then
             lun%z_0_town(l) = 2.2_r8
          else
             lun%z_0_town(l) = lun%ht_roof(l) * (1._r8 - lun%z_d_town(l) / lun%ht_roof(l)) * &
                  exp(-1.0_r8 * (0.5_r8 * beta * C_d / vkc**2 * &
                  (1 - lun%z_d_town(l) / lun%ht_roof(l)) * frontal_ai)**(-0.5_r8))
          end if

       else ! Not urban point 

          this%eflx_traffic_factor(l) = spval
          this%t_building_min(l) = spval

          this%vf_sr(l) = spval
          this%vf_wr(l) = spval
          this%vf_sw(l) = spval
          this%vf_rw(l) = spval
          this%vf_ww(l) = spval

       end if
    end do

    ! Deallocate memory for urbinp datatype
    
    call UrbanInput(bounds%begg, bounds%endg, mode='finalize')

  end subroutine Init

  !-----------------------------------------------------------------------
  subroutine UrbanInput(begg, endg, mode)
    !
    ! !DESCRIPTION: 
    ! Allocate memory and read in urban input data
    !
    ! !USES:
    use clm_varpar      , only : numrad, numsolar, nlevurb
    use landunit_varcon , only : numurbl
    use fileutils       , only : getavu, relavu, getfil, opnfil
    use domainMod       , only : ldomain
    use ncdio_pio       , only : file_desc_t, ncd_io, ncd_inqvdlen, ncd_inqfdims 
    use ncdio_pio       , only : ncd_pio_openfile, ncd_pio_closefile, ncd_inqdid, ncd_inqdlen
    !
    ! !ARGUMENTS:
    implicit none
    integer, intent(in) :: begg, endg
    character(len=*), intent(in) :: mode
    !
    ! !LOCAL VARIABLES:
    character(len=256) :: locfn_fsurdat ! local file name for surface dataset
    character(len=256) :: locfn_uprop   ! local file name for urban properties dataset
    type(file_desc_t)  :: ncid       ! netcdf id
    integer :: dimid                 ! netCDF id
    integer :: nw,n,k,i,j,ns,g       ! indices
    integer :: nlevurb_i             ! input grid: number of urban vertical levels
    integer :: numrad_i              ! input grid: number of solar bands (VIS/NIR)
    integer :: numurbl_i             ! input grid: number of urban landunits
    integer :: ier,ret               ! error status
    logical :: readvar               ! true => variable is on dataset
    logical :: has_density_class     ! true => density_class dimension is on dataset
    logical :: urban_skip_abort_on_invalid_data_check
    integer ,pointer :: urban_region_id(:)
    real(r8),pointer :: urbn_classes_g(:,:)  ! percent cover of each urban class, as % of grid cell
    character(len=32) :: subname = 'UrbanInput' ! subroutine name
    !-----------------------------------------------------------------------

    if ( nlevurb == 0 ) return

    if (mode == 'initialize') then

       urban_skip_abort_on_invalid_data_check = .false.

       ns = endg - begg + 1

       allocate(urban_region_id(begg:endg))
       allocate(urbn_classes_g(begg:endg,numurbl))

       ! Read urban_region_id from surface dataset

       if (masterproc) then
          write(iulog,*)' Reading in URBAN_REGION_ID and PCT_URBAN from fsurdat file ...'
       end if
      
       if (masterproc) then
          write(iulog,*) 'Attempting to read surface boundary data .....'
          if (fsurdat == ' ') then
             write(iulog,*)'fsurdat must be specified'
             call endrun(msg=errMsg(sourcefile, __LINE__))
          end if
       end if

       call getfil( fsurdat, locfn_fsurdat, 0 )
       call ncd_pio_openfile (ncid, trim(locfn_fsurdat), 0)

       call ncd_io(ncid=ncid, varname='URBAN_REGION_ID', flag='read', data=urban_region_id, &
            dim1name=grlnd, readvar=readvar)
       if (.not. readvar) call endrun( msg= ' ERROR: URBAN_REGION_ID NOT on surfdata file'//errMsg(sourcefile, __LINE__))

       call ncd_io(ncid=ncid, varname='PCT_URBAN', flag='read', data=urbn_classes_g, &
            dim1name=grlnd, readvar=readvar)
       if (.not. readvar) call endrun( msg= ' ERROR: PCT_URBAN NOT on surfdata file'//errMsg(sourcefile, __LINE__))

       call ncd_pio_closefile(ncid)

       ! Read urban input data from urban properties file
       
       if (masterproc) then
          write(iulog,*)' Reading in urban input data from urban_properties file ...'
          call shr_sys_flush(iulog)
       end if

       if (masterproc) then
          write(iulog,*) 'Attempting to read urban properties data .....'
          if (urban_properties == ' ') then
             write(iulog,*)'urban_properties must be specified'
             call endrun(msg=errMsg(sourcefile, __LINE__))
          end if
       end if
       
       call getfil (urban_properties, locfn_uprop, 0)
       call ncd_pio_openfile (ncid, locfn_uprop, 0)

       ! Check whether this file has new-format urban data
       call ncd_inqdid(ncid, 'density_class', dimid, dimexist=has_density_class)

       ! If file doesn't have density_class, then it is old-format urban;
       ! in this case, set nlevurb to zero
       if (.not. has_density_class) then
         nlevurb = 0
         if (masterproc) write(iulog,*)'PCT_URBAN is not multi-density, nlevurb set to 0'
       end if

       if ( nlevurb == 0 ) return

       call ncd_inqdlen(ncid, dimid, numurbl_i)
       if (numurbl_i /= numurbl) then
          write(iulog,*)trim(subname)// ': parameter numurbl= ',numurbl, &
               'does not equal input dataset density class= ',numurbl_i
          call endrun(msg=errmsg(sourcefile, __LINE__))
       endif

       call ncd_pio_closefile(ncid)

       ! Allocate dynamic memory
       allocate(urbinp%canyon_hwr(begg:endg, numurbl), &  
                urbinp%wtlunit_roof(begg:endg, numurbl), &  
                urbinp%wtroad_perv(begg:endg, numurbl), &
                urbinp%em_roof(begg:endg, numurbl), &     
                urbinp%em_improad(begg:endg, numurbl), &    
                urbinp%em_perroad(begg:endg, numurbl), &    
                urbinp%em_wall(begg:endg, numurbl), &    
                urbinp%alb_roof_dir(begg:endg, numurbl, numrad), &    
                urbinp%alb_roof_dif(begg:endg, numurbl, numrad), &    
                urbinp%alb_improad_dir(begg:endg, numurbl, numrad), &    
                urbinp%alb_perroad_dir(begg:endg, numurbl, numrad), &    
                urbinp%alb_improad_dif(begg:endg, numurbl, numrad), &    
                urbinp%alb_perroad_dif(begg:endg, numurbl, numrad), &    
                urbinp%alb_wall_dir(begg:endg, numurbl, numrad), &    
                urbinp%alb_wall_dif(begg:endg, numurbl, numrad), &
                urbinp%ht_roof(begg:endg, numurbl), &
                urbinp%wind_hgt_canyon(begg:endg, numurbl), &
                urbinp%tk_wall(begg:endg, numurbl,nlevurb), &
                urbinp%tk_roof(begg:endg, numurbl,nlevurb), &
                urbinp%tk_improad(begg:endg, numurbl,nlevurb), &
                urbinp%cv_wall(begg:endg, numurbl,nlevurb), &
                urbinp%cv_roof(begg:endg, numurbl,nlevurb), &
                urbinp%cv_improad(begg:endg, numurbl,nlevurb), &
                urbinp%thick_wall(begg:endg, numurbl), &
                urbinp%thick_roof(begg:endg, numurbl), &
                urbinp%nlev_improad(begg:endg, numurbl), &
                urbinp%t_building_min(begg:endg, numurbl), &
                stat=ier)
       if (ier /= 0) then
          call endrun(msg="Allocation error "//errmsg(sourcefile, __LINE__))
       endif

       call mkurbanpar(datfname=urban_properties, region_o=urban_region_id, &
           urbn_classes_gcell_o=urbn_classes_g, &
           urban_skip_abort_on_invalid_data_check=urban_skip_abort_on_invalid_data_check) 

       if (masterproc) then
          write(iulog,*)' Sucessfully read urban input data' 
          write(iulog,*)
          call shr_sys_flush(iulog)
       end if

    else if (mode == 'finalize') then

       if ( nlevurb == 0 ) return

       deallocate(urbinp%canyon_hwr, &
                  urbinp%wtlunit_roof, &
                  urbinp%wtroad_perv, &
                  urbinp%em_roof, &
                  urbinp%em_improad, &
                  urbinp%em_perroad, &
                  urbinp%em_wall, &
                  urbinp%alb_roof_dir, &
                  urbinp%alb_roof_dif, &
                  urbinp%alb_improad_dir, &
                  urbinp%alb_perroad_dir, &
                  urbinp%alb_improad_dif, &
                  urbinp%alb_perroad_dif, &
                  urbinp%alb_wall_dir, &
                  urbinp%alb_wall_dif, &
                  urbinp%ht_roof, &
                  urbinp%wind_hgt_canyon, &
                  urbinp%tk_wall, &
                  urbinp%tk_roof, &
                  urbinp%tk_improad, &
                  urbinp%cv_wall, &
                  urbinp%cv_roof, &
                  urbinp%cv_improad, &
                  urbinp%thick_wall, &
                  urbinp%thick_roof, &
                  urbinp%nlev_improad, &
                  urbinp%t_building_min, &
                  stat=ier)
       if (ier /= 0) then
          call endrun(msg='initUrbanInput: deallocation error '//errmsg(sourcefile, __LINE__))
       end if
    else
       write(iulog,*)'initUrbanInput error: mode ',trim(mode),' not supported '
       call endrun(msg=errmsg(sourcefile, __LINE__))
    end if

  end subroutine UrbanInput

  !-----------------------------------------------------------------------
  subroutine CheckUrban(begg, endg, pcturb, caller)

    !-----------------------------------------------------------------------
    ! !DESCRIPTION:
    ! Confirm that we have valid urban data for all points with pct urban > 0. If this isn't
    ! true, abort with a message.
    !
    ! !USES:
    use clm_instur      , only : urban_valid
    use landunit_varcon , only : numurbl
    !
    ! !ARGUMENTS:
    implicit none
    integer         , intent(in) :: begg, endg           ! beg & end grid cell indices
    real(r8)        , intent(in) :: pcturb(begg:,:)      ! % urban
    character(len=*), intent(in) :: caller               ! identifier of caller, for more meaningful error messages
    !
    ! !REVISION HISTORY:
    ! Created by Bill Sacks 7/2013, mostly by moving code from surfrd_special
    !
    ! !LOCAL VARIABLES:
    logical :: found
    integer :: nl, n
    integer :: nindx, dindx
    integer :: nlev
    !-----------------------------------------------------------------------

    found = .false.
    do nl = begg,endg
       do n = 1, numurbl
          if ( pcturb(nl,n) > 0.0_r8 ) then
             if ( .not. urban_valid(nl) .or. &
                  urbinp%canyon_hwr(nl,n)            <= 0._r8 .or. &
                  urbinp%em_improad(nl,n)            <= 0._r8 .or. &
                  urbinp%em_perroad(nl,n)            <= 0._r8 .or. &
                  urbinp%em_roof(nl,n)               <= 0._r8 .or. &
                  urbinp%em_wall(nl,n)               <= 0._r8 .or. &
                  urbinp%ht_roof(nl,n)               <= 0._r8 .or. &
                  urbinp%thick_roof(nl,n)            <= 0._r8 .or. &
                  urbinp%thick_wall(nl,n)            <= 0._r8 .or. &
                  urbinp%t_building_min(nl,n)        <= 0._r8 .or. &
                  urbinp%wind_hgt_canyon(nl,n)       <= 0._r8 .or. &
                  urbinp%wtlunit_roof(nl,n)          <= 0._r8 .or. &
                  urbinp%wtroad_perv(nl,n)           <= 0._r8 .or. &
                  any(urbinp%alb_improad_dir(nl,n,:) <= 0._r8) .or. &
                  any(urbinp%alb_improad_dif(nl,n,:) <= 0._r8) .or. &
                  any(urbinp%alb_perroad_dir(nl,n,:) <= 0._r8) .or. &
                  any(urbinp%alb_perroad_dif(nl,n,:) <= 0._r8) .or. &
                  any(urbinp%alb_roof_dir(nl,n,:)    <= 0._r8) .or. &
                  any(urbinp%alb_roof_dif(nl,n,:)    <= 0._r8) .or. &
                  any(urbinp%alb_wall_dir(nl,n,:)    <= 0._r8) .or. &
                  any(urbinp%alb_wall_dif(nl,n,:)    <= 0._r8) .or. &
                  any(urbinp%tk_roof(nl,n,:)         <= 0._r8) .or. &
                  any(urbinp%tk_wall(nl,n,:)         <= 0._r8) .or. &
                  any(urbinp%cv_roof(nl,n,:)         <= 0._r8) .or. &
                  any(urbinp%cv_wall(nl,n,:)         <= 0._r8)) then
                found = .true.
                nindx = nl
                dindx = n
                exit
             else
                if (urbinp%nlev_improad(nl,n) > 0) then
                   nlev = urbinp%nlev_improad(nl,n)
                   if ( any(urbinp%tk_improad(nl,n,1:nlev) <= 0._r8) .or. &
                        any(urbinp%cv_improad(nl,n,1:nlev) <= 0._r8)) then
                      found = .true.
                      nindx = nl
                      dindx = n
                      exit
                   end if
                end if
             end if
             if (found) exit
          end if
       end do
    end do
    if ( found ) then
       write(iulog,*) trim(caller), ' ERROR: no valid urban data for nl=',nindx
       write(iulog,*)'density type:    ',dindx
       write(iulog,*)'urban_valid:     ',urban_valid(nindx)
       write(iulog,*)'canyon_hwr:      ',urbinp%canyon_hwr(nindx,dindx)
       write(iulog,*)'em_improad:      ',urbinp%em_improad(nindx,dindx)
       write(iulog,*)'em_perroad:      ',urbinp%em_perroad(nindx,dindx)
       write(iulog,*)'em_roof:         ',urbinp%em_roof(nindx,dindx)
       write(iulog,*)'em_wall:         ',urbinp%em_wall(nindx,dindx)
       write(iulog,*)'ht_roof:         ',urbinp%ht_roof(nindx,dindx)
       write(iulog,*)'thick_roof:      ',urbinp%thick_roof(nindx,dindx)
       write(iulog,*)'thick_wall:      ',urbinp%thick_wall(nindx,dindx)
       write(iulog,*)'t_building_min:  ',urbinp%t_building_min(nindx,dindx)
       write(iulog,*)'wind_hgt_canyon: ',urbinp%wind_hgt_canyon(nindx,dindx)
       write(iulog,*)'wtlunit_roof:    ',urbinp%wtlunit_roof(nindx,dindx)
       write(iulog,*)'wtroad_perv:     ',urbinp%wtroad_perv(nindx,dindx)
       write(iulog,*)'alb_improad_dir: ',urbinp%alb_improad_dir(nindx,dindx,:)
       write(iulog,*)'alb_improad_dif: ',urbinp%alb_improad_dif(nindx,dindx,:)
       write(iulog,*)'alb_perroad_dir: ',urbinp%alb_perroad_dir(nindx,dindx,:)
       write(iulog,*)'alb_perroad_dif: ',urbinp%alb_perroad_dif(nindx,dindx,:)
       write(iulog,*)'alb_roof_dir:    ',urbinp%alb_roof_dir(nindx,dindx,:)
       write(iulog,*)'alb_roof_dif:    ',urbinp%alb_roof_dif(nindx,dindx,:)
       write(iulog,*)'alb_wall_dir:    ',urbinp%alb_wall_dir(nindx,dindx,:)
       write(iulog,*)'alb_wall_dif:    ',urbinp%alb_wall_dif(nindx,dindx,:)
       write(iulog,*)'tk_roof:         ',urbinp%tk_roof(nindx,dindx,:)
       write(iulog,*)'tk_wall:         ',urbinp%tk_wall(nindx,dindx,:)
       write(iulog,*)'cv_roof:         ',urbinp%cv_roof(nindx,dindx,:)
       write(iulog,*)'cv_wall:         ',urbinp%cv_wall(nindx,dindx,:)
       if (urbinp%nlev_improad(nindx,dindx) > 0) then
          nlev = urbinp%nlev_improad(nindx,dindx)
          write(iulog,*)'tk_improad: ',urbinp%tk_improad(nindx,dindx,1:nlev)
          write(iulog,*)'cv_improad: ',urbinp%cv_improad(nindx,dindx,1:nlev)
       end if
       call endrun(msg=errmsg(sourcefile, __LINE__))
    end if

  end subroutine CheckUrban

  !-----------------------------------------------------------------------

  !-----------------------------------------------------------------------
  !BOP
  !
  ! !IROUTINE: UrbanReadNML
  !
  ! !INTERFACE:
  !
  subroutine UrbanReadNML ( NLFilename )
    !
    ! !DESCRIPTION:
    !
    ! Read in the urban namelist
    !
    ! !USES:
    use shr_mpi_mod   , only : shr_mpi_bcast
    use abortutils    , only : endrun
    use spmdMod       , only : masterproc, mpicom
    use fileutils     , only : getavu, relavu, opnfil
    use shr_nl_mod    , only : shr_nl_find_group_name
    implicit none
    !
    ! !ARGUMENTS:
    character(len=*), intent(IN) :: NLFilename ! Namelist filename
    !
    ! !LOCAL VARIABLES:
    integer :: ierr                 ! error code
    integer :: unitn                ! unit for namelist file
    character(len=32) :: subname = 'UrbanReadNML'  ! subroutine name

    namelist / clmu_inparm / urban_hac, urban_traffic, building_temp_method, &
                             urban_properties
    !EOP
    !-----------------------------------------------------------------------

    ! ----------------------------------------------------------------------
    ! Read namelist from input namelist filename
    ! ----------------------------------------------------------------------

    if ( masterproc )then

       unitn = getavu()
       write(iulog,*) 'Read in clmu_inparm  namelist'
       call opnfil (NLFilename, unitn, 'F')
       call shr_nl_find_group_name(unitn, 'clmu_inparm', status=ierr)
       if (ierr == 0) then
          read(unitn, clmu_inparm, iostat=ierr)
          if (ierr /= 0) then
             call endrun(msg="ERROR reading clmu_inparm namelist"//errmsg(sourcefile, __LINE__))
          end if
       else
          call endrun(msg="ERROR finding clmu_inparm namelist"//errmsg(sourcefile, __LINE__))
       end if
       call relavu( unitn )

    end if

    ! Broadcast namelist variables read in
    call shr_mpi_bcast(urban_hac,             mpicom)
    call shr_mpi_bcast(urban_traffic,         mpicom)
    call shr_mpi_bcast(building_temp_method,  mpicom)
    call shr_mpi_bcast(urban_properties,      mpicom)

    !
    if (urban_traffic) then
       write(iulog,*)'Urban traffic fluxes are not implemented currently'
       call endrun(msg=errMsg(sourcefile, __LINE__))
    end if
    !
    if ( masterproc )then
       write(iulog,*) '   urban air conditioning/heating and wasteheat   = ', urban_hac
       write(iulog,*) '   urban traffic flux   = ', urban_traffic
    end if
    !
    if ( masterproc )then
       if (urban_properties == ' ') then
          write(iulog,*) '   urban_properties dataset not set'
       else
          write(iulog,*) '   urban properties data = ',trim(urban_properties)
       end if
    end if

    ReadNamelist = .true.

  end subroutine UrbanReadNML

  !-----------------------------------------------------------------------

  !-----------------------------------------------------------------------
  !BOP
  !
  ! !IROUTINE: mkurbanpar
  !
  ! !INTERFACE:
  subroutine mkurbanpar(datfname, region_o, urbn_classes_gcell_o, urban_skip_abort_on_invalid_data_check)
  !
  ! !DESCRIPTION:
  ! Make Urban Parameter data
  !
  ! Note that, in a grid cell with region_o==r, parameter values are filled from region r
  ! for ALL density classes. Thus, the parameter variables have a numurbl dimension along
  ! with their other dimensions.
  !
  ! Note that we will have a 'nodata' value (given by the fill_val value associated with
  ! each parameter) wherever (1) we have a nodata value for region_o, or (2) the parameter
  ! has nodata for the given region/density combination in the input lookup table.
  !
  ! !USES:
   use mkindexmapMod   , only : dim_slice_type, lookup_2d_netcdf
   use mkncdio
   use clm_varpar      , only : numrad, numsolar, nlevurb
   use landunit_varcon , only : numurbl
!
! !ARGUMENTS:
   implicit none
   character(len=*)  , intent(in) :: datfname                  ! input data file name
   integer           , intent(in) :: region_o(:)               ! output grid: region ID (length: ns_o)
   real(r8)          , intent(in) :: urbn_classes_gcell_o(:,:) ! output grid: percent urban in each density class
                                                               ! (% of total grid cell area) (dimensions: ns_o, numurbl)
   logical           , intent(in) :: urban_skip_abort_on_invalid_data_check

! !CALLED FROM:
! subroutine UrbanInput in module UrbanParamsType
!
! !REVISION HISTORY:
! Author: Bill Sacks
!
!
! !LOCAL VARIABLES:
!EOP
   ! Type to store information about each urban parameter
   type param
      character(len=32) :: name          ! name in input & output files
      real(r8)          :: fill_val      ! value to put where we have no data in output
      logical           :: check_invalid ! should we check whether there are any invalid data in the output?
   end type param

   real(r8), allocatable :: data_scalar_o(:,:)   ! output array for parameters with no extra dimensions
   real(r8), allocatable :: data_rad_o(:,:,:,:)  ! output array for parameters dimensioned by numrad & numsolar
   real(r8), allocatable :: data_levurb_o(:,:,:) ! output array for parameters dimensioned by nlevurb
   integer , allocatable :: unity_dens_o(:,:)    ! artificial density indices
   integer  :: nlevurb_i                         ! input  grid: number of urban vertical levels
   integer  :: numsolar_i                        ! input  grid: number of solar type (DIR/DIF)
   integer  :: numrad_i                          ! input  grid: number of solar bands (VIS/NIR)
   integer  :: m,n,no,ns_o,p,k                   ! indices
   integer  :: ncidi,dimid,varid                 ! netCDF id's
   integer  :: ier                               ! error status
   character(len=nf_max_name) :: varname         ! variable name

   ! information on extra dimensions for lookup tables greater than 2-d:
   type(dim_slice_type), allocatable :: extra_dims(:)

   ! suffix for variables dimensioned by numsolar, for each value of numsolar:
   character(len=8), parameter :: solar_suffix(numsolar) = (/'_DIR', '_DIF'/)

   ! value to put where we have no data in output variables, for real-valued parameters
   real(r8), parameter :: fill_val_real = 0._r8

   ! To add a new urban parameter, simply add an element to one of the below lists
   ! (params_scalar, params_rad or params_levurb)

   ! Urban parameters with no extra dimensions
   type(param), parameter :: params_scalar(13) = &
        (/ param('CANYON_HWR', fill_val_real, .true.), &
           param('EM_IMPROAD', fill_val_real, .true.), &
           param('EM_PERROAD', fill_val_real, .true.), &
           param('EM_ROOF', fill_val_real, .true.), &
           param('EM_WALL', fill_val_real, .true.), &
           param('HT_ROOF', fill_val_real, .true.), &
           param('THICK_ROOF', fill_val_real, .true.), &
           param('THICK_WALL', fill_val_real, .true.), &
           param('T_BUILDING_MIN', fill_val_real, .true.), &
           param('WIND_HGT_CANYON', fill_val_real, .true.), &
           param('WTLUNIT_ROOF', fill_val_real, .true.), &
           param('WTROAD_PERV', fill_val_real, .true.), &

           ! Note that NLEV_IMPROAD is written as an integer, meaning that type conversion occurs
           ! by truncation. Thus we expect the values in the NLEV_IMPROAD lookup table to be exact;
           ! e.g., if a value were 1.99999 rather than 2.0000, it would be written as 1 instead of 2
           ! Also note: we use fill_val=-1 rather than 0, because 0 appears in the lookup table
           param('NLEV_IMPROAD', -1, .true.) /)

   ! Urban parameters dimensioned by numrad & numsolar
   type(param), parameter :: params_rad(4) = &
        (/ param('ALB_IMPROAD', fill_val_real, .true.), &
           param('ALB_PERROAD', fill_val_real, .true.), &
           param('ALB_ROOF', fill_val_real, .true.), &
           param('ALB_WALL', fill_val_real, .true.) /)

   ! Urban parameters dimensioned by nlevurb
   type(param), parameter :: params_levurb(6) = &
        (/ param('TK_ROOF', fill_val_real, .true.), &
           param('TK_WALL', fill_val_real, .true.), &
           param('CV_ROOF', fill_val_real, .true.), &
           param('CV_WALL', fill_val_real, .true.), &

           ! Impervious road thermal conductivity and heat capacity have varying levels of
           ! data. Thus, we expect to find some missing values in the lookup table -- we
           ! do not want to treat that as an error -- thus, we set check_invalid=.false.
           param('CV_IMPROAD', fill_val_real, .false.), &
           param('TK_IMPROAD', fill_val_real, .false.) /)


   character(len=*), parameter :: subname = 'mkurbanpar'
!-----------------------------------------------------------------------

   if (masterproc) then
      write (6,*) 'Attempting to make Urban Parameters .....'
      call shr_sys_flush(6)
   end if

   ! Determine & error-check array sizes
   ns_o = size(region_o)
   if (size(urbn_classes_gcell_o, 1) /= ns_o) then
      write(6,*) modname//':'//subname//' ERROR: array size mismatch'
      write(6,*) 'size(region_o) = ', size(region_o)
      write(6,*) 'size(urbn_classes_gcell_o, 1) = ', size(urbn_classes_gcell_o, 1)
      call abort()
   end if
   if (size(urbn_classes_gcell_o, 2) /= numurbl) then
      write(6,*) modname//':'//subname//' ERROR: array size mismatch'
      write(6,*) 'size(urbn_classes_gcell_o, 2) = ', size(urbn_classes_gcell_o, 2)
      write(6,*) 'numurbl = ', numurbl
   end if

   ! Read dimensions from input file

   if (masterproc) then
      write (6,*) 'Open urban parameter file: ', trim(datfname)
   end if
   call check_ret(nf_open(datfname, 0, ncidi), subname)
   call check_ret(nf_inq_dimid(ncidi, 'nlevurb', dimid), subname)
   call check_ret(nf_inq_dimlen(ncidi, dimid, nlevurb_i), subname)
   call check_ret(nf_inq_dimid(ncidi, 'numsolar', dimid), subname)
   call check_ret(nf_inq_dimlen(ncidi, dimid, numsolar_i), subname)
   call check_ret(nf_inq_dimid(ncidi, 'numrad', dimid), subname)
   call check_ret(nf_inq_dimlen(ncidi, dimid, numrad_i), subname)

   if (nlevurb_i /= nlevurb) then
      write(6,*)'MKURBANPAR: parameter nlevurb= ',nlevurb, &
           'does not equal input dataset nlevurb= ',nlevurb_i
      stop
   endif
   if (numsolar_i /= numsolar) then
      write(6,*)'MKURBANPAR: parameter numsolar= ',numsolar, &
           'does not equal input dataset numsolar= ',numsolar_i
      stop
   endif
   if (numrad_i /= numrad) then
      write(6,*)'MKURBANPAR: parameter numrad= ',numrad, &
           'does not equal input dataset numrad= ',numrad_i
      stop
   endif

   ! Create an array that will hold the density indices
   ! In a given grid cell, we output parameter values for all density classes, for the
   ! region of that grid cell. In order to do this while still using the lookup_2d
   ! routine, we create a dummy unity_dens_o array that contains the density values
   ! passed to the lookup routine.

   allocate(unity_dens_o(ns_o, numurbl))
   do k = 1, numurbl
      unity_dens_o(:,k) = k
   end do

   ! Handle urban parameters with no extra dimensions

   allocate(data_scalar_o(ns_o, numurbl), stat=ier)
   if (ier /= 0) then
      write(6,*)'mkurbanpar allocation error'; call abort()
   end if

   do p = 1, size(params_scalar)
      call lookup_and_check_err(params_scalar(p)%name, params_scalar(p)%fill_val, &
           params_scalar(p)%check_invalid, urban_skip_abort_on_invalid_data_check, &
           data_scalar_o, 0)
      if (params_scalar(p)%name == "CANYON_HWR") then
         urbinp%canyon_hwr = data_scalar_o
      else if (params_scalar(p)%name == "EM_IMPROAD") then
         urbinp%em_improad = data_scalar_o
      else if (params_scalar(p)%name == "EM_PERROAD") then
         urbinp%em_perroad = data_scalar_o
      else if (params_scalar(p)%name == "EM_ROOF") then
         urbinp%em_roof = data_scalar_o
      else if (params_scalar(p)%name == "EM_WALL") then
         urbinp%em_wall = data_scalar_o
      else if (params_scalar(p)%name == "HT_ROOF") then
         urbinp%ht_roof = data_scalar_o
      else if (params_scalar(p)%name == "THICK_ROOF") then
         urbinp%thick_roof = data_scalar_o
      else if (params_scalar(p)%name == "THICK_WALL") then
         urbinp%thick_wall = data_scalar_o
      else if (params_scalar(p)%name == "T_BUILDING_MIN") then
         urbinp%t_building_min = data_scalar_o
      else if (params_scalar(p)%name == "WIND_HGT_CANYON") then
         urbinp%wind_hgt_canyon = data_scalar_o
      else if (params_scalar(p)%name == "WTLUNIT_ROOF") then
         urbinp%wtlunit_roof = data_scalar_o
      else if (params_scalar(p)%name == "WTROAD_PERV") then
         urbinp%wtroad_perv = data_scalar_o
      else if (params_scalar(p)%name == "NLEV_IMPROAD") then
         urbinp%nlev_improad = data_scalar_o
      end if
   end do

   deallocate(data_scalar_o)

   ! Handle urban parameters dimensioned by numrad & numsolar

   allocate(data_rad_o(ns_o, numurbl, numrad, numsolar), stat=ier)
   if (ier /= 0) then
      write(6,*)'mkurbanpar allocation error'; call abort()
   end if

   allocate(extra_dims(2))
   extra_dims(1)%name = 'numrad'
   extra_dims(2)%name = 'numsolar'

   do p = 1, size(params_rad)
      do m = 1,numsolar
         extra_dims(2)%val = m
         do n = 1,numrad
            extra_dims(1)%val = n

            call lookup_and_check_err(params_rad(p)%name, params_rad(p)%fill_val, &
                 params_rad(p)%check_invalid, urban_skip_abort_on_invalid_data_check, &
                 data_rad_o(:,:,n,m), &
                 2, extra_dims)
         end do
      end do

      ! Special handling of numsolar: rather than outputting variables with a numsolar
      ! dimension, we output separate variables for each value of numsolar
      do m = 1,numsolar
         if (len_trim(params_rad(p)%name) + len_trim(solar_suffix(m)) > len(varname)) then
            write(6,*) 'variable name exceeds length of varname'
            write(6,*) trim(params_rad(p)%name)//trim(solar_suffix(m))
            call abort()
         end if
         varname = trim(params_rad(p)%name)//trim(solar_suffix(m))
         if (varname == "ALB_IMPROAD_DIR") then
            urbinp%alb_improad_dir = data_rad_o(:,:,:,m)
         else if (varname == "ALB_IMPROAD_DIF") then
            urbinp%alb_improad_dif = data_rad_o(:,:,:,m)
         else if (varname == "ALB_PERROAD_DIR") then
            urbinp%alb_perroad_dir = data_rad_o(:,:,:,m)
         else if (varname == "ALB_PERROAD_DIF") then
            urbinp%alb_perroad_dif = data_rad_o(:,:,:,m)
         else if (varname == "ALB_ROOF_DIR") then
            urbinp%alb_roof_dir = data_rad_o(:,:,:,m)
         else if (varname == "ALB_ROOF_DIF") then
            urbinp%alb_roof_dif = data_rad_o(:,:,:,m)
         else if (varname == "ALB_WALL_DIR") then
            urbinp%alb_wall_dir = data_rad_o(:,:,:,m)
         else if (varname == "ALB_WALL_DIF") then
            urbinp%alb_wall_dif = data_rad_o(:,:,:,m)
         end if
      end do
   end do

   deallocate(data_rad_o)
   deallocate(extra_dims)

   ! Handle urban parameters dimensioned by nlevurb

   allocate(data_levurb_o(ns_o, numurbl, nlevurb), stat=ier)
   if (ier /= 0) then
      write(6,*)'mkurbanpar allocation error'; call abort()
   end if

   allocate(extra_dims(1))
   extra_dims(1)%name = 'nlevurb'

   do p = 1, size(params_levurb)
      do n = 1,nlevurb
         extra_dims(1)%val = n

         call lookup_and_check_err(params_levurb(p)%name, params_levurb(p)%fill_val, &
              params_levurb(p)%check_invalid, &
              urban_skip_abort_on_invalid_data_check, data_levurb_o(:,:,n), &
              1, extra_dims)
         if (params_levurb(p)%name == "TK_ROOF") then
            urbinp%tk_roof(:,:,n) = data_levurb_o(:,:,n)
         else if (params_levurb(p)%name == "TK_WALL") then
            urbinp%tk_wall(:,:,n) = data_levurb_o(:,:,n)
         else if (params_levurb(p)%name == "CV_ROOF") then
            urbinp%cv_roof(:,:,n) = data_levurb_o(:,:,n)
         else if (params_levurb(p)%name == "CV_WALL") then
            urbinp%cv_wall(:,:,n) = data_levurb_o(:,:,n)
         else if (params_levurb(p)%name == "CV_IMPROAD") then
            urbinp%cv_improad(:,:,n) = data_levurb_o(:,:,n)
         else if (params_levurb(p)%name == "TK_IMPROAD") then
            urbinp%tk_improad(:,:,n) = data_levurb_o(:,:,n)
         end if
      end do

   end do

   deallocate(data_levurb_o)
   deallocate(extra_dims)


   call check_ret(nf_close(ncidi), subname)

   if (masterproc) then
      write (6,*) 'Successfully made Urban Parameters'
      write (6,*)
      call shr_sys_flush(6)
   end if

   deallocate(unity_dens_o)

contains
!------------------------------------------------------------------------------
  subroutine lookup_and_check_err(varname, fill_val, check_invalid, &
       urban_skip_abort_on_invalid_data_check, data, n_extra_dims, extra_dims)

   ! Wrapper to lookup_2d_netcdf: Loops over each density class, calling lookup_2d_netcdf
   ! with that density class and filling the appropriate slice of the data array. Also
   ! checks for any errors, aborting if there were any.
   !
   ! Note that the lookup_2d_netcdf routine is designed to work with a single value of
   ! each of the indices. However, we want to fill parameter values for ALL density
   ! classes. This is why we loop over density class in this routine.
   !
   ! Note: inherits a number of variables from the parent routine

      use mkindexmapMod, only : lookup_2d_netcdf

      implicit none
      character(len=*), intent(in) :: varname       ! name of lookup table
      real(r8)        , intent(in) :: fill_val      ! value to put where we have no data in output variables
      logical         , intent(in) :: check_invalid ! should we check whether there are any invalid data in the output?
      logical         , intent(in) :: urban_skip_abort_on_invalid_data_check

      real(r8)        , intent(out):: data(:,:)     ! output from lookup_2d_netcdf
      integer         , intent(in) :: n_extra_dims  ! number of extra dimensions in the lookup table

      ! slice to use if lookup table variable has more than 2 dimensions:
      type(dim_slice_type), intent(in), optional :: extra_dims(:)

      ! Local variables:

      integer :: k,n   ! indices
      integer :: ierr  ! error return code
      integer, parameter :: urban_invalid_region = 0   ! urban_region_id indicating invalid point

      do k = 1, numurbl
         ! In the following, note that unity_dens_o(:,k) has been constructed so that
         ! unity_dens_o(:,k)==k everywhere. Thus, we fill data(:,k) with the parameter
         ! values corresponding to density class k.
         ! Also note: We use invalid_okay=.true. because we fill all density classes,
         ! some of which may have invalid entries. Because doing so disables some error
         ! checking, we do our own error checking after the call.
         call lookup_2d_netcdf(ncidi, varname, .true., &
                               'density_class', 'region', n_extra_dims, &
                               unity_dens_o(:,k), region_o, fill_val, data(:,k), ierr, &
                               extra_dims=extra_dims, nodata=index_nodata, &
                               invalid_okay=.true.)

         if (ierr /= 0) then
            write(6,*) modname//':'//subname//' ERROR in lookup_2d_netcdf for ', &
                 trim(varname), ' class', k, ': err=', ierr
            call abort()
         end if

         if (check_invalid) then
            ! Make sure we have valid parameter values wherever we have non-zero urban cover
            do n = 1, ns_o
               ! This check assumes that fill_val doesn't appear in any of the valid entries
               ! of the lookup table
               if (urbn_classes_gcell_o(n,k) > 0. .and. data(n,k) == fill_val) then
                  write(6,*) modname//':'//subname//' ERROR: fill value found in output where urban cover > 0'
                  write(6,*) 'var: ', trim(varname)
                  write(6,*) 'class: ', k
                  write(6,*) 'n: ', n
                  write(6,*) 'region: ', region_o(n)
                  write(6,*) 'urbn_classes_gcell_o(n,k): ', urbn_classes_gcell_o(n,k)
                  if (.not. urban_skip_abort_on_invalid_data_check) then
                     ! NOTE(bja, 2015-01) added to work around a ?bug? noted in
                     ! /glade/p/cesm/cseg/inputdata/lnd/clm2/surfdata_map/README_c141219
                     call abort()
                  end if
               end if
            end do
         end if

      end do

   end subroutine lookup_and_check_err

end subroutine mkurbanpar

  !-----------------------------------------------------------------------

  !-----------------------------------------------------------------------
  !BOP
  !
  ! !IROUTINE: IsSimpleBuildTemp
  !
  ! !INTERFACE:
  !
  logical function IsSimpleBuildTemp( )
    !
    ! !DESCRIPTION:
    !
    ! If the simple building temperature method is being used
    !
    ! !USES:
    implicit none
    !EOP
    !-----------------------------------------------------------------------

    if ( .not. ReadNamelist )then
       write(iulog,*)'Testing on building_temp_method before urban namelist was read in'
       call endrun(msg=errMsg(sourcefile, __LINE__))
    end if
    IsSimpleBuildTemp = building_temp_method == BUILDING_TEMP_METHOD_SIMPLE

  end function IsSimpleBuildTemp

  !-----------------------------------------------------------------------

  !-----------------------------------------------------------------------
  !BOP
  !
  ! !IROUTINE: IsProgBuildTemp
  !
  ! !INTERFACE:
  !
  logical function IsProgBuildTemp( )
    !
    ! !DESCRIPTION:
    !
    ! If the prognostic building temperature method is being used
    !
    ! !USES:
    implicit none
    !EOP
    !-----------------------------------------------------------------------

    if ( .not. ReadNamelist )then
       write(iulog,*)'Testing on building_temp_method before urban namelist was read in'
       call endrun(msg=errMsg(sourcefile, __LINE__))
    end if
    IsProgBuildTemp = building_temp_method == BUILDING_TEMP_METHOD_PROG

  end function IsProgBuildTemp

  !-----------------------------------------------------------------------

end module UrbanParamsType




