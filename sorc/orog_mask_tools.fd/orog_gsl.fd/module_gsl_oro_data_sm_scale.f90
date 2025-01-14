!> @file
!! @brief Calculates small-scale GWD orographic stats for FV3GFS grids
!! @author Michael Toy, NOAA/GSL
!! @date 2021-03-12
!!
!! This module calculates the parameters required for the subgrid-
!! scale orographic gravity-wave drag (GWDO) scheme on the FV3
!! grid.  These parameters are for the small-scale GWD (Tsiringakis et al.,
!! 2017) and the turbulent orographic form drag (TOFD) (Beljaars, 2004)
!! schemes of the GSL drag suite.  30 second (~1km) global topography
!! is used.  The topographic data comes from the 'fix' file
!! HGT.Beljaars_filtered.lat-lon.30s_res.nc.
!! The output fields are:
!! - stddev      standard deviation of subgrid-scale topograpy
!! - convexity   convexity (kurtosis) of subgrid-scale topography
!! - ol{1,2,3,4} orographic effective lengths of subgrid-scale topography
!!   for 4 orientations: 1-westerly, 2-southerly, 3-southwesterly, 4-northwesterly
!! - oa{1,2,3,4} orographic asymmetries of subgrid-scale topography
!!   for 4 orientations: 1-westerly, 2-southerly, 3-southwesterly, 4-northwesterly
!!
!! Based on code by Michael Duda provided by NCAR/MMM
!!
module gsl_oro_data_sm_scale

implicit none

integer, parameter :: real_kind = selected_real_kind(6) !< single precision
integer, parameter :: dbl_kind = selected_real_kind(13) !< double precision

real, parameter :: pi = 3.1415926535897_real_kind !< pi
integer :: dimX_fine !< x-dimension of fine grid
integer :: dimY_fine !< y-dimension of fine grid

real (kind = real_kind), allocatable :: lat1d_fine(:) !< latitude of fine grid pts
real (kind = real_kind), allocatable :: lon1d_fine(:) !< longitude of fine grid pts

real (kind = real_kind), parameter :: p5 = 0.5_real_kind !< one half

contains

!> Subroutine to compute orographic statistics needed for small-scale
!! orograhic drag (gravity wave and form drag) schemes
!!
!! @param[in] tile_num (tile number)
!! @param[in] res_indx (resolution)
!! @param[in] halo (halo number)
!! @param[out] duplicate_oro_data_file (equals .true. if min grid size <= 7.5km)
!! @author Michael Toy, NOAA/GSL
subroutine calc_gsl_oro_data_sm_scale(tile_num,res_indx,halo,        &
                                          duplicate_oro_data_file)

use netcdf

implicit none

character(len=2), intent(in) :: tile_num   ! tile number
character(len=7), intent(in) :: res_indx   ! grid-resolution index, e.g., 96, 192, 384, etc 
character(len=4), intent(in) :: halo       ! halo value (for input grid data)

logical, intent(out) :: duplicate_oro_data_file  ! flag to let main program know that
                                      ! oro_data_ls file was created by this subroutine
                                      ! as dupliate of oro_data_ss file due to grid size
                                      ! being below 7.5km

real (kind = real_kind) :: min_area_FV3  ! minimum grid area in m^2
real (kind = real_kind) :: min_DX        ! minimum grid size in km

integer :: i,j,ii,jj

integer :: ncid_in,ncid_out,err
integer :: varid
integer :: dimid,latid,lonid
integer, dimension (2) :: dimids

integer :: nfinepoints   ! number of fine grid points in each coarse grid cell

real (kind = real_kind) :: sum2, sum4, var


real (kind = real_kind), allocatable ::                               &
           zs(:,:)
           
logical :: zs_accum

real (kind = real_kind) :: zs_mean
real (kind = real_kind), allocatable ::                               &
           std_dev(:),convexity(:),                &
           OA1(:),OA2(:),OA3(:),OA4(:),            &
           OL1(:),OL2(:),OL3(:),OL4(:)

real (kind = real_kind), parameter :: max_convexity = 10._real_kind  ! max value for convexity

integer :: nu, nd, nw, nt
real (kind = real_kind) :: ratio


real, parameter :: ae = 6371200._real_kind   ! Earth radius in meters

character(len=35)  :: FV3_grid_input_file_name
character(len=150) :: fine_topo_source_file_name
character(len=35)  :: oro_data_output_file_name

integer :: temp_int, dimX_FV3, dimY_FV3
real (kind = dbl_kind),  allocatable :: lat_FV3_raw(:,:), lon_FV3_raw(:,:)
real (kind = real_kind), allocatable :: lat_FV3(:,:), lon_FV3(:,:)
real (kind = real_kind), allocatable :: lat_FV3_deg(:,:)  ! saved version of latitude for output
real (kind = real_kind), allocatable :: lon_FV3_deg(:,:)  ! saved version of longitude for output
real (kind = dbl_kind),  allocatable :: area_FV3_qtr(:,:)  ! meters squared
real (kind = real_kind), allocatable :: area_FV3(:,:)     ! meters squared
real (kind = real_kind), allocatable :: HGT_M_fine(:,:)
real (kind = real_kind) :: dlta_lat, dlta_lon

integer :: i_blk, j_blk
integer :: ii_loc, jj_loc, ii_m, jj_m
integer, dimension(3) :: s_ii, e_ii, s_jj, e_jj
real (kind = real_kind), dimension(3) :: lat_blk, lon_blk
integer :: cell_count  ! allows for use of 1D arrays for GWD statistics fields
integer :: halo_int    ! integer form of halo

logical :: fexist

print *, "Creating oro_data_ss file"
print *

! File name for file that contains grid information
if ( halo.eq."-999" ) then  ! global or nested tile
   FV3_grid_input_file_name = "C" // trim(res_indx) // "_grid.tile" //  &
                                     trim(tile_num) // ".nc"
else   ! stand-alone regional tile
   FV3_grid_input_file_name = "C" // trim(res_indx) // "_grid.tile" //  &
                   trim(tile_num) // ".halo" // trim(halo) // ".nc"
end if

print *, "Reading from file: ", FV3_grid_input_file_name


! Check that input file exists -- exit with error message if not
inquire(file=FV3_grid_input_file_name,exist=fexist)
if (.not.fexist) then
   print *
   print *, "Fatal error: Input file "//trim(FV3_grid_input_file_name)// &
            " does not exist"
   print *, "Exiting program gsl_oro_data.f90"
   print *
   call exit(4)
end if 


! In preparation for reading in grid data, account for existence
! of halo points
if ( halo.eq."-999" ) then   ! global or nested tile
   halo_int = 0
else
   read(halo,*) halo_int     ! integer form of halo
end if


! Open Cxxx_grid netCDF file for input and get dimensions
err = nf90_open(FV3_grid_input_file_name,nf90_nowrite,ncid_in)
call netcdf_err(err, 'opening: '//FV3_grid_input_file_name)

err = nf90_inq_dimid(ncid_in,'nx',dimid)
call netcdf_err(err, 'reading nx id')
err = nf90_inquire_dimension(ncid_in,dimid,len=temp_int)
call netcdf_err(err, 'reading nx value')
dimX_FV3 = temp_int/2 - 2*halo_int  ! shaving off any halo points from edges

err = nf90_inq_dimid(ncid_in,'ny',dimid)
call netcdf_err(err, 'reading ny id')
err = nf90_inquire_dimension(ncid_in,dimid,len=temp_int)
call netcdf_err(err, 'reading ny value')
dimY_FV3 = temp_int/2 - 2*halo_int  ! shaving off any halo points from edges

print *, "dimX_FV3 =", dimX_FV3  ! number of model cells in x-direction (halo0)
print *, "dimY_FV3 =", dimY_FV3  ! number of model cells in y-direction (halo0)
print *

! Read in lat/lon (in degrees)
allocate (lat_FV3_raw((2*dimX_FV3+1),(2*dimY_FV3+1)))
err = nf90_inq_varid(ncid_in,'y',varid)
call netcdf_err(err, 'reading y id')
err = nf90_get_var(ncid_in,varid,lat_FV3_raw,                        &
                   start=(/1+2*halo_int,1+2*halo_int/),              &
                   count=(/2*dimX_FV3+1,2*dimY_FV3+1/))
call netcdf_err(err, 'reading y')

allocate (lon_FV3_raw((2*dimX_FV3+1),(2*dimY_FV3+1)))
err = nf90_inq_varid(ncid_in,'x',varid)
call netcdf_err(err, 'reading x id')
err = nf90_get_var(ncid_in,varid,lon_FV3_raw,                        &
                   start=(/1+2*halo_int,1+2*halo_int/),              &
                   count=(/2*dimX_FV3+1,2*dimY_FV3+1/))
call netcdf_err(err, 'reading x')

! Read in quarter grid-cell areas
allocate (area_FV3_qtr((2*dimX_FV3),(2*dimY_FV3)))
err = nf90_inq_varid(ncid_in,'area',varid)
call netcdf_err(err, 'reading area id')
err = nf90_get_var(ncid_in,varid,area_FV3_qtr,                       &
                   start=(/1+2*halo_int,1+2*halo_int/),              &
                   count=(/2*dimX_FV3,2*dimY_FV3/))
call netcdf_err(err, 'reading area')

! Calculate lat/lon at mass points (cell-centers)
! Stride by 2 starting with 2nd point
! NOTE:  "Converting" from dbl_kind to real_kind
allocate (lat_FV3(dimX_FV3,dimY_FV3))
allocate (lon_FV3(dimX_FV3,dimY_FV3))
allocate (lat_FV3_deg(dimX_FV3,dimY_FV3))
allocate (lon_FV3_deg(dimX_FV3,dimY_FV3))
do j = 1,dimY_FV3
   do i = 1,dimX_FV3
      lat_FV3(i,j) = lat_FV3_raw(2*i,2*j)
      lon_FV3(i,j) = lon_FV3_raw(2*i,2*j)
   end do
end do
lat_FV3_deg(:,:) = lat_FV3(:,:)
lon_FV3_deg(:,:) = lon_FV3(:,:)
deallocate(lat_FV3_raw)
deallocate(lon_FV3_raw)

! Convert lat/lon to radians
lat_FV3 = lat_FV3*pi/180._real_kind
lon_FV3 = lon_FV3*pi/180._real_kind

! Create full grid-cell areas (4 raw areas per grid cell area)
! NOTE:  "Converting" from dbl_kind to real_kind
allocate(area_FV3(dimX_FV3,dimY_FV3))
do j = 1,dimY_FV3
   do i = 1,dimX_FV3
      area_FV3(i,j) = area_FV3_qtr(2*i-1,2*j-1) + area_FV3_qtr(2*i-1,2*j) + &
                      area_FV3_qtr(2*i,2*j-1) + area_FV3_qtr(2*i,2*j)
   end do
end do
deallocate(area_FV3_qtr)

err = nf90_close(ncid_in)



! Open file containing 30sec topo data (fine grid)
fine_topo_source_file_name = "HGT.Beljaars_filtered.lat-lon.30s_res.nc"
! Check that input file exists -- exit with error message if not
inquire(file=fine_topo_source_file_name,exist=fexist)
if (.not.fexist) then
   print *
   print *, "Fatal error: Topo source file "//                           &
            trim(fine_topo_source_file_name)//" does not exist"
   print *, "Exiting program gsl_oro_data.f90"
   print *
   call exit(4)
end if
err = nf90_open(trim(fine_topo_source_file_name),nf90_nowrite,ncid_in)
call netcdf_err(err, 'opening: '//trim(fine_topo_source_file_name))

! Get dimensions
err = nf90_inq_dimid(ncid_in,'west_east',dimid)
call netcdf_err(err, 'reading west_east id')
err = nf90_inquire_dimension(ncid_in,dimid,len=dimX_fine)
call netcdf_err(err, 'reading west_east value')

err = nf90_inq_dimid(ncid_in,'south_north',dimid)
call netcdf_err(err, 'reading south_north id')
err = nf90_inquire_dimension(ncid_in,dimid,len=dimY_fine)
call netcdf_err(err, 'reading south_north value')

print *, "Source file for high-resolution topography: ",                &
               trim(fine_topo_source_file_name)
print *, "dimX_fine =", dimX_fine
print *, "dimY_fine =", dimY_fine
print *


! Read in lat/lon of fine grid
allocate(lat1d_fine(dimY_fine))
allocate(lon1d_fine(dimX_fine))
err = nf90_inq_varid(ncid_in,'CLAT',varid)
call netcdf_err(err, 'reading CLAT id')
err = nf90_get_var(ncid_in,varid,lat1d_fine,start=(/1,1/),     &
                   count=(/1,dimY_fine/))
call netcdf_err(err, 'reading CLAT')

err = nf90_inq_varid(ncid_in,'CLONG',varid)
call netcdf_err(err, 'reading CLONG id')
err = nf90_get_var(ncid_in,varid,lon1d_fine,start=(/1,1/),     &
                   count=(/dimX_fine,1/))
call netcdf_err(err, 'reading CLONG')

! Convert lat/lon to radians
lat1d_fine = lat1d_fine*pi/180._real_kind
lon1d_fine = lon1d_fine*pi/180._real_kind


! Reassign FV3 longitude to vary from -pi to pi to match lon1d_fine range
do j = 1,dimY_FV3
   do i = 1,dimX_FV3
      if ( lon_FV3(i,j).gt.pi ) then
         lon_FV3(i,j) = lon_FV3(i,j) - 2*pi
      end if
   end do
end do


! Read in fine-scale topography
allocate(HGT_M_fine(dimX_fine,dimY_fine))
err = nf90_inq_varid(ncid_in,'HGT_M',varid)
call netcdf_err(err, 'reading HGT_M id')
err = nf90_get_var(ncid_in,varid,HGT_M_fine,start=(/1,1/),     &
                   count=(/dimX_fine,dimY_fine/))
call netcdf_err(err, 'reading HGT_M')


err = nf90_close(ncid_in)


! Allocate GWD statistics fields
allocate (std_dev(dimX_FV3*dimY_FV3))
allocate (convexity(dimX_FV3*dimY_FV3))
allocate (OA1(dimX_FV3*dimY_FV3))
allocate (OA2(dimX_FV3*dimY_FV3))
allocate (OA3(dimX_FV3*dimY_FV3))
allocate (OA4(dimX_FV3*dimY_FV3))
allocate (OL1(dimX_FV3*dimY_FV3))
allocate (OL2(dimX_FV3*dimY_FV3))
allocate (OL3(dimX_FV3*dimY_FV3))
allocate (OL4(dimX_FV3*dimY_FV3))

! Initialize GWD statistics fields
std_dev(:) = 0._real_kind
convexity(:) = 0._real_kind
OA1(:) = 0._real_kind
OA2(:) = 0._real_kind
OA3(:) = 0._real_kind
OA4(:) = 0._real_kind
OL1(:) = 0._real_kind
OL2(:) = 0._real_kind
OL3(:) = 0._real_kind
OL4(:) = 0._real_kind



! Calculate the minimum coarse grid cell size as implied by the cell area
min_area_FV3 = 1.E+14
do j = 1,dimY_FV3
   do i = 1,dimX_FV3
      min_area_FV3 = min(min_area_FV3,area_FV3(i,j))
   end do
end do
! The square root of min_area_FV3 will count as the minimum cell size
min_DX = sqrt(min_area_FV3)/1000._real_kind  ! grid size in km
! NOTE:  min_DX will be used after the big loop below to determine whether
!        to copy topographic statistics to "large-scale" file




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This is a loop over all the FV3 (coarse) grid cells
! The subgrid-scale topographic variables needed for the large-scale
! orographic gravity wave drag schemes are calculated by the following steps:
! 1) Sample the fine-scale (30sec) topography contained within each
!    coarse grid cell.
! 2) Calculate the orographic statistics: stddev,convexity,oa1,...oa4,
!    ol1,...,ol4
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(I,DLTA_LAT,DLTA_LON) &
!$OMP PRIVATE(I_BLK,J_BLK,S_II,S_JJ,E_II,E_JJ,LON_BLK,LAT_BLK,II_M,JJ_M) &
!$OMP PRIVATE(ZS,II,JJ,II_LOC,JJ_LOC,SUM2,NFINEPOINTS,CELL_COUNT,ZS_ACCUM,ZS_MEAN) &
!$OMP PRIVATE(SUM4,VAR,NU,ND,RATIO,NW,NT)
do j = 1,dimY_FV3
   do i = 1,dimX_FV3

      cell_count = ( (j-1) * dimX_FV3 ) + i

      ! Calculate approximate side-lengths of square lat-long "coarse" grid
      ! cell centered on FV3 cell (units = radians)
      dlta_lat = sqrt(area_FV3(i,j))/ae
      dlta_lon = sqrt(area_FV3(i,j))/(ae*COS(lat_FV3(i,j)))

      ! Determine lat/lon of 9 lat-lon block centers
      ! Note:  lat_blk(2)/lon_blk(2) = lat_FV3(i,j)/lon_FV3(i,j)
      ! Note:  abs(lon_blk) may exceed pi
      do i_blk = 1,3
         lon_blk(i_blk) = lon_FV3(i,j) + (i_blk-2)*dlta_lon
      end do
      ! Note:  abs(lat_blk) may exceed pi/2 (90 degrees)
      do j_blk = 1,3
         lat_blk(j_blk) = lat_FV3(i,j) + (j_blk-2)*dlta_lat
      end do

      ! Find starting and ending fine-grid i,j indices for each
      ! of the 9 "coarse-grid" blocks
      ! Note:  Index value of -999 is returned if latitude of grid points
      !        exceed 90 degrees north or south
      do i_blk = 1,3
         s_ii(i_blk) = nearest_i_east(lon_blk(i_blk)-p5*dlta_lon)
         e_ii(i_blk) = nearest_i_west(lon_blk(i_blk)+p5*dlta_lon)
      end do
      do j_blk = 1,3
         s_jj(j_blk) = nearest_j_north(lat_blk(j_blk)-p5*dlta_lat)
         e_jj(j_blk) = nearest_j_south(lat_blk(j_blk)+p5*dlta_lat)
      end do


      ! Calculate lat/lon relevant to each "coarse grid" block
      do i_blk = 1,3

         ! "Shave" blocks on north or south due to proximity to poles
         ! if necessary
         j_blk = 1  ! southern row
         ! Check for "shaved" block due to proximity to south pole
         if ( (s_jj(j_blk).eq.-999).and.(e_jj(j_blk).ne.-999) ) then
            s_jj(j_blk) = 1   ! southern boundary of shaved block
            ! Reassign latitude of block center
            lat_blk(j_blk) = p5*(lat1d_fine(1)+lat1d_fine(e_jj(j_blk)))
         end if

         j_blk = 2  ! center row
         ! Check for "shaved" block due to proximity to south or north pole
         ! Note:  We're assuming e_jj(2) and s_jj(2) can't both be -999
         if ( s_jj(j_blk).eq.-999 ) then
            s_jj(j_blk) = 1  ! block shaved on the south
            ! Reassign latitude of block center
            lat_blk(j_blk) = p5*(lat1d_fine(1)+lat1d_fine(e_jj(j_blk)))
         end if
         if ( e_jj(j_blk).eq.-999 ) then
            e_jj(j_blk) = dimY_fine  ! block shaved on the north
            ! Reassign latitude of block center
            lat_blk(j_blk) = p5*(lat1d_fine(s_jj(j_blk))+lat1d_fine(dimY_fine))
         end if

         j_blk = 3  ! northern row
         ! Check for "shaved" block due to proximity to north pole
         if ( (e_jj(j_blk).eq.-999).and.(s_jj(j_blk).ne.-999) ) then
            e_jj(j_blk) = dimY_fine  ! northern boundary of shaved block
            ! Reassign latitude of block center 
            lat_blk(j_blk) = p5*(lat1d_fine(s_jj(j_blk))+lat1d_fine(dimY_fine))
         end if

      end do


      ! Calculate number of fine-grid points within center coarse block (2,2)
      ! Check if center block straddles date line
      if ( s_ii(2).gt.e_ii(2) ) then
         ii_m = dimX_fine - s_ii(2) + 1 + e_ii(2)
      else
         ii_m = e_ii(2) - s_ii(2) + 1
      end if
      jj_m = e_jj(2) - s_jj(2) + 1


      ! Assign values to "zs", which is the fine-grid surface topography field
      ! that we will calculate statistics on, i.e, stddev, convexity, etc.
      allocate (zs(ii_m,jj_m))

      do jj = s_jj(2), e_jj(2)
         jj_loc = jj - s_jj(2) + 1  ! local j-index (1 ... jj_m)
         ! Check if block straddles the date line
         if ( s_ii(2).gt.e_ii(2) ) then
            do ii = s_ii(2), dimX_fine  ! west of the date line
               ii_loc = ii - s_ii(2) + 1   ! local i-index ( 1 ... ii_m)
               zs(ii_loc,jj_loc) = HGT_M_fine(ii,jj)
            end do
            do ii = 1, e_ii(2)   ! east of the date line
               ii_loc = ii_loc + 1  ! local i-index ( 1 ... ii_m )
               zs(ii_loc,jj_loc) = HGT_M_fine(ii,jj)
            end do
         else   ! no crossing of the date line
            do ii = s_ii(2), e_ii(2)
               ii_loc = ii - s_ii(2) + 1   ! local i-index ( 1 ... ii_m)
               zs(ii_loc,jj_loc) = HGT_M_fine(ii,jj)
            end do
         end if
      end do



      !
      ! Finally, we can now calculate the topographic statistics fields needed
      ! for the gravity wave drag scheme
      !

      ! Make sure statistics are zero if there is no terrain in the grid cell
      ! Note:  This is a proxy for a landmask
      zs_accum = .false.
      do jj = 1,jj_m
         do ii = 1,ii_m
            if ( abs(zs(ii,jj)).gt.1.E-1 ) zs_accum = .true.
         end do
      end do
      if ( .not.zs_accum ) then   ! no terrain in the grid cell
         std_dev(cell_count) = 0._real_kind
         convexity(cell_count) = 0._real_kind
         OA1(cell_count) = 0._real_kind
         OA2(cell_count) = 0._real_kind
         OA3(cell_count) = 0._real_kind
         OA4(cell_count) = 0._real_kind
         OL1(cell_count) = 0._real_kind
         OL2(cell_count) = 0._real_kind
         OL3(cell_count) = 0._real_kind
         OL4(cell_count) = 0._real_kind
         deallocate(zs)
         cycle   ! move on to next (coarse) grid cell 
      end if


      !
      ! Calculate standard deviation of subgrid-scale terrain height
      !

      ! Calculate mean height
      sum2 = 0._real_kind
      nfinepoints = ii_m*jj_m
      do jj = 1,jj_m
         do ii = 1,ii_m
            sum2 = sum2 + zs(ii,jj)
         end do
      end do
      zs_mean = sum2 / real(nfinepoints,real_kind)

      ! Calculate standard deviation
      sum2 = 0._real_kind
      do jj = 1,jj_m
         do ii = 1,ii_m
            sum2 = sum2 + ( zs(ii,jj) - zs_mean )**2
         end do
      end do
      std_dev(cell_count) = sqrt( sum2/real(nfinepoints,real_kind) )

      !
      ! Calculate convexity of sub-grid-scale terrain
      !

      sum2 = 0._real_kind
      sum4 = 0._real_kind
      do jj = 1,jj_m
         do ii = 1,ii_m
            sum2 = sum2 + ( zs(ii,jj) - zs_mean )**2
            sum4 = sum4 + ( zs(ii,jj) - zs_mean )**4
         end do
      end do

      var = sum2 / real(nfinepoints,real_kind)
      if ( abs(var) < 1.0E-05_real_kind ) then
         convexity(cell_count) = 0._real_kind
      else
         convexity(cell_count) = min( sum4 / ( var**2 *                    &
                        real(nfinepoints,real_kind) ), max_convexity )
      end if


      !
      ! Calculate orographic asymmetries
      !

      ! OA1 -- orographic asymmetry in West direction
      nu = 0
      nd = 0
      do jj = 1,jj_m
         if(mod(ii_m,2).eq.0.) then
           do ii = 1,ii_m/2   ! left half of box
              if ( zs(ii,jj) > zs_mean ) nu = nu + 1
           end do
         else
           do ii = 1,ii_m/2+1   ! left half of box
              if ( zs(ii,jj) > zs_mean ) nu = nu + 1
           end do
         endif
         do ii = ii_m/2 + 1, ii_m  ! right half of box
            if ( zs(ii,jj) > zs_mean ) nd = nd + 1
         end do
      end do
      if ( nu + nd > 0 ) then
         OA1(cell_count) = real((nu - nd),real_kind) /     &
                                   real((nu + nd),real_kind)
      else
         OA1(cell_count) = 0._real_kind
      end if

      ! OA2 -- orographic asymmetry in South direction
      nu = 0
      nd = 0
      if(mod(jj_m,2).eq.0.) then
        do jj = 1,jj_m/2   ! bottom half of box
           do ii = 1,ii_m
              if ( zs(ii,jj) > zs_mean ) nu = nu + 1
           end do
        end do
      else
        do jj = 1,jj_m/2+1   ! bottom half of box
           do ii = 1,ii_m
              if ( zs(ii,jj) > zs_mean ) nu = nu + 1
           end do
        end do
      endif
      do jj = jj_m/2 + 1,jj_m   ! top half of box
         do ii = 1, ii_m
            if ( zs(ii,jj) > zs_mean ) nd = nd + 1
         end do
      end do
      if ( nu + nd > 0 ) then
         OA2(cell_count) = real((nu - nd),real_kind) /     &
                                   real((nu + nd),real_kind)
      else
         OA2(cell_count) = 0._real_kind
      end if

      ! OA3 -- orographic asymmetry in South-West direction
      nu = 0
      nd = 0
      ratio = real(jj_m,real_kind)/real(ii_m,real_kind)
      do jj = 1,jj_m
         do ii = 1,ii_m
            if ( nint(real(ii,real_kind)*ratio) <= (jj_m - jj + 1) ) then
               ! south-west half of box
               if ( zs(ii,jj) > zs_mean ) nu = nu + 1
            endif
            if ( nint(real(ii,real_kind)*ratio) >= (jj_m - jj + 1) ) then
               ! north-east half of box
               if ( zs(ii,jj) > zs_mean ) nd = nd + 1
            end if
         end do
      end do
      if ( nu + nd > 0 ) then
         OA3(cell_count) = real((nu - nd),real_kind) /     &
                                   real((nu + nd),real_kind)
      else
         OA3(cell_count) = 0._real_kind
      end if

      ! OA4 -- orographic asymmetry in North-West direction
      nu = 0
      nd = 0
      ratio = real(jj_m,real_kind)/real(ii_m,real_kind)
      do jj = 1,jj_m
         do ii = 1,ii_m
            if ( nint(real(ii,real_kind)*ratio) <= jj ) then
               ! north-west half of box
               if ( zs(ii,jj) > zs_mean ) nu = nu + 1
            end if
            if ( nint(real(ii,real_kind)*ratio) >= jj ) then
                ! south-east half of box
               if ( zs(ii,jj) > zs_mean ) nd = nd + 1
            end if
         end do
      end do
      if ( nu + nd > 0 ) then
         OA4(cell_count) = real((nu - nd),real_kind) /     &
                                   real((nu + nd),real_kind)
      else
         OA4(cell_count) = 0._real_kind
      end if


      !
      ! Calculate orographic effective lengths
      !

      ! OL1 -- orographic effective length for Westerly flow
      nw = 0
      nt = 0
      do jj = max(jj_m/4,1), 3*jj_m/4
         ! within central east-west band of box
         do ii = 1, ii_m
            if ( zs(ii,jj) > zs_mean ) nw = nw + 1
            nt = nt + 1
         end do
      end do
      if ( nt /= 0 ) then
         OL1(cell_count) = real(nw,real_kind) / real(nt,real_kind)
      else
         OL1(cell_count) = 0._real_kind
      end if

      ! OL2 -- orographic effective length for Southerly flow
      nw = 0
      nt = 0
      do jj = 1, jj_m
         do ii = max(ii_m/4,1), 3*ii_m/4
            ! within central north-south band of box
            if ( zs(ii,jj) > zs_mean ) nw = nw + 1
            nt = nt + 1
         end do
      end do
      if ( nt /= 0 ) then
         OL2(cell_count) = real(nw,real_kind) / real(nt,real_kind)
      else
         OL2(cell_count) = 0._real_kind
      end if

      ! OL3 -- orographic effective length for South-Westerly flow
      nw = 0
      nt = 0
      do jj = 1, jj_m/2
         do ii = 1, ii_m/2
            if ( zs(ii,jj) > zs_mean ) nw = nw + 1
            nt = nt + 1
         end do
      end do
      do jj = jj_m/2+1, jj_m
         do ii = ii_m/2+1, ii_m
            if ( zs(ii,jj) > zs_mean ) nw = nw + 1
            nt = nt + 1
         end do
      end do
      if ( nt /= 0 ) then
         OL3(cell_count) = real(nw,real_kind) / real(nt,real_kind)
      else
         OL3(cell_count) = 0._real_kind
      end if

      ! OL4 -- orographic effective length for North-Westerly flow
      nw = 0
      nt = 0
      do jj = jj_m/2+1, jj_m
         do ii = 1, ii_m/2
            if ( zs(ii,jj) > zs_mean ) nw = nw + 1
            nt = nt + 1
         end do
      end do
      do jj = 1, jj_m/2
         do ii = ii_m/2+1, ii_m
            if ( zs(ii,jj) > zs_mean ) nw = nw + 1
            nt = nt + 1
         end do
      end do
      if ( nt /= 0 ) then
         OL4(cell_count) = real(nw,real_kind) / real(nt,real_kind)
      else
         OL4(cell_count) = 0._real_kind
      end if

      deallocate (zs)

   end do   ! j = 1,dimY_FV3
end do      ! i = 1,dimX_FV3
!$OMP END PARALLEL DO

!
! Output GWD statistics fields to netCDF file
!


if ( halo.eq."-999" ) then   ! global or nested tile
   oro_data_output_file_name = "C" // trim(res_indx) // "_oro_data_ss.tile" &
                                   // trim(tile_num) // ".nc"
else   ! stand-alone regional tile
   oro_data_output_file_name = "C" // trim(res_indx) // "_oro_data_ss.tile" &
                                   // trim(tile_num) // ".halo0.nc"
end if

! Open netCDF file for output
err = nf90_create(oro_data_output_file_name, NF90_CLOBBER, ncid_out)
call netcdf_err(err, 'creating: '//oro_data_output_file_name)

err = nf90_redef(ncid_out)

! Define dimensions
err = nf90_def_dim(ncid_out,'lon',dimX_FV3,lonid)
call netcdf_err(err, 'defining lon dimension')
err = nf90_def_dim(ncid_out,'lat',dimY_FV3,latid)
call netcdf_err(err, 'defining lat dimension')

! Define the 'dimensions vector' dimids to be used for writing
! the 2-dimensional variables to the netCDF file
dimids(1) = lonid
dimids(2) = latid

! Define variables and attributes to put in the netCDF file
err = nf90_def_var(ncid_out,'geolon',NF90_FLOAT,dimids,varid)
call netcdf_err(err, 'defining geolon')
err = nf90_put_att(ncid_out,varid,'units','degrees')
err = nf90_put_att(ncid_out,varid,'description','longitude')
err = nf90_def_var(ncid_out,'geolat',NF90_FLOAT,dimids,varid)
call netcdf_err(err, 'defining geolat')
err = nf90_put_att(ncid_out,varid,'units','degrees')
err = nf90_put_att(ncid_out,varid,'description','latitude')
err = nf90_def_var(ncid_out,'stddev',NF90_FLOAT,dimids,varid)
call netcdf_err(err, 'stddev')
err = nf90_put_att(ncid_out,varid,'units','meters')
err = nf90_put_att(ncid_out,varid,'description',                     &
                      'standard deviation of subgrid topography')
err = nf90_def_var(ncid_out,'convexity',NF90_FLOAT,dimids,varid)
call netcdf_err(err, 'defining convexity')
err = nf90_put_att(ncid_out,varid,'units','-')
err = nf90_put_att(ncid_out,varid,'description',                     &
                      'convexity of subgrid topography')
err = nf90_def_var(ncid_out,'oa1',NF90_FLOAT,dimids,varid)
call netcdf_err(err, 'defining oa1')
err = nf90_put_att(ncid_out,varid,'units','-')
err = nf90_put_att(ncid_out,varid,'description',                     &
                      'orographic asymmetry in west direction')
err = nf90_def_var(ncid_out,'oa2',NF90_FLOAT,dimids,varid)
call netcdf_err(err, 'defining oa2')
err = nf90_put_att(ncid_out,varid,'units','-')
err = nf90_put_att(ncid_out,varid,'description',                     &
                      'orographic asymmetry in south direction')
err = nf90_def_var(ncid_out,'oa3',NF90_FLOAT,dimids,varid)
call netcdf_err(err, 'defining oa3')
err = nf90_put_att(ncid_out,varid,'units','-')
err = nf90_put_att(ncid_out,varid,'description',                     &
                      'orographic asymmetry in south-west direction')
err = nf90_def_var(ncid_out,'oa4',NF90_FLOAT,dimids,varid)
call netcdf_err(err, 'defining oa4')
err = nf90_put_att(ncid_out,varid,'units','-')
err = nf90_put_att(ncid_out,varid,'description',                     &
                      'orographic asymmetry in north-west direction')
err = nf90_def_var(ncid_out,'ol1',NF90_FLOAT,dimids,varid)
call netcdf_err(err, 'defining ol1')
err = nf90_put_att(ncid_out,varid,'units','-')
err = nf90_put_att(ncid_out,varid,'description',                     &
                      'orographic effective length for westerly flow')
err = nf90_def_var(ncid_out,'ol2',NF90_FLOAT,dimids,varid)
call netcdf_err(err, 'defining ol2')
err = nf90_put_att(ncid_out,varid,'units','-')
err = nf90_put_att(ncid_out,varid,'description',                     &
                      'orographic effective length for southerly flow')
err = nf90_def_var(ncid_out,'ol3',NF90_FLOAT,dimids,varid)
call netcdf_err(err, 'defining ol3')
err = nf90_put_att(ncid_out,varid,'units','-')
err = nf90_put_att(ncid_out,varid,'description',                     &
                 'orographic effective length for south-westerly flow')
err = nf90_def_var(ncid_out,'ol4',NF90_FLOAT,dimids,varid)
call netcdf_err(err, 'defining ol4')
err = nf90_put_att(ncid_out,varid,'units','-')
err = nf90_put_att(ncid_out,varid,'description',                     &
                 'orographic effective length for north-westerly flow')

! Add global attributes
err = nf90_put_att(ncid_out,nf90_global,                               &
            'source_file_for_high-resolution_topography',              &
            trim(fine_topo_source_file_name))

err = nf90_enddef(ncid_out)


! Write data to output netCDF file
err = nf90_inq_varid(ncid_out,'geolon',varid)
call netcdf_err(err, 'reading geolon id')
err = nf90_put_var(ncid_out,varid,lon_FV3_deg,start=(/1,1/),           &
                   count=(/dimX_FV3,dimY_FV3/))
call netcdf_err(err, 'writing geolon')
err = nf90_inq_varid(ncid_out,'geolat',varid)
call netcdf_err(err, 'reading geolat id')
err = nf90_put_var(ncid_out,varid,lat_FV3_deg,start=(/1,1/),           &
                   count=(/dimX_FV3,dimY_FV3/))
call netcdf_err(err, 'writing geolat')
err = nf90_inq_varid(ncid_out,'stddev',varid)
call netcdf_err(err, 'reading stddev id')
err = nf90_put_var(ncid_out,varid,std_dev,start=(/1,1/),               &
                   count=(/dimX_FV3,dimY_FV3/))
call netcdf_err(err, 'writing stddev')
err = nf90_inq_varid(ncid_out,'convexity',varid)
call netcdf_err(err, 'reading convexity id')
err = nf90_put_var(ncid_out,varid,convexity,start=(/1,1/),             &
                   count=(/dimX_FV3,dimY_FV3/))
call netcdf_err(err, 'writing convexity')
err = nf90_inq_varid(ncid_out,'oa1',varid)
call netcdf_err(err, 'reading oa1 id')
err = nf90_put_var(ncid_out,varid,OA1,start=(/1,1/),                   &
                   count=(/dimX_FV3,dimY_FV3/))
call netcdf_err(err, 'writing oa1')
err = nf90_inq_varid(ncid_out,'oa2',varid)
call netcdf_err(err, 'reading oa2 id')
err = nf90_put_var(ncid_out,varid,OA2,start=(/1,1/),                   &
                   count=(/dimX_FV3,dimY_FV3/))
call netcdf_err(err, 'writing oa2')
err = nf90_inq_varid(ncid_out,'oa3',varid)
call netcdf_err(err, 'reading oa3 id')
err = nf90_put_var(ncid_out,varid,OA3,start=(/1,1/),                   &
                   count=(/dimX_FV3,dimY_FV3/))
call netcdf_err(err, 'writing oa3')
err = nf90_inq_varid(ncid_out,'oa4',varid)
call netcdf_err(err, 'reading oa4 id')
err = nf90_put_var(ncid_out,varid,OA4,start=(/1,1/),                   &
                   count=(/dimX_FV3,dimY_FV3/))
call netcdf_err(err, 'writing oa4')
err = nf90_inq_varid(ncid_out,'ol1',varid)
call netcdf_err(err, 'reading ol1 id')
err = nf90_put_var(ncid_out,varid,OL1,start=(/1,1/),                   &
                   count=(/dimX_FV3,dimY_FV3/))
call netcdf_err(err, 'writing ol1')
err = nf90_inq_varid(ncid_out,'ol2',varid)
call netcdf_err(err, 'reading ol2 id')
err = nf90_put_var(ncid_out,varid,OL2,start=(/1,1/),                   &
                   count=(/dimX_FV3,dimY_FV3/))
call netcdf_err(err, 'writing ol2')
err = nf90_inq_varid(ncid_out,'ol3',varid)
call netcdf_err(err, 'reading ol3 id')
err = nf90_put_var(ncid_out,varid,OL3,start=(/1,1/),                   &
                   count=(/dimX_FV3,dimY_FV3/))
call netcdf_err(err, 'writing ol3')
err = nf90_inq_varid(ncid_out,'ol4',varid)
call netcdf_err(err, 'reading ol4 id')
err = nf90_put_var(ncid_out,varid,OL4,start=(/1,1/),                   &
                   count=(/dimX_FV3,dimY_FV3/))
call netcdf_err(err, 'writing ol4')

err = nf90_close(ncid_out)



! Determine whether grid size falls below threshold for use by large-scale
! orographic gravity wave drag and blocking scheme.  If it is, then
! create "dummy" oro_data_ls file containing the same data as oro_data_ss file

duplicate_oro_data_file = .false.

if ( min_DX.le.7.5 ) then

   duplicate_oro_data_file = .true.
   print *, "Creating oro_data_ls file as duplicate of oro_data_ss"
   print *, "Minimum grid cell size = ", min_DX, " km"
   print *


   if ( halo.eq."-999" ) then   ! global or nested tile
      oro_data_output_file_name = "C" // trim(res_indx) // "_oro_data_ls.tile" &
                                      // trim(tile_num) // ".nc"
   else   ! stand-alone regional tile
      oro_data_output_file_name = "C" // trim(res_indx) // "_oro_data_ls.tile" &
                                      // trim(tile_num) // ".halo0.nc"
   end if

   ! Open netCDF file for output
   err = nf90_create(oro_data_output_file_name, NF90_CLOBBER, ncid_out)
   call netcdf_err(err, 'creating: '//oro_data_output_file_name)

   err = nf90_redef(ncid_out)

   ! Define dimensions
   err = nf90_def_dim(ncid_out,'lon',dimX_FV3,lonid)
   call netcdf_err(err, 'defining lon dimension')
   err = nf90_def_dim(ncid_out,'lat',dimY_FV3,latid)
   call netcdf_err(err, 'defining lat dimension')

   ! Define the 'dimensions vector' dimids to be used for writing
   ! the 2-dimensional variables to the netCDF file
   dimids(1) = lonid
   dimids(2) = latid

   ! Define variables and attributes to put in the netCDF file
   err = nf90_def_var(ncid_out,'geolon',NF90_FLOAT,dimids,varid)
   call netcdf_err(err, 'defining geolon')
   err = nf90_put_att(ncid_out,varid,'units','degrees')
   err = nf90_put_att(ncid_out,varid,'description','longitude')
   err = nf90_def_var(ncid_out,'geolat',NF90_FLOAT,dimids,varid)
   call netcdf_err(err, 'defining geolat')
   err = nf90_put_att(ncid_out,varid,'units','degrees')
   err = nf90_put_att(ncid_out,varid,'description','latitude')
   err = nf90_def_var(ncid_out,'stddev',NF90_FLOAT,dimids,varid)
   call netcdf_err(err, 'defining stddev')
   err = nf90_put_att(ncid_out,varid,'units','meters')
   err = nf90_put_att(ncid_out,varid,'description',                     &
                         'standard deviation of subgrid topography')
   err = nf90_def_var(ncid_out,'convexity',NF90_FLOAT,dimids,varid)
   call netcdf_err(err, 'defining convexity')
   err = nf90_put_att(ncid_out,varid,'units','-')
   err = nf90_put_att(ncid_out,varid,'description',                     &
                         'convexity of subgrid topography')
   err = nf90_def_var(ncid_out,'oa1',NF90_FLOAT,dimids,varid)
   call netcdf_err(err, 'defining oa1')
   err = nf90_put_att(ncid_out,varid,'units','-')
   err = nf90_put_att(ncid_out,varid,'description',                     &
                         'orographic asymmetry in west direction')
   err = nf90_def_var(ncid_out,'oa2',NF90_FLOAT,dimids,varid)
   call netcdf_err(err, 'defining oa2')
   err = nf90_put_att(ncid_out,varid,'units','-')
   err = nf90_put_att(ncid_out,varid,'description',                     &
                         'orographic asymmetry in south direction')
   err = nf90_def_var(ncid_out,'oa3',NF90_FLOAT,dimids,varid)
   call netcdf_err(err, 'defining oa3')
   err = nf90_put_att(ncid_out,varid,'units','-')
   err = nf90_put_att(ncid_out,varid,'description',                     &
                         'orographic asymmetry in south-west direction')
   err = nf90_def_var(ncid_out,'oa4',NF90_FLOAT,dimids,varid)
   call netcdf_err(err, 'defining oa4')
   err = nf90_put_att(ncid_out,varid,'units','-')
   err = nf90_put_att(ncid_out,varid,'description',                     &
                         'orographic asymmetry in north-west direction')
   err = nf90_def_var(ncid_out,'ol1',NF90_FLOAT,dimids,varid)
   call netcdf_err(err, 'defining ol1')
   err = nf90_put_att(ncid_out,varid,'units','-')
   err = nf90_put_att(ncid_out,varid,'description',                     &
                         'orographic effective length for westerly flow')
   err = nf90_def_var(ncid_out,'ol2',NF90_FLOAT,dimids,varid)
   call netcdf_err(err, 'defining ol2')
   err = nf90_put_att(ncid_out,varid,'units','-')
   err = nf90_put_att(ncid_out,varid,'description',                     &
                         'orographic effective length for southerly flow')
   err = nf90_def_var(ncid_out,'ol3',NF90_FLOAT,dimids,varid)
   call netcdf_err(err, 'defining ol3')
   err = nf90_put_att(ncid_out,varid,'units','-')
   err = nf90_put_att(ncid_out,varid,'description',                     &
                    'orographic effective length for south-westerly flow')
   err = nf90_def_var(ncid_out,'ol4',NF90_FLOAT,dimids,varid)
   call netcdf_err(err, 'defining ol4')
   err = nf90_put_att(ncid_out,varid,'units','-')
   err = nf90_put_att(ncid_out,varid,'description',                     &
                    'orographic effective length for north-westerly flow')

   ! Add global attributes
   err = nf90_put_att(ncid_out,nf90_global,                               &
                  'NOTE','This is a duplicate of the oro_data_ss file')

   err = nf90_enddef(ncid_out)


   ! Write data to output netCDF file
   err = nf90_inq_varid(ncid_out,'geolon',varid)
   call netcdf_err(err, 'reading geolon id')
   err = nf90_put_var(ncid_out,varid,lon_FV3_deg,start=(/1,1/),           &
                      count=(/dimX_FV3,dimY_FV3/))
   call netcdf_err(err, 'writing geolon')
   err = nf90_inq_varid(ncid_out,'geolat',varid)
   call netcdf_err(err, 'reading geolat id')
   err = nf90_put_var(ncid_out,varid,lat_FV3_deg,start=(/1,1/),           &
                      count=(/dimX_FV3,dimY_FV3/))
   call netcdf_err(err, 'writing geolat')
   err = nf90_inq_varid(ncid_out,'stddev',varid)
   call netcdf_err(err, 'reading stddev id')
   err = nf90_put_var(ncid_out,varid,std_dev,start=(/1,1/),               &
                      count=(/dimX_FV3,dimY_FV3/))
   call netcdf_err(err, 'writing stddev')
   err = nf90_inq_varid(ncid_out,'convexity',varid)
   call netcdf_err(err, 'reading convexity id')
   err = nf90_put_var(ncid_out,varid,convexity,start=(/1,1/),             &
                      count=(/dimX_FV3,dimY_FV3/))
   call netcdf_err(err, 'writing convexity')
   err = nf90_inq_varid(ncid_out,'oa1',varid)
   call netcdf_err(err, 'reading oa1 id')
   err = nf90_put_var(ncid_out,varid,OA1,start=(/1,1/),                   &
                      count=(/dimX_FV3,dimY_FV3/))
   call netcdf_err(err, 'writing oa1')
   err = nf90_inq_varid(ncid_out,'oa2',varid)
   call netcdf_err(err, 'reading oa2 id')
   err = nf90_put_var(ncid_out,varid,OA2,start=(/1,1/),                   &
                      count=(/dimX_FV3,dimY_FV3/))
   call netcdf_err(err, 'writing oa2')
   err = nf90_inq_varid(ncid_out,'oa3',varid)
   call netcdf_err(err, 'reading oa3 id')
   err = nf90_put_var(ncid_out,varid,OA3,start=(/1,1/),                   &
                      count=(/dimX_FV3,dimY_FV3/))
   call netcdf_err(err, 'writing oa3')
   err = nf90_inq_varid(ncid_out,'oa4',varid)
   call netcdf_err(err, 'reading oa4 id')
   err = nf90_put_var(ncid_out,varid,OA4,start=(/1,1/),                   &
                      count=(/dimX_FV3,dimY_FV3/))
   call netcdf_err(err, 'writing oa4')
   err = nf90_inq_varid(ncid_out,'ol1',varid)
   call netcdf_err(err, 'reading ol1 id')
   err = nf90_put_var(ncid_out,varid,OL1,start=(/1,1/),                   &
                      count=(/dimX_FV3,dimY_FV3/))
   call netcdf_err(err, 'writing ol1')
   err = nf90_inq_varid(ncid_out,'ol2',varid)
   call netcdf_err(err, 'reading ol2 id')
   err = nf90_put_var(ncid_out,varid,OL2,start=(/1,1/),                   &
                      count=(/dimX_FV3,dimY_FV3/))
   call netcdf_err(err, 'writing ol2')
   err = nf90_inq_varid(ncid_out,'ol3',varid)
   call netcdf_err(err, 'reading ol3 id')
   err = nf90_put_var(ncid_out,varid,OL3,start=(/1,1/),                   &
                      count=(/dimX_FV3,dimY_FV3/))
   call netcdf_err(err, 'writing ol3')
   err = nf90_inq_varid(ncid_out,'ol4',varid)
   call netcdf_err(err, 'reading ol4 id')
   err = nf90_put_var(ncid_out,varid,OL4,start=(/1,1/),                   &
                      count=(/dimX_FV3,dimY_FV3/))
   call netcdf_err(err, 'writing ol4')

   err = nf90_close(ncid_out)

else

   print *, "Minimum grid cell size = ", min_DX, " km"
   print *

end if



! Deallocate arrays
deallocate(lat_FV3)
deallocate(lon_FV3)
deallocate(lat_FV3_deg)
deallocate(lon_FV3_deg)
deallocate(area_FV3)
deallocate(lat1d_fine)
deallocate(lon1d_fine)
deallocate(HGT_M_fine)
deallocate(std_dev)
deallocate(convexity)
deallocate(OA1)
deallocate(OA2)
deallocate(OA3)
deallocate(OA4)
deallocate(OL1)
deallocate(OL2)
deallocate(OL3)
deallocate(OL4)


end subroutine calc_gsl_oro_data_sm_scale

!> Finds nearest fine-grid i index to the east of a given longitude
!!
!! @param[in] lon_in longitude (radians)
!! @return nearest_i_east Nearest grid point i-index east of selected point
!! @author Michael Toy, NOAA/GSL
function nearest_i_east(lon_in)
! Calculates nearest fine-grid i index to the east of (or on) a given longitude
implicit none

integer :: nearest_i_east
real (kind=real_kind), intent(in) :: lon_in
real (kind=real_kind) :: lon
integer :: i

lon = lon_in
! Make sure longitude is between -pi and pi
do while ( (lon.lt.(-pi)).or.(lon.gt.pi) )
   if ( lon.lt.(-pi) ) lon = lon + 2*pi
   if ( lon.gt.pi ) lon = lon - 2*pi
end do

if ( lon.gt.lon1d_fine(dimX_fine) ) then
   nearest_i_east = 1
else
   i = 1
   do while ( lon1d_fine(i).lt.lon )
      i = i + 1
   end do
   nearest_i_east = i
end if

end function nearest_i_east

!> Finds nearest fine-grid i index to the west of a given longitude
!!
!! @param[in] lon_in longitude (radians)
!! @return nearest_i_west Nearest grid point i-index west of selected point
!! @author Michael Toy, NOAA/GSL
function nearest_i_west(lon_in)
! Calculates nearest fine-grid i index to the west of a given longitude
implicit none

integer :: nearest_i_west
real (kind=real_kind), intent(in) :: lon_in
real (kind=real_kind) :: lon
integer :: i

lon = lon_in
! Make sure longitude is between -pi and pi
do while ( (lon.lt.(-pi)).or.(lon.gt.pi) )
   if ( lon.lt.(-pi) ) lon = lon + 2*pi
   if ( lon.gt.pi ) lon = lon - 2*pi
end do

if ( (lon.lt.lon1d_fine(1)).or.(lon.ge.lon1d_fine(dimX_fine)) ) then
   nearest_i_west = dimX_fine
else
   i = 1
   do while ( lon1d_fine(i).le.lon )
      i = i + 1
   end do
   nearest_i_west = i - 1
end if

end function nearest_i_west

!> Calculates nearest fine-grid j index to the north of a given latitude
!!
!! @param[in] lat_in Latitude (radians)
!! @return nearest_j_north Nearest fine-grid j index to the north of a given latitude
!! @author Michael Toy, NOAA/GSL
function nearest_j_north(lat_in)
! Calculates nearest fine-grid j index to the north of a given latitude
! Note:  If the abs(latitude) is greater than pi/2 (90 degrees) then
!        the value -999 is returned
implicit none

integer :: nearest_j_north
real (kind=real_kind), intent(in) :: lat_in
real (kind=real_kind) :: lat
integer :: j

lat = lat_in
if ( abs(lat_in).gt.p5*pi ) then
   nearest_j_north = -999
else
   j = 1
   do while ( (lat1d_fine(j).lt.lat).and.(j.lt.dimY_fine) )
      j = j + 1
   end do
   nearest_j_north = j
end if

end function nearest_j_north

!> Calculates nearest fine-grid j index to the south of a given latitude
!!
!! @param[in] lat_in Latitude (radians)
!! @return nearest_j_south Nearest fine-grid j index to the south of a given latitude
!! @author Michael Toy, NOAA/GSL
function nearest_j_south(lat_in)
! Calculates nearest fine-grid j index to the south of a given latitude
! Note:  If the abs(latitude) is greater than pi/2 (90 degrees) then
!        the value -999 is returned
implicit none

integer :: nearest_j_south
real (kind=real_kind), intent(in) :: lat_in
real (kind=real_kind) :: lat
integer :: j

lat = lat_in
if ( abs(lat_in).gt.p5*pi ) then
   nearest_j_south = -999
elseif ( lat_in.le.lat1d_fine(1) ) then
   nearest_j_south = 1
else
   j = 2
   do while ( (lat1d_fine(j).le.lat).and.(j.le.dimY_fine) )
      j = j + 1
   end do
   nearest_j_south = j - 1
end if

end function nearest_j_south

!> Interpolates (or extrapolates) linear function y = y(x)
!! 
!! @param[in] x Input "x" value
!! @param[in] x1 Known point 1
!! @param[in] x2 Known point 2
!! @param[in] y1 Known y(x1)
!! @param[in] y2 Known y(x2)
!! @return interp_1d Interpolated y value at x
!! @author Michael Toy, NOAA/GSL
function interp_1d(x,x1,x2,y1,y2)
! Interpolates (or extrapolates) linear function y = y(x)
! to x given y1 = y(x1) and y2 = y(x2)
implicit none

real (kind=real_kind) :: interp_1d
real (kind=real_kind), intent(in) :: x,x1,x2,y1,y2
real (kind=real_kind) :: slope

! Formula for a line: y = y1 + slope*(x - x1)
slope = (y2-y1)/(x2-x1)
interp_1d = y1 + slope*(x-x1)

end function interp_1d

!> Returns netCDF error given input err code
!!
!! @param[in] err Error code from netCDF routine
!! @param[in] string Portion of error message
!! @author Michael Toy, NOAA/GSL
subroutine netcdf_err(err,string)

use netcdf

implicit none

integer, intent(in) :: err
character(len=*), intent(in) :: string
character(len=256) :: errmsg

if (err.eq.NF90_NOERR ) return
errmsg = NF90_STRERROR(err)
print *, ""
print *, "FATAL ERROR: ", trim(string), ": ", trim(errmsg)
print *, "STOP."
call exit(4)

return
end subroutine netcdf_err

end module gsl_oro_data_sm_scale
