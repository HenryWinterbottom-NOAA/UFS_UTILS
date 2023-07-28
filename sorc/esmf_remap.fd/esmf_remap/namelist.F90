!> @file namelist.F90
!! @details This module contains the FORTRAN namelist interface.
!! @author Henry R. Winterbottom
!! @date 30 June 2023
!! @version 0.0.1
!! @license LGPL v2.1
module namelist_interface
  use netcdf, only: nf90_double
  use netcdf_interface !! # TODO: Update with `only`; ncdata, ncvarinfo
  use variables_interface, only: abort_remap, destroy_struct, dstgrid_struct, &
       esmffile_struct, get_boolean, ilong, init_struct, interp_struct, maxchar, &
       rdouble, varinfo_struct
  implicit none
  private
  public :: read_namelist

  character(len=maxchar) :: namelist_input = "./esmf_remap.input"
  character(len=maxchar) :: bilinear = "NOT USED"
  character(len=maxchar) :: conserve = "NOT USED"
  character(len=maxchar) :: grid_ncfile = "NOT USED"
  character(len=maxchar) :: grid_nclatname = "NOT USED"
  character(len=maxchar) :: grid_nclonname = "NOT USED"  
  character(len=maxchar) :: nearests2d = "NOT USED"
  character(len=maxchar) :: output_netcdf = "NOT USED"
  character(len=maxchar) :: varsfile = "NOT USED"
  integer(ilong), parameter :: unit_nml = 99
  integer(ilong) :: nlevs = 1
  namelist /gridinfo/ grid_ncfile, grid_nclatname, grid_nclonname
  namelist /esmf/ bilinear, conserve, nearests2d
  namelist /share/ output_netcdf, varsfile, nlevs
contains

  !> @brief Determines the interpolation type from the input variable
  !!        attributes.
  function get_interp(interp) result(interp_info)
    type(interp_struct) :: interp_info
    character(len=maxchar), intent(in):: interp
    character(len=maxchar) :: msg
    character(len=1) :: interp_check

    interp_check = trim(adjustl(interp))
    if (interp_check == "B" .or. interp_check == "b") then
       interp_info%bilinear = .true.
    elseif (interp_check == "C" .or. interp_check == "c") then
       interp_info%conserve = .true.
    elseif (interp_check == "N" .or. interp_check == "n") then
       interp_info%nearests2d = .true.
    else
       write(msg,500) trim(adjustl(interp))
       call abort_remap(msg=msg)
    end if
500 format("The interpolation type could not be determined from the value", &
         & 1x, a, 1x, ". Aborting!!!")
  end function get_interp
  
  !> @brief Determines the number of input variable item attributes.
  function get_nitems(filename) result(nitems)
    character(len=maxchar), intent(in) :: filename
    character(len=maxchar) :: dummy
    integer(ilong), parameter :: iounit = 88
    integer(ilong) :: nitems
    integer(ilong) :: ios
    
    nitems = 0
    open(unit=iounit, file=trim(adjustl(filename)), status="old", &
         action="read", iostat=ios)
    do
       read(iounit,500,iostat=ios) dummy
       if (ios==-1) exit
       nitems = nitems + 1
    end do
    close(iounit)
500 format(a)
  end function get_nitems

  !> @brief Determines the number of dimension variables for the
  !!        output netCDF file path.
  function get_ncdims(ncfile) result(ncdims)
    character(len=maxchar) :: ncfile
    integer(ilong) :: ncdims

    ncdims = 0
    if (trim(adjustl(ncfile)) /= "NOT USED") then
       ncdims = ncdims + 2
    end if
  end function get_ncdims
  
  !> @brief Builds the output netCDF formatted file.
  subroutine build_output(output_netcdf, dstgrid, esmffile, varinfo)
    type(esmffile_struct), intent(in) :: esmffile
    type(varinfo_struct), intent(in) :: varinfo
    type(dstgrid_struct), intent(inout) :: dstgrid
    type(ncvarinfo_struct) :: ncvarinfo
    character(len=maxchar), intent(inout) :: output_netcdf
    integer(ilong) :: idx

    call define_ncvars(varinfo=varinfo,ncvarinfo=ncvarinfo)
    

    call destroy_struct(dstgrid)
  end subroutine build_output

  !> @brief Defines the dimension variables for the output netCDF
  !!        formatted file.
  !!
  !! @params[in] esmffile
  !!    - An initialized `esmffile_struct` data structure variable.
  !!
  !! @params[inout] ncvarinfo
  !!    - A `ncvarinfo_struct` data structure variable.
  subroutine define_ncdims(esmffile, ncvarinfo)
    type(esmffile_struct), intent(in) :: esmffile
    type(ncvarinfo_struct), intent(inout) :: ncvarinfo

    ncvarinfo%ndims = 1
    ncvarinfo%ndims = ncvarinfo%ndims + get_ncdims(esmffile%bilinear)
    ncvarinfo%ndims = ncvarinfo%ndims + get_ncdims(esmffile%conserve)
    ncvarinfo%ndims = ncvarinfo%ndims + get_ncdims(esmffile%nearests2d)
  end subroutine define_ncdims

  !> @brief Defines the variables variables attributes.
  !!
  subroutine define_ncvars(varinfo,ncvarinfo)
    type(varinfo_struct), intent(in) :: varinfo
    type(ncvarinfo_struct), intent(inout) :: ncvarinfo
    integer(ilong) :: idx, ncvar
    
    ncvarinfo%nvars = varinfo%nvars + ncvarinfo%ndims
    call init_ncvarinfo(ncvarinfo=ncvarinfo)
    !! # TODO: This should be defined from the source grid variable
    !! # type; currently we assume all are double precision.
    ncvarinfo%dtype(1:ncvarinfo%nvars) = nf90_double 
    ncvar = 3
    !! # TODO Build structure here using generic subroutine.

  end subroutine define_ncvars

  !> @brief Define the destination grid attributes.
  !!
  !! @params[inout] dstgrid
  !!    - A `dstgrid_struct` data structure.
  subroutine get_gridinfo(dstgrid)
    type(dstgrid_struct), intent(inout) :: dstgrid
    type(ncdata) :: nccls
    character(len=maxchar) :: msg
    character(len=maxchar) :: varname
    real(rdouble), dimension(:), allocatable :: grid_var1d
    integer(ilong), dimension(2) :: grid_dims

    !! Check that the destination grid attributes are valid.
    if (trim(adjustl(grid_ncfile)) /= "NOT USED" .and. &
         trim(adjustl(grid_nclatname)) /= "NOT USED" .and. &
         trim(adjustl(grid_nclonname)) /= "NOT USED") then
       dstgrid%ncfile = grid_ncfile
       dstgrid%nclatname = grid_nclatname
       dstgrid%nclonname = grid_nclonname
    else
       write(msg,500) trim(adjustl(namelist_input))
       call abort_remap(msg=msg)
    end if

    !! Collect the destination grid attributes.
    nccls%ncfile = dstgrid%ncfile
    nccls%read = .true.
    call nccls%ncopen()
    varname = "grid_dims" !! # TODO: Is this general enough?
    call ncread(nccls=nccls, varname=varname, vararr=grid_dims)
    dstgrid%nlon = grid_dims(1)
    dstgrid%nlat = grid_dims(2)
    if (.not. allocated(grid_var1d)) &
         allocate(grid_var1d(dstgrid%nlon * dstgrid%nlat))
    dstgrid%nlevs = nlevs
    call init_struct(dstgrid)
    varname = dstgrid%nclatname
    call ncread(nccls=nccls, varname=varname, vararr=grid_var1d)
    dstgrid%lat = reshape(grid_var1d, (/dstgrid%nlon, dstgrid%nlat/))
    varname = dstgrid%nclonname
    call ncread(nccls=nccls, varname=varname, vararr=grid_var1d)
    dstgrid%lon = reshape(grid_var1d, (/dstgrid%nlon, dstgrid%nlat/))
    if (allocated(grid_var1d)) deallocate(grid_var1d)
    call nccls%ncclose()
500 format("The destination grid attributes have not been properly defined; "&
         "check that `grid_ncfile`, `grid_nclatname`, and `grid_nclonname ", &
         "have been defined within",1x,a,1x,". Aborting!!!")
  end subroutine get_gridinfo
  
  !> @brief Collect the ESMF files containing the interpolation
  !!        coefficients for the respective (supported) interpolation
  !!        types.
  !!
  !! @params[inout] esmffile
  !!    - # TODO
  subroutine get_esmf(esmffile)
    type(esmffile_struct), intent(inout) :: esmffile

    esmffile%bilinear = bilinear
    if (trim(adjustl(esmffile%bilinear)) /= "NOT_USED") &
         esmffile%bilinear_valid = .true.
    esmffile%conserve = conserve
    if (trim(adjustl(esmffile%conserve)) /= "NOT_USED") &
         esmffile%conserve_valid = .true.
    esmffile%nearests2d = nearests2d
    if (trim(adjustl(esmffile%nearests2d)) /= "NOT_USED") &
         esmffile%nearests2d_valid = .true.
  end subroutine get_esmf

  !> @brief Parses a comma-delimited file of strings and collects the
  !!        respective variable attributes.
  !!
  !! @params[inout] varinfo
  !!    - # TODO
  subroutine get_varinfo(varinfo)
    type(varinfo_struct), intent(inout) :: varinfo
    type(interp_struct) :: interpinfo
    character(len=maxchar) :: varstr
    character(len=1), parameter :: delimiter = ","
    integer(ilong), dimension(:), allocatable :: commaidx
    integer(ilong), parameter :: iounit = 777
    integer(ilong) :: idx, ios, nitem, sdx, strlen
    
    varinfo%nvars = get_nitems(filename=varsfile)
    call init_struct(varinfo)
    open(unit=iounit, file=trim(adjustl(varsfile)), status="old", &
         action="read")
    do idx = 1, varinfo%nvars
       read(iounit, *, iostat=ios) varstr
       if (ios == -1) then
          close(iounit)
          exit
       end if
       strlen = len(trim(adjustl(varstr)))
       if (.not. allocated(commaidx)) allocate(commaidx(strlen))
       nitem = 0
       do sdx = 1, strlen
          if (varstr(sdx:sdx) == delimiter) then
             nitem = nitem + 1
             commaidx(nitem) = sdx
          end if
       end do
       
       varinfo%var(idx)%ncvarin = varstr(1:commaidx(1)-1)
       varinfo%var(idx)%ncvarout = varstr(commaidx(1)+1:commaidx(2)-1)
       varinfo%var(idx)%interp_type = varstr(commaidx(2)+1:commaidx(3)-1)
       varinfo%var(idx)%ncfilein = varstr(commaidx(3)+1:commaidx(4)-1)
       if (allocated(commaidx)) deallocate(commaidx)
       interpinfo = get_interp(interp=varinfo%var(idx)%interp_type)
       varinfo%var(idx)%bilinear = interpinfo%bilinear
       varinfo%var(idx)%conserve = interpinfo%conserve
       varinfo%var(idx)%nearests2d = interpinfo%nearests2d
    end do
    close(iounit)
  end subroutine get_varinfo

  !> 
  
  !> @brief Reads the namelist file `esmf_remap.input`.
  !!
  !! @params # TODO
  subroutine read_namelist(esmffile, varinfo)
    type(esmffile_struct), intent(inout) :: esmffile
    type(varinfo_struct), intent(inout) :: varinfo   
    type(dstgrid_struct) :: dstgrid
    
    open(file=trim(adjustl(namelist_input)), unit=unit_nml, status="old", &
         form="formatted", action="read")
    !! # TODO: This needs to be generalized such that the namelist
    !! # block order does not matter.
    read(unit_nml,NML=share)
    read(unit_nml,NML=esmf)
    read(unit_nml,NML=gridinfo)
    close(unit_nml)
    call get_gridinfo(dstgrid=dstgrid)
    call get_esmf(esmffile=esmffile)
    call get_varinfo(varinfo=varinfo)
    call build_output(dstgrid=dstgrid, varinfo=varinfo,esmffile=esmffile, &
         output_netcdf=output_netcdf)
    call destroy_struct(grid=dstgrid)
  end subroutine read_namelist
end module namelist_interface
