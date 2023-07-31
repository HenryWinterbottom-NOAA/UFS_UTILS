!> @file namelist.F90
!! @details This module contains the FORTRAN namelist interface.
!! @author Henry R. Winterbottom
!! @date 30 June 2023
!! @version 0.0.1
!! @license LGPL v2.1
module namelist_interface
  use netcdf, only: nf90_double
  use netcdf_interface, only: destroy_ncvarinfo, init_ncvarinfo, ncdata, ncdimval, &
       ncread, ncvardims, ncvarinfo_struct, ncwrite
  use variables_interface, only: abort_remap, destroy_struct, dstgrid_struct, &
       esmffile_struct, get_boolean, ilong, init_struct, interp_struct, maxchar, &
       rdouble, var_struct, varinfo_struct
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
  namelist /share/ output_netcdf, varsfile
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

  !> @brief Determines the maximum number of levels using the
  !!        attributes from the specified variables to be remapped.
  !!
  !! @params[in] # TODO
  !!
  !!
  !! @returns nlevs
  !!    - The total number of levels for the specified variable.
  subroutine get_diminfo(var, ndims, nlevs)
    type(var_struct), intent(in) :: var
    type(ncdata) :: nccls
    character(len=maxchar) :: dimname
    integer(ilong), intent(out) :: nlevs
    integer(ilong), intent(out) :: ndims
    
    nccls%ncfile = var%ncfilein
    nccls%read = .true.
    call nccls%ncopen()
    call ncvardims(nccls=nccls, varname=var%ncvarin, ndims=ndims)
    if (ndims == 4) then
       call ncdimval(nccls=nccls, dimid=4, dimname=dimname, dimval=nlevs)
    elseif (ndims == 3) then
       call ncdimval(nccls=nccls, dimid=3, dimname=dimname, dimval=nlevs)
    else
       nlevs=1
    end if
    call nccls%ncclose()
  end subroutine get_diminfo
  
  !> @brief Builds the output netCDF formatted file.
  subroutine build_output(dstgrid, esmffile, varinfo)
    type(esmffile_struct), intent(in) :: esmffile
    type(ncdata) :: nccls
    type(varinfo_struct), intent(in) :: varinfo
    type(dstgrid_struct), intent(inout) :: dstgrid
    type(ncvarinfo_struct) :: ncvarinfo
    integer(ilong) :: nlevs

    nccls%ncfile = trim(adjustl(output_netcdf))
    nccls%write = .true.
    call nccls%ncopen()
    call define_ncvars(nccls=nccls,varinfo=varinfo,ncvarinfo=ncvarinfo, &
         dstgrid=dstgrid)
    call init_ncvarinfo(ncvarinfo=ncvarinfo)
    call nccls%ncwritedef(ncvarinfo=ncvarinfo)
    call write_output(nccls=nccls, dstgrid=dstgrid)
    call nccls%ncclose()
    call destroy_struct(dstgrid)
  end subroutine build_output

  !> @brief Define the netCDF-formatted file dimension attributes.
  subroutine define_ncdims(nccls, ncvarinfo, varinfo, dstgrid)
    type(dstgrid_struct), intent(inout) :: dstgrid
    type(ncdata) :: nccls
    type(ncvarinfo_struct), intent(inout) :: ncvarinfo
    type(varinfo_struct), intent(in) :: varinfo
    integer(ilong) :: idx
    
    !! Define the netCDF dimension attributes.

    dstgrid%nlevs = maxval(varinfo%var(:)%nlevs)
    call init_struct(dstgrid)
    ncvarinfo%dimname(1) = "nlons"
    ncvarinfo%dimid(1) = 1
    ncvarinfo%dimval(1) = dstgrid%nlon
    ncvarinfo%dimname(2) = "nlats"
    ncvarinfo%dimid(2) = 2
    ncvarinfo%dimval(2) = dstgrid%nlat
    ncvarinfo%dimname(3) = "nlevels"
    ncvarinfo%dimid(3) = 3
    ncvarinfo%dimval(3) = dstgrid%nlevs
    do idx = 1, ncvarinfo%dimval(3)
       dstgrid%levels(idx) = real(idx)
    end do
    ncvarinfo%varndim(1) = 2
    ncvarinfo%varname(1) = "lons"
    ncvarinfo%varid(1) = 1
    ncvarinfo%vardimid(1,1) = 1; ncvarinfo%vardimid(1,2) = 2
    ncvarinfo%varndim(2) = 2
    ncvarinfo%varname(2) = "lats"
    ncvarinfo%varid(2) = 2
    ncvarinfo%vardimid(2,1) = 1; ncvarinfo%vardimid(2,2) = 2
    ncvarinfo%varndim(3) = 1
    ncvarinfo%varname(3) = "levels"
    ncvarinfo%varid(3) = 3
    ncvarinfo%vardimid(3,1) = 3
  end subroutine define_ncdims
    
  !> @brief Defines the variables variables attributes.
  !! # TODO
  subroutine define_ncvars(nccls,varinfo,ncvarinfo, dstgrid)
    type(dstgrid_struct), intent(inout) :: dstgrid
    type(ncdata), intent(in) :: nccls
    type(varinfo_struct), intent(in) :: varinfo
    type(ncvarinfo_struct), intent(inout) :: ncvarinfo
    character(len=maxchar) :: varname
    
    !! # TODO: This can be generalized further; right now we assume
    !! # only (nx,ny,nz) type arrays.
    ncvarinfo%ndims = 3  
    ncvarinfo%nvars = ncvarinfo%ndims !! + varinfo%nvars
    call init_ncvarinfo(ncvarinfo=ncvarinfo)
    ncvarinfo%dtype(1:ncvarinfo%nvars) = nf90_double 
    call define_ncdims(nccls=nccls, ncvarinfo=ncvarinfo, dstgrid=dstgrid, &
         varinfo=varinfo) 
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
    type(interp_struct) :: interpinfo
    type(ncdata) :: nccls
    type(varinfo_struct), intent(inout) :: varinfo
    character(len=maxchar) :: varstr
    character(len=1), parameter :: delimiter = ","
    integer(ilong), dimension(:), allocatable :: commaidx
    integer(ilong), parameter :: iounit = 777
    integer(ilong) :: idx, ios, nitem, sdx, strlen

    nlevs = 1
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
       varinfo%var(idx)%ncfilein = varstr(commaidx(3)+1:len(varstr))       
       if (allocated(commaidx)) deallocate(commaidx)
       call get_diminfo(var=varinfo%var(idx), ndims=varinfo%var(idx)%ndims, &
            nlevs=varinfo%var(idx)%nlevs)
       nlevs = max(nlevs, varinfo%var(idx)%nlevs)
       interpinfo = get_interp(interp=varinfo%var(idx)%interp_type)
       varinfo%var(idx)%bilinear = interpinfo%bilinear
       varinfo%var(idx)%conserve = interpinfo%conserve
       varinfo%var(idx)%nearests2d = interpinfo%nearests2d
    end do
    close(iounit)
  end subroutine get_varinfo

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
    call build_output(dstgrid=dstgrid,varinfo=varinfo,esmffile=esmffile)
    call destroy_struct(grid=dstgrid)
  end subroutine read_namelist

  !> @brief
  subroutine write_output(nccls, dstgrid)
    type(dstgrid_struct) :: dstgrid
    type(ncdata) :: nccls
    character(len=maxchar) :: msg
    
    call ncwrite(nccls=nccls,varname="lons", vararr=dstgrid%lon)
    write(msg,500) "lons", trim(adjustl(nccls%ncfile)); write(6,*) trim(adjustl(msg))
    call ncwrite(nccls=nccls,varname="lats", vararr=dstgrid%lat)
    write(msg,500) "lats", trim(adjustl(nccls%ncfile)); write(6,*) trim(adjustl(msg))
    call ncwrite(nccls=nccls,varname="levels", vararr=dstgrid%levels)
    write(msg,500) "levels", trim(adjustl(nccls%ncfile)); write(6,*) trim(adjustl(msg))
    
500 format("Writing variable", 1x, a, 1x, "to netCDF file path", 1x, a, 1x,".")
  end subroutine write_output
end module namelist_interface
