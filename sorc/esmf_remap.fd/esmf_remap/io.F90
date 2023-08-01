!> @file io.F90
!! @details Handles all file input and output related to the remapping
!!          application(s).
!! @author Henry R. Winterbottom
!! @date 01 August 2023
!! @version 0.0.1
!! @license LGPL v2.1
module io_interface
  use netcdf_interface, only: ncdata, ncdimval, ncread, ncvardims
  use variables_interface, only: dstgrid_struct, esmffile_struct, ilong, &
       init_struct, interp_struct, maxchar, rdouble, var_struct, varinfo_struct
  implicit none
  private
  public :: abort_remap, get_diminfo, get_esmf, get_gridinfo, get_interp, get_nitems, &
       get_varinfo, write_msg
contains

  !> @brief Determines the interpolation type from the input variable
  !!        attributes.
  !!
  !! @params[in] interp_info
  !!    - The `interp_struct` variable containing the allowable
  !!      interpolation type attributes.
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
  !!
  !! @params[in] filename 
  !!    - The file path containing the comma-delimited list of
  !!      variables to be remapped.
  !!
  !! @result nitems
  !!    - The total number of variables to be remapped.
  function get_nitems(filename) result(nitems)
    character(len=maxchar), intent(in) :: filename
    character(len=maxchar) :: dummy
    integer(ilong), parameter :: iounit = 88
    integer(ilong) :: ios, nitems
    
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

  !> @brief Prints a message and aborts the program.
  !!
  !! @params[in] Character string to be written to `stdout` prior to
  !!             aborting the program.
  subroutine abort_remap(msg)
    character(len=maxchar), intent(in) :: msg

    write(6,*) msg
    stop 9999
  end subroutine abort_remap
  
  !> @brief Determines the maximum number of levels using the
  !!        attributes from the specified variables to be remapped.
  !!
  !! @params[in] var
  !!    - The `var_struct` variable containing the attributes for the
  !!      respective variable.
  !!
  !! @returns var
  !!    - The `var_struct` variable updated to contain the variable
  !!      dimension attributes.
  subroutine get_diminfo(var)
    type(var_struct), intent(inout) :: var
    type(ncdata) :: nccls
    character(len=maxchar) :: dimname
    integer(ilong) :: ndims, nlevs
    
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
    var%ndims = ndims; var%nlevs = nlevs
    call nccls%ncclose()
  end subroutine get_diminfo

  !> @brief Collect the ESMF files containing the interpolation
  !!        coefficients for the respective (supported) interpolation
  !!        types.
  !!
  !! @params[inout] esmffile
  !!    - The `esmffile_struct` variable containing file paths the
  !!      allowable remapping types.
  subroutine get_esmf(esmffile, bilinear, conserve, nearests2d)
    type(esmffile_struct), intent(inout) :: esmffile
    character(len=maxchar) :: bilinear, conserve, nearests2d

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

  !> @brief Define the destination grid attributes.
  !!
  !! @params[inout] dstgrid
  !!    - A `dstgrid_struct` data structure.
  !!
  !! @params grid_ncfile
  !!    - The destination grid netCDF-formatted file path.
  !!
  !! @params grid_nclonname
  !!    - The destination grid netCDF variable name for the longitude
  !!      coordinate variable.
  !!
  !! @params grid_nclatname
  !!    - The destination grid netCDF variable name for the latitude
  !!      coordinate variable.
  !!
  !! @params namelist_input
  !!    - The esmf_remap.input (e.g., namelist file) file path.
  subroutine get_gridinfo(dstgrid, grid_ncfile, grid_nclonname, &
       grid_nclatname, namelist_input)
    type(dstgrid_struct), intent(inout) :: dstgrid
    type(ncdata) :: nccls
    character(len=maxchar) :: grid_nclatname, grid_nclonname, &
         grid_ncfile, msg, namelist_input, varname
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
    varname = "grid_dims" !! # TODO: Is this general enough?; this
                          !! assumes SCRIP.
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
  
  !> @brief Parses a comma-delimited file of strings and collects the
  !!        respective remap variable attributes.
  !!
  !! @params[inout] varinfo
  !!    - The `varinfo_struct` variable updated upon exit containing
  !!      the necessary attributes for the remapping of the respective
  !!      variable.
  !!
  !! @params varsfile
  !!    - The path to the comma-delimitted file containing the
  !!      remapping variable attributes.
  subroutine get_varinfo(varinfo, varsfile)
    type(varinfo_struct), intent(inout) :: varinfo
    type(interp_struct) :: interpinfo
    type(ncdata) :: nccls
    character(len=maxchar) :: varsfile, varstr
    character(len=1), parameter :: delimiter = ","
    integer(ilong), dimension(:), allocatable :: commaidx
    integer(ilong), parameter :: iounit = 777
    integer(ilong) :: idx, ios, nitem, nlevs, sdx, strlen

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
       call get_diminfo(var=varinfo%var(idx))
       nlevs = max(nlevs, varinfo%var(idx)%nlevs)
       interpinfo = get_interp(interp=varinfo%var(idx)%interp_type)
       varinfo%var(idx)%bilinear = interpinfo%bilinear
       varinfo%var(idx)%conserve = interpinfo%conserve
       varinfo%var(idx)%nearests2d = interpinfo%nearests2d
    end do
    close(iounit)
  end subroutine get_varinfo

  !> @brief Writes a message string to standard output.
  !!
  !! @params[in] msg
  !!    - The message string to be written to standard output.
  subroutine write_msg(msg)
    character(len=maxchar) msg

    write(6,*) trim(adjustl(msg))
  end subroutine write_msg
end module io_interface
