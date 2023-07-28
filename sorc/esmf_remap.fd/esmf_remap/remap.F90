!> @file remap_interface.F90
!! @details Remaps specified variables from a source grid to a
!!          destination grid using pre-computed ESMF remapping
!!          coefficients.
!! @author Henry R. Winterbottom
!! @date 24 July 2023
!! @version 0.0.1
!! @license LGPL v2.1
module remap_interface
  use netcdf_interface, only: ncdata, ncread
  use variables_interface, only: abort_remap, destroy_struct, esmf_struct, &
       esmffile_struct, ilong, init_struct, maxchar, rdouble, rsingle, var_struct, &
       varinfo_struct
  implicit none
  private
  public :: remap
contains
  
  !> @brief Read in the respective ESMF remapping attributes.
  !!
  !! @params[inout] esmf 
  !!    - The `esmf_struct` variable; it is assumed that the
  !!      `filename` attribute has been defined prior to entry.
  subroutine esmf_read(esmf)
    type(esmf_struct), intent(inout) :: esmf
    type(ncdata) :: nccls
    character(len=maxchar) :: dimname, varname
 
    nccls%ncfile = esmf%filename
    nccls%read = .true.
    call nccls%ncopen()
    dimname = "n_s"
    call nccls%ncreaddim(dimname=dimname, dimval=esmf%n_s)
    dimname = "dst_grid_rank"
    call nccls%ncreaddim(dimname=dimname, dimval=esmf%dst_grid_rank)
    dimname = "src_grid_rank"
    call nccls%ncreaddim(dimname=dimname, dimval=esmf%src_grid_rank)
    call init_struct(esmf)
    varname = "S"
    call ncread(nccls=nccls, varname=varname, vararr=esmf%s)
    varname = "col"
    call ncread(nccls=nccls, varname=varname, vararr=esmf%col)
    varname = "row"
    call ncread(nccls=nccls, varname=varname, vararr=esmf%row)
    varname = "dst_grid_dims"
    call ncread(nccls=nccls, varname=varname, vararr=esmf%dst_grid_dims)
    varname = "src_grid_dims"
    call ncread(nccls=nccls, varname=varname, vararr=esmf%src_grid_dims)
    call nccls%ncclose()
  end subroutine esmf_read

  !> @brief Check that the ESMF remapping file is valid.
  !!
  !! @params[in] esmf_filename
  !!    - The path to the respective ESMF remapping coefficient file.
  !!
  !! @returns filename
  !!    - The valid ESMF remapping coefficient file path.
  function check_esmffile(esmf_filename) result(filename)
    character(len=maxchar), intent(in) :: esmf_filename
    character(len=maxchar) :: filename
    character(len=maxchar) :: msg
    
    filename = trim(adjustl(esmf_filename))
    if (filename == "NOT USED") then
       write(msg, 500) trim(adjustl(esmf_filename))
       call abort_remap(msg=msg)
    end if
  500 format("ESMF Interpolation file type", 1x, a, 1x, "has not been " &
           "specified. Aborting!!!")
  end function check_esmffile
  
  !> @brief Define the ESMF remapping file relative to the
  !!         interpolation type.
  !!
  !! @params[in] esmffile
  !!    - The `esmffile_struct` data structure containing the
  !!      available ESMF remapping coefficient files.
  !!
  !! @returns filename
  !!    - The ESMF remapping coefficient file path.
  function get_esmf_file(esmffile, var) result(filename)
    type(esmffile_struct), intent(in) :: esmffile
    type(var_struct), intent(in) :: var
    character(len=maxchar) :: filename

    if (var%bilinear) then
       filename = check_esmffile(esmf_filename=esmffile%bilinear)
       return
    end if
    if (var%conserve) then
       filename = check_esmffile(esmf_filename=esmffile%conserve)
       return
    end if    
    if (var%nearests2d) then
       filename = check_esmffile(esmf_filename=esmffile%nearests2d)
       return
    end if
  end function get_esmf_file
  
  !> @brief Remaps the input variable using the respective ESMF
  !!         remapping attributes.
  !!
  !! # TODO
  subroutine interp(esmf_grid, varin, varout)
    type(esmf_struct), intent(in) :: esmf_grid
    real(rdouble), dimension(:), intent(in) :: varin
    real(rdouble), dimension(:), intent(inout) :: varout
    integer(ilong) :: idx

    !! Compute the output variable `varout` by interpolating the
    !! respective input variable `varin` using the ESMF remapping
    !! coefficients.
    varout = 0.0_rdouble
    do idx = 1, esmf_grid%n_s
       varout(esmf_grid%row(idx)) = varout(esmf_grid%row(idx)) &
            + esmf_grid%s(idx) * varin(esmf_grid%col(idx))
    end do
  end subroutine interp
  
  !> @brief Remaps the specified variables from the source to the
  !!         destination grid projection.
  !!
  !! @params[in] esmffile
  !!    - The `esmffile_struct` data structure containing the
  !!      available ESMF remapping coefficient files.
  !!
  !! @params[in] varinfo
  !!    - The `varinfo_struct` data structure containing the
  !!      attributes for the respective variables specified for
  !!      interpolation.
  subroutine remap(esmffile, varinfo)
    type(esmffile_struct), intent(in) :: esmffile
    type(varinfo_struct), intent(in) :: varinfo
    type(esmf_struct) :: esmf
    integer(ilong) :: idx

    !! Loop through variables and interpolate accordingly.
    do idx = 1, varinfo%nvars
       esmf%filename = get_esmf_file(esmffile=esmffile, var=varinfo%var(idx))
       call esmf_read(esmf=esmf)
       
       !! # TODO: Here
       call destroy_struct(esmf)
    end do
  end subroutine remap
end module remap_interface
