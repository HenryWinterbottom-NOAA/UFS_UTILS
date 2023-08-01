!> @file remap_interface.F90
!! @details Remaps specified variables from a source grid to a
!!          destination grid using pre-computed ESMF remapping
!!          coefficients.
!! @author Henry R. Winterbottom
!! @date 01 August 2023
!! @version 0.0.1
!! @license LGPL v2.1
module remap_interface
  use io_interface, only: abort_remap, write_msg
  use namelist_interface, only: output_netcdf
  use netcdf_interface, only: ncdata, ncread, ncwrite
  use variables_interface, only: destroy_struct, dstgrid_struct, esmf_struct, &
       esmffile_struct, ilong, init_struct, maxchar, rdouble, var_struct, &
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
    character(len=maxchar) :: filename, msg
    
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
    character(len=maxchar) :: filename, msg

    if (var%bilinear) then
       filename = check_esmffile(esmf_filename=esmffile%bilinear)
       return
    elseif (var%conserve) then
       filename = check_esmffile(esmf_filename=esmffile%conserve)
       return
    elseif (var%nearests2d) then
       filename = check_esmffile(esmf_filename=esmffile%nearests2d)
       return
    else
       write(msg,500)
       call abort_remap(msg=msg)
    end if
500 format("Interpolation type either not specified or not supported. " &
         "Aborting!!!")
  end function get_esmf_file
  
  !> @brief Remaps the input variable using the respective ESMF
  !!         remapping attributes.
  !!
  !! @params[in] esmf_grid
  !!    - The initialized `esmf_grid` variable.
  !!
  !! @params[in] varin
  !!    - A 1-dimensional double-precision array of the source grid
  !!      variable for a given level (e.g., vertical slice).
  !!
  !! @params[inout] varout
  !!    - A 1-dimensional double-precision array of the destination
  !!      grid variables for a given level (e.g., vertical slice).
  subroutine interp(esmf_grid, varin, varout)
    type(esmf_struct), intent(in) :: esmf_grid
    real(rdouble), dimension(:), intent(inout) :: varout
    real(rdouble), dimension(:), intent(in) :: varin
    integer(ilong) :: idx

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
    type(ncdata) :: ncclsin, ncclsout
    type(esmf_struct) :: esmf
    character(len=maxchar) :: msg
    real(rdouble), dimension(:,:,:), allocatable :: ncvarin, ncvarout
    real(rdouble), dimension(:), allocatable :: varin, varout
    integer(ilong) :: idx, ilevs

    !! Loop through the specified variables.
    do idx = 1, varinfo%nvars

       !! Read the ESMF remapping coefficients for the respective
       !! interpolation type.
       esmf%filename = get_esmf_file(esmffile=esmffile, var=varinfo%var(idx))
       call esmf_read(esmf=esmf)
       if (.not. allocated(ncvarin)) &
            allocate(ncvarin(esmf%src_grid_dims(1),esmf%src_grid_dims(2), &
            varinfo%var(idx)%nlevs))
       if (.not. allocated(ncvarout)) &
            allocate(ncvarout(esmf%dst_grid_dims(1),esmf%dst_grid_dims(2), &
            varinfo%var(idx)%nlevs))
       if (.not. allocated(varin)) &
            allocate(varin(esmf%src_grid_dims(1)*esmf%src_grid_dims(2)))
       if (.not. allocated(varout)) &
            allocate(varout(esmf%dst_grid_dims(1)*esmf%dst_grid_dims(2)))

       !! Read the source grid variable and interpolate as a function
       !! of vertical level (slice).
       ncclsin%ncfile = varinfo%var(idx)%ncfilein
       ncclsin%read = .true.
       call ncclsin%ncopen()
       call ncread(nccls=ncclsin,varname=varinfo%var(idx)%ncvarin,vararr=ncvarin)
       call ncclsin%ncclose()
       do ilevs = 1, varinfo%var(idx)%nlevs
          write(msg,500) trim(adjustl(varinfo%var(idx)%ncvarin)), ilevs
          call write_msg(msg=msg)
          varin = reshape(ncvarin(:,:,ilevs), (/shape(varin)/))
          call interp(esmf_grid=esmf,varin=varin,varout=varout)
          ncvarout(:,:,ilevs) = reshape(varout, (/shape(ncvarout(:,:,ilevs))/))
       end do
       if (allocated(ncvarin)) deallocate(ncvarin)
       if (allocated(varin)) deallocate(varin)

       !! Write the interpolated variable to the output
       !! netCDF-formatted file path.
       ncclsout%ncfile = output_netcdf
       ncclsout%read_write = .true.
       write(msg,501)  trim(adjustl(varinfo%var(idx)%ncvarout)), &
            trim(adjustl(output_netcdf))
       call write_msg(msg=msg)
       call ncclsout%ncopen()
       call ncwrite(nccls=ncclsout,varname=varinfo%var(idx)%ncvarout,vararr=ncvarout)
       call ncclsout%ncclose()
       if (allocated(ncvarout)) deallocate(ncvarout)
       if (allocated(varout)) deallocate(varout)
       call destroy_struct(grid=esmf)
    end do
500 format("Interpolating variable",1x,a,1x,"at level",1x,i3.3,".")
501 format("Writing interpolated variable",1x,a,1x,"to netCDF file path",1x,a,".")
  end subroutine remap
end module remap_interface
