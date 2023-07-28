!> @file netcdf.F90
!! @details This module contains the base-class object for the netCDF
!!          API.
!! @author Henry R. Winterbottom
!! @date 08 July 2023
!! @version 0.0.1
!! @license LGPL v2.1
module netcdf_interface
   use netcdf
   use variables_interface, only: abort_remap, ilong, maxchar, rdouble, rsingle
   implicit none
   private
   public :: destroy_ncvarinfo
   public :: init_ncvarinfo
   public :: ncread
   public :: ncvardims
   public :: ncvarinfo_struct
   public :: ncwrite
   
   interface ncread
      module procedure read_arr1d_double
      module procedure read_arr1d_integer
      module procedure read_arr1d_single
      module procedure read_arr2d_double
      module procedure read_arr2d_single
      module procedure read_arr3d_double
      module procedure read_arr3d_single
      module procedure read_scalar_double
      module procedure read_scalar_single
   end interface ncread
   
   interface ncwrite
      module procedure write_arr1d_double
      module procedure write_arr1d_single
      module procedure write_arr2d_double
      module procedure write_arr2d_single
      module procedure write_arr3d_double
      module procedure write_arr3d_single
      module procedure write_scalar_double
      module procedure write_scalar_single
   end interface ncwrite
   
   type, public :: ncdata
      character(len=maxchar) :: ncfile
      logical :: read = .false.
      logical :: read_write = .false.
      logical :: write = .false.
      integer(ilong) :: ncdimid
      integer(ilong) :: ncfileid
      integer(ilong) :: ncvarid
      integer(ilong) :: ncstatus
    contains
      procedure, public :: ncclose
      procedure, public :: ncopen
      procedure, public :: ncreaddim
      procedure, public :: ncwritedef
   end type ncdata
   
   type, public :: ncvarinfo_struct
      character(len=maxchar), dimension(:), allocatable :: dimname
      character(len=maxchar), dimension(:), allocatable :: varname
      integer(ilong), dimension(:, :), allocatable :: vardimid
      integer(ilong), dimension(:), allocatable :: varndim
      integer(ilong), dimension(:), allocatable :: dimval
      integer(ilong), dimension(:), allocatable :: dimid
      integer(ilong), dimension(:), allocatable :: dtype
      integer(ilong), dimension(:), allocatable :: varid
      integer(ilong) :: nattrs
      integer(ilong) :: ndims
      integer(ilong) :: nvars
   end type ncvarinfo_struct
 contains

   !> @brief Destroys the `ncvarinfo_struct` variable.
   subroutine destroy_ncvarinfo(ncvarinfo)
     type(ncvarinfo_struct) :: ncvarinfo
     
     if (allocated(ncvarinfo%dtype)) deallocate (ncvarinfo%dtype)
     if (allocated(ncvarinfo%dimid)) deallocate (ncvarinfo%dimid)
     if (allocated(ncvarinfo%dimname)) deallocate (ncvarinfo%dimname)
     if (allocated(ncvarinfo%dimval)) deallocate (ncvarinfo%dimval)
     if (allocated(ncvarinfo%varname)) deallocate (ncvarinfo%varname)
     if (allocated(ncvarinfo%vardimid)) deallocate (ncvarinfo%vardimid)
     if (allocated(ncvarinfo%varndim)) deallocate (ncvarinfo%varndim)
     if (allocated(ncvarinfo%varid)) deallocate (ncvarinfo%varid)
   end subroutine destroy_ncvarinfo
   
   !> @brief Initializes the `ncvarinfo_struct` variable.
   subroutine init_ncvarinfo(ncvarinfo)
     type(ncvarinfo_struct) :: ncvarinfo
     
     if (.not. allocated(ncvarinfo%dtype)) allocate (ncvarinfo%dtype(ncvarinfo%nvars))
     if (.not. allocated(ncvarinfo%dimid)) allocate (ncvarinfo%dimid(ncvarinfo%ndims))
     if (.not. allocated(ncvarinfo%dimname)) allocate (ncvarinfo%dimname(ncvarinfo%ndims))
     if (.not. allocated(ncvarinfo%dimval)) allocate (ncvarinfo%dimval(ncvarinfo%ndims))
     if (.not. allocated(ncvarinfo%varname)) allocate (ncvarinfo%varname(ncvarinfo%nvars))
     if (.not. allocated(ncvarinfo%vardimid)) &
          allocate (ncvarinfo%vardimid(ncvarinfo%nvars, ncvarinfo%ndims))
     if (.not. allocated(ncvarinfo%varndim)) allocate (ncvarinfo%varndim(ncvarinfo%nvars))
     if (.not. allocated(ncvarinfo%varid)) allocate (ncvarinfo%varid(ncvarinfo%nvars))
   end subroutine init_ncvarinfo
   
   !> @brief Closes an open netCDF file object.
   subroutine ncclose(this)
     class(ncdata), intent(inout) :: this
     character(len=maxchar) :: msg
     
     this%ncstatus = nf90_close(this%ncfileid)
     if (this%ncstatus /= 0) call ncerror(nccls=this)
   end subroutine ncclose
   
   !> @brief Defines the integer dimension variable identification key
   !!         within an open netCDF-formatted file path.
   !!
   !! @params[inout] nccls
   !!    - An initialized netCDF object.
   !!
   !! @params[in] dimname
   !!    - The netCDF dimension variable name.
   subroutine ncdimid(nccls, dimname)
     class(ncdata), intent(inout) :: nccls
     character(len=maxchar) :: dimname
     character(len=maxchar) :: msg
     
     nccls%ncstatus = nf90_inq_dimid(nccls%ncfileid, trim(adjustl(dimname)), &
          nccls%ncdimid)
   end subroutine ncdimid
   
   !> @brief Raises an exception for errors encountered by the
   !!        respective netCDF class.
   !!
   !! @params[inout] nccls
   !!    - An initialized netCDF object.
   subroutine ncerror(nccls)
     class(ncdata), intent(in) :: nccls
     character(len=maxchar) :: msg
     
     write (msg, 500) trim(nf90_strerror(nccls%ncstatus))
     call abort_remap(msg=msg)
500  format("NetCDF failed with error", 1x, a, 1x, ". Aborting!!!")
   end subroutine ncerror
   
   !> @brief Defines/opens a netCDF file object.
   subroutine ncopen(this)
     class(ncdata), intent(inout) :: this
     character(len=maxchar) :: filename
     character(len=maxchar) :: msg
     
     !! Open the respective netCDF-formatted file accordingly.
     filename = this%ncfile
     if (this%read) then
        this%ncstatus = nf90_open(trim(adjustl(filename)), nf90_nowrite, &
             this%ncfileid)
     elseif (this%read_write) then
        this%ncstatus = nf90_open(trim(adjustl(filename)), nf90_write, &
             this%ncfileid)
     elseif (this%write) then
        this%ncstatus = nf90_create(trim(adjustl(filename)), nf90_clobber, &
             this%ncfileid)
     else
        if (this%ncstatus /= 0) call ncerror(nccls=this)
     end if
   end subroutine ncopen
   
   !> @brief Reads dimension variable value.
   !!
   !! @params[inout] nccls
   !!    - An initialized netCDF object.
   !!
   !! @params[in] dimname
   !!    - The netCDF dimension variable name.
   !!
   !! @returns dimval
   !!    - The netCDF dimension variable value.
   subroutine ncreaddim(nccls, dimname, dimval)
     class(ncdata), intent(inout) :: nccls
     character(len=maxchar) :: dimname
     character(len=maxchar) :: msg
     integer(ilong), intent(out) :: dimval
     
     nccls%ncstatus = nf90_inq_dimid(nccls%ncfileid, trim(adjustl(dimname)), &
          nccls%ncdimid)
     if (nccls%ncstatus /= 0) call ncerror(nccls=nccls)
     nccls%ncstatus = nf90_inquire_dimension(nccls%ncfileid, nccls%ncdimid, &
          len=dimval)
     if (nccls%ncstatus /= 0) call ncerror(nccls=nccls)
   end subroutine ncreaddim
   
   !> @brief Returns the number of dimensions for the specified
   !!        variable `varname`.
   !!
   !! @params[inout] nccls
   !!    - An initialized netCDF object.
   !!
   !! @params[in] varname
   !!    - The netCDF variable name.
   !!
   !! @returns ndims
   !!    - The number of dimensions for the specified variable
   !!      `varname`.
   subroutine ncvardims(nccls, varname, ndims)
     class(ncdata), intent(inout) :: nccls
     character(len=maxchar) :: varname
     integer(ilong) :: ndims
     
     call ncvarid(nccls=nccls, varname=varname)
     nccls%ncstatus = nf90_inquire_variable(nccls%ncfileid, nccls%ncvarid, &
          ndims=ndims)
     if (nccls%ncstatus /= 0) call ncerror(nccls=nccls)
   end subroutine ncvardims
   
   !> @brief Defines the integer variable identification key within an
   !!        open netCDF-formatted file path.
   !!
   !! @params[inout] nccls
   !!    - An initialized netCDF object.
   !!
   !! @params[in] varname
   !!    - The netCDF variable name.
   subroutine ncvarid(nccls, varname)
     class(ncdata), intent(inout) :: nccls
     character(len=maxchar) :: varname
     character(len=maxchar) :: msg
     
     nccls%ncstatus = nf90_inq_varid(nccls%ncfileid, trim(adjustl(varname)), &
          nccls%ncvarid)
     if (nccls%ncstatus /= 0) call ncerror(nccls=nccls)
   end subroutine ncvarid
   
   !> @brief Writes the netCDF-formatted file path dimension and
   !!        variable attributes.
   !!
   !! @params[inout] nccls
   !!    - An initialized netCDF object.
   !!
   !! @params[in] ncvarinfo
   !!    - A `ncvarinfo_struct` variable.
   subroutine ncwritedef(nccls, ncvarinfo)
     class(ncdata), intent(inout) :: nccls
     type(ncvarinfo_struct) :: ncvarinfo
     character(len=maxchar) :: msg
     integer(ilong) :: idx

     !! Build the dimension attributes.
     do idx = 1, ncvarinfo%ndims
        nccls%ncstatus = nf90_def_dim(nccls%ncfileid, &
             trim(adjustl(ncvarinfo%dimname(idx))), &
             ncvarinfo%dimval(idx), ncvarinfo%dimid(idx))
     end do

     !! Build the variable arrays.
     do idx = 1, ncvarinfo%nvars
        nccls%ncstatus = nf90_def_var(nccls%ncfileid, &
             trim(adjustl(ncvarinfo%varname(idx))), &
             ncvarinfo%dtype(idx), ncvarinfo%vardimid(idx, 1:ncvarinfo%varndim(idx)), &
             ncvarinfo%varid(idx))
        if (nccls%ncstatus /= 0) call ncerror(nccls=nccls)
     end do
     nccls%ncstatus = nf90_enddef(nccls%ncfileid)
   end subroutine ncwritedef

  !> @brief Read a double-precision 1-dimensional variable array.
  !!
  !! @params[inout] nccls
  !!    - An initialized netCDF object.
  !!
  !! @params[in] varname
  !!    - The netCDF variable name.
  !!
  !! @returns[out] varrar
  !!    - The netCDF variable 1-dimensional array values.
  subroutine read_arr1d_double(nccls, varname, vararr)
    class(ncdata), intent(inout) :: nccls
    character(len=maxchar) :: varname
    real(rdouble), dimension(:), intent(out) :: vararr
     
    call ncvarid(nccls=nccls, varname=varname)
    nccls%ncstatus = nf90_get_var(nccls%ncfileid, nccls%ncvarid, vararr)
    if (nccls%ncstatus /= 0) call ncerror(nccls=nccls)
  end subroutine read_arr1d_double

  !> @brief Read an integer 1-dimensional variable array.
  !!
  !! @params[inout] nccls
  !!    - An initialized netCDF object.
  !!
  !! @params[in] varname
  !!    - The netCDF variable name.
  !!
  !! @returns[out] varrar
  !!    - The netCDF variable 1-dimensional array values.
  subroutine read_arr1d_integer(nccls, varname, vararr)
    class(ncdata), intent(inout) :: nccls
    character(len=maxchar) :: varname
    integer(ilong), dimension(:), intent(out) :: vararr
     
    call ncvarid(nccls=nccls, varname=varname)
    nccls%ncstatus = nf90_get_var(nccls%ncfileid, nccls%ncvarid, vararr)
    if (nccls%ncstatus /= 0) call ncerror(nccls=nccls)
  end subroutine read_arr1d_integer

  !> @brief Read a single-precision 1-dimensional variable array.
  !!
  !! @params[inout] nccls
  !!    - An initialized netCDF object.
  !!
  !! @params[in] varname
  !!    - The netCDF variable name.
  !!
  !! @params[inout] vararr
  !!    - An initialized netCDF variable scalar/array.
  !!
  !! @returns varrar
  !!    - The netCDF variable 1-dimensional array values.
  subroutine read_arr1d_single(nccls, varname, vararr)
    class(ncdata), intent(inout) :: nccls
    character(len=maxchar) :: varname
    real(rsingle), dimension(:), intent(out) :: vararr
    
    call ncvarid(nccls=nccls, varname=varname)
    nccls%ncstatus = nf90_get_var(nccls%ncfileid, nccls%ncvarid, vararr)
    if (nccls%ncstatus /= 0) call ncerror(nccls=nccls)
  end subroutine read_arr1d_single

  !> @brief Read a double-precision 2-dimensional variable array.
  !!
  !! @params[inout] nccls
  !!    - An initialized netCDF object.
  !!
  !! @params[in] varname
  !!    - The netCDF variable name.
  !!
  !! @params[inout] vararr
  !!    - An initialized netCDF variable scalar/array.
  !!
  !! @returns varrar
  !!    - The netCDF variable 2-dimensional array values.
   subroutine read_arr2d_double(nccls, varname, vararr)
     class(ncdata), intent(inout) :: nccls
     character(len=maxchar) :: varname
     real(rdouble), dimension(:, :), intent(out) :: vararr
     
     call ncvarid(nccls=nccls, varname=varname)
     nccls%ncstatus = nf90_get_var(nccls%ncfileid, nccls%ncvarid, vararr)
     if (nccls%ncstatus /= 0) call ncerror(nccls=nccls)
   end subroutine read_arr2d_double

  !> @brief Read a single-precision 2-dimensional variable array.
  !!
  !! @params[inout] nccls
  !!    - An initialized netCDF object.
  !!
  !! @params[in] varname
  !!    - The netCDF variable name.
  !!
  !! @params[inout] vararr
  !!    - An initialized netCDF variable scalar/array.
  !!
  !! @returns varrar
  !!    - The netCDF variable 2-dimensional array values.
   subroutine read_arr2d_single(nccls, varname, vararr)
     class(ncdata), intent(inout) :: nccls
     character(len=maxchar) :: varname
     real(rsingle), dimension(:, :), intent(out) :: vararr
     
     call ncvarid(nccls=nccls, varname=varname)
     nccls%ncstatus = nf90_get_var(nccls%ncfileid, nccls%ncvarid, vararr)
     if (nccls%ncstatus /= 0) call ncerror(nccls=nccls)
   end subroutine read_arr2d_single
   
  !> @brief Read a double-precision 3-dimensional variable array.
  !!
  !! @params[inout]:nccls
  !!    - An initialized netCDF object.
  !!
  !! @params[in] varname
  !!    - The netCDF variable name.
  !!
  !! @params[inout] vararr
  !!    - An initialized netCDF variable scalar/array.
  !!
  !! @returns varrar
  !!    - The netCDF variable 3-dimensional array values.
   subroutine read_arr3d_double(nccls, varname, vararr)
     class(ncdata), intent(inout) :: nccls
     character(len=maxchar) :: varname
     real(rdouble), dimension(:, :, :), intent(out) :: vararr
     
     call ncvarid(nccls=nccls, varname=varname)
     nccls%ncstatus = nf90_get_var(nccls%ncfileid, nccls%ncvarid, vararr)
     if (nccls%ncstatus /= 0) call ncerror(nccls=nccls)
   end subroutine read_arr3d_double
   
   !> @brief Read a single-precision 3-dimensional variable array.
   !!
  !! @params[inout] nccls
  !!    - An initialized netCDF object.
  !!
  !! @params[in] varname
  !!    - The netCDF variable name.
  !!
  !! @params[inout] vararr
  !!    - An initialized netCDF variable scalar/array.
  !!
  !! @returns varrar
  !!    - The netCDF variable 3-dimensional array values.
   subroutine read_arr3d_single(nccls, varname, vararr)
     class(ncdata), intent(inout) :: nccls
     character(len=maxchar) :: varname
     real(rsingle), dimension(:, :, :), intent(out) :: vararr
     
     call ncvarid(nccls=nccls, varname=varname)
     nccls%ncstatus = nf90_get_var(nccls%ncfileid, nccls%ncvarid, vararr)
     if (nccls%ncstatus /= 0) call ncerror(nccls=nccls)
   end subroutine read_arr3d_single

   !> @brief Read a double-precision scalar variable.
  !!
  !! @params[inout] nccls
  !!    - An initialized netCDF object.
  !!
  !! @params[in] varname
  !!    - The netCDF variable name.
  !!
  !! @params[inout] vararr
  !!    - An initialized netCDF variable scalar/array.
  !!
  !! @returns varrar
  !!    - The netCDF variable value(s).
   subroutine read_scalar_double(nccls, varname, vararr)
     class(ncdata), intent(inout) :: nccls
     character(len=maxchar) :: varname
     real(rdouble), intent(out) :: vararr
     
     call ncvarid(nccls=nccls, varname=varname)
     nccls%ncstatus = nf90_get_var(nccls%ncfileid, nccls%ncvarid, vararr)
     if (nccls%ncstatus /= 0) call ncerror(nccls=nccls)
   end subroutine read_scalar_double

   !> @brief Read a single-precision scalar variable.
  !!
  !! @params[inout] nccls
  !!    - An initialized netCDF object.
  !!
  !! @params[in] varname
  !!    - The netCDF variable name.
  !!
  !! @params[inout] vararr
  !!    - An initialized netCDF variable scalar/array.
  !!
  !! @returns varrar
  !!    - The netCDF variable value(s).
   subroutine read_scalar_single(nccls, varname, vararr)
     class(ncdata), intent(inout) :: nccls
     character(len=maxchar) :: varname
     real(rsingle), intent(out) :: vararr
     
     call ncvarid(nccls=nccls, varname=varname)
     nccls%ncstatus = nf90_get_var(nccls%ncfileid, nccls%ncvarid, vararr)
     if (nccls%ncstatus /= 0) call ncerror(nccls=nccls)
   end subroutine read_scalar_single
   
   !> @brief Writes a double precision 1-dimensional array of values.
  !!
  !! @params[inout] nccls
  !!    - An initialized netCDF object.
  !!
  !! @params[in] varname
  !!    - The netCDF variable name.
  !!
  !! @params[in] vararr
  !!    - The netCDF variable scalar/array.
   subroutine write_arr1d_double(nccls, varname, vararr)
     class(ncdata), intent(inout) :: nccls
     character(len=maxchar) :: varname
     real(rdouble), dimension(:) :: vararr
     
     call ncvarid(nccls=nccls, varname=varname)
     nccls%ncstatus = nf90_put_var(nccls%ncfileid, nccls%ncvarid, vararr)
     if (nccls%ncstatus /= 0) call ncerror(nccls=nccls)
   end subroutine write_arr1d_double

   !> @brief Writes a single precision 1-dimensional array of values.
  !!
  !! @params[inout] nccls
  !!    - An initialized netCDF object.
  !!
  !! @params[in] varname
  !!    - The netCDF variable name.
  !!
  !! @params[in] vararr
  !!    - The netCDF variable scalar/array.
   subroutine write_arr1d_single(nccls, varname, vararr)
     class(ncdata), intent(inout) :: nccls
     character(len=maxchar) :: varname
     real(rsingle), dimension(:) :: vararr
     
     call ncvarid(nccls=nccls, varname=varname)
     nccls%ncstatus = nf90_put_var(nccls%ncfileid, nccls%ncvarid, vararr)
     if (nccls%ncstatus /= 0) call ncerror(nccls=nccls)
   end subroutine write_arr1d_single

   !> @brief Writes a double precision 2-dimensional array of values.
  !!
  !! @params[inout] nccls
  !!    - An initialized netCDF object.
  !!
  !! @params[in] varname
  !!    - The netCDF variable name.
  !!
  !! @params[in] vararr
  !!    - The netCDF variable scalar/array.
   subroutine write_arr2d_double(nccls, varname, vararr)
     class(ncdata), intent(inout) :: nccls
     character(len=maxchar) :: varname
     real(rdouble), dimension(:, :) :: vararr
     
     call ncvarid(nccls=nccls, varname=varname)
     nccls%ncstatus = nf90_put_var(nccls%ncfileid, nccls%ncvarid, vararr)
     if (nccls%ncstatus /= 0) call ncerror(nccls=nccls)
   end subroutine write_arr2d_double

   !> @brief Writes a single precision 2-dimensional array of values.
  !!
  !! @params[inout] nccls
  !!    - An initialized netCDF object.
  !!
  !! @params[in] varname
  !!    - The netCDF variable name.
  !!
  !! @params[in] vararr
  !!    - The netCDF variable scalar/array.
   subroutine write_arr2d_single(nccls, varname, vararr)
     class(ncdata), intent(inout) :: nccls
     character(len=maxchar) :: varname
     real(rsingle), dimension(:, :) :: vararr
     
     call ncvarid(nccls=nccls, varname=varname)
     nccls%ncstatus = nf90_put_var(nccls%ncfileid, nccls%ncvarid, vararr)
     if (nccls%ncstatus /= 0) call ncerror(nccls=nccls)
   end subroutine write_arr2d_single

   !> @brief Writes a double precision 3-dimensional array of values.
  !!
  !! @params[inout] nccls
  !!    - An initialized netCDF object.
  !!
  !! @params[in] varname
  !!    - The netCDF variable name.
  !!
  !! @params[in] vararr
  !!    - The netCDF variable scalar/array.
   subroutine write_arr3d_double(nccls, varname, vararr)
     class(ncdata), intent(inout) :: nccls
     character(len=maxchar) :: varname
     real(rdouble), dimension(:, :, :) :: vararr
     
     call ncvarid(nccls=nccls, varname=varname)
     nccls%ncstatus = nf90_put_var(nccls%ncfileid, nccls%ncvarid, vararr)
     if (nccls%ncstatus /= 0) call ncerror(nccls=nccls)
   end subroutine write_arr3d_double
   
   !> @brief Writes a single precision 3-dimensional array of values.
   !!
   !! @params[inout] nccls
   !!    - An initialized netCDF object.
   !!
   !! @params[in] varname
   !!    - The netCDF variable name.
  !!
  !! @params[in] vararr
  !!    - The netCDF variable scalar/array.
   subroutine write_arr3d_single(nccls, varname, vararr)
     class(ncdata), intent(inout) :: nccls
     character(len=maxchar) :: varname
     real(rsingle), dimension(:, :, :) :: vararr
     
     call ncvarid(nccls=nccls, varname=varname)
     nccls%ncstatus = nf90_put_var(nccls%ncfileid, nccls%ncvarid, vararr)
     if (nccls%ncstatus /= 0) call ncerror(nccls=nccls)
   end subroutine write_arr3d_single

   !> @brief Writes a double-precision scalar variable.
  !!
  !! @params[inout] nccls
  !!    - An initialized netCDF object.
  !!
  !! @params[in] varname
  !!    - The netCDF variable name.
  !!
  !! @params[in] vararr
   !!    - The netCDF variable scalar/array.
   subroutine write_scalar_double(nccls, varname, vararr)
     class(ncdata), intent(inout) :: nccls
     character(len=maxchar) :: varname
     real(rdouble) :: vararr
     
     call ncvarid(nccls=nccls, varname=varname)
     nccls%ncstatus = nf90_put_var(nccls%ncfileid, nccls%ncvarid, vararr)
     if (nccls%ncstatus /= 0) call ncerror(nccls=nccls)
   end subroutine write_scalar_double

   !> @brief Writes a double-precision scalar variable.
  !!
  !! @params[inout] nccls
  !!    - An initialized netCDF object.
  !!
  !! @params[in] varname
  !!    - The netCDF variable name.
  !!
  !! @params[in] vararr
  !!    - The netCDF variable scalar/array.
   subroutine write_scalar_single(nccls, varname, vararr)
     class(ncdata), intent(inout) :: nccls
     character(len=maxchar) :: varname
     real(rsingle) :: vararr
     
     call ncvarid(nccls=nccls, varname=varname)
     nccls%ncstatus = nf90_put_var(nccls%ncfileid, nccls%ncvarid, vararr)
     if (nccls%ncstatus /= 0) call ncerror(nccls=nccls)
   end subroutine write_scalar_single
 end module netcdf_interface
