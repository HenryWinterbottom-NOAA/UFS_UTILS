! > @file variables.F90
! ! @details This module contains the allowable data types, data
! !          structures, and the respective data structure allocation
! !          and deallocation interface. for supported variable types.
! ! @author Henry R. Winterbottom
! ! @date 01 August 2023
! ! @version 0.0.1
! ! @license LGPL v2.1
module variables_interface
    implicit none
    private
    public :: destroy_struct, dstgrid_struct, esmf_struct, esmffile_struct, &
        get_boolean, init_struct, interp_struct, var_struct, varinfo_struct

    ! > @brief The supported/allowable data types.
    integer, public, parameter :: ilong = selected_int_kind(8)
    integer, public, parameter :: maxchar = 1024
    integer, public, parameter :: rsingle = selected_real_kind(6)
    integer, public, parameter :: rdouble = selected_real_kind(15)

    ! > @brief The interface level for data structure deallocations.
    interface destroy_struct
        module procedure destroy_dstgrid_struct
        module procedure destroy_esmf_struct
        module procedure destroy_varinfo_struct
    end interface destroy_struct

    ! > @brief The interface level for data structure allocations.
    interface init_struct
        module procedure init_dstgrid_struct
        module procedure init_esmf_struct
        module procedure init_varinfo_struct
    end interface init_struct

    ! > @brief The `dstgrid_struct` data structure.
    ! !
    ! ! # TODO: This needs to be generalized for instances of unstructure
    ! ! # grids.
    type dstgrid_struct
    character(len=maxchar) :: ncfile
    character(len=maxchar) :: nclatname
    character(len=maxchar) :: nclonname
    real(rdouble), dimension(:,:), allocatable :: lat
    real(rdouble), dimension(:,:), allocatable :: lon
    real(rdouble), dimension(:), allocatable :: levels
    integer(ilong) :: nlat
    integer(ilong) :: nlevs
    integer(ilong) :: nlon
    end type dstgrid_struct

    ! > @brief The `esmf_struct` data structure.
    type :: esmf_struct
    character(len=maxchar) :: filename
    real(rdouble), dimension(:), allocatable :: s
    integer(ilong), dimension(:), allocatable :: col
    integer(ilong), dimension(:), allocatable :: dst_grid_dims
    integer(ilong), dimension(:), allocatable :: row
    integer(ilong), dimension(:), allocatable :: src_grid_dims
    integer(ilong) :: dst_grid_rank, src_grid_rank
    integer(ilong) :: n_a, n_b, n_s
    end type esmf_struct

    ! > @brief The `esmffile_struct` data structure.
    type :: esmffile_struct
    character(len=maxchar) :: bilinear
    character(len=maxchar) :: conserve
    character(len=maxchar) :: nearests2d
    logical :: bilinear_valid = .false.
    logical :: conserve_valid = .false.
    logical :: nearests2d_valid = .false.
    end type esmffile_struct

    ! > @brief The `interp_struct` data structure.
    type interp_struct
    logical :: bilinear = .false.
    logical :: conserve = .false.
    logical :: nearests2d = .false.
    end type interp_struct

    ! > @brief The `var_struct` data structure.
    type :: var_struct
    character(len=maxchar) :: interp_type
    character(len=maxchar) :: ncfilein
    character(len=maxchar) :: ncvarin
    character(len=maxchar) :: ncvarout
    character(len=maxchar) :: remapfile
    logical :: bilinear
    logical :: conserve
    logical :: nearests2d
    integer :: ndims
    integer :: nlevs
    end type var_struct

    ! > @brief The `varinfo_struct` data structure.
    type :: varinfo_struct
    type(var_struct), dimension(:), allocatable :: var
    integer(ilong) :: nvars
    end type varinfo_struct
contains

    ! > @brief Returns a boolean values based on the string value.
    ! !
    ! ! @params[in] instr
    ! !    - A character string containing a boolean string (e.g.,
    ! !      `True`, `False`, `true`, `false`, etc.,)
    ! !
    ! ! @returns outbool
    ! !    - A boolean valued variable.
    function get_boolean(instr) result(outbool)
        character(len=maxchar), intent(in) :: instr
        character(len=maxchar) :: boolstr
        logical :: outbool

        outbool = .false.
        boolstr = trim(adjustl(instr))
        if (boolstr(1:1) == "T") outbool = .true.
        if (boolstr(1:1) == "t") outbool = .true.
        if (boolstr(1:2) == ".T") outbool = .true.
        if (boolstr(1:2) == ".t") outbool = .true.
    end function get_boolean

    ! > @brief Destroys the `dstgrid_struct` data structure.
    ! !
    ! ! @param[inout] grid
    ! !    - An initialized `dstgrid_struct` data structure variable.
    subroutine destroy_dstgrid_struct(grid)
        type(dstgrid_struct), intent(inout) :: grid

        if (allocated(grid%lat)) deallocate(grid%lat)
        if (allocated(grid%lon)) deallocate(grid%lon)
        if (allocated(grid%levels)) deallocate(grid%levels)
    end subroutine destroy_dstgrid_struct

    ! > @brief Destroys the `esmf_struct` data structure.
    ! !
    ! ! @param[inout] grid
    ! !    - An initialized `esmf_struct` data structure variable.
    subroutine destroy_esmf_struct(grid)
        type(esmf_struct), intent(inout) :: grid

        if (allocated(grid%s)) deallocate(grid%s)
        if (allocated(grid%col)) deallocate(grid%col)
        if (allocated(grid%dst_grid_dims)) deallocate(grid%dst_grid_dims)
        if (allocated(grid%row)) deallocate(grid%row)
        if (allocated(grid%src_grid_dims)) deallocate(grid%src_grid_dims)
    end subroutine destroy_esmf_struct

    ! > @brief Destroys the `varinfo_struct` data structure.
    ! !
    ! ! @param[inout] grid
    ! !    - An initialized `varinfo_struct` data structure variable.
    subroutine destroy_varinfo_struct(grid)
        type(varinfo_struct), intent(inout) :: grid

        if (allocated(grid%var)) deallocate(grid%var)
    end subroutine destroy_varinfo_struct

    ! > @brief Initializes the `dstgrid_struct` data structure.
    ! !
    ! ! @param[inout] grid
    ! !    - An initialized `dstgrid_struct` data structure variable.
    ! !
    ! ! # TODO: This needs to be generalized for instances of unstructure
    ! ! # grids.
    subroutine init_dstgrid_struct(grid)
        type(dstgrid_struct), intent(inout) :: grid
        integer(ilong) :: idx

        if (.not. allocated(grid%lat)) allocate(grid%lat(grid%nlon, grid%nlat))
        if (.not. allocated(grid%lon)) allocate(grid%lon(grid%nlon, grid%nlat))
        if (.not. allocated(grid%levels)) allocate(grid%levels(grid%nlevs))

        ! ! # TODO Define the vertical levels; this needs to be more
        ! ! # general.
        do idx = 1, grid%nlevs
            grid%levels(idx) = real(idx, rdouble)
        end do
    end subroutine init_dstgrid_struct

    ! > @brief Initializes the `esmf_struct` data structure.
    ! !
    ! ! @param[inout] grid
    ! !    - An `esmf_struct` data structure variable to be initialized.
    subroutine init_esmf_struct(grid)
        type(esmf_struct), intent(inout) :: grid

        if (.not. allocated(grid%s)) allocate(grid%s(grid%n_s))
        if (.not. allocated(grid%col)) allocate(grid%col(grid%n_s))
        if (.not. allocated(grid%dst_grid_dims)) &
            allocate(grid%dst_grid_dims(grid%dst_grid_rank))
        if (.not. allocated(grid%row)) allocate(grid%row(grid%n_s))
        if (.not. allocated(grid%src_grid_dims)) &
            & allocate(grid%src_grid_dims(grid%src_grid_rank))
    end subroutine init_esmf_struct

    ! > @brief Initializes the `varinfo_struct` data structure.
    ! !
    ! ! @param[inout] grid
    ! !    - An initialized `varinfo_struct` data structure variable.
    subroutine init_varinfo_struct(grid)
        type(varinfo_struct), intent(inout) :: grid

        if (.not. allocated(grid%var)) allocate(grid%var(grid%nvars))
    end subroutine init_varinfo_struct
end module variables_interface
