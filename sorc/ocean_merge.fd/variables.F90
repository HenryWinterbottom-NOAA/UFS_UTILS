! > @file
! ! @brief Variable definitions and interfaces.
! ! @author Henry R. Winterbottom
module variables_interface
    use kinds_interface, only: rsingle, rdouble, ilong, maxchar
    use module_ncio, only: Dataset, open_dataset, close_dataset, Variable
    implicit none
    private
    public :: clean_merge_struct, init_merge_struct, merge_struct, nml_struct, nml_lake_struct, nml_ocean_struct, init_struct, clean_struct

    ! > @brief Data structure clean-up procedures.
    interface clean_struct
        module procedure clean_merge_struct
    end interface clean_struct

    ! > @brief Data strucuture initialization procedures.
    interface init_struct
        module procedure init_merge_struct
    end interface init_struct

    ! > @brief Merge application variable declarations.
    type :: merge_struct
    type(Dataset) :: dataset
    character(len=maxchar) filename, nlon_str, nlat_str
    real(rdouble), dimension(:, :), allocatable :: lake_frac
    real(rdouble), dimension(:, :), allocatable :: lake_depth
    real(rdouble), dimension(:, :), allocatable :: land_frac
    real(rdouble), dimension(:, :), allocatable :: ocn_frac
    real(rdouble), dimension(:, :), allocatable :: slmsk
    real(rdouble), dimension(:, :), allocatable :: lon2d
    real(rdouble), dimension(:, :), allocatable :: lat2d
    integer(ilong) :: nlon, nlat
    end type merge_struct

    ! > @brief Namelist `nml_info` block variable declarations.
    type :: nml_info_struct
    character(len=maxchar) :: out_dir, ocnres, atmres
    real(rsingle) :: min_land, def_lakedp
    integer(ilong) :: ntiles, binary_lake
    end type nml_info_struct

    ! > @brief Namelist `nml_lake` block variable declarations.
    type :: nml_lake_struct
    character(len=maxchar) :: lake_mask_dir
    character(len=maxchar) :: res, nlon_str, nlat_str
    end type nml_lake_struct

    ! > @brief Namelist `nml_ocean` block variable declarations.
    type :: nml_ocean_struct
    character(len=maxchar) :: ocean_mask_dir
    character(len=maxchar) :: res, nlon_str, nlat_str
    end type nml_ocean_struct

    ! > @brief Namelist block variable declarations.
    type :: nml_struct
    type(nml_info_struct) :: nml_info
    type(nml_lake_struct) :: nml_lake
    type(nml_ocean_struct) :: nml_ocean
    end type nml_struct
contains

    ! > @brief Deallocate memory for `merge_struct` variables.
    ! ! @params[inout] :: a FORTRAN `merge_struct` variable.
    subroutine clean_merge_struct(struct)
        type(merge_struct), intent(inout) :: struct

        if (allocated(struct%lake_frac)) deallocate (struct%lake_frac)
        if (allocated(struct%lake_depth)) deallocate (struct%lake_depth)
        if (allocated(struct%land_frac)) deallocate (struct%land_frac)
        if (allocated(struct%ocn_frac)) deallocate (struct%ocn_frac)
        if (allocated(struct%slmsk)) deallocate (struct%slmsk)
        if (allocated(struct%lon2d)) deallocate (struct%lon2d)
        if (allocated(struct%lat2d)) deallocate (struct%lat2d)
    end subroutine clean_merge_struct

    ! > @brief Allocate memory for `merge_struct` variables.
    ! ! @params[inout] :: a FORTRAN `merge_struct` variable.
    subroutine init_merge_struct(struct)
        type(merge_struct), intent(inout) :: struct

        if (.not. allocated(struct%land_frac)) allocate (struct%lake_frac(struct%nlon, struct%nlat))
        if (.not. allocated(struct%lake_depth)) allocate (struct%lake_depth(struct%nlon, struct%nlat))
        if (.not. allocated(struct%land_frac)) allocate (struct%land_frac(struct%nlon, struct%nlat))
        if (.not. allocated(struct%ocn_frac)) allocate (struct%ocn_frac(struct%nlon, struct%nlat))
        if (.not. allocated(struct%slmsk)) allocate (struct%slmsk(struct%nlon, struct%nlat))
        if (.not. allocated(struct%lon2d)) allocate (struct%lon2d(struct%nlon, struct%nlat))
        if (.not. allocated(struct%lat2d)) allocate (struct%lat2d(struct%nlon, struct%nlat))
    end subroutine init_merge_struct
end module variables_interface
