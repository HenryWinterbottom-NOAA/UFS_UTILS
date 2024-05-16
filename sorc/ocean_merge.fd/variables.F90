module variables_interface
  use kinds_interface, only: rsingle, ilong, maxchar
  use module_ncio, only: Dataset, open_dataset
  implicit none
  private
  public :: clean_merge_struct, init_merge_struct,  merge_struct, nml_struct

  !> @brief
  interface clean_struct
     module procedure clean_merge_struct
  end interface clean_struct

  !> @brief
  interface init_struct
     module procedure init_merge_struct
  end interface init_struct
  
  !> @brief
  type :: merge_struct
     character(len=maxchar) :: nlon_str, nlat_str
     real(rsingle), dimension(:,:), allocatable :: lake_frac
     real(rsingle), dimension(:,:), allocatable :: lake_depth
     real(rsingle), dimension(:,:), allocatable :: land_frac
     real(rsingle), dimension(:,:), allocatable :: ocn_frac
     real(rsingle), dimension(:,:), allocatable :: slmsk
     real(rsingle), dimension(:,:), allocatable :: lat2d
     integer(ilong) :: nlon, nlat, ntiles
  end type merge_struct

  !> @brief
  type :: nml_struct
     character(len=maxchar) :: ocean_mask_dir
     character(len=maxchar) :: lake_mask_dir
     character(len=maxchar) :: out_dir
     character(len=maxchar) :: atmres
     character(len=maxchar) :: ocnres
     integer(ilong) :: binary_lake
  end type nml_struct
contains

  !> @brief
  subroutine clean_merge_struct(struct)
    type(merge_struct) :: struct

    if (allocated(struct%land_frac)) deallocate(struct%lake_frac)
    if (allocated(struct%lake_depth)) deallocate(struct%lake_depth)
    if (allocated(struct%land_frac)) deallocate(struct%land_frac)
    if (allocated(struct%ocn_frac)) deallocate(struct%ocn_frac)
    if (allocated(struct%slmsk)) deallocate(struct%slmsk)
    if (allocated(struct%lat2d)) deallocate(struct%lat2d)
  end subroutine clean_merge_struct
  
  !> @brief
  subroutine init_merge_struct(struct)
    type(merge_struct) :: struct
    type(Dataset) :: dataset
    character(len=maxchar) :: datafile

    !! @todo Each of these can be abstracted out to the I/O level.
    struct%nlon_str = "grid_xt"
    struct%nlat_str = "grid_yt"
    struct%ntiles = 6
    
    if (.not. allocated(struct%land_frac)) allocate(struct%lake_frac(struct%nlon,struct%nlat))
    if (.not. allocated(struct%lake_depth)) allocate(struct%lake_depth(struct%nlon,struct%nlat))
    if (.not. allocated(struct%land_frac)) allocate(struct%land_frac(struct%nlon,struct%nlat))
    if (.not. allocated(struct%ocn_frac)) allocate(struct%ocn_frac(struct%nlon,struct%nlat))
    if (.not. allocated(struct%slmsk)) allocate(struct%slmsk(struct%nlon,struct%nlat))
    if (.not. allocated(struct%lat2d)) allocate(struct%lat2d(struct%nlon,struct%nlat))
  end subroutine init_merge_struct
end module variables_interface
