!> @file: variables.F90
!! @details: This module contains data structures for supported
!!          variable types.
!! @author: Henry R. Winterbottom
!! @date: 30 June 2023
!! @version: 0.0.1
!! @license: LGPL v2.1
module variables_interface
  use ftnutils_kinds, only: ilong, maxchar, rdouble, rsingle
  implicit none
  private
  public :: destroy_struct, esmf_struct, init_struct

  !! Interface level for data structure deallocations.
  interface destroy_struct
     module procedure destroy_esmf_struct
  end interface destroy_struct

  !! Interface level for data structure allocations.
  interface init_struct
     module procedure init_esmf_struct
  end interface init_struct

  !! `esmf_struct` data structure.
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

  !! Destroys the `esmf_struct` data structure.
  subroutine destroy_esmf_struct(grid)
    type(esmf_struct) :: grid
    if (allocated(grid%coeffs)) deallocate(grid%coeffs)
    if (allocated(grid%col)) deallocate(grid%col)
    if (allocated(grid%dst_grid_dims)) deallocate(grid%dst_grid_dims)
    if (allocated(grid%row)) deallocate(grid%row)
    if (allocate(grid%src_grid_dims)) deallocate(grid%src_grid_dims)
  end subroutine destroy_esmf_struct

  ! Initializes the `esmf_struct` data structure.
  subroutine init_esmf_struct(grid)
    type(esmf_struct) :: grid
    if (.not. allocated(grid%coeffs)) allocate(grid%coeffs(grid%n_s))
    if (.not. allocated(grid%col)) allocate(grid%col(grid%n_s))
    if (.not. allocated(grid%dst_grid_dims)) &
         allocate(grid%dst_grid_dims(grid%dst_grid_rank))
    if (.not. allocated(grid%row)) allocate(grid%row(grid%n_s))
    if (.not. allocated(grid%src_grid_dims)) &
         & allocate(grid%src_grid_dims(grid%src_grid_rank))
  end subroutine init_esmf_struct
end module variables_interface
