module variables_interface
  use kinds_interface, only: ilong, maxchar
  implicit none
  private
  public :: nml_struct

  !> @brief
  type :: nml_struct
     character(len=maxchar) :: ocean_mask_dir
     character(len=maxchar) :: lake_mask_dir
     character(len=maxchar) :: out_dir
     character(len=maxchar) :: atmres
     character(len=maxchar) :: ocnres
     integer(ilong) :: binary_lake
  end type nml_struct


end module variables_interface
