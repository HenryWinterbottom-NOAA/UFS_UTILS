!> @file
module merge_interface
  use kinds_interface, only: rsingle, rdouble
  use variables_interface, only: merge_struct, nml_struct
  use namelist_interface, only: nml_attrs
  use module_ncio

  implicit none
  private
  public :: merge_lake_ocnmsk, merge_attrs

  type(merge_struct) :: merge_attrs
contains

  !> @brief
  !! @author Henry R. Winterbottom
  !! @date 16 May 2024
  subroutine merge_lake_ocnmsk()

    


  end subroutine merge_lake_ocnmsk

  !> @brief
  !! @author Henry R. Winterbottom
  !! @date 16 May 2024
  subroutine read_ocnmsk()
    

  end subroutine read_ocnmsk
  
end module merge_interface
