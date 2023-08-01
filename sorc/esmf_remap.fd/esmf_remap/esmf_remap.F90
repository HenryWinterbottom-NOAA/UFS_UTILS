!> @file esmf_remap.F90
!! @details Orchestrates the remapping of specified variables from a
!!          source grid to a destination grid using pre-computed ESMF
!!          remapping coefficients.
!! @author Henry R. Winterbottom
!! @date 01 August 2023
!! @version 0.0.1
!! @license LGPL v2.1
module esmf_remap_interface
  use namelist_interface, only: setup_remap
  use remap_interface, only: remap
  use variables_interface, only: destroy_struct, esmffile_struct, varinfo_struct
  implicit none
  private
  public :: esmf_remap
contains

  !> @brief Remap the specified variable fields using ESMF remapping
  !!        coefficients.
  subroutine esmf_remap()
    type(esmffile_struct) :: esmffile
    type(varinfo_struct) :: varinfo
    
    call setup_remap(esmffile=esmffile, varinfo=varinfo)
    call remap(esmffile=esmffile, varinfo=varinfo) 
    call destroy_struct(grid=varinfo)    
  end subroutine esmf_remap
end module esmf_remap_interface
