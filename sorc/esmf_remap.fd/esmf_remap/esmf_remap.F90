!> @file esmf_remap.F90
!! @details Orchestrates the remapping of specified variables from a
!!          source grid to a destination grid using pre-computed ESMF
!!          remapping coefficients.
!! @author Henry R. Winterbottom
!! @date 24 July 2023
!! @version 0.0.1
!! @license LGPL v2.1
module esmf_remap_interface
  use namelist_interface, only: read_namelist
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
    
    !! Read the namelist and configure the application attributes.
    call read_namelist(esmffile=esmffile, varinfo=varinfo)

    !! Remap the specified variables and update the output netCDF
    !! formatted file.
    call remap(esmffile=esmffile, varinfo=varinfo) !! # TODO: Pass the
                                                   !! output netCDF
                                                   !! file path here.

    !! Cleanup.
    call destroy_struct(varinfo)    
  end subroutine esmf_remap
end module esmf_remap_interface
