!> @file
!! @brief Remaps specified variables from a source grid to a
!! destination grid using pre-computed ESMF remapping coefficients.
!!
!! @author Henry R. Winterbottom NOAA/EMC

!> Read in pre-computed ESMF remapping coefficients and remap
!! variables defined on a source grid to a destination grid;
!! subsequently write out the respective variables.
!!
!! @return 0 for success.
!! @author Henry R. Winterbottom NOAA/EMC



program esmf_remap
  include "netcdf.inc"
  implicit none
  
  !! Read in all ESMF remapping coefficient types and define the ESMF
  !! object/class.

  !! Define the output netCDF-formatted file.
  
  !! Loop through all variables and do as follows.

  !! (1) Read in the variable.

  !! (2) Remap using the pre-computed ESMF remapping coefficients.

  !! (3) Write the variable to the netCDF-formatted file path.

  !! Deallocate all allocated memory.

end program esmf_remap
