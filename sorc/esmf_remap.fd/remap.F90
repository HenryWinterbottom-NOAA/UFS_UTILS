!> @file: remap_interface.F90
!! @details: Remaps specified variables from a source grid to a
!!           destination grid using pre-computed ESMF remapping
!!           coefficients.
!! @author: Henry R. Winterbottom
!! @date: 24 July 2023
!! @version: 0.0.1
!! @license: LGPL v2.1
module remap_interface
  use ftnutils_json, only: json
  use ftnutils_kinds, only: ilong, maxchar, rdouble, rsingle
  use ftnutils_log, only: logger
  use ftnutils_netcdf, only: ncdata, ncreaddim, ncvarinfo_struct
  implicit none
  private
  public :: esmf_remap
  type(logger) :: logcls
contains

  !! # TODO
  function interp(esmf_grid, varin) result(varout)
    type(esmf_struct), intent(in) :: esmf_grid
    real(r_double), dimension(:), intent(in) :: varin
    real(r_double), dimension(:), intent(out) :: varout
    integer(ilong) :: idx

    !! Compute the output variable `varout` by interpolating the
    !! respective input variable `varin` using the ESMF remapping
    !! coefficients.
    varout = 0.0_rdouble
    do idx = 1, esmf_grid%ns
       varout(esmf_grid%row(idx)) = varout(esmf_grid%row(idx)) &
            + esmf_grid%s(idx)*varin(esmf_grid%col(idx))
    end do
  end function interp
  
  !! The interface to the ESMF remapping/interpolation algorithm.
  subroutine esmf_remap(jsoncls)
    type(json) :: jsoncls
    character(len=maxchar) :: msg

    !! # TODO: Read the JSON-formatted input file.
    
    !! # TODO: Initialize the output variable file.

    !! # TODO: Loop through the respective variables and interpolate
    !! # accordingly; write the interpolated variables sequentially.
  end subroutine esmf_remap

  !! Remaps the specified variables from the source to the destination
  !! grid projection.
  subroutine remap(jsoncls, ncvarinfo)
    type(json), intent(in) :: jsoncls
    type(ncvarinfo_struct), intent(out) :: ncvarinfo
    type(esmf_struct) :: esmf_grid
    type(ncdata) :: nccls_src, nccls_dst
    integer(ilong) :: idx
    
    !! # TODO: Loop through each variable, interpolate, and write out.
  end subroutine remap
end module remap_interface
