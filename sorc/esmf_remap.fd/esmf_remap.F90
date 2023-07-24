!> @file: esmf_interface.F90
!! @details: Orchestrates the remapping of specified variables from a
!!           source grid to a destination grid using pre-computed ESMF
!!           remapping coefficients.
!! @author: Henry R. Winterbottom
!! @date: 24 July 2023
!! @version: 0.0.1
!! @license: LGPL v2.1
module esmf_remap_interface
  use ftnutils_json, only: json
  use ftnutils_kinds, only: maxchar
  use ftnutils_log, only: logger
  implicit none
  private
  public :: esmf_remap
  type(logger) :: logcls
contains

  !> @brief: Remap the specified variable fields using ESMF remapping
  !> coefficients.
  subroutine esmf_remap()
    type(json) :: jsoncls
    character(len=maxchar) :: msg

    !! Collect the configuration attributes from the external
    !! JSON-formatted file.
    call get_command_argument(1, jsoncls%info%filename)
    write (msg, 500) trim(adjustl(jsoncls%info%filename))
    call logcls%info(msg=msg)
    call jsoncls%init()


    

  end subroutine esmf_remap
end module esmf_remap_interface
