!> @file main.F90
!! @details This driver level program for the SLINT remapping
!!          application.
!! @author Henry R. Winterbottom
!! @date 30 June 2023
!! @version 0.0.1
!! @license LGPL v2.1
program esmf_remap_main
  use esmf_remap_interface, only: esmf_remap
  use variables_interface, only: maxchar, rdouble
  implicit none
  character(len=maxchar) :: msg
  real(rdouble) :: elapsed_seconds, start_time, stop_time

  call cpu_time(start_time)
  msg = "Beginning remapping application."
  write(6, *) trim(adjustl(msg))
  call esmf_remap()
  msg = "Completed remapping application."
  write(6, *) trim(adjustl(msg))
  call cpu_time(stop_time)
  elapsed_seconds = stop_time - start_time
  write (msg, 500) elapsed_seconds
  write(6, *) trim(adjustl(msg))
500 format("Total Elapsed Time:", 1x, f13.5, 1x, "seconds.")
end program esmf_remap_main
