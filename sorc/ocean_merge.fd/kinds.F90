! > @file
! ! @brief Defines the available data types.
! ! @author Henry R. Winterbottom
module kinds_interface
   implicit none
   private

   integer, public, parameter :: maxchar = 1024
   integer, public, parameter :: ilong = selected_int_kind(8)
   integer, public, parameter :: rdouble = selected_real_kind(15)
   integer, public, parameter :: rsingle = selected_real_kind(6)
end module kinds_interface
