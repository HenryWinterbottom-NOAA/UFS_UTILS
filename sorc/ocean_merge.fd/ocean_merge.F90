!> @file
!! @brief Determines the water mask by merging the lake mask with
!! the mapped ocean mask from MOM6.
!! @author Henry R. Winterbottom (refactor author)
!! @author Shan Sun (original author)
!! @author Rahul Mahajan (original author)
!! @author Sanath Kumar (original author)
program ocean_merge
  use namelist_interface, only: read_namelist, nml_attrs
  use merge_interface, only: merge_lake_ocnmsk, merge_attrs 
  use kinds_interface, only: maxchar
  implicit none

  call read_namelist()
  call merge_lake_ocnmsk()
  

  
end program ocean_merge
