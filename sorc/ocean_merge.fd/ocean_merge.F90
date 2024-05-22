!> @file
!! @brief Determines the water mask by merging the lake mask with
!! the mapped ocean mask from MOM6.
!! @author Henry R. Winterbottom (refactor author)
!! @author Shan Sun (original author)
!! @author Rahul Mahajan (original author)
!! @author Sanath Kumar (original author)
program ocean_merge
  use namelist_interface, only: read_namelist
  use merge_interface, only: merge_mask
  use variables_interface, only: nml_struct
  implicit none
  
  type(nml_struct) :: nml_attrs
  
  call read_namelist(nml_attrs=nml_attrs)
  call merge_mask(nml_attrs=nml_attrs)

end program ocean_merge
