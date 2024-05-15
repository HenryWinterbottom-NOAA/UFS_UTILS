!> @file
!! @brief Read the FORTRAN 90 namelist-formatted file.
!! @author Henry R. Winterbottom (refactor author)
!! @author Rahul Mahajan (original author)
!! @author Sanath Kumar (original author)
!! @author George Gayno (original author)
module namelist_interface
  use kinds_interface, only: maxchar, ilong
  use variables_interface, only: nml_struct
  
  implicit none
  private
  public :: read_namelist, nml_attrs

  type(nml_struct) :: nml_attrs
  character(len=maxchar), parameter :: file_nml = "./input.nml"
  integer(ilong), parameter :: unit_nml = 999
  integer(ilong) :: ierr
contains

  !> @brief Reads the input namelist file `input.nml` and builds the
  !> respective data structures.
  !! @author Henry R. Winterbottom
  !! @date 14 May 2024
  subroutine read_namelist()
    write(6, 500) trim(adjustl(file_nml))
    open(unit=unit_nml, file=trim(adjustl(file_nml)), status="old", &
         & form="formatted", action="read", iostat=ierr)
    if (ierr /= 0) then
       write(6,501) trim(adjustl(file_nml))
       return
    end if
    call build_mask_nml()
    close(unit_nml)
500 format("Reading FORTRAN 90 namelist", 1x, a, ".")
501 format("Error opening file ",1x, a,1x, "; Aborting!!!")
  end subroutine read_namelist

  !> @brief Collects the namelist attributes and defines the
  !> respective data structures.
  !! @author Henry R. Winterbottom
  !! @date 15 May 2024
  subroutine build_mask_nml()
    character(len=maxchar) :: ocean_mask_dir, lake_mask_dir, out_dir, atmres, ocnres
    integer(ilong) :: binary_lake
    namelist /mask_nml/ ocean_mask_dir, lake_mask_dir, out_dir, atmres, ocnres, binary_lake
    
    read(unit_nml, NML=mask_nml, iostat=ierr)
    if (ierr /= 0) then
       write(6, 500) "mask_nml", trim(adjustl(file_nml))
    end if
    nml_attrs%ocean_mask_dir = ocean_mask_dir
    nml_attrs%lake_mask_dir = lake_mask_dir
    nml_attrs%out_dir = out_dir
    nml_attrs%atmres = atmres
    nml_attrs%ocnres = ocnres
    nml_attrs%binary_lake = binary_lake
500 format("Error reading namelist", 1x,a,1x, "from file", 1x, a, ". Aborting!!!")
  end subroutine build_mask_nml
end module namelist_interface
