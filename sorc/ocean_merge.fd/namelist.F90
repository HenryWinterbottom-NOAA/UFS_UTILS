! > @file
! ! @brief Read the FORTRAN 90 namelist-formatted file.
! ! @author Henry R. Winterbottom (refactor author)
! ! @author Rahul Mahajan (original author)
! ! @author Sanath Kumar (original author)
! ! @author George Gayno (original author)
module namelist_interface
    use kinds_interface, only: maxchar, ilong, rsingle
    use variables_interface, only: nml_struct
    implicit none
    private
    public :: read_namelist
    character(len=maxchar), parameter :: file_nml = "./input.nml"
    character(len=maxchar) :: out_dir = "./"
    character(len=maxchar) :: ocean_mask_dir, lake_mask_dir, atmres, ocnres
    character(len=maxchar) :: nlon_str, nlat_str
    real(rsingle) :: min_land = 1.e-4, def_lakedp = 10.0
    integer(ilong), parameter :: unit_nml = 999
    integer(ilong) :: ntiles = 6, ierr, binary_lake
    namelist / nml_info / out_dir, ntiles, binary_lake, atmres, ocnres, min_land, def_lakedp
    namelist / nml_lake / lake_mask_dir, nlon_str, nlat_str
    namelist / nml_ocean / ocean_mask_dir, nlon_str, nlat_str
contains

    ! > @brief Reads the input namelist file `input.nml` and builds the
    ! > respective data structures.
    ! ! @author Henry R. Winterbottom
    ! ! @date 14 May 2024

    ! ! @params[inout] :: nml_attrs; `nml_struct` variable containing the
    ! ! attributes collected from the namelist file.
    subroutine read_namelist(nml_attrs)
        type(nml_struct), intent(inout) :: nml_attrs

        write (6, 500) trim(adjustl(file_nml))
        open (unit=unit_nml, file=trim(adjustl(file_nml)), status="old", &
            & form = "formatted", action = "read", iostat = ierr)
        if (ierr /= 0) then
            write (6, 501) trim(adjustl(file_nml))
            return
        end if
        call build_nml(nml_attrs)
        close (unit_nml)

500     format("READ_NAMELIST: Reading FORTRAN 90 namelist", 1x, a, ".")
501     format("READ_NAMELIST: Error opening file ", 1x, a, 1x, "; Aborting!!!")
    end subroutine read_namelist

    ! > @brief Collects the namelist attributes and defines the
    ! > respective data structures.
    ! ! @author Henry R. Winterbottom
    ! ! @date 15 May 2024

    ! ! @params[inout] :: nml_attrs; `nml_struct` variable containing the
    ! ! attributes collected from the namelist file.
    subroutine build_nml(nml_attrs)
        type(nml_struct), intent(inout) :: nml_attrs

        read (unit_nml, NML = nml_info, iostat = ierr)
        if (ierr /= 0) then
            write (6, 500) "nml_info", trim(adjustl(file_nml)), ierr
        end if
        nml_attrs%nml_info%ntiles = ntiles
        nml_attrs%nml_info%out_dir = out_dir
        nml_attrs%nml_info%binary_lake = binary_lake
        nml_attrs%nml_info%atmres = atmres
        nml_attrs%nml_info%ocnres = ocnres
        nml_attrs%nml_info%min_land = min_land
        nml_attrs%nml_info%def_lakedp = def_lakedp
        read (unit_nml, NML = nml_lake, iostat = ierr)
        if (ierr /= 0) then
            write (6, 500) "nml_lake", trim(adjustl(file_nml)), ierr
        end if
        nml_attrs%nml_lake%lake_mask_dir = lake_mask_dir
        nml_attrs%nml_lake%nlon_str = nlon_str
        nml_attrs%nml_lake%nlat_str = nlat_str
        read (unit_nml, NML = nml_ocean, iostat = ierr)
        if (ierr /= 0) then
            write (6, 500) "nml_ocean", trim(adjustl(file_nml)), ierr
        end if
        nml_attrs%nml_ocean%ocean_mask_dir = ocean_mask_dir
        nml_attrs%nml_ocean%nlon_str = nlon_str
        nml_attrs%nml_ocean%nlat_str = nlat_str
        nml_attrs%nml_lake%res = nml_attrs%nml_info%atmres
        nml_attrs%nml_ocean%res = nml_attrs%nml_info%ocnres

500     format("BUILD_NML: Error reading namelist", 1x, a, 1x, "from file", 1x, a, 1x, "returned error ", i5.5, ". Aborting!!!")
    end subroutine build_nml
end module namelist_interface
