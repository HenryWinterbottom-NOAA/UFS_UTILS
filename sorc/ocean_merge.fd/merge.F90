! > @file
! ! @brief Module containing land/ocean mask update applications.
! ! @author Henry R. Winterbottom
! ! @date 16 May 2024
module merge_interface
    use kinds_interface, only: rsingle, rdouble, ilong, maxchar
    use variables_interface, only: merge_struct, nml_struct, nml_lake_struct, nml_ocean_struct, init_struct, clean_struct
    use module_ncio, only: open_dataset, create_dataset, close_dataset, get_dim, read_vardata, Dimension, Dataset, write_vardata
    use netcdf
    implicit none
    private
    public :: merge_mask
contains

    ! > @brief Driver interface for land/ocean mask update applications.
    ! ! @author Henry R. Winterbottom
    ! ! @date 16 May 2024

    ! ! @params[inout] nml_attrs :: a FORTRAN `nml_struct` variable
    ! ! containing the FORTRAN 90-formatted namelist attributes.
    subroutine merge_mask(nml_attrs)
        type(nml_struct), intent(inout) :: nml_attrs
        type(merge_struct) :: merge_attrs
        integer(ilong) :: tile

        do tile = 1, nml_attrs%nml_info%ntiles
            write(6, 500) tile
            call read_lndinfo(tile = tile, merge_attrs = merge_attrs, nml_attrs = nml_attrs)
            call read_ocninfo(tile = tile, merge_attrs = merge_attrs, nml_attrs = nml_attrs)
            call update_mask(merge_attrs = merge_attrs, nml_attrs = nml_attrs)
            call write_maskupdate(tile = tile, merge_attrs = merge_attrs, nml_attrs = nml_attrs)
            call clean_struct(struct = merge_attrs)
        end do

500     format("MERGE_MASK: Reading tile ", 1x, i1.1, ".")
    end subroutine merge_mask

    ! > @brief Update (e.g., merge) the land/ocean masks.
    ! ! @author Henry R. Winterbottom
    ! ! @date 22 May 2024

    ! ! @params[inout] merge_attrs :: a FORTRAN `merge_struct` variable
    ! ! containing the variables required to update/merge the respective
    ! ! land-masks.

    ! ! @params[inout] nml_attrs :: a FORTRAN `nml_struct` variable
    ! ! containing the FORTRAN 90-formatted namelist attributes.
    subroutine update_mask(merge_attrs, nml_attrs)
        type(merge_struct), intent(inout) :: merge_attrs
        type(nml_struct), intent(inout) :: nml_attrs
        integer(ilong) :: idx, jdx, lake_pt, nodp_pt

        lake_pt = 0; nodp_pt = 0
        do idx = 1, merge_attrs%nlon
            do jdx = 1, merge_attrs%nlat
                if (nml_attrs%nml_info%binary_lake == 1) merge_attrs%lake_frac(idx, jdx) = nint(merge_attrs%lake_frac(idx, jdx))
                if (merge_attrs%lat2d(idx, jdx) <= - 60.0) merge_attrs%lake_frac(idx, jdx) = 0.
                merge_attrs%land_frac(idx, jdx) = 1.0 - merge_attrs%ocn_frac(idx, jdx)
                if (merge_attrs%land_frac(idx, jdx) < nml_attrs%nml_info%min_land) merge_attrs%land_frac(idx, jdx) = 0.0
                if (merge_attrs%land_frac(idx, jdx) > 1.0 - nml_attrs%nml_info%min_land) merge_attrs%land_frac(idx, jdx) = 1.0
                if (1.0 - merge_attrs%land_frac(idx, jdx) > 0.0) merge_attrs%land_frac(idx, jdx) = 0.0
                if (merge_attrs%land_frac(idx, jdx) > 0.0) then
                    lake_pt = lake_pt + 1
                    if (nml_attrs%nml_info%binary_lake == 1) then
                        merge_attrs%land_frac(idx, jdx) = 0.0
                    else
                        merge_attrs%land_frac(idx, jdx) = 1.0 - merge_attrs%lake_frac(idx, jdx)
                    end if
                    if (merge_attrs%lake_depth(idx, jdx) <= 0.0) then
                        merge_attrs%lake_depth(idx, jdx) = nml_attrs%nml_info%def_lakedp
                        nodp_pt = nodp_pt + 1
                    end if
                else
                    merge_attrs%lake_depth(idx, jdx) = 0.0
                end if
            end do
        end do
        write(6, 500) lake_pt, nodp_pt

500     format("UPDATE_MASK: Total lake point", 1x, i6, 1x,"where", 1x, i6, 1x,"has no depth.")
    end subroutine update_mask

    ! > @brief Collect the land attribute(s) information.
    ! ! @author Henry R. Winterbottom
    ! ! @date 16 May 2024

    ! ! @params[in] tile :: a FORTRAN `integer` variables specfiying the
    ! ! respective cubed-sphere tile.

    ! ! @params[inout] merge_attrs :: a FORTRAN `merge_struct` variable
    ! ! containing the variables required to update/merge the respective
    ! ! land-masks.

    ! ! @params[in] nml_attrs :: a FORTRAN `nml_struct` variable
    ! ! containing the FORTRAN 90-formatted namelist attributes.
    subroutine read_lndinfo(tile, merge_attrs, nml_attrs)
        type(merge_struct), intent(inout) :: merge_attrs
        type(nml_struct), intent(in) :: nml_attrs
        type(Dataset) :: ds
        type(Dimension) :: diminfo
        character(len=maxchar) :: ncfile, ncvar
        integer(ilong), intent(in) :: tile

        write(ncfile, 500) trim(adjustl(nml_attrs%nml_lake%lake_mask_dir)), trim(adjustl(nml_attrs%nml_lake%res)), tile
        write(6, 501) trim(adjustl(ncfile))
        ds = open_dataset(trim(adjustl(ncfile)))
        diminfo = get_dim(ds, trim(adjustl(nml_attrs%nml_lake%nlon_str)))
        merge_attrs%nlon = diminfo%len
        diminfo = get_dim(ds, trim(adjustl(nml_attrs%nml_lake%nlat_str)))
        merge_attrs%nlat = diminfo%len
        call init_struct(merge_attrs)

        ! ! TODO: To increase flexibility further, define the netCDF
        ! ! variable names for the latitude and longitude grids at the
        ! ! namelist level.
        ncvar = "geolon"
        write(6, 502) trim(adjustl(ncvar))
        call read_vardata(ds, trim(adjustl(ncvar)), merge_attrs%lon2d)
        ncvar = "geolat"
        write(6, 502) trim(adjustl(ncvar))
        call read_vardata(ds, trim(adjustl(ncvar)), merge_attrs%lat2d)
        ncvar = "lake_frac"
        write(6, 502) trim(adjustl(ncvar))
        call read_vardata(ds, trim(adjustl(ncvar)), merge_attrs%lake_frac)
        ncvar = "lake_depth"
        write(6, 502) trim(adjustl(ncvar))
        call read_vardata(ds, trim(adjustl(ncvar)), merge_attrs%lake_depth)
        ncvar = "land_frac"
        write(6, 502) trim(adjustl(ncvar))
        call read_vardata(ds, trim(adjustl(ncvar)), merge_attrs%land_frac)
        ncvar = "slmsk"
        write(6, 502) trim(adjustl(ncvar))
        call read_vardata(ds, trim(adjustl(ncvar)), merge_attrs%slmsk)
        call close_dataset(ds)

500     format(a, "/", "oro.", a, ".tile", i1.1,".nc")
501     format("READ_LNDINFO: Reading file", 1x, a, ".")
502     format("READ_LNDINFO: Reading netCDF variable", 1x, a, ".")
    end subroutine read_lndinfo

    ! > @brief Collect the ocean attribute(s) information.
    ! ! @author Henry R. Winterbottom
    ! ! @date 16 May 2024

    ! ! @params[in] tile :: a FORTRAN `integer` variables specfiying the
    ! ! respective cubed-sphere tile.

    ! ! @params[inout] merge_attrs :: a FORTRAN `merge_struct` variable
    ! ! containing the variables required to update/merge the respective
    ! ! land-masks.

    ! ! @params[in] nml_attrs :: a FORTRAN `nml_struct` variable
    ! ! containing the FORTRAN 90-formatted namelist attributes.
    subroutine read_ocninfo(tile, merge_attrs, nml_attrs)
        type(merge_struct), intent(inout) :: merge_attrs
        type(nml_struct), intent(in) :: nml_attrs
        type(Dataset) :: ds
        type(Dimension) :: diminfo
        character(len=maxchar) :: ncfile, ncvar
        integer(ilong), intent(in) :: tile

        write(ncfile, 500) trim(adjustl(nml_attrs%nml_ocean%ocean_mask_dir)), trim(adjustl(nml_attrs%nml_lake%res)), trim(adjustl(nml_attrs%nml_ocean%res)), tile
        write(6, 501) trim(adjustl(ncfile))
        ds = open_dataset(trim(adjustl(ncfile)))
        diminfo = get_dim(ds, trim(adjustl(nml_attrs%nml_ocean%nlon_str)))
        merge_attrs%nlon = diminfo%len
        diminfo = get_dim(ds, trim(adjustl(nml_attrs%nml_ocean%nlat_str)))
        merge_attrs%nlat = diminfo%len
        call init_struct(merge_attrs)
        ncvar = "land_frac"
        write(6, 502) trim(adjustl(ncvar))
        call read_vardata(ds, trim(adjustl(ncvar)), merge_attrs%ocn_frac)
        call close_dataset(ds)

500     format(a, "/", a, ".", a, ".tile", i1.1, ".nc")
501     format("READ_OCNINFO: Reading file", 1x, a, ".")
502     format("READ_OCNINFO: Reading netCDF variable", 1x, a, ".")
    end subroutine read_ocninfo

    ! > @brief Write the updated land/ocean mask variables.
    ! ! @author Henry R. Winterbottom
    ! ! @date 23 May 2024

    ! ! @params[in] tile :: a FORTRAN `integer` variables specfiying the
    ! ! respective cubed-sphere tile.

    ! ! @params[inout] merge_attrs :: a FORTRAN `merge_struct` variable
    ! ! containing the variables required to update/merge the respective
    ! ! land-masks.

    ! ! @params[in] nml_attrs :: a FORTRAN `nml_struct` variable
    ! ! containing the FORTRAN 90-formatted namelist attributes.
    subroutine write_maskupdate(tile, merge_attrs, nml_attrs)
        type(merge_struct), intent(inout) :: merge_attrs
        type(nml_struct), intent(in) :: nml_attrs
        character(len=maxchar) :: ncfile
        integer(ilong), dimension(:), allocatable :: dimid, varid
        integer(ilong), intent(in) :: tile
        integer(ilong) :: ncid

        if (.not. allocated(dimid)) allocate(dimid(2))
        if (.not. allocated(varid)) allocate(varid(4))
        write(ncfile, 500) trim(adjustl(nml_attrs%nml_info%out_dir)), trim(adjustl(nml_attrs%nml_lake%res)), trim(adjustl(nml_attrs%nml_ocean%res)), tile
        call ncerrhdl(nf90_create(path = trim(adjustl(ncfile)), &
            cmode = or(nf90_clobber, nf90_64bit_offset), ncid = ncid))
        call ncerrhdl(nf90_def_dim(ncid, "lon", merge_attrs%nlon, dimid(1)))
        call ncerrhdl(nf90_def_dim(ncid, "lat", merge_attrs%nlat, dimid(2)))
        call ncerrhdl(nf90_def_var(ncid, "land_frac", nf90_float, dimid(1:2), varid(1)))
        call ncerrhdl(nf90_def_var(ncid, "lake_frac", nf90_float, dimid(1:2), varid(2)))
        call ncerrhdl(nf90_def_var(ncid, "lake_depth", nf90_float, dimid(1:2), varid(3)))
        call ncerrhdl(nf90_def_var(ncid, "slmsk", nf90_float, dimid(1:2), varid(4)))
        call ncerrhdl(nf90_enddef(ncid))
        call ncerrhdl(nf90_put_var(ncid, varid(1), merge_attrs%land_frac))
        call ncerrhdl(nf90_put_var(ncid, varid(2), merge_attrs%lake_frac))
        call ncerrhdl(nf90_put_var(ncid, varid(3), merge_attrs%lake_depth))
        call ncerrhdl(nf90_put_var(ncid, varid(4), merge_attrs%slmsk))
        call ncerrhdl(nf90_close(ncid))
        if (allocated(dimid)) deallocate(dimid)
        if (allocated(varid)) deallocate(varid)

500     format(a, "/", a, ".", a, ".tile", i1.1, ".nc")
501     format("WRITE_MASKUPDATE: Writing updated mask to", 1x, a, ".")
    end subroutine write_maskupdate

    ! > @brief netCDF return code evaluation.
    ! ! @author Henry R. Winterbottom
    ! ! @date 23 May 2024

    ! ! @params[in] ret NetCDF return code.
    subroutine ncerrhdl(ret)
        integer(ilong), intent(in) :: ret

        if (ret /= NF90_NOERR) then
            write(6, 500) nf90_strerror(ret)
            stop 999
        end if

500     format("NCERRHDL: netCDF action failed with error", 1x, a, ". Aborting!!!")
    end subroutine ncerrhdl
end module merge_interface
