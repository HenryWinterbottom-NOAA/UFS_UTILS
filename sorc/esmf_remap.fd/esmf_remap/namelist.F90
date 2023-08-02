! > @file namelist.F90
! ! @details This module contains the FORTRAN namelist interface.
! ! @author Henry R. Winterbottom
! ! @date 01 August 2023
! ! @version 0.0.1
! ! @license LGPL v2.1
module namelist_interface
    use io_interface, only: abort_remap, get_diminfo, get_esmf, get_gridinfo, get_interp, &
        get_nitems, get_varinfo, write_msg
    use netcdf, only: nf90_double
    use netcdf_interface, only: destroy_ncvarinfo, init_ncvarinfo, ncdata, ncdimval, &
        ncread, ncvardims, ncvarinfo_struct, ncwrite
    use variables_interface, only: destroy_struct, dstgrid_struct, esmffile_struct, &
        get_boolean, ilong, init_struct, interp_struct, maxchar, rdouble, var_struct, &
        varinfo_struct
    implicit none
    private
    public :: ncsqueeze, output_netcdf, setup_remap

    character(len=maxchar) :: namelist_input = "./esmf_remap.input"
    character(len=maxchar) :: bilinear = "NOT USED"
    character(len=maxchar) :: conserve = "NOT USED"
    character(len=maxchar) :: grid_ncfile = "NOT USED"
    character(len=maxchar) :: grid_nclatname = "NOT USED"
    character(len=maxchar) :: grid_nclonname = "NOT USED"
    character(len=maxchar) :: nearests2d = "NOT USED"
    character(len=maxchar) :: output_netcdf = "NOT USED"
    character(len=maxchar) :: varsfile = "NOT USED"
    logical :: ncsqueeze = .false.
    integer(ilong), parameter :: unit_nml = 99
    namelist / gridinfo / grid_ncfile, grid_nclatname, grid_nclonname
    namelist / esmf / bilinear, conserve, nearests2d
    namelist / share / output_netcdf, varsfile, ncsqueeze
contains

    ! > @brief Builds the output netCDF formatted file path.
    ! !
    ! ! @params[inout] dstgrid
    ! !    - The initialized `dstgrid_struct` variable.
    ! !
    ! ! @params[inout] varinfo
    ! !    - The initialized `varinfo_struct` variable.
    ! !
    ! ! @params[in] esmffile
    ! !    - The initialized `esmffile_struct` variable.
    subroutine build_output(dstgrid, varinfo, esmffile)
        type(varinfo_struct), intent(inout) :: varinfo
        type(dstgrid_struct), intent(inout) :: dstgrid
        type(esmffile_struct), intent(in) :: esmffile
        type(ncdata) :: nccls
        type(ncvarinfo_struct) :: ncvarinfo

        nccls%ncfile = trim(adjustl(output_netcdf))
        nccls%write = .true.
        call nccls%ncopen()
        call define_ncvars(nccls = nccls, varinfo = varinfo, ncvarinfo = ncvarinfo, &
             dstgrid = dstgrid)
        call init_ncvarinfo(ncvarinfo = ncvarinfo)
        call nccls%ncwritedef(ncvarinfo = ncvarinfo)
        call write_ncheader(nccls = nccls, dstgrid = dstgrid)
        call nccls%ncclose()
        call destroy_struct(grid = dstgrid)
    end subroutine build_output

    ! > @brief Define the netCDF-formatted file dimension attributes.
    subroutine define_ncdims(nccls, ncvarinfo, varinfo, dstgrid)
        type(dstgrid_struct), intent(inout) :: dstgrid
        type(ncdata) :: nccls
        type(ncvarinfo_struct), intent(inout) :: ncvarinfo
        type(varinfo_struct), intent(in) :: varinfo
        integer(ilong) :: idx

        ! ! Define the netCDF variable dimension attributes.
        dstgrid%nlevs = maxval(varinfo%var(:)%nlevs)
        call init_struct(grid = dstgrid)
        ncvarinfo%dimname(1) = "nlons"
        ncvarinfo%dimid(1) = 1
        ncvarinfo%dimval(1) = dstgrid%nlon
        ncvarinfo%dimname(2) = "nlats"
        ncvarinfo%dimid(2) = 2
        ncvarinfo%dimval(2) = dstgrid%nlat
        ncvarinfo%dimname(3) = "nlevels"
        ncvarinfo%dimid(3) = 3
        ncvarinfo%dimval(3) = dstgrid%nlevs
        do idx = 1, ncvarinfo%dimval(3)
            dstgrid%levels(idx) = real(idx)
        end do
        ncvarinfo%varndim(1) = 2
        ncvarinfo%varname(1) = "lons"
        ncvarinfo%varid(1) = 1
        ncvarinfo%vardimid(1, 1) = 1; ncvarinfo%vardimid(1, 2) = 2
        ncvarinfo%varndim(2) = 2
        ncvarinfo%varname(2) = "lats"
        ncvarinfo%varid(2) = 2
        ncvarinfo%vardimid(2, 1) = 1; ncvarinfo%vardimid(2, 2) = 2
        ncvarinfo%varndim(3) = 1
        ncvarinfo%varname(3) = "levels"
        ncvarinfo%varid(3) = 3
        ncvarinfo%vardimid(3, 1) = 3
    end subroutine define_ncdims

    ! > @brief Defines the output netCDF variable attributes.
    ! !
    ! ! @params[in] nccls
    ! !    - The initialized `ncdata` variable.
    ! !
    ! ! @params[inout] varinfo
    ! !    - The initialized `varinfo_struct` variable.
    ! !
    ! ! @params[inout] ncvarinfo
    ! !    - The initialized `ncvarinfo_struct` variable.
    ! !
    ! ! @params[inout] dstgrid
    ! !    - The initialized `dstgrid_struct` variable.
    subroutine define_ncvars(nccls, varinfo, ncvarinfo, dstgrid)
        type(dstgrid_struct), intent(inout) :: dstgrid
        type(ncvarinfo_struct), intent(inout) :: ncvarinfo
        type(varinfo_struct), intent(inout) :: varinfo
        type(ncdata), intent(in) :: nccls
        character(len=maxchar) :: varname
        integer(ilong) :: idx, ncidx, ncdims_squeeze

        ! ! # TODO: This can be generalized further; right now we assume
        ! ! # only (nx,ny,nz) type arrays.
        ncvarinfo%ndims = 3
        ncvarinfo%nvars = ncvarinfo%ndims + varinfo%nvars
        call init_ncvarinfo(ncvarinfo = ncvarinfo)
        ncvarinfo%dtype(1:ncvarinfo%nvars) = nf90_double
        call define_ncdims(nccls = nccls, ncvarinfo = ncvarinfo, dstgrid = dstgrid, &
            varinfo = varinfo)

        do idx = 1, varinfo%nvars
            ncidx = idx + ncvarinfo%ndims
            ncdims_squeeze = varinfo%var(idx)%ndims
            if (ncsqueeze) then
                if (varinfo%var(idx)%ndims == 4) ncdims_squeeze = 3
                if (varinfo%var(idx)%ndims == 3) ncdims_squeeze = 2
            end if
            varinfo%var(idx)%ndims = ncdims_squeeze
            ncvarinfo%varndim(ncidx) = varinfo%var(idx)%ndims
            ncvarinfo%varname(ncidx) = varinfo%var(idx)%ncvarout
            ncvarinfo%varid(ncidx) = ncidx
            ncvarinfo%vardimid(ncidx, 1) = 1; ncvarinfo%vardimid(ncidx, 2) = 2
            if (ncdims_squeeze == 3) ncvarinfo%vardimid(ncidx, 3) = 3
        end do
    end subroutine define_ncvars

    ! > @brief Reads the namelist file `esmf_remap.input`.
    ! !
    ! ! @params[inout] esmffile
    ! !    - The `esmffile_struct` variable; upon exit this variable is
    ! !      populated with the file paths for the available remapping
    ! !      type coefficients.
    ! !
    ! ! @params[inout] varinfo
    ! !    - The `varinfo_struct` variable; upon exit this variable
    ! !      contains attributes for the variables to be remapped.
    subroutine setup_remap(esmffile, varinfo)
        type(esmffile_struct), intent(inout) :: esmffile
        type(varinfo_struct), intent(inout) :: varinfo
        type(dstgrid_struct) :: dstgrid

        open(file=trim(adjustl(namelist_input)), unit=unit_nml, status="old", &
            form = "formatted", action = "read")
        ! ! # TODO: This needs to be generalized such that the namelist
        ! ! # block order does not matter.
        read(unit_nml, NML = share)
        read(unit_nml, NML = esmf)
        read(unit_nml, NML = gridinfo)
        close(unit_nml)
        call get_gridinfo(dstgrid = dstgrid, grid_ncfile = grid_ncfile, &
            grid_nclatname = grid_nclatname, grid_nclonname = grid_nclonname, &
            namelist_input = namelist_input)
        call get_esmf(esmffile = esmffile, bilinear = bilinear, nearests2d = nearests2d, &
            conserve = conserve)
        call get_varinfo(varinfo = varinfo, varsfile = varsfile)
        call build_output(dstgrid = dstgrid, varinfo = varinfo, esmffile = esmffile)
        call destroy_struct(grid = dstgrid)
    end subroutine setup_remap

    ! > @brief Initializes the output netCDF-formatted file path; the
    ! !        dimension attributes in accordance with the COORDS
    ! !        convention are defined.
    ! !
    ! ! @params nccls
    ! !    - The initialized `ncdata` variable.
    ! !
    ! ! @params dstgrid
    ! !    - The initialized `dstgrid_struct` variable; it is assumed
    ! !      that the COORDS compliant dimensions have been defined prior
    ! !      to entry.
    subroutine write_ncheader(nccls, dstgrid)
        type(dstgrid_struct) :: dstgrid
        type(ncdata) :: nccls
        character(len=maxchar) :: msg

        call ncwrite(nccls=nccls, varname="lons", vararr=dstgrid%lon)
        write(msg, 500) "lons", trim(adjustl(nccls%ncfile)); call write_msg(msg)
        call ncwrite(nccls=nccls, varname="lats", vararr=dstgrid%lat)
        write(msg, 500) "lats", trim(adjustl(nccls%ncfile)); call write_msg(msg)
        call ncwrite(nccls=nccls, varname="levels", vararr=dstgrid%levels)
        write(msg, 500) "levels", trim(adjustl(nccls%ncfile)); call write_msg(msg)
500     format("Writing variable", 1x, a, 1x, "to netCDF file path", 1x, a, 1x,".")
    end subroutine write_ncheader
end module namelist_interface
