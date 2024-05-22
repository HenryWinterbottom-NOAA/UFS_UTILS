!> @file
module merge_interface
  use kinds_interface, only: rsingle, rdouble, ilong, maxchar
  use variables_interface, only: merge_struct, nml_struct, nml_lake_struct, nml_ocean_struct, init_struct, clean_struct
  use module_ncio, only: open_dataset, close_dataset, get_dim, read_vardata, Dimension, Dataset
  implicit none
  private
  public :: merge_mask

contains

  !> @brief
  !! @author Henry R. Winterbottom
  !! @date 16 May 2024
  subroutine merge_mask(nml_attrs)
    type(nml_struct), intent(inout) :: nml_attrs
    type(merge_struct) :: merge_attrs
    integer(ilong) :: tile

    do tile = 1, nml_attrs%nml_info%ntiles
       write(6, 500) tile
       call read_lndinfo(tile=tile, merge_attrs=merge_attrs, nml_attrs=nml_attrs)
       call read_ocninfo(tile=tile, merge_attrs=merge_attrs, nml_attrs=nml_attrs)
       call update_mask(merge_attrs=merge_attrs, nml_attrs=nml_attrs)
       call clean_struct(struct=merge_attrs)
    end do

500 format("MERGE_MASK: Reading tile ", 1x, i1.1, ".")
  end subroutine merge_mask

  !> @brief
  !! @author Henry R. Winterbottom
  !! @date 22 May 2024
  subroutine update_mask(merge_attrs, nml_attrs)
    type(merge_struct), intent(inout) :: merge_attrs
    type(nml_struct), intent(inout) :: nml_attrs
    integer(ilong) :: idx, jdx

    do idx = 1, merge_attrs%nlon
       do jdx = 1, merge_attrs%nlat
          if (nml_attrs%nml_info%binary_lake .eq. 1) merge_attrs%lake_frac(idx,jdx) = nint(merge_attrs%lake_frac(idx,jdx))
          if (merge_attrs%lat2d(idx,jdx) .le. -60.0) merge_attrs%lake_frac(idx,jdx) = 0.
          merge_attrs%land_frac(idx,jdx) = 1.0 - merge_attrs%ocn_frac(idx,jdx)
          if (merge_attrs%land_frac(idx,jdx) .lt. nml_attrs%nml_info%min_land) merge_attrs%land_frac(idx,jdx) = 0.0
          if (merge_attrs%land_frac(idx,jdx) .gt. 1.0 - nml_attrs%nml_info%min_land) merge_attrs%land_frac(idx,jdx) = 1.0
          if (1.0 - merge_attrs%land_frac(idx,jdx) > 0.0) merge_attrs%land_frac(idx,jdx) = 0.0
          if (merge_attrs%land_frac(idx,jdx) .gt. 0.0) then
             if (nml_attrs%nml_info%binary_lake .eq. 1) then
                merge_attrs%land_frac(idx,jdx) = 0.0
             else
                merge_attrs%land_frac(idx,jdx) = 1.0 - merge_attrs%lake_frac(idx,jdx)
             end if
             if (merge_attrs%lake_depth(idx,jdx) .le. 0.0) then
                merge_attrs%lake_depth(idx,jdx) = nml_attrs%nml_info%def_lakedp
             end if
          else
             merge_attrs%lake_depth(idx,jdx) = 0.0
          end if
       end do
    end do  
  end subroutine update_mask

  !> @brief
  !! @author Henry R. Winterbottom
  !! @date 16 May 2024
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
    !! TODO: To increase flexibility further, define the netCDF
    !! variable names for the latitude and longitude grids at the
    !! namelist level.
    ncvar = "geolon"
    write(6,502) trim(adjustl(ncvar))
    call read_vardata(ds, trim(adjustl(ncvar)), merge_attrs%lon2d)
    ncvar = "geolat"
    write(6,502) trim(adjustl(ncvar))
    call read_vardata(ds, trim(adjustl(ncvar)), merge_attrs%lat2d)
    ncvar = "lake_frac"
    write(6,502) trim(adjustl(ncvar))
    call read_vardata(ds, trim(adjustl(ncvar)), merge_attrs%lake_frac)
    ncvar = "lake_depth"
    write(6,502) trim(adjustl(ncvar))
    call read_vardata(ds, trim(adjustl(ncvar)), merge_attrs%lake_depth)
    ncvar = "land_frac"
    write(6,502) trim(adjustl(ncvar))
    call read_vardata(ds, trim(adjustl(ncvar)), merge_attrs%land_frac)
    ncvar = "slmsk"
    write(6,502) trim(adjustl(ncvar))
    call read_vardata(ds, trim(adjustl(ncvar)), merge_attrs%slmsk)
    call close_dataset(ds)

500 format(a, "/", "oro.", a, ".tile",i1.1,".nc")
501 format("READ_LNDINFO: Reading file", 1x, a, ".")
502 format("READ_LNDINFO: Reading netCDF variable", 1x, a, ".")
  end subroutine read_lndinfo

  !> @brief
  !! @author Henry R. Winterbottom
  !! @date 16 May 2024
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
    write(6,502) trim(adjustl(ncvar))
    call read_vardata(ds, trim(adjustl(ncvar)), merge_attrs%ocn_frac)
    call close_dataset(ds)
    
500 format(a, "/", a, ".", a, ".tile", i1.1, ".nc")
501 format("READ_OCNINFO: Reading file", 1x, a, ".")
502 format("READ_OCNINFO: Reading netCDF variable", 1x, a, ".")
  end subroutine read_ocninfo

  
end module merge_interface
