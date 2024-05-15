program simple_unit_test
  use namelist_interface
  use kinds_interface, only: maxchar, ilong
    
    implicit none
    
    ! Declare variables
    character(len=maxchar) :: test_file = 'test_input.nml'
    logical :: passed
    integer(ilong) :: unit_nml
    integer(ilong) :: ierr
    
    ! Initialize variables
    passed = .true.
    
    ! Perform tests
    
    ! Test reading a valid namelist file
    call test_read_valid_file(passed)
    
    ! Test reading an invalid namelist file
    call test_read_invalid_file(passed)
    
    ! Print test results
    if (passed) then
        print*, "All tests passed!"
    else
        print*, "Some tests failed!"
    end if
    
contains

    ! Subroutine to test reading a valid namelist file
    subroutine test_read_valid_file(passed)
        logical, intent(out) :: passed
        character(len=maxchar) :: expected_ocean_mask_dir = '/path/to/ocean'
        character(len=maxchar) :: expected_lake_mask_dir = '/path/to/lake'
        character(len=maxchar) :: expected_out_dir = '/path/to/out'
        character(len=maxchar) :: expected_atmres = 'C48'
        character(len=maxchar) :: expected_ocnres = 'mx500'
        integer(ilong) :: expected_binary_lake = 0
        
        ! Create valid namelist file
        call create_valid_namelist_file()
        
        ! Call read_namelist subroutine
        call read_namelist()
        
        ! Check if namelist parameters are correctly read
        if (nml_attrs%ocean_mask_dir /= expected_ocean_mask_dir .or. &
            nml_attrs%lake_mask_dir /= expected_lake_mask_dir .or. &
            nml_attrs%out_dir /= expected_out_dir .or. &
            nml_attrs%atmres /= expected_atmres .or. &
            nml_attrs%ocnres /= expected_ocnres .or. &
            nml_attrs%binary_lake /= expected_binary_lake) then
            passed = .false.
            print*, "Test failed: Failed to read valid namelist file"
        end if
    end subroutine test_read_valid_file
    
    ! Subroutine to test reading an invalid namelist file
    subroutine test_read_invalid_file(passed)
        logical, intent(out) :: passed
        
        ! Create invalid namelist file
        call create_invalid_namelist_file()
        
        ! Call read_namelist subroutine
        call read_namelist()
        
        ! Check if subroutine handles invalid namelist files properly
        if (.not. test_failed()) then
            passed = .false.
            print*, "Test failed: Failed to handle invalid namelist file"
        end if
    end subroutine test_read_invalid_file
    
    ! Subroutine to create a valid namelist file for testing
    subroutine create_valid_namelist_file()
        ! Open file
        open(unit=unit_nml, file=test_file, status='unknown', action='write', iostat=ierr)
        if (ierr /= 0) then
            print*, "Error: Failed to create namelist file"
            return
        end if
        
        ! Write valid namelist data
        write(unit_nml, "('ocean_mask_dir = ''/path/to/ocean'', ', 'lake_mask_dir = ''/path/to/lake'', ', &
                     & 'out_dir = ''/path/to/out'', ', 'atmres = ''high'', ', 'ocnres = ''low'', ', &
                     & 'binary_lake = 0')")        
        ! Close file
        close(unit_nml)
    end subroutine create_valid_namelist_file
    
    ! Subroutine to create an invalid namelist file for testing
    subroutine create_invalid_namelist_file()
        ! Open file
        open(unit=unit_nml, file=test_file, status='unknown', action='write', iostat=ierr)
        if (ierr /= 0) then
            print*, "Error: Failed to create namelist file"
            return
        end if
        
        ! Write invalid namelist data (missing required fields)
        write(unit_nml, "('atmres = ''high'', ', 'ocnres = ''low'', ', 'binary_lake = 0')")        
        ! Close file
        close(unit_nml)
    end subroutine create_invalid_namelist_file

end program simple_unit_test
