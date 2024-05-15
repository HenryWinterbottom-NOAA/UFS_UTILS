# This is the cmake build file for the tests directory of the
# UFS_UTILS project.
#
# Henry R. Winterbottom

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -r8 -assume byterecl")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-line-length-0 -fdefault-real-8")
endif()

include_directories(${PROJECT_SOURCE_DIR})

add_executable(ftst_read_namelist ftst_read_namelist.F90)
add_test(NAME ocean_merge-ftst_read_namelist COMMAND ftst_read_namelist)
target_link_libraries(ftst_read_namelist ocean_merge_lib)


