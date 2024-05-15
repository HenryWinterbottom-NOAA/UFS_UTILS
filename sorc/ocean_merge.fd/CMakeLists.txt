set(lib_src
    kinds.F90
    variables.F90
    namelist.F90
    )

set(exe_src ocean_merge.F90)

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -r8 -assume byterecl")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-line-length-0 -fdefault-real-8")
endif()

set(exe_name ocean_merge)

add_library(ocean_merge_lib STATIC ${lib_src})
add_executable(${exe_name} ${exe_src})

set(mod_dir "${CMAKE_CURRENT_BINARY_DIR}/mod")
set_target_properties(ocean_merge_lib PROPERTIES Fortran_MODULE_DIRECTORY ${mod_dir})
target_include_directories(ocean_merge_lib INTERFACE ${mod_dir})

target_link_libraries(
  ocean_merge_lib)

target_link_libraries(${exe_name} PRIVATE ocean_merge_lib)

install(TARGETS ${exe_name})


