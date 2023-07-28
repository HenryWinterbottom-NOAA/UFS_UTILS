# CMakeLists.txt
# Build the `slint_remap` application.
# Author: Henry R. Winterbottom
# Date: 20 July 2023
# Version: 0.0.1
# License: LGPL v2.1

project(slint_remap VERSION 0.0.1 LANGUAGES Fortran)
set (EXECUTABLE "esmf_remap_test.x")
set (CMAKE_Fortran_FLAGS_RELEASE -g -fcheck=all -fbacktrace -Wall -Werror=line-truncation)
set (CMAKE_Fortran_FLAGS_DEBUG "-O0")

# Define preprocessor definitions
set(DDEFS "FTNUTILS" "NETCDF")
foreach(item ${DDEFS})
    list(APPEND DEFINITIONS "-D${item}")
endforeach()
add_definitions(${DEFINITIONS})

# Set library paths and include directories
set(FTNUTILS_LIBS ${FTNUTILS}/lib)
set(NETCDF_LIBS ${NETCDF}/lib)
set(LIBS
    ${FTNUTILS_LIBS}/libftnutils.so
    ${FTNUTILS_LIBS}/libfson.so
    ${NETCDF_LIBS}/libnetcdf.so
    ${NETCDF_LIBS}/libnetcdff.so
)
include_directories(
    ${FTNUTILS}/include
    ${NETCDF}/include
)

# Gather Fortran source files
file(GLOB f90_objs *.F90)
set(F90_OBJS ${f90_objs})

# Create the executable and link libraries
add_executable(${EXECUTABLE} ${F90_OBJS})
target_link_libraries(${EXECUTABLE} ${LIBS})
