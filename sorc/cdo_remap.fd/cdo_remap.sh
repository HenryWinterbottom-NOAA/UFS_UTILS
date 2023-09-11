#!/bin/bash

#
# Bash Script: cdo_remap.sh
# Description: This remaps a source grid variable to a destination
#              grid projection using the Climate Data Operators (CDO)
#              application.
# Author: Henry R. Winterbottom
# Date: 11 September 2023
#

# Check for the correct number of command-line arguments.
if [ "$#" -ne 3 ]; then
    echo "Usage: $0 <variable_file> <dstgrid_path> <output_path>"
    exit 1
fi

# Assign command-line arguments to variables.

# TODO: This could also be done using a bash configure-type file.
variable_file="${1}"
dstgrid_path="${2}"
output_path="${3}"

# Function to concatenate NetCDF files.

# TODO: This step is necessary for the following reasons.

#  - CDO supports multiple interpolation types; however, multiple
#    interpolation types cannot be passed for a single `cdo` call;

#  - Instances of analysis variable spread across multiple files is
#    possible (cite: nominal 0p25 MOM6 restarts); similiar to the
#    above, a single `cdo` call only permits one source grid file.
function nc_concat(){
    local var_interp_path="${1}"
    tmp_nc_file="${PWD}/tmp_nc.nc"
    if test -e "${output_path}"; then
	echo "netCDF-formatted file path ${output_path} exists; merging ${var_interp_path}"
	$(command -v cdo) merge "${output_path}" "${var_interp_path}" "${tmp_nc_file}"
	$(command -v mv) "${tmp_nc_file}" "${output_path}"
	$(command -v rm) "${var_output_path}" >> /dev/null
    else
	echo "netCDF-formatted file path ${output_path} does not exist; creating..."
	$(command -v mv) "${var_interp_path}" "${output_path}"
    fi
}

# Function to interpolate the respective variables.

# TODO: This is just a wrapper around the `cdo` interpolation
# application.
function run_cdo(){
    local varname="${1}"
    local varfile="${2}"
    local interp_type="${3}"
    local var_interp_path="${varname}.interp.nc"
    $(command -v cdo) "${interp_type}","${dstgrid_path}" -selname,"${varname}" "${varfile}" "${var_interp_path}"
    nc_concat "${var_interp_path}"
}

# TODO: This just reads an ASCII formatted file containing the
# variable attributes.

# TODO: The advantage of doing it this way is that both CICE and MOM6
# variables can be interpolated and written to the same output file
# path.
while IFS= read -r line; do 

    varname=$(echo "${line}" | $(command -v awk) '{print $1}')
    varfile=$(echo "${line}" | $(command -v awk) '{print $3}')
    interp_type=$(echo "${line}" | $(command -v awk) '{print $2}')
    run_cdo "${varname}" "${varfile}" "${interp_type}"

done < "${variable_file}"


