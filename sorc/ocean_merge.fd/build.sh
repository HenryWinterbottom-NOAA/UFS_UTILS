NCIO_DIR="/Users/henry.winterbottom/trunk/UFS/spack-stack/envs/ufs-utils/install/apple-clang/14.0.0/ncio-1.1.2-p7eq46r"
NC_DIR="/Users/henry.winterbottom/trunk/UFS/spack-stack/envs/ufs-utils/install/apple-clang/14.0.0/netcdf-fortran-4.6.1-5gl4asz"

rm *.mod >& /dev/null
#/usr/local/bin/mpif90 kinds.F90 variables.F90 namelist.F90 merge.F90 ocean_merge.F90 -L${NCIO_DIR}/lib -L${NC_DIR}/lib -I${NCIO_DIR}/include -I${NC_DIR}/include -lm -lncio -lnetcdff -o ocean_merge.x

/usr/local/bin/mpif90 kinds.F90 variables.F90 namelist.F90 merge.F90 ocean_merge.F90 -L${NCIO_DIR}/lib -L${NC_DIR}/lib -I${NCIO_DIR}/include -I${NC_DIR}/include -lm -lncio -lnetcdff -o ocean_merge.x -fbacktrace
