#!/bin/zsh

NVA=../nva.exe
# ARCH=sm_75
# ARCH=sm_60
# ARCH=sm_50
# ARCH=sm_30

function compile()
{
  CU=$1
  mkdir -p ${ARCH}
  #KEEP=-X=-keep
  "$NVA" ${CU} \
    --arch ${ARCH} \
    -lines\
    -o=${ARCH}/${CU:r}.sass \
    --save-ptx=${ARCH}/${CU:r}.ptx \
    --save-cubin=${ARCH}/${CU:r}.cubin
}

if [ $? -ne 0 ];  then
  for CU in *.cu; do
    compile "${CU}"
  done
else
  for CU in $*; do
    compile "${CU}"
  done
fi
