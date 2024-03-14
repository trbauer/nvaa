ARCH=sm_75
WORKLOAD=copies
# NVCC_EXTRA_FLAGS=--expt-relaxed-constexpr
NVCC_EXTRA_FLAGS=-std=c++17

help:
	@echo make  exe
	@echo make  ptx
	@echo make  sass

all: exe ptx sass

exe: ${WORKLOAD}.exe
${WORKLOAD}: ${WORKLOAD}.exe
${WORKLOAD}.exe: Makefile ../mincu.hpp ${WORKLOAD}.cu
	nvcc ${NVCC_EXTRA_FLAGS} ${WORKLOAD}.cu -arch ${ARCH} -I .. -o ${WORKLOAD}.exe

ptx: Makefile ${WORKLOAD}-${ARCH}.ptx
${WORKLOAD}-${ARCH}.ptx: Makefile ../mincu.hpp ${WORKLOAD}.cu
	nvcc ${NVCC_EXTRA_FLAGS} ${WORKLOAD}.cu --generate-line-info --ptx --source-in-ptx -I .. -arch ${ARCH}
	mv ${WORKLOAD}.ptx ${WORKLOAD}-${ARCH}.ptx

sass: Makefile ${WORKLOAD}-${ARCH}.sass ${WORKLOAD}.exe
${WORKLOAD}-${ARCH}.sass: Makefile ../mincu.hpp ${WORKLOAD}.cu
	nvcc ${NVCC_EXTRA_FLAGS} ${WORKLOAD}.cu --generate-line-info --source-in-ptx -c -o ${WORKLOAD}-${ARCH}.cubin -I .. -cubin -arch ${ARCH}
	nvdisasm.exe \
		--print-instruction-encoding \
		-g \
		${WORKLOAD}-${ARCH}.cubin \
		> ${WORKLOAD}-raw-${ARCH}.sass
	..\..\nva.exe \
		--arch=${ARCH} \
		-lines \
		${WORKLOAD}-raw-${ARCH}.sass \
		-o=${WORKLOAD}-${ARCH}.sass


