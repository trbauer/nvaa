# ARCH=sm_75
ARCH=sm_90
WORKLOAD=fatomics
# NVCC_EXTRA_FLAGS=--expt-relaxed-constexpr
NVCC_EXTRA_FLAGS=-std=c++20

help:
	@echo make  all
	@echo make  exe
	@echo make  ptx
	@echo make  sass

all: exe ptx sass

exe: ${WORKLOAD}.exe
${WORKLOAD}: ${WORKLOAD}.exe
${WORKLOAD}.exe: Makefile ../../tools/mincu.hpp ${WORKLOAD}.cu
	nvcc ${NVCC_EXTRA_FLAGS} ${WORKLOAD}.cu -arch ${ARCH} -I .. -o ${WORKLOAD}.exe

ptx: Makefile ${WORKLOAD}-${ARCH}.ptx
${WORKLOAD}-${ARCH}.ptx: Makefile ../../tools/mincu.hpp ${WORKLOAD}.cu
	nvcc ${NVCC_EXTRA_FLAGS} ${WORKLOAD}.cu --generate-line-info --ptx --source-in-ptx -I .. -arch ${ARCH}
	mv ${WORKLOAD}.ptx ${WORKLOAD}-${ARCH}.ptx

sass: Makefile ${WORKLOAD}-${ARCH}.sass
${WORKLOAD}-${ARCH}.sass: Makefile ../../tools/mincu.hpp ${WORKLOAD}.cu
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


atom: atomic-support-${ARCH}.sass
atomic-support-${ARCH}.sass: ../../tools/mincu.hpp Makefile atomic-support.cu
	..\..\nva.exe \
		--arch=${ARCH} \
		-text -ptx -lines \
		-I=../../tools \
		atomic-support.cu \
		-o=atomic-support-${ARCH}.sass
atomic-support-raw-${ARCH}.sass: ../../tools/mincu.hpp Makefile atomic-support.cu
	nvcc ${NVCC_EXTRA_FLAGS} atomic-support.cu \
		--generate-line-info --source-in-ptx -c -o atomic-support-${ARCH}.cubin \
		-I .. -cubin -arch ${ARCH}
	nvdisasm.exe \
		--print-instruction-encoding -g atomic-support-${ARCH}.cubin \
			> atomic-support-raw-${ARCH}.sass