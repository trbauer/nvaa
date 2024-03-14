ARCH=sm_75
ARCH_NEW=sm_86
WORKLOAD=shuffles
# NVCC_EXTRA_FLAGS=--expt-relaxed-constexpr
NVCC_EXTRA_FLAGS=-std=c++20

help:
	@echo make  exe
	@echo make  ptx
	@echo make  sass

all: exe ptx sass

exe: ${WORKLOAD}.exe
${WORKLOAD}: ${WORKLOAD}.exe
${WORKLOAD}.exe: Makefile ../mincu.hpp main.cu stats.hpp timers.cuh
	nvcc ${NVCC_EXTRA_FLAGS} main.cu -arch ${ARCH} -I .. -o ${WORKLOAD}.exe

ptx: Makefile ${WORKLOAD}-${ARCH}.ptx
${WORKLOAD}-${ARCH}.ptx: Makefile ../mincu.hpp main.cu stats.hpp timers.cuh
	nvcc ${NVCC_EXTRA_FLAGS} main.cu --generate-line-info --ptx --source-in-ptx -I .. -arch ${ARCH}
	mv ${WORKLOAD}.ptx ${WORKLOAD}-${ARCH}.ptx

sass: Makefile ${WORKLOAD}-${ARCH}.sass ${WORKLOAD}.exe
${WORKLOAD}-${ARCH}.sass: Makefile ../mincu.hpp main.cu stats.hpp timers.cuh
	nvcc ${NVCC_EXTRA_FLAGS} main.cu \
		--generate-line-info --source-in-ptx \
		-c -o ${WORKLOAD}-${ARCH}.cubin \
		-I .. -cubin -arch ${ARCH}
	nvdisasm.exe \
		--print-instruction-encoding \
		-g \
		${WORKLOAD}-${ARCH}.cubin \
		> ${WORKLOAD}-raw-${ARCH}.sass
	nva.exe \
		--arch=${ARCH} \
		-lines \
		${WORKLOAD}-raw-${ARCH}.sass \
		-o=${WORKLOAD}-${ARCH}.sass

	nvcc ${NVCC_EXTRA_FLAGS} main.cu \
		--generate-line-info --source-in-ptx \
		-c -o ${WORKLOAD}-${ARCH_NEW}.cubin \
		-I .. -cubin -arch ${ARCH_NEW}
	nvdisasm.exe \
		--print-instruction-encoding \
		-g \
		${WORKLOAD}-${ARCH_NEW}.cubin \
		> ${WORKLOAD}-raw-${ARCH_NEW}.sass
	nva.exe \
		--arch=${ARCH_NEW} \
		-text \
		-lines \
		${WORKLOAD}-raw-${ARCH_NEW}.sass \
		-o=${WORKLOAD}-${ARCH_NEW}.sass


