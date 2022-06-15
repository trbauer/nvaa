ARCH_OLD=sm_75
ARCH_NEW=sm_86
WORKLOAD=barriers
# NVCC_EXTRA_FLAGS=--expt-relaxed-constexpr
NVCC_EXTRA_FLAGS=

help:
	@echo make  exe
	@echo make  ptx
	@echo make  sass

all: exe ptx sass

exe: ${WORKLOAD}.exe
${WORKLOAD}: ${WORKLOAD}.exe
${WORKLOAD}.exe: Makefile mincu.hpp main.cu
	nvcc ${NVCC_EXTRA_FLAGS} main.cu -arch ${ARCH_OLD} -o ${WORKLOAD}.exe

ptx: Makefile ${WORKLOAD}-${ARCH_NEW}.ptx
${WORKLOAD}-${ARCH_NEW}.ptx: Makefile mincu.hpp main.cu
	nvcc ${NVCC_EXTRA_FLAGS} main.cu --generate-line-info --ptx --source-in-ptx -arch ${ARCH_NEW}
	mv ${WORKLOAD}.ptx ${WORKLOAD}-${ARCH_NEW}.ptx

sass: Makefile ${WORKLOAD}-${ARCH_NEW}.sass ${WORKLOAD}.exe
${WORKLOAD}-${ARCH_NEW}.sass: Makefile mincu.hpp main.cu
	nvcc ${NVCC_EXTRA_FLAGS} main.cu \
		--generate-line-info --source-in-ptx \
		-c -o ${WORKLOAD}-${ARCH_NEW}.cubin \
		-cubin -arch ${ARCH_NEW}
	nvdisasm.exe \
		--print-instruction-encoding \
		-g \
		${WORKLOAD}-${ARCH_NEW}.cubin \
		> ${WORKLOAD}-raw-${ARCH_NEW}.sass
	nva.exe \
		--arch=${ARCH_NEW} \
		-lines \
		${WORKLOAD}-raw-${ARCH_NEW}.sass \
		-o=${WORKLOAD}-${ARCH_NEW}.sass

#	nvcc ${NVCC_EXTRA_FLAGS} main.cu \
#		--generate-line-info --source-in-ptx \
#		-c -o ${WORKLOAD}-${ARCH_OLD}.cubin \
#		-cubin -arch ${ARCH_OLD}
#	nvdisasm.exe \
#		--print-instruction-encoding \
#		-g \
#		${WORKLOAD}-${ARCH_OLD}.cubin \
#		> ${WORKLOAD}-raw-${ARCH_OLD}.sass
#	nva.exe \
#		--arch=${ARCH_OLD} \
#		-text \
#		-lines \
#		${WORKLOAD}-raw-${ARCH_OLD}.sass \
#		-o=${WORKLOAD}-${ARCH_OLD}.sass


