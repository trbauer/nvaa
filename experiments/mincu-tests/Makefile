WORKLOAD=mincu-tests
NVCC_EXTRA_FLAGS=-std=c++20
ARCH=sm_75


help:
	@echo make  exe
	@echo make  ptx
	@echo make  sass

exe: ${WORKLOAD}.exe
${WORKLOAD}.exe: ${WORKLOAD}.cu Makefile  ../../tools/mincu.hpp
	nvcc -std=c++20 -arch ${ARCH} -I ../../tools ${WORKLOAD}.cu -o ${WORKLOAD}.exe

ptx: Makefile ${WORKLOAD}-${ARCH}.ptx
${WORKLOAD}-${ARCH}.ptx: Makefile ../../tools/mincu.hpp ${WORKLOAD}.cu
	nvcc ${NVCC_EXTRA_FLAGS} ${WORKLOAD}.cu --generate-line-info --ptx -I ../../tools -arch ${ARCH}
	# nvcc ${NVCC_EXTRA_FLAGS} ${WORKLOAD}.cu --generate-line-info --ptx --source-in-ptx -I ../../tools -arch ${ARCH}
	mv ${WORKLOAD}.ptx ${WORKLOAD}-${ARCH}.ptx

sass: Makefile ${WORKLOAD}-${ARCH}.sass ${WORKLOAD}.exe
${WORKLOAD}-${ARCH}.sass: Makefile ../../tools/mincu.hpp ${WORKLOAD}.cu
	nvcc ${NVCC_EXTRA_FLAGS} ${WORKLOAD}.cu --generate-line-info --source-in-ptx -c -o ${WORKLOAD}-${ARCH}.cubin -I ../../tools -cubin -arch ${ARCH}
	nvdisasm.exe \
		--print-instruction-encoding \
		--print-code \
		-g \
		${WORKLOAD}-${ARCH}.cubin \
		> ${WORKLOAD}-raw-${ARCH}.sass
	..\..\nva.exe \
		--arch=${ARCH} \
		-lines \
		${WORKLOAD}-raw-${ARCH}.sass \
		-o=${WORKLOAD}-${ARCH}.sass
