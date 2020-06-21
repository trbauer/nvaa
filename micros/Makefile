ARCH=sm_75
WORKLOAD=64b_math

help:
	@echo make  exe
	@echo make  ptx
	@echo make  sass


exe: ${WORKLOAD}.exe
${WORKLOAD}: ${WORKLOAD}.exe
${WORKLOAD}.exe: Makefile ../tools/mincu.hpp ${WORKLOAD}.cu
	nvcc ${WORKLOAD}.cu -arch ${ARCH} -I ../tools -o ${WORKLOAD}.exe

ptx: Makefile ${WORKLOAD}-${ARCH}.ptx
${WORKLOAD}-${ARCH}.ptx: Makefile ../tools/mincu.hpp ${WORKLOAD}.cu
	nvcc ${WORKLOAD}.cu --generate-line-info --ptx --source-in-ptx -I ../tools -arch ${ARCH}
	mv ${WORKLOAD}.ptx ${WORKLOAD}-${ARCH}.ptx

sass: Makefile ${WORKLOAD}-${ARCH}.sass
${WORKLOAD}-${ARCH}.sass: Makefile ../tools/mincu.hpp ${WORKLOAD}.cu
	nvcc ${WORKLOAD}.cu --source-in-ptx -c -o ${WORKLOAD}-${ARCH}.cubin -I ../tools -cubin -arch ${ARCH}
	nvdisasm.exe \
		--print-instruction-encoding \
		-g \
		${WORKLOAD}-${ARCH}.cubin \
		> ${WORKLOAD}-raw-${ARCH}.sass
	..\nva.exe \
		--arch=sm_75 \
		${WORKLOAD}-raw-${ARCH}.sass \
		-o=${WORKLOAD}-${ARCH}.sass