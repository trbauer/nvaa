WORKLOAD=pipelines
NVCC_EXTRA_FLAGS=-std=c++20
ARCH=90
EXE_ARCH=75


help:
	@echo make  exe${ARCH}, exe${EXE_ARCH}
	@echo make  ptx${ARCH}
	@echo make  sass${ARCH}, sass${EXE_ARCH}
	@echo make  times

exe: ${WORKLOAD}${EXE_ARCH}.exe
exe${EXE_ARCH}: ${WORKLOAD}${EXE_ARCH}.exe
${WORKLOAD}${EXE_ARCH}.exe: ${WORKLOAD}.cu Makefile  ../../tools/mincu.hpp
	nvcc -std=c++20 -arch sm_${EXE_ARCH} -I ../../tools ${WORKLOAD}.cu -o ${WORKLOAD}${EXE_ARCH}.exe

ptx${ARCH}: Makefile ${WORKLOAD}-sm_${ARCH}.ptx
${WORKLOAD}-sm_${ARCH}.ptx: Makefile ../../tools/mincu.hpp ${WORKLOAD}.cu
	nvcc ${NVCC_EXTRA_FLAGS} ${WORKLOAD}.cu --generate-line-info --ptx -I ../../tools -arch sm_${ARCH}
	# nvcc ${NVCC_EXTRA_FLAGS} ${WORKLOAD}.cu --generate-line-info --ptx --source-in-ptx -I ../../tools -arch sm_${ARCH}
	mv ${WORKLOAD}.ptx ${WORKLOAD}-sm_${ARCH}.ptx


${WORKLOAD}-sm_${ARCH}.cubin: Makefile ../../tools/mincu.hpp ${WORKLOAD}.cu
	nvcc ${NVCC_EXTRA_FLAGS} ${WORKLOAD}.cu --generate-line-info \
		--source-in-ptx -c -o ${WORKLOAD}-sm_${ARCH}.cubin -I ../../tools -cubin -arch sm_${ARCH}

${WORKLOAD}-sm_${EXE_ARCH}.cubin: Makefile ../../tools/mincu.hpp ${WORKLOAD}.cu
	nvcc ${NVCC_EXTRA_FLAGS} ${WORKLOAD}.cu --generate-line-info \
		--source-in-ptx -c -o ${WORKLOAD}-sm_${EXE_ARCH}.cubin -I ../../tools -cubin -arch sm_${EXE_ARCH}



sass${ARCH}: ${WORKLOAD}-sm_${ARCH}.sass
${WORKLOAD}-sm_${ARCH}.sass: ${WORKLOAD}-sm_${ARCH}.cubin
	nvdisasm.exe \
		--print-instruction-encoding \
		--print-code \
		-g \
		${WORKLOAD}-sm_${ARCH}.cubin \
		> ${WORKLOAD}-raw-sm_${ARCH}.sass
	..\..\nva.exe \
		--arch=sm_${ARCH} \
		-lines \
		${WORKLOAD}-raw-sm_${ARCH}.sass \
		-o=${WORKLOAD}-sm_${ARCH}.sass

sass${EXE_ARCH}: ${WORKLOAD}-sm_${EXE_ARCH}.sass
${WORKLOAD}-sm_${EXE_ARCH}.sass: ${WORKLOAD}-sm_${EXE_ARCH}.cubin
	nvdisasm.exe \
		--print-instruction-encoding \
		--print-code \
		-g \
		${WORKLOAD}-sm_${EXE_ARCH}.cubin \
		> ${WORKLOAD}-raw-sm_${EXE_ARCH}.sass
	..\..\nva.exe \
		--arch=sm_${EXE_ARCH} \
		-lines \
		${WORKLOAD}-raw-sm_${EXE_ARCH}.sass \
		-o=${WORKLOAD}-sm_${EXE_ARCH}.sass


times: ${WORKLOAD}${EXE_ARCH}.exe
	${WORKLOAD}${EXE_ARCH}.exe > ${WORKLOAD}-sm_${EXE_ARCH}.times