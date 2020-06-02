ARCH=sm_75
WORKLOAD=itester

all: ${WORKLOAD}.exe

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
		> ${WORKLOAD}-${ARCH}.sass
