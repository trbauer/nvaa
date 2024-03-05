ARCH=sm_90
WORKLOAD=asynccopy

default: ${WORKLOAD}-raw-${ARCH}.sass

help:
	@echo make clean
	@echo make ${WORKLOAD}-${ARCH}.cubin
	@echo make ${WORKLOAD}-raw-${ARCH}.sass

${WORKLOAD}-${ARCH}.cubin: ${WORKLOAD}.cu Makefile
	nvcc  ${WORKLOAD}.cu \
		--generate-line-info --source-in-ptx -c -o ${WORKLOAD}-${ARCH}.cubin \
		-I../../cuda-samples/common -arch sm_90
${WORKLOAD}-raw-${ARCH}.sass: ${WORKLOAD}-${ARCH}.cubin Makefile
	nvdisasm.exe \
		--print-instruction-encoding \
		-g \
		${WORKLOAD}-${ARCH}.cubin \
		> ${WORKLOAD}-raw-${ARCH}.sass

clean:
	rm -f ${WORKLOAD}-${ARCH}.cubin

# this worked: needed -cubin (not -c)
test:
	nvcc  asynccopy.cu \
		--generate-line-info --source-in-ptx -cubin \
		-I../../cuda-samples/common -arch sm_90

# % nvcc ... -time nvcc.times
# lists nvcc times
#
# % nvcc ... --optimization-info inline
# lists optimization info
test_inline_info:
	nvcc  asynccopy.cu \
		--generate-line-info --source-in-ptx -cubin \
		-I../../cuda-samples/common -arch sm_90 --optimization-info inline

#

micro.exe: micro.cu Makefile ../../tools/mincu.hpp
	nvcc  micro.cu \
		--generate-line-info -std=c++20 \
		-I../../cuda-samples/common -I ../../tools \
		-arch sm_75 -o micro.exe
micro75.ptx: micro.cu Makefile ../../tools/mincu.hpp
	nvcc  micro.cu \
		--generate-line-info -std=c++20 \
		-I../../cuda-samples/common -I ../../tools \
		-arch sm_75 --ptx -o micro75.ptx
micro75.sass: micro.cu Makefile ../../tools/mincu.hpp
	nvcc  micro.cu \
		--generate-line-info -std=c++20 \
		-I../../cuda-samples/common -I ../../tools \
		-arch sm_75 -cubin -o micro.cubin
	nva micro.cubin --arch sm_75 \
			> micro75.sass

micro90: micro90.ptx micro90.sass
micro90.ptx: micro.cu Makefile ../../tools/mincu.hpp
	nvcc  micro.cu \
		--generate-line-info -std=c++20 \
		-I../../cuda-samples/common -I ../../tools \
		-arch sm_90 --ptx -o micro90.ptx
micro90.sass: micro.cu Makefile ../../tools/mincu.hpp
	nvcc  micro.cu \
		--generate-line-info -std=c++20 \
		-I../../cuda-samples/common -I ../../tools \
		-arch sm_90 -cubin -o micro.cubin
	nva micro.cubin --arch sm_90 \
			> micro90.sass
	nva micro.cubin --arch sm_90 --no-filter-asm \
			> micro90-raw.sass

micro: micro.cu Makefile ../../tools/mincu.hpp
	nvcc  micro.cu \
		--generate-line-info --source-in-ptx -cubin \
		-I../../cuda-samples/common -I ../../tools -arch sm_90
	nvdisasm.exe \
		--print-instruction-encoding -g -gi \
		micro.cubin \
			> micro-raw-${ARCH}.sass
	nva micro.cubin --arch sm_90 \
			> micro-raw-${ARCH}.sass

NGT_FILES=\
	count_gt_main.cu \
	count_gt_sync.cu \
	count_gt_async.cu \
	count_gt.h

ngt_tile.exe: ${NGT_FILES} Makefile ../../tools/mincu.hpp
	nvcc  count_gt_main.cu count_gt_sync.cu count_gt_async.cu \
		--generate-line-info -std=c++20 \
		-I ../../tools \
		-arch sm_75 -o ngt_tile.exe

ngt_tile.sass: ${NGT_FILES} Makefile ../../tools/mincu.hpp
	nvcc  count_gt_sync.cu -cubin \
		--generate-line-info -std=c++20 \
		-I ../../tools \
		-arch sm_90 -o count_gt_sync.cubin
	nva count_gt_sync.cubin -text --arch sm_90 \
		> count_gt_sync.sass
	nvcc  count_gt_async.cu -cubin \
		--generate-line-info -std=c++20 \
		-I ../../tools \
		-arch sm_90 -o count_gt_async.cubin
	nva count_gt_async.cubin -text --arch sm_90 \
		> count_gt_async.sass
