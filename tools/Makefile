
runbin-skeleton.exe: runbin-skeleton.cu Makefile
	nvcc runbin-skeleton.cu --expt-relaxed-constexpr -o=runbin-skeleton.exe -arch sm_75

clean:
	rm -f runbin-skeleton.exe runbin-skeleton.exp runbin-skeleton.lib

