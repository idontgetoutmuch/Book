all : 	repa-laplace-c

.PHONY : clean
clean :
	rm repa-laplace-c

LAPLACE_SRCS = \
	Chap1a.c \
	libc/Matrix.c \
	libc/ColorRamp.c \
	libc/Timing.c

repa-laplace-c : $(LAPLACE_SRCS)
	@echo "* Building Laplace"
	gcc --std=c99 -O3 $+ -Ilibc -o $@
	@echo
