all : 	repa-laplace-c

.PHONY : clean
clean :
	rm -Rf dist

LAPLACE_SRCS = \
	Main.c \
	libc/Matrix.c \
	libc/ColorRamp.c \
	libc/Timing.c

repa-laplace-c : $(LAPLACE_SRCS)
	@echo "* Building Laplace"
	mkdir -p dist/build/repa-laplace-c
	gcc --std=c99 -O3 $+ -Ilibc -o $@
	@echo
