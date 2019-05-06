G=gfortran
mp=-fopenmp
main.out: TASK.o main.o
	$(G) $^ -o $@
main.o: main.f90 MSize.mod
	$(G) -c $<
TASK.o: TASK.f90
	$(G) -c $<
MSize.mod: MSize.f90
	$(G) -c $<
