F=mpiifort
main.out: TASK.o main.o
	$(F) $^ -o $@
main.o: main.f90 MSize.mod
	$(F) -c $<
TASK.o: TASK.f90
	$(F) -c $<
MSize.mod: MSize.f90
	$(F) -c $<
