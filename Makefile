MI=mpiifort
main.out: TASK.o main.o
	$(MI) $^ -o $@
main.o: main.f90
	$(MI) -c $<
TASK.o: TASK.f90
	$(MI) -c $<
