VPATH = .:../support:../lib
main:one_sided one_sided_trick

one_sided: one_sided.c array.o display.o mpi_setup.o
	mpicc -g -I ../support -o one_sided ../lib/array.o ../lib/display.o ../lib/mpi_setup.o one_sided.c
one_sided_trick: one_sided.c array.o display.o mpi_setup.o
	mpicc -g -I ../support -o one_sided_trick ../lib/array.o ../lib/display.o ../lib/mpi_setup.o one_sided_trick.c
mpi_setup.o: mpi_setup.c array.o
	mpicc -g -c -I ../support -o ../lib/mpi_setup.o ../support/mpi_setup.c
array.o:array.c array.h
	mpicc -g -c -I ../support -o ../lib/array.o ../support/array.c
display.o:display.c display.h array.o
	mpicc -g -c -I ../support -o ../lib/display.o ../support/display.c
