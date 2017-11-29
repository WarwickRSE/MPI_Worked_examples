VPATH = .:../support:../lib
main:output_file view_file

output_file: output_file.c array.o display.o mpi_setup.o
	mpicc -g -I ../support -o output_file ../lib/array.o ../lib/display.o ../lib/mpi_setup.o output_file.c
view_file: view_file.c array.o display.o mpi_setup.o
	mpicc -g -I ../support -o view_file ../lib/array.o ../lib/display.o ../lib/mpi_setup.o view_file.c
mpi_setup.o: mpi_setup.c array.o
	mpicc -g -c -I ../support -o ../lib/mpi_setup.o ../support/mpi_setup.c
array.o:array.c array.h
	mpicc -g -c -I ../support -o ../lib/array.o ../support/array.c
display.o:display.c display.h array.o
	mpicc -g -c -I ../support -o ../lib/display.o ../support/display.c
