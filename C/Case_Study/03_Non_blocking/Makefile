VPATH = .:../support:../lib
main:non_blocking requests

non_blocking: non_blocking.c array.o display.o mpi_setup.o
	mpicc -g -I ../support -o non_blocking ../lib/array.o ../lib/display.o ../lib/mpi_setup.o non_blocking.c
requests: requests.c array.o display.o mpi_setup.o
	mpicc -g -I ../support -o requests ../lib/array.o ../lib/display.o ../lib/mpi_setup.o requests.c
mpi_setup.o: mpi_setup.c array.o
	mpicc -g -c -I ../support -o ../lib/mpi_setup.o ../support/mpi_setup.c
array.o:array.c array.h
	mpicc -g -c -I ../support -o ../lib/array.o ../support/array.c
display.o:display.c display.h array.o
	mpicc -g -c -I ../support -o ../lib/display.o ../support/display.c
