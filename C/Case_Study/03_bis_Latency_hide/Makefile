VPATH = .:../support:../lib
main:latency_hide

latency_hide: latency_hide.c array.o display.o mpi_setup.o
	mpicc -g -I ../support -o latency_hide ../lib/array.o ../lib/display.o ../lib/mpi_setup.o latency_hide.c
mpi_setup.o: mpi_setup.c array.o
	mpicc -g -c -I ../support -o ../lib/mpi_setup.o ../support/mpi_setup.c
array.o:array.c array.h
	mpicc -g -c -I ../support -o ../lib/array.o ../support/array.c
display.o:display.c display.h array.o
	mpicc -g -c -I ../support -o ../lib/display.o ../support/display.c
