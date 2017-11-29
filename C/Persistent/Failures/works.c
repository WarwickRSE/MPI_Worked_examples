#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

#define TAG 100
#define NELEMENTS 10

int main(int argc, char ** argv)
{

  int rank, maxval, nproc, left, right, repcount, cycle;
  double time1, time2, time3, dtime1, dtime2, dtime1_r, dtime2_r;
  MPI_Request requests[2];
  MPI_Status statuses[2];
  //Dynamic arrays this time
  int *send, *recv;

  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  //Set up periodic domain
  left = rank - 1;
  if (left < 0) left = nproc - 1;
  right = rank + 1;
  if (right > nproc - 1) right = 0;

  time1 = MPI_Wtime();
  //In Fortran, MPI can detect if an array is allocated or not so allocate here
  //Only here to keep C and Fortran codes similar
  send = (int*)malloc(sizeof(int)*NELEMENTS);
  recv = (int*)malloc(sizeof(int)*NELEMENTS);

  for(cycle =0 ;cycle<NELEMENTS;++cycle){
    send[cycle] = rank;
  }

  MPI_Send_init(send, NELEMENTS, MPI_INTEGER, right, TAG, MPI_COMM_WORLD,
      requests);
  MPI_Recv_init(recv, NELEMENTS, MPI_INTEGER, left , TAG, MPI_COMM_WORLD,
      requests+1);
  time2 = MPI_Wtime();
  //In this one, don't deallocate data arrays
  for (repcount = 0; repcount <100000; ++repcount) {
    MPI_Startall(2, requests);
    MPI_Waitall(2, requests, statuses);
  }
  time3 = MPI_Wtime();

  dtime1 = time3 - time1;
  MPI_Reduce(&dtime1, &dtime1_r, 1, MPI_DOUBLE, MPI_MAX, 0,
      MPI_COMM_WORLD);

  dtime2 = time3 - time2;
  MPI_Reduce(&dtime2, &dtime2_r, 1, MPI_DOUBLE, MPI_MAX, 0,
      MPI_COMM_WORLD);

  maxval = 0;
  for (cycle = 0; cycle<NELEMENTS; ++cycle){
    if (recv[cycle] > maxval) maxval = recv[cycle];
  }

  printf("Rank %3d got value from rank %3d of %3d. Should be %3d\n",
      rank, left, maxval, left);
  MPI_Barrier(MPI_COMM_WORLD);
  if (rank == 0) {
    printf("It took %f seconds to complete send/receives\n", dtime1_r);
    printf("It took %f seconds to to complete setup\n", dtime2_r);
  }

  MPI_Finalize();

}
