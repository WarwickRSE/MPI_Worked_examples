#include <stdio.h>
#include <mpi.h>

#define TAG 100

int main(int argc, char ** argv)
{

  int rank, recv_rank, nproc, left, right, repcount;
  double time1, time2, dtime;

  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  //Set up periodic domain
  left = rank - 1;
  if (left < 0) left = nproc - 1;
  right = rank + 1;
  if (right > nproc - 1) right = 0;

  time1 = MPI_Wtime();
  for (repcount = 0; repcount <100000; ++repcount) {
    if (rank%2 == 0) {
      MPI_Ssend(&rank, 1, MPI_INT, right, TAG, MPI_COMM_WORLD);
      MPI_Recv(&recv_rank, 1, MPI_INT, left, TAG, MPI_COMM_WORLD,
          MPI_STATUS_IGNORE);
    } else {
      MPI_Recv(&recv_rank, 1, MPI_INT, left, TAG, MPI_COMM_WORLD,
          MPI_STATUS_IGNORE);
      MPI_Ssend(&rank, 1, MPI_INT, right, TAG, MPI_COMM_WORLD);
    }
  }
  time2 = MPI_Wtime();

  dtime = time2 - time1;

  printf("Rank %3d got message from rank %3d of %3d\n", rank, left, recv_rank);
  MPI_Barrier(MPI_COMM_WORLD);
  if (rank == 0) {
    printf("It took %f seconds to complete\n", dtime);
  }

  MPI_Finalize();

}
