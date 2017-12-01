#include <stdio.h>
#include <mpi.h>

int main(int argc, char ** argv)
{

  int rank, nproc, recv;
  MPI_Request handle;

  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  MPI_Ireduce(&rank, &recv, 1, MPI_INT, MPI_MAX, 0, MPI_COMM_WORLD,
      &handle);
  MPI_Wait(&handle, MPI_STATUS_IGNORE);

  printf("On rank %3d MPI_Ireduce gives maximum rank as %3d\n", rank, recv);

  MPI_Finalize();

}
