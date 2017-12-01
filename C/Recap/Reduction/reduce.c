#include <stdio.h>
#include <mpi.h>

#define TAG 100

int main(int argc, char ** argv)
{

  int rank, nproc, recv;

  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  MPI_Reduce(&rank, &recv, 1, MPI_INT, MPI_MAX, 0, MPI_COMM_WORLD);

  printf("On rank %3d MPI_Reduce gives maximum rank as %3d\n", rank, recv);

  MPI_Finalize();

}
