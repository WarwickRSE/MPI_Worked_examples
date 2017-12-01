#include <stdio.h>
#include <mpi.h>

#define TAG 100

int main(int argc, char ** argv)
{

  int rank, nproc, recv;

  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  MPI_Reduce(&rank, &recv, 1, MPI_INT, MPI_MIN, 0, MPI_COMM_WORLD);
  if (rank == 0) printf("Minimum value of rank is %3d\n", recv);

  MPI_Reduce(&rank, &recv, 1, MPI_INT, MPI_MAX, 0, MPI_COMM_WORLD);
  if (rank == 0) printf("Maximum value of rank is %3d\n", recv);

  MPI_Reduce(&rank, &recv, 1, MPI_INT, MPI_SUM, 0, MPI_COMM_WORLD);
  if (rank == 0) printf("Sum of all ranks is %3d\n", recv);

  MPI_Finalize();

}
