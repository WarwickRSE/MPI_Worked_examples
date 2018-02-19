/*Program to use MPI_Scan to calculate the scan (partial reduction)
This calculates the sum of all ranks <= the current rank
So the results should be
0 => 0
1 => 0 + 1 = 1
2 => 0 + 1 + 2 = 3
3 => 0 + 1 + 2 + 3 = 6
etc.*/

#include <stdio.h>
#include <mpi.h>

#define TAG 100

int main(int argc, char ** argv)
{

  int rank, nproc, recv;

  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  MPI_Scan(&rank, &recv, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
  printf("The sum of all ranks up to current on rank %3d is %3d\n", rank, recv);

  MPI_Finalize();

}
