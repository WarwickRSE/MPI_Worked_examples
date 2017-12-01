#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

#define TAG 100

int main(int argc, char ** argv)
{

  int rank, nproc, recv, irank;
  int *values;

  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  //This is using ANSI C, so a bit old fashioned
  values = (int*) malloc(sizeof(int) * nproc);

  if (rank == 0) {
    for (irank = 0; irank < nproc; ++irank) {
      printf("Please enter an integer number for processor %3d:",irank);
      fflush(stdout);
      scanf("%d", values+irank);
    }

    printf("Output\n");
    printf("------\n");
    printf("Scattering input values to other processors\n");
  }
  MPI_Barrier(MPI_COMM_WORLD);
  MPI_Scatter(values, 1, MPI_INT, &recv, 1, MPI_INT, 0,
      MPI_COMM_WORLD);
  //Can now free values
  free(values);

  printf("Rank %3d has value from scatter of %3d\n", rank, recv);

  MPI_Finalize();

}
