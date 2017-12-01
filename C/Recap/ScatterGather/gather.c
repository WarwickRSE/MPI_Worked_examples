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
    printf("Adding local rank to scattered value and sending value "
        "back via gather\n");
  }
  MPI_Scatter(values, 1, MPI_INT, &recv, 1, MPI_INT, 0,
      MPI_COMM_WORLD);

  //Add the local rank to the received value
  recv += rank;

  MPI_Gather(&recv, 1, MPI_INT, values, 1, MPI_INT, 0,
      MPI_COMM_WORLD);

  if (rank == 0) {
    printf("Values collected on processor zero are :\n");
    for (irank =0; irank < nproc; ++irank) {
      printf("%3d ",values[irank]);
    }
    printf("\n");
  }

  free(values);

  MPI_Finalize();

}
