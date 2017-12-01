#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

#define TAG 100

int main(int argc, char ** argv)
{

  int rank, nproc, recv, irank, charpos;
  int *values_s, *values_r;
  char output[51];

  charpos = 0;

  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  //This is using ANSI C, so a bit old fashioned
  values_s = (int*) malloc(sizeof(int) * nproc);
  values_r = (int*) malloc(sizeof(int) * nproc);

  for(irank = 0; irank < nproc; ++irank){
    values_s[irank] = rank * nproc + irank;
  }

  MPI_Alltoall(values_s, 1, MPI_INT, values_r, 1, MPI_INT,
      MPI_COMM_WORLD);

  if (rank == 0) {
    printf(" Rank|Numbers\n");
    printf(" ------------\n");
    }

  charpos+=sprintf(output+charpos,"%5d|",rank);
  for(irank = 0; irank < nproc ; ++irank){
    charpos+=sprintf(output+charpos,"%5d ", values_r[irank]);
  }
  printf("%s \n", output);

  free(values_r);
  free(values_s);

  MPI_Finalize();

}
