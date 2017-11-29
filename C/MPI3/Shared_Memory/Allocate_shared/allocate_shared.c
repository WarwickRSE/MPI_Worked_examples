#include <stdio.h>
#include <mpi.h>

#define TAG 100

int main(int argc, char ** argv)
{

  int rank, nproc, iproc;
  int rank_local, nproc_local, disp_size;
  int *data;
  MPI_Aint nitems;
  MPI_Win window;
  MPI_Comm comm_local;

  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  //Get a communicator for processors that can share memory
  MPI_Comm_split_type(MPI_COMM_WORLD, MPI_COMM_TYPE_SHARED, 0, MPI_INFO_NULL,
      &comm_local);
  MPI_Comm_size(comm_local, &nproc_local);
  MPI_Comm_rank(comm_local, &rank_local);

  if (rank ==0){
    printf("Total ranks = %3d\n", nproc);
    printf("Node ranks = %3d\n", nproc_local);
  }

  if (rank_local == 0) {
    nitems = sizeof(int) * nproc_local;
    MPI_Win_allocate_shared(nitems, 4, MPI_INFO_NULL, comm_local,
          &data, &window);
  } else {
    nitems = 0;
    //Null size here because don't want another window
    //Could use MPI_Win_create_dynamic, but that's more work
    MPI_Win_allocate_shared(nitems, 4, MPI_INFO_NULL, comm_local,
          &data, &window);
    //Use MPI_Win_shared_query to get a reference to the window allocated on
    //Processor zero
    MPI_Win_shared_query(window, 0, &nitems, &disp_size, &data);
  }

  MPI_Win_fence(0, window);
  //Data is now just a normal pointer, so use it
  data[rank_local] = rank;
  MPI_Win_fence(0, window);

  if (rank_local == 0){
    printf("On processor 0 of this local set, accumulated values are ");
    for (iproc = 0; iproc<nproc_local; ++iproc){
      printf("%3d ", data[iproc]);
    }
    printf("\n");
  }

  MPI_Finalize();

}
