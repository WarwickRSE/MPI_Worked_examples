#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

#define TAG 100

int main(int argc, char ** argv)
{

  int rank, recv_rank, nproc, icycle;
  MPI_Aint size_of_window, offset;
  MPI_Win window;

  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  offset = 0;
  size_of_window = sizeof(int);

  //Create the window. This is a piece of memory that's available for remote
  //access. In this case, a single 4 byte integer
  MPI_Win_create(&recv_rank, size_of_window, sizeof(int), MPI_INFO_NULL,
      MPI_COMM_WORLD, &window);

  offset = 0;
  //Actual call to put the data in the remote processor
  if (rank == 0) {
    for (icycle = 1; icycle<nproc; ++icycle){
      //Lock the window on the remote rank
      MPI_Win_lock(MPI_LOCK_EXCLUSIVE, icycle, 0, window);
      //Actual call to put the data in the remote processor
      MPI_Put(&icycle, 1, MPI_INTEGER, icycle, offset, 1, MPI_INTEGER, window);
      //Call to flush to complete the put now, because the code can't do
      //anything until the put is complete. This may not in general be true
      //Take care though, once you have called unlock, you can't flush
      MPI_Win_flush(icycle, window);
      //Unlock the window on the remote rank
      MPI_Win_unlock(icycle, window);
    }
  }

  MPI_Win_free(&window);

  printf("Rank %3d got message from rank 0 of %3d\n", rank, recv_rank);

  MPI_Finalize();

}
