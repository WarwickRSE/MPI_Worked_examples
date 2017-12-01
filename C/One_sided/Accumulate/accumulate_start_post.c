#include <stdio.h>
#include <mpi.h>

#define TAG 100

int main(int argc, char ** argv)
{

  int rank, nproc, src, dest, ranks;
  MPI_Aint size_of_window, offset;
  MPI_Win window;
  MPI_Group comm_group, zero_group;

  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  offset = 0;
  size_of_window = sizeof(int);

  //Create the window. This is a piece of memory that's available for remote
  //access. In this case, a single 4 byte integer
  MPI_Win_create(&dest, size_of_window, sizeof(int), MPI_INFO_NULL,
      MPI_COMM_WORLD, &window);

  ranks = 0;
  MPI_Comm_group(MPI_COMM_WORLD, &comm_group);
  MPI_Group_incl(comm_group, 1, &ranks, &zero_group);

  //Zero is where data is being put, so only 0 needs to call MPI_Win_post
  if (rank == 0) MPI_Win_post(comm_group, 0, window);
  //Because zero is writing to itself, it also has to call MPI_Win_start
  MPI_Win_start(zero_group, 0, window);

  //Put the result into the first (zeroth) slot
  offset = 0;
  //Accumulate ranks and zero destination
  src = rank;
  dest = 0;

  MPI_Accumulate(&src, 1, MPI_INT, 0, offset, 1, MPI_INT,
      MPI_SUM, window);

  MPI_Win_complete(window);
  if (rank == 0) MPI_Win_wait(window);

  if (rank == 0) printf("Accumulated value is %3d\n", dest);

  MPI_Finalize();

}
