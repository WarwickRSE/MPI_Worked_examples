#include <stdio.h>
#include <mpi.h>

void print_counts(int rank, int type_id, char *desc)
{

  MPI_Comm comm_local;
  int nproc_local, rank_local;

  MPI_Comm_split_type(MPI_COMM_WORLD, type_id, 0,
      MPI_INFO_NULL, &comm_local);

  MPI_Comm_size(comm_local, &nproc_local);
  MPI_Comm_rank(comm_local, &rank_local);

  MPI_Comm_free(&comm_local);

  if (rank == 0)
      printf("Using %s there are %3d processors\n", desc, nproc_local);

  fflush(stdout);
}

int main(int argc, char ** argv)
{

  int rank, nproc;

  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  print_counts(rank, MPI_COMM_TYPE_SHARED, "MPI_COMM_TYPE_SHARED");

  //These are OpenMPI specific. If you're not working with OpenMPI comment
  //these lines out
  print_counts(rank, OMPI_COMM_TYPE_CORE, "OMPI_COMM_TYPE_CORE");
  print_counts(rank, OMPI_COMM_TYPE_SOCKET, "OMPI_COMM_TYPE_SOCKET");
  print_counts(rank, OMPI_COMM_TYPE_NUMA, "OMPI_COMM_TYPE_NUMA");

  MPI_Finalize();

}
