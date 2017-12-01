#include <stdio.h>
#include <mpi.h>

#define NITEMS 100

int main(int argc, char** argv)
{

  int rank, recv_rank, nproc, left, right, iitems, sum, sum_recv;
  int values[NITEMS], values_recv[NITEMS], values_recv_ref[NITEMS];
  MPI_Datatype contig_type;

  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  //Set up periodic domain
  left = rank - 1;
  if (left < 0) left = nproc - 1;
  right = rank + 1;
  if (right > nproc - 1) right = 0;

  for (iitems = 0; iitems < NITEMS; ++iitems){
    values[iitems] = rank;
  }

  MPI_Type_contiguous(NITEMS, MPI_INT, &contig_type);
  MPI_Type_commit(&contig_type);

  if (rank == 0) printf("MPI_Type_contiguous used as send and recieve types\n");

  MPI_Reduce(values, values_recv, 1, contig_type, MPI_SUM, 0, MPI_COMM_WORLD);
  MPI_Reduce(values, values_recv_ref, NITEMS, MPI_INT, MPI_SUM, 0,
      MPI_COMM_WORLD);

  MPI_Type_free(&contig_type);

  sum = 0;
  sum_recv = 0;
  for (iitems = 0; iitems < NITEMS; ++iitems){
    sum += values_recv[iitems];
    sum_recv += values_recv[iitems];
  }

  if (rank == 0) {
    printf("Sum over all processors = %5d", sum);
    printf("Sum over all processors (reference version) = %5d", sum_recv);
  }

  MPI_Finalize();

}
