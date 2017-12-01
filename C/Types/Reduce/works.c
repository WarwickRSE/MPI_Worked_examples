#include <stdio.h>
#include <mpi.h>

#define NITEMS 100

void adder(void* newvals, void* accumvals, int *nels, MPI_Datatype *type)
{
  MPI_Aint lb, extent;
  MPI_Aint cycle, end;
  int *nv2, *av2;

  nv2 = newvals;
  av2 = accumvals;

  MPI_Type_get_extent(*type, &lb, &extent);
  //Convert from bytes and element count to number of integers
  end = extent * (MPI_Aint)*nels / ((MPI_Aint)sizeof(int));
  for (cycle = 0; cycle < end; ++cycle){
    av2[cycle] = av2[cycle] + nv2[cycle];
  }
}

int main(int argc, char** argv)
{

  int rank, recv_rank, nproc, left, right, iitems, sum, sum_recv;
  int values[NITEMS], values_recv[NITEMS], values_recv_ref[NITEMS];
  MPI_Datatype contig_type;
  MPI_Op custom_sum;

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

  //Create an operator to add together this type
  MPI_Op_create(&adder, 1, &custom_sum);

  MPI_Reduce(values, values_recv, 1, contig_type, custom_sum, 0, 
      MPI_COMM_WORLD);
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
    printf("Sum over all processors = %5d\n", sum);
    printf("Sum over all processors (reference version) = %5d\n", sum_recv);
  }

  MPI_Finalize();

}
