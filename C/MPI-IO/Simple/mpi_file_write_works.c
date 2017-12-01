#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

int main(int argc, char ** argv)
{

  int rank, recv_rank, nproc, charlength, iproc;
  MPI_File file_handle;
  char outstr[50];
  int *offsets;
  MPI_Offset my_offset;

  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  offsets = (int*) malloc(nproc * sizeof(int));

  //Delete the existing file
  MPI_File_delete("out.txt", MPI_INFO_NULL);

  //Open the file for writing
  MPI_File_open(MPI_COMM_WORLD, "out.txt",
      MPI_MODE_WRONLY + MPI_MODE_CREATE, MPI_INFO_NULL, &file_handle);

  //MPI_IO is a binary output format. Have to manually add new line characters
  //Not unusual in C, but needs noting in Fortran
  charlength = sprintf(outstr, "Hello from processor %3d\n", rank);

  //Get the lengths of all other writes
  MPI_Allgather(&charlength, 1, MPI_INT, offsets, 1, MPI_INT,
      MPI_COMM_WORLD);

  //Calculate this processors offset in the file
  my_offset = 0;
  for (iproc = 0; iproc< rank; ++iproc){
    my_offset += offsets[iproc];
  }

  free(offsets);

  //Move the file pointer to that place
  MPI_File_seek(file_handle, my_offset, MPI_SEEK_SET);

  //Write using the individual file pointer
  MPI_File_write(file_handle, outstr, charlength,
      MPI_CHARACTER, MPI_STATUS_IGNORE);

  MPI_File_close(&file_handle);

  MPI_Finalize();
}
