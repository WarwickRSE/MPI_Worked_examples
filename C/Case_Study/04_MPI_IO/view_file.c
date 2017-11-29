#include "array.h"
#include "mpi_setup.h"
#include "display.h"
#include "defines.h"
#include <stdio.h>
#include <stdlib.h>


MPI_Datatype type_subarray, type_no_guard;
MPI_File file_handle;
MPI_Offset offset = 0;

//Subroutine to write the output file
//Notice that this is called on all cores
//unlike the output to screen
void input_from_file(grid_type * data)
{
  MPI_File_set_view(file_handle, offset, MPI_FLOAT, type_subarray,
      "native", MPI_INFO_NULL);
  MPI_File_read_all(file_handle, data->data, 1, type_no_guard,
      MPI_STATUS_IGNORE);

  //Shift the offset by the amount of data written
  offset = offset + (nx * ny * sizeof(float));
}

void create_single_type(int *sizes, int *subsizes, int *starts,
    MPI_Datatype *newtype)
{
    //MPI_ORDER_FORTRAN because array is Fortran ordered by me
    MPI_Type_create_subarray(2, sizes, subsizes, starts,
        MPI_ORDER_FORTRAN, MPI_FLOAT, newtype);
    MPI_Type_commit(newtype);
}

void create_types()
{
  int sizes[2], subsizes[2], starts[2];

  //Now create the types used for MPI_IO
  //First, represent the main array without it's guard cells
  sizes[0] = nx_local + 2; sizes[1] = ny_local + 2;
  subsizes[0] = nx_local; subsizes[1] = ny_local;
  starts[0] = 1; starts[1] = 1;

  create_single_type(sizes, subsizes, starts, &type_no_guard);

  //Now represent the part of the global array that is represented
  //on this processor
  sizes[0] = nx; sizes[1] = ny;
  subsizes[0] = nx_local; subsizes[1] = ny_local;
  //Minus 1 because rest of code used Fortran like 1 based arrays
  //MPI ALWAYS uses C style 0 based
  starts[0] = x_cell_min_local -1; starts[1] = y_cell_min_local - 1;

  create_single_type(sizes, subsizes, starts, &type_subarray);
}

int main(int argc, char ** argv)
{

  int icycle;
  grid_type values, values_local;
  //Allocate a 2D array with indices that run 0->nx+1 and 0->ny+1
  //This replicates Fortran's arrays with variable starts and ends
  allocate_grid(&values, 0, nx+1, 0, ny+1);
  setup_mpi(&argc, &argv);
  create_types();

  allocate_grid(&values_local, 0, nx_local+1, 0, ny_local+1);

  MPI_File_open(cart_comm, "out.dat", MPI_MODE_RDONLY,
      MPI_INFO_NULL, &file_handle);

  //Gather everything on rank 0 for display
  for (icycle = 0; icycle<=10; ++icycle){
    input_from_file(&values_local);
    gather_to_zero(&values, &values_local);
    if (rank == 0) {
      display_result(&values);
      printf("Please press a key to advance\n");
      getchar();
    }
  }

  MPI_File_close(&file_handle);
  deallocate_grid(&values);
  deallocate_grid(&values_local);
  MPI_Finalize();
}
