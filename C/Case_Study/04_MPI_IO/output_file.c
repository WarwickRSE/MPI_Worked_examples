#include "array.h"
#include "mpi_setup.h"
#include "defines.h"
#include <stdio.h>
#include <stdlib.h>


MPI_Datatype type_l_r, type_l_s, type_r_r, type_r_s;
MPI_Datatype type_u_r, type_u_s, type_d_r, type_d_s;
MPI_Request requests[8];
MPI_Datatype type_subarray, type_no_guard;
MPI_File file_handle;
MPI_Offset offset = 0;

//Subroutine to write the output file
//Notice that this is called on all cores
//unlike the output to screen
void output_to_file(grid_type * data)
{
  MPI_File_set_view(file_handle, offset, MPI_FLOAT, type_subarray,
      "native", MPI_INFO_NULL);
  MPI_File_write_all(file_handle, data->data, 1, type_no_guard,
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

  //Always the same sizes. This is local to each CPU
  //So use nx_local, not nx. This is only true because this code has equal
  //domains on each processor
  sizes[0] = nx_local + 2; sizes[1] = ny_local + 2;
  //Same subsizes for all sends and receives in x direction
  subsizes[0] = 1; subsizes[1] = ny_local;

  //Receive on left boundary
  starts[0] = 0; starts[1] = 1;
  create_single_type(sizes, subsizes, starts, &type_l_r);

  //Send on left boundary
  starts[0] = 1; starts[1] = 1;
  create_single_type(sizes, subsizes, starts, &type_l_s);

  //Receive on right boundary
  starts[0] = nx_local+1; starts[1] = 1;
  create_single_type(sizes, subsizes, starts, &type_r_r);

  //Send on right boundary
  starts[0] = nx_local; starts[1] = 1;
  create_single_type(sizes, subsizes, starts, &type_r_s);

  //Same subsizes for all sends and receives in y direction
  subsizes[0] = nx_local; subsizes[1] =  1;

  starts[0] = 1; starts[1] = 0;
  create_single_type(sizes, subsizes, starts, &type_d_r);

  starts[0] = 1; starts[1] = 1;
  create_single_type(sizes, subsizes, starts, &type_d_s);

  starts[0] =1; starts[1] = ny_local+1;
  create_single_type(sizes, subsizes, starts, &type_u_r);

  starts[0] = 1; starts[1] = ny_local;
  create_single_type(sizes, subsizes, starts, &type_u_s);

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

void setup_requests(grid_type *data)
{

  //NOTE that these are bound to a data object now. Can't use array temporaries
  //unless you never deallocate or reallocate them without redoing these
  MPI_Send_init(data->data, 1, type_l_s, x_min_rank, TAG, cart_comm, requests);
  //NB requests+1 == &requests[1]
  MPI_Send_init(data->data, 1, type_r_s, x_max_rank, TAG, cart_comm, requests+1);
  MPI_Send_init(data->data, 1, type_d_s, y_min_rank, TAG, cart_comm, requests+2);
  MPI_Send_init(data->data, 1, type_u_s, y_max_rank, TAG, cart_comm, requests+3);

  MPI_Recv_init(data->data, 1, type_l_r, x_min_rank, TAG, cart_comm, requests+4);
  MPI_Recv_init(data->data, 1, type_r_r, x_max_rank, TAG, cart_comm, requests+5);
  MPI_Recv_init(data->data, 1, type_d_r, y_min_rank, TAG, cart_comm, requests+6);
  MPI_Recv_init(data->data, 1, type_u_r, y_max_rank, TAG, cart_comm, requests+7);

}

int main(int argc, char ** argv)
{

  MPI_Status statuses[8];
  grid_type values, values_local, temp_local;
  int ix, iy, icycle;
  //Allocate a 2D array with indices that run 0->nx+1 and 0->ny+1
  //This replicates Fortran's arrays with variable starts and ends
  allocate_grid(&values, 0, nx+1, 0, ny+1);
  setup_mpi(&argc, &argv);
  create_types();

  allocate_grid(&values_local, 0, nx_local+1, 0, ny_local+1);
  allocate_grid(&temp_local, 0, nx_local+1, 0, ny_local+1);
  setup_requests(&values_local);
  //Assign the value 5.5 to the whole grid
  assign_grid(&values_local, 0, nx_local+1, 0, ny_local+1, 5.5);
  assign_grid(&values, 0, nx+1, 0, ny+1, 5.5);
  //Assign the boundary conditions. 1.0 along the left and bottom
  //10.0 along the right and top
  assign_grid(&values_local, 0, 0, 0, ny_local+1, 1.0);
  assign_grid(&values_local, nx_local+1, nx_local+1, 0, ny_local+1, 10.0);
  assign_grid(&values_local, 0, nx_local+1, 0, 0, 1.0);
  assign_grid(&values_local, 0, nx_local+1, ny_local+1, ny_local+1, 10.0);

  MPI_File_delete("out.dat", MPI_INFO_NULL);
  MPI_File_open(cart_comm, "out.dat", MPI_MODE_WRONLY + MPI_MODE_CREATE,
      MPI_INFO_NULL, &file_handle);

  /*Now apply the interprocessor boundary conditions
  Note, not using a BCS function because there's no need
  Can't write a generic one anyway because the request is prebound
  to a particular variable*/
  MPI_Startall(8, requests);
  MPI_Waitall(8, requests, statuses);
  output_to_file(&values_local);

  //To a C programmer, this looks backwards, but the array is using
  //Fortran ordering deliberately
  for (icycle=0;icycle<500;++icycle){
    for (iy=1;iy<=ny_local;++iy){
      for (ix=1;ix<=nx_local;++ix){
        *(access_grid(&temp_local, ix, iy)) = 0.25 * (
            *(access_grid(&values_local, ix+1, iy  )) +
            *(access_grid(&values_local, ix  , iy+1)) +
            *(access_grid(&values_local, ix-1, iy  )) +
            *(access_grid(&values_local, ix  , iy-1)));
      }
    }
    copy_grid(&values_local, &temp_local, 1, nx_local, 1, ny_local);

    /*Now apply the interprocessor boundary conditions
    Note, not using a BCS function because there's no need
    Can't write a generic one anyway because the request is prebound
    to a particular variable*/
    MPI_Startall(8, requests);
    MPI_Waitall(8, requests, statuses);

    if(icycle%50==0){
      output_to_file(&values_local);
    }
  }
  MPI_File_close(&file_handle);
  deallocate_grid(&values);
  deallocate_grid(&values_local);
  deallocate_grid(&temp_local);
  MPI_Finalize();
}
