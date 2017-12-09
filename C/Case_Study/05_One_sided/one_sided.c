#include "array.h"
#include "display.h"
#include "mpi_setup.h"
#include "defines.h"
#include <stdio.h>
#include <stdlib.h>


MPI_Datatype type_x_dir, type_y_dir;
MPI_Win window;

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
  //So use nx_local, not nx
  sizes[0] = nx_local+2; sizes[1] = ny_local+2;
  //Same subsizes for all sends and receives in x direction
  subsizes[0] = 1; subsizes[1] = ny_local;
  //All operations in x direction
  starts[0] = 0; starts[1] = 1;
  create_single_type(sizes, subsizes, starts, &type_x_dir);

  //Same subsizes for all sends and receives in y direction
  subsizes[0] = nx_local; subsizes[1] = 1;
  starts[0] = 1; starts[1] = 0;
  create_single_type(sizes, subsizes, starts, &type_y_dir);
}

void bcs(grid_type *dest)
{

  MPI_Aint offset;

  //Note that all offsets are in multiples of size of float (4 bytes)
  //Because I told MPI that the displacement was the size of a float
  //in MPI_Win_create

  //Note also that my mechanism for calculating offsets is for Fortran ordered
  //arrays. If I was using a C ordered array then x offsets would be
  //multiplied by (ny_local+2)

  //Use MPI_MODE_NOSTORE because until the next fence we're only updating
  //Memory using MPI_Get, we're not doing any normal access (stores) to the
  //array
  MPI_Win_fence(MPI_MODE_NOSTORE, window);

  offset = (MPI_Aint)1; //Getting from (1, 0)
  //Inserting into (nx_local+1, 0)
  MPI_Get(access_grid(dest, nx_local + 1, 0 ), 1, type_x_dir, x_max_rank,
      offset, 1, type_x_dir, window);

  offset = (MPI_Aint)nx_local; //Getting from (nx_local, 0)
  //Inserting into (0,0)
  MPI_Get(access_grid(dest, 0, 0 ), 1, type_x_dir, x_min_rank,
      offset, 1, type_x_dir, window);

  offset = (MPI_Aint)(1) *
           (MPI_Aint)(nx_local+2);//Getting from (0, 1)
           //Multiply by length in x to get offset
  //Inserting into (0, ny_local + 1)
  MPI_Get(access_grid(dest, 0, ny_local+1 ), 1, type_y_dir, y_max_rank,
      offset, 1, type_y_dir, window);

  offset = (MPI_Aint)(ny_local) *
           (MPI_Aint)(nx_local+2);//Getting from (0, ny_local)
             //Multiply by length in x to get offset
    //Inserting into (0, 0)
  MPI_Get(access_grid(dest, 0, 0 ), 1, type_y_dir, y_min_rank,
      offset, 1, type_y_dir, window);

  MPI_Win_fence(MPI_MODE_NOSTORE, window);

}

int main(int argc, char ** argv)
{

  grid_type values, values_local, temp_local;
  int ix, iy, icycle;
  MPI_Aint window_length;
  //Allocate a 2D array with indices that run 0->nx+1 and 0->ny+1
  //This replicates Fortran's arrays with variable starts and ends
  allocate_grid(&values, 0, nx+1, 0, ny+1);
  setup_mpi(&argc, &argv);
  create_types();

  allocate_grid(&values_local, 0, nx_local+1, 0, ny_local+1);
  allocate_grid(&temp_local, 0, nx_local+1, 0, ny_local+1);
  //Assign the value 5.5 to the whole grid
  assign_grid(&values_local, 0, nx_local+1, 0, ny_local+1, 5.5);
  assign_grid(&values, 0, nx+1, 0, ny+1, 5.5);
  //Assign the boundary conditions. 1.0 along the left and bottom
  //10.0 along the right and top
  assign_grid(&values_local, 0, 0, 0, ny_local+1, 1.0);
  assign_grid(&values_local, nx_local+1, nx_local+1, 0, ny_local+1, 10.0);
  assign_grid(&values_local, 0, nx_local+1, 0, 0, 1.0);
  assign_grid(&values_local, 0, nx_local+1, ny_local+1, ny_local+1, 10.0);

  window_length = (MPI_Aint)(nx_local+2) * (MPI_Aint)(ny_local+2) *
      (MPI_Aint)(sizeof(float));

  MPI_Win_create(values_local.data, window_length, sizeof(float),
      MPI_INFO_NULL, cart_comm, &window);

  bcs(&values_local);
  gather_to_zero(&values, &values_local);
  if (rank == 0) {
    display_result(&values);
    printf("Please press a key to advance\n");
    getchar();
  }

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
    bcs(&values_local);
    if(icycle%50==0){
      gather_to_zero(&values, &values_local);
      if(rank == 0) {
        display_result(&values);
        printf("Please press a key to advance\n");
        getchar();
      }
    }
  }
  deallocate_grid(&values);
  deallocate_grid(&values_local);
  deallocate_grid(&temp_local);
  MPI_Finalize();
}
