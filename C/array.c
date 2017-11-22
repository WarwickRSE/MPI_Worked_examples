struct grid_s {
  int min_x;
  int min_y;
  int max_x;
  int max_y;
  size_t n_elements;
  char *data;
} grid_type;

char* access_grid(grid_type *grid, int ix, int iy)
{
  //This uses Fortran ordering. Bit odd, but C does give you a choice
  return grid->data +
      ((ix-grid->min_x) + (iy-grid->min_y) * (grid->max_x-grid->min_x));
}

void allocate_grid(grid_type *grid, int min_x, int max_x, int min_y, int max_y){

  grid->min_x = min_x;
  grid->max_x = max_x;
  grid->min_y = min_y;
  grid->max_y = max_y;
  grid->n_elements = (size_t)(max_x - min_x) * (size_t)(max_y-min_y);
  grid->data = (char*) malloc(grid->n_elements);
}

