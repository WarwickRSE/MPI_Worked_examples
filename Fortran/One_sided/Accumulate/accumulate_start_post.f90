PROGRAM accumulate

  USE mpi
  IMPLICIT NONE

  INTEGER, PARAMETER :: tag = 100

  INTEGER :: rank, src, dest, nproc, dummy
  INTEGER :: comm_group, zero_group
  INTEGER :: ierr, window, intsize
  INTEGER(KIND=MPI_ADDRESS_KIND) :: size_of_window, offset

  CALL MPI_Init(ierr)

  CALL MPI_Comm_size(MPI_COMM_WORLD, nproc, ierr)
  CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

  !In C would just use sizeof()
  !In Fortran2008, could use C_SIZEOF
  !NOTE! This routine returns the size of AN ELEMENT
  !Not the size of an array if you give it an array
  CALL MPI_Sizeof(src, intsize, ierr)
  !Just using a single int here
  size_of_window = intsize

  !Create the window. This is a piece of memory that's available for remote
  !access. In this case, a single 4 byte integer
  CALL MPI_Win_create(dest, size_of_window, intsize, MPI_INFO_NULL, &
      MPI_COMM_WORLD, window, ierr)

  CALL MPI_Comm_group(MPI_COMM_WORLD, comm_group, ierr)
  CALL MPI_Group_incl(comm_group, 1, (/0/), zero_group, ierr)

  IF (rank == 0) CALL MPI_Win_post(comm_group, 0, window, ierr)
  CALL MPI_Win_start(zero_group, 0, window, ierr)

  src = rank
  dest = 0

  !Put the result into the first (zeroth) slot
  offset = 0

  !This call accumulates whatever data is in src on all processors and puts
  !them in the window on processor 0. Here using MPI_Win_post and MPI_Win_start
  !to put things in the most restrictive epoch possible
  CALL MPI_Accumulate(src, 1, MPI_INTEGER, 0, offset, 1, MPI_INTEGER, &
      MPI_SUM, window, ierr)

  CALL MPI_Win_complete(window, ierr)
  IF (rank == 0) CALL MPI_Win_wait(Window, ierr)


  IF (rank == 0) PRINT *,"Accumulated value is ", dest

  CALL MPI_Finalize(ierr)

END PROGRAM accumulate
