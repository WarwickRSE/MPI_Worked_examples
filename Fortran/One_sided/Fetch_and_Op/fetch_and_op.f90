PROGRAM fetch_and_op


  !WARNING! This program uses MPI3 features
  !Some clusters may not yet have MPI3 installed

  USE mpi
  IMPLICIT NONE

  INTEGER, PARAMETER :: tag = 100

  INTEGER :: rank, src, dest, nproc, dummy
  INTEGER :: left, right
  INTEGER :: ierr, window, intsize
  INTEGER(KIND=MPI_ADDRESS_KIND) :: size_of_window, offset

  CALL MPI_Init(ierr)

  CALL MPI_Comm_size(MPI_COMM_WORLD, nproc, ierr)
  CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

  !Set up periodic domain
  left = rank - 1
  IF (left < 0) left = nproc - 1
  right = rank + 1
  IF (right > nproc - 1) right = 0

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

  !Use collective synchronization model. After this command any processor
  !can use MPI_Put or MPI_Get on any other processor
  CALL MPI_Win_fence(0, window, ierr)

  src = rank
  dest = 0

  !Put the result into the first (zeroth) slot
  offset = 0

  !This is a slightly odd call. You accumulate all of the values in 
  !"src" on all processors and put them into the MPI window (here dest)
  !The second parameter contains the value of in the MPI window the
  !content of the target buffer before accumulation
  CALL MPI_Fetch_and_op(src, dummy, MPI_INTEGER, 0, offset, MPI_SUM, &
      window, ierr)

  !Call Win_fence again to end the access and exposure epochs
  CALL MPI_Win_fence(0, window, ierr)

  IF (rank == 0) PRINT *,"Accumulated value is ", dest

  CALL MPI_Finalize(ierr)

END PROGRAM fetch_and_op
