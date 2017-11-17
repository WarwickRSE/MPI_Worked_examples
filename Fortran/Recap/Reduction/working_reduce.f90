PROGRAM reduce

  USE mpi
  IMPLICIT NONE

  INTEGER, PARAMETER :: tag = 100

  INTEGER :: rank, recv
  INTEGER :: nproc, ierr

  CALL MPI_Init(ierr)

  CALL MPI_Comm_size(MPI_COMM_WORLD, nproc, ierr)
  CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

  CALL MPI_Reduce(rank, recv, 1, MPI_INTEGER, MPI_MIN, 0, MPI_COMM_WORLD, ierr)
  IF (rank == 0) PRINT *, ' Minimum value of rank is ', recv

  CALL MPI_Reduce(rank, recv, 1, MPI_INTEGER, MPI_MAX, 0, MPI_COMM_WORLD, ierr)
  IF (rank == 0) PRINT *, ' Maximum value of rank is ', recv

  CALL MPI_Reduce(rank, recv, 1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
  IF (rank == 0) PRINT *, ' Sum of all ranks is ', recv

  CALL MPI_Finalize(ierr)

END PROGRAM reduce
