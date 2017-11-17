PROGRAM reduce

  USE mpi
  IMPLICIT NONE

  INTEGER, PARAMETER :: tag = 100

  INTEGER :: rank, recv
  INTEGER :: nproc, ierr

  CALL MPI_Init(ierr)

  CALL MPI_Comm_size(MPI_COMM_WORLD, nproc, ierr)
  CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

  CALL MPI_Allreduce(rank, recv, 1, MPI_INTEGER, MPI_MAX, MPI_COMM_WORLD, ierr)
  PRINT *, 'On rank ', rank, ' MPI_Allreduce gives maximum rank as ', recv

  CALL MPI_Finalize(ierr)

END PROGRAM reduce
