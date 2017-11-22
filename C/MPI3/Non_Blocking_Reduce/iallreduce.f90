PROGRAM iallreduce

  USE mpi
  IMPLICIT NONE

  INTEGER, PARAMETER :: tag = 100

  INTEGER :: rank, recv, request
  INTEGER :: nproc, ierr

  CALL MPI_Init(ierr)

  CALL MPI_Comm_size(MPI_COMM_WORLD, nproc, ierr)
  CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

  CALL MPI_Iallreduce(rank, recv, 1, MPI_INTEGER, MPI_MAX, &
      MPI_COMM_WORLD, request, ierr)
  CALL MPI_Wait(request, MPI_STATUS_IGNORE, ierr)
  PRINT *, 'On rank ', rank, ' MPI_Iallreduce gives maximum rank as ', recv

  CALL MPI_Finalize(ierr)

END PROGRAM iallreduce
