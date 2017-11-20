!Warning! This program uses MPI3 features. Not all computers will have
!MPI3 libraries available yet
PROGRAM ireduce

  USE mpi
  IMPLICIT NONE

  INTEGER, PARAMETER :: tag = 100

  INTEGER :: rank, recv
  INTEGER :: nproc, ierr, handle

  CALL MPI_Init(ierr)

  CALL MPI_Comm_size(MPI_COMM_WORLD, nproc, ierr)
  CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

  CALL MPI_Ireduce(rank, recv, 1, MPI_INTEGER, MPI_MAX, 0, &
      MPI_COMM_WORLD, handle, ierr)
  CALL MPI_Wait(handle, MPI_STATUS_IGNORE, ierr)
  PRINT *, 'On rank ', rank, ' MPI_Ireduce gives maximum rank as ', recv

  CALL MPI_Finalize(ierr)

END PROGRAM ireduce
