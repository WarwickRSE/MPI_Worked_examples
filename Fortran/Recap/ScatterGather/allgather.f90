PROGRAM scatter_gather

  USE mpi
  IMPLICIT NONE

  INTEGER, PARAMETER :: tag = 100

  INTEGER :: rank, recv
  INTEGER :: nproc, ierr, irank
  INTEGER, DIMENSION(:), ALLOCATABLE :: values

  CALL MPI_Init(ierr)

  CALL MPI_Comm_size(MPI_COMM_WORLD, nproc, ierr)
  CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
  ALLOCATE(values(0:nproc-1))

  IF (rank == 0) THEN
    DO irank = 0, nproc - 1
      WRITE(*,'(A I3 A)', ADVANCE='NO') &
          'Please enter an integer number for processor', irank, ":"
      READ(*,'(2I20)') values(irank)
    END DO
  END IF

  IF (rank == 0) THEN
    PRINT *,'Output'
    PRINT *,'------'
  END IF
  CALL MPI_Scatter(values, 1, MPI_INTEGER, recv, 1, MPI_INTEGER, 0, &
      MPI_COMM_WORLD, ierr)

  IF (rank == 0) THEN
    PRINT *,""
    PRINT *,'Adding local rank to scattered value &
        & and sending to all processors via allgather'
  END IF
  IF (rank == 0) THEN
    PRINT *,""
    PRINT *,'Doing the same with all_gather'
  END IF

  CALL MPI_Allgather(recv+rank, 1, MPI_INTEGER, values, 1, MPI_INTEGER, &
      MPI_COMM_WORLD, ierr)

  IF (rank == nproc - 1) THEN
     WRITE(*,'(A I3 A)', ADVANCE='NO') &
         "Values collected on processor ", nproc-1, " are :"
     PRINT *, values
  END IF

  CALL MPI_Finalize(ierr)

END PROGRAM scatter_gather
