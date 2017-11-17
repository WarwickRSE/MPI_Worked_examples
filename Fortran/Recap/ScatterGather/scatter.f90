PROGRAM scatter

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
    PRINT *,'Scattering input values to other processors'
  END IF
  CALL MPI_Barrier(MPI_COMM_WORLD, ierr)
  CALL MPI_Scatter(values, 1, MPI_INTEGER, recv, 1, MPI_INTEGER, 0, &
      MPI_COMM_WORLD, ierr)
  WRITE(*,'(A I3 A I10)') 'Rank ', rank, ' has value from scatter of ', recv

  CALL MPI_Finalize(ierr)

END PROGRAM scatter
