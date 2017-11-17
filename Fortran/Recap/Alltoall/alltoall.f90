PROGRAM alltoall

  USE mpi
  IMPLICIT NONE

  INTEGER, PARAMETER :: tag = 100

  INTEGER :: rank, recv
  INTEGER :: nproc, ierr, irank
  INTEGER, DIMENSION(:), ALLOCATABLE :: values_s, values_r
  CHARACTER(LEN=50) :: output, temp

  CALL MPI_Init(ierr)

  CALL MPI_Comm_size(MPI_COMM_WORLD, nproc, ierr)
  CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
  ALLOCATE(values_s(0:nproc - 1))
  ALLOCATE(values_r(0:nproc - 1))

  DO irank = 0, nproc - 1
    values_s(irank) = rank * nproc + irank
  END DO

  CALL MPI_Alltoall(values_s, 1, MPI_INTEGER, values_r, 1, MPI_INTEGER, &
    MPI_COMM_WORLD, ierr)

  IF (rank == 0) THEN
    WRITE(*,*), " Rank|Numbers"
    WRITE(*,*), " ------------"
  END IF
  CALL MPI_BARRIER(MPI_COMM_WORLD, ierr)
  WRITE(output,"(I5 A)"), rank, "|"
  DO irank = 0, nproc - 1
    WRITE(temp,"(I5)"), values_r(irank)
    output = TRIM(output) // TRIM(temp)
  END DO
  PRINT *,output

  CALL MPI_Finalize(ierr)

END PROGRAM alltoall
