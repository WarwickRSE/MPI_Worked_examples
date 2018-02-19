!Program to use MPI_Scan to calculate the scan (partial reduction)
!This calculates the sum of all ranks <= the current rank
!So the results should be
!0 => 0
!1 => 0 + 1 = 1
!2 => 0 + 1 + 2 = 3
!3 => 0 + 1 + 2 + 3 = 6
!etc.
PROGRAM scan

  USE mpi
  IMPLICIT NONE

  INTEGER, PARAMETER :: tag = 100

  INTEGER :: rank, recv
  INTEGER :: nproc, ierr

  CALL MPI_Init(ierr)

  CALL MPI_Comm_size(MPI_COMM_WORLD, nproc, ierr)
  CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

  CALL MPI_Scan(rank, recv, 1, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, ierr)
  PRINT "(A,I3,A,I3)","The sum of all ranks up to current on rank ", &
      rank, " is ", recv

  CALL MPI_Finalize(ierr)

END PROGRAM scan
