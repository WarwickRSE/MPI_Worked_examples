PROGRAM type

  USE mpi
  IMPLICIT NONE

  INTEGER, PARAMETER :: tag = 100
  INTEGER, PARAMETER :: nitems = 100

  INTEGER :: rank
  INTEGER :: nproc
  INTEGER :: ierr
  INTEGER :: contig_type, custom_sum
  INTEGER, DIMENSION(nitems) :: values, values_recv, values_recv_ref

  CALL MPI_Init(ierr)

  CALL MPI_Comm_size(MPI_COMM_WORLD, nproc, ierr)
  CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

  values = rank

  !Create the type
  CALL MPI_Type_contiguous(nitems, MPI_INTEGER, contig_type, ierr)

  !Register the type
  CALL MPI_Type_commit(contig_type, ierr)

  CALL MPI_Reduce(values, values_recv, 1, contig_type, MPI_SUM, 0, &
      MPI_COMM_WORLD, ierr)

  CALL MPI_Reduce(values, values_recv_ref, nitems, MPI_INTEGER, MPI_SUM, 0, &
      MPI_COMM_WORLD, ierr)

  IF (rank == 0) THEN
    PRINT *,"Sum over all processors = ", SUM(values_recv)
    PRINT *,"Sum over all processors (reference version) = ", &
        SUM(values_recv_ref)
  END IF

  CALL MPI_Finalize(ierr)

END PROGRAM type
