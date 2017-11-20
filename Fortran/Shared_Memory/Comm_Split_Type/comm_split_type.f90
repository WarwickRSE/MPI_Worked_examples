!ARNING! This program uses MPI3 features. Some computers might not have
!a new enough version of MPI for it to work. Some parts are SPECIFIC to
!OpenMPI and are not (yet) part of the MPI standard

MODULE helper

  USE mpi

  CONTAINS

  SUBROUTINE print_counts(rank, type_id, desc)

    INTEGER, INTENT(IN) :: rank, type_id
    INTEGER :: comm_local, nproc_local, rank_local, ierr
    CHARACTER(LEN=*), INTENT(IN) :: desc

    CALL MPI_Comm_split_type(MPI_COMM_WORLD, type_id, 0, &
        MPI_INFO_NULL, comm_local, ierr)

    CALL MPI_Comm_size(comm_local, nproc_local, ierr)
    CALL MPI_Comm_rank(comm_local, rank_local, ierr)

    CALL MPI_Comm_free(comm_local, ierr)

    IF (rank == 0) THEN
      PRINT '(A,A,A,I3,A)', "Using ", TRIM(desc), " there are ", nproc_local, &
          " processors"
    END IF

    FLUSH(5)

  END SUBROUTINE print_counts

END MODULE helper

PROGRAM comm_split_type

  USE mpi
  USE helper
  IMPLICIT NONE

  INTEGER :: rank, nproc, ierr
  

  CALL MPI_Init(ierr)

  CALL MPI_Comm_size(MPI_COMM_WORLD, nproc, ierr)
  CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

  CALL print_counts(rank, MPI_COMM_TYPE_SHARED, "MPI_COMM_TYPE_SHARED")

  !These are OpenMPI specific. If you're not working with OpenMPI comment
  !these lines out
  CALL print_counts(rank, OMPI_COMM_TYPE_CORE, "OMPI_COMM_TYPE_CORE")
  CALL print_counts(rank, OMPI_COMM_TYPE_SOCKET, "OMPI_COMM_TYPE_SOCKET")
  CALL print_counts(rank, OMPI_COMM_TYPE_NUMA, "OMPI_COMM_TYPE_NUMA")

  CALL MPI_Finalize(ierr)

END PROGRAM comm_split_type
