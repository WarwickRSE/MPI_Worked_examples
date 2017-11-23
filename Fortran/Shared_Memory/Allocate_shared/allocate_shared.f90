!WARNING! This program uses MPI3 features. Some computers might not have
!a new enough version of MPI for it to work. Some parts are SPECIFIC to
!OpenMPI and are not (yet) part of the MPI standard

PROGRAM allocate_shared

  USE mpi
  USE iso_c_binding
  IMPLICIT NONE

  INTEGER :: rank, nproc, ierr, window
  INTEGER :: comm_local, rank_local, nproc_local
  INTEGER :: disp
  INTEGER(KIND = MPI_ADDRESS_KIND) :: nitems
  TYPE(C_PTR) :: c_pointer
  INTEGER, DIMENSION(:), POINTER :: f_pointer
  

  CALL MPI_Init(ierr)

  CALL MPI_Comm_size(MPI_COMM_WORLD, nproc, ierr)
  CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

  CALL MPI_Comm_split_type(MPI_COMM_WORLD, MPI_COMM_TYPE_SHARED, 0, &
      MPI_INFO_NULL, comm_local, ierr)

  CALL MPI_Comm_size(comm_local, nproc_local, ierr)
  CALL MPI_Comm_rank(comm_local, rank_local, ierr)

  IF (rank == 0) THEN
    PRINT *,'Total ranks = ', nproc
    PRINT *,'Node ranks = ', nproc_local
  END IF

  IF (rank_local == 0) THEN
    nitems = nproc_local * 4
    CALL MPI_Win_allocate_shared(nitems, 4, MPI_INFO_NULL, comm_local, &
        c_pointer, window, ierr)
  ELSE
    !Null size here because don't want another window
    !Could use MPI_Win_create_dynamic, but that's more work
    nitems = 0
    CALL MPI_Win_allocate_shared(nitems, 4, MPI_INFO_NULL, comm_local, &
        c_pointer, window, ierr)
    !Use MPI_Win_shared_query to get a reference to the window allocated on
    !Processor zero
    CALL MPI_Win_shared_query(window, 0, nitems, disp, c_pointer, ierr)
  END IF

  CALL MPI_Win_fence(0, window, ierr)

  CALL C_F_POINTER(c_pointer, f_pointer, shape=(/nproc_local/))

  !Plus 1 because fortran array 1 based, but rank 0 based
  f_pointer(rank_local+1) = rank_local

  CALL MPI_Win_fence(0, window, ierr)

  IF (rank_local == 0) PRINT *,'On processor 0 of this local set, accumulated &
      & values are ', f_pointer

  CALL MPI_Finalize(ierr)

END PROGRAM allocate_shared
