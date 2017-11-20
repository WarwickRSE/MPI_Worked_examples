PROGRAM passive

  USE mpi
  IMPLICIT NONE

  INTEGER :: rank, recv_rank, nproc
  INTEGER :: ierr, window, intsize, icycle
  INTEGER(KIND=MPI_ADDRESS_KIND) :: size_of_window, offset
  INTEGER, DIMENSION(:), ALLOCATABLE :: ranks

  CALL MPI_Init(ierr)

  CALL MPI_Comm_size(MPI_COMM_WORLD, nproc, ierr)
  CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

  !In C would just use sizeof()
  !In Fortran2008, could use C_SIZEOF
  !NOTE! This routine returns the size of AN ELEMENT
  !Not the size of an array if you give it an array
  CALL MPI_Sizeof(recv_rank, intsize, ierr)
  !Just using a single int here
  size_of_window = intsize


  !Create the window. This is a piece of memory that's available for remote
  !access. In this case, a single 4 byte integer
  CALL MPI_Win_create(recv_rank, size_of_window, intsize, MPI_INFO_NULL, &
      MPI_COMM_WORLD, window, ierr)

  recv_rank = -1

  !Put the result into the first (zeroth) slot
  offset = 0
  IF (rank == 0) THEN
    DO icycle = 0, nproc - 1
      !Lock the window on the remote rank
      CALL MPI_Win_lock(MPI_LOCK_EXCLUSIVE, icycle, 0, window, ierr)
      !Actual call to put the data in the remote processor
      CALL MPI_Put(icycle, 1, MPI_INTEGER, icycle, offset, 1, MPI_INTEGER, &
          window, ierr)
      !Call to flush to complete the put now, because the code can't do
      !anything until the put is complete. This may not in general be true
      !Take care though, once you have called unlock, you can't flush
      CALL MPI_Win_flush(icycle, window, ierr)
      !Unlock the window on the remote rank
      CALL MPI_Win_unlock(icycle, window, ierr)
    END DO
  END IF

  CALL MPI_Win_free(window, ierr)

  PRINT *,"Rank ", rank, " got message from rank 0 of ", recv_rank

  CALL MPI_Finalize(ierr)

END PROGRAM passive
