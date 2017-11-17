PROGRAM simple_write

  USE mpi
  IMPLICIT NONE

  INTEGER :: rank, nproc, ierr
  INTEGER :: file_handle
  CHARACTER(len=50) :: outstr
  INTEGER, DIMENSION(:), ALLOCATABLE :: offsets
  INTEGER(KIND=MPI_OFFSET_KIND) :: my_offset

  CALL MPI_Init(ierr)

  CALL MPI_Comm_size(MPI_COMM_WORLD, nproc, ierr)
  CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

  ALLOCATE(offsets(1:nproc))

  !Delete the existing file
  CALL MPI_File_delete('out.txt', MPI_INFO_NULL, ierr)

  !Open the file for writing
  CALL MPI_File_open(MPI_COMM_WORLD, 'out.txt', &
      MPI_MODE_WRONLY + MPI_MODE_CREATE, MPI_INFO_NULL, file_handle, ierr)

  !MPI_IO is a binary output format. Have to manually add new line characters
  WRITE(outstr,'(A,I3,A)') "Hello from processor ", rank, NEW_LINE(' ')

  !Get the lengths of all other writes
  CALL MPI_Allgather(LEN(TRIM(outstr)), 1, MPI_INTEGER, offsets, 1, &
      MPI_INTEGER, MPI_COMM_WORLD, ierr)

  !Calculate this processors offset in the file
  my_offset = SUM(offsets(1:rank-1))

  !Move the file pointer to that place
  CALL MPI_File_seek(file_handle, my_offset, MPI_SEEK_SET, ierr)

  !Write using the individual file pointer
  CALL MPI_File_write(file_handle, TRIM(outstr), LEN(TRIM(outstr)), &
      MPI_CHARACTER, MPI_STATUS_IGNORE, ierr)

  !Close the file
  CALL MPI_File_close(file_handle, ierr)


  CALL MPI_Finalize(ierr)

END PROGRAM simple_write
