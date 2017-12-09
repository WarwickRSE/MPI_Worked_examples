MODULE display

  USE mpi
  IMPLICIT NONE

  !Information about the global array
  INTEGER, PARAMETER :: nx = 20, ny = 20
  INTEGER, PARAMETER :: tag = 100
  REAL, DIMENSION(0:nx+1, 0:ny+1) :: values

  !Information about the arrays on this processor
  REAL, DIMENSION(:, :), ALLOCATABLE :: values_local, temp_local
  INTEGER :: nx_local, ny_local
  INTEGER :: x_cell_min_local, x_cell_max_local
  INTEGER :: y_cell_min_local, y_cell_max_local

  !Pure MPI information
  INTEGER :: nproc, rank, cart_comm
  INTEGER, DIMENSION(2) :: nprocs, coordinates
  INTEGER :: x_min_rank, x_max_rank, y_min_rank, y_max_rank
  INTEGER :: type_x_dir
  INTEGER :: type_y_dir
  INTEGER :: window

  CONTAINS

  !This routine displays the output. It isn't a part of this course. Annotation
  !is only for general interest. Note that normally you'd use a library like
  !ncurses (https://en.wikipedia.org/wiki/Ncurses) to do the terminal trickery
  !that I'm doing here. This is just for maximum compatability
  SUBROUTINE display_result(array)

    REAL, DIMENSION(0:,0:), INTENT(IN) :: array
    CHARACTER(LEN=3) :: clrstr = '[2J'
    CHARACTER(LEN=5), DIMENSION(3) :: colours = (/'[34m', '[39m', '[31m'/)
    CHARACTER(LEN=1), DIMENSION(3) :: vals = (/'*', '.', '+'/)
    INTEGER, DIMENSION(2) :: sizes
    INTEGER :: ix, iy, index

    !Special string to clear screen using VT100 terminal codes, see
    !(http://wiki.bash-hackers.org/scripting/terminalcodes)
    WRITE(*,'(A)') CHAR(27) // TRIM(clrstr)

    sizes = SHAPE(array)
    !Outer array is flipped because screen indexes work from top left
    !Everything else works from bottom left
    DO iy = sizes(2) - 2, 1, -1
      DO ix = 1, sizes(1) - 2
        !Get the symbol and colour for the value in this cell
        !Colours are more VT100 terminal codes
        index = NINT(array(ix,iy)/10.0 * REAL(SIZE(vals)-1))+1

        !Write out the special VT100 colour control code and symbol
        WRITE(*,'(A,A)', ADVANCE='NO') ACHAR(27) // TRIM(colours(index)) , &
            vals(index) // " "
        !Version without colour code
!        WRITE(*,'(A)', ADVANCE='NO') vals(index) // " "
      END DO
      WRITE(*,*) ""
    END DO
    !Set terminal colours back to default
    WRITE(*,'(A)', ADVANCE='NO') ACHAR(27) // TRIM('[39m')

  END SUBROUTINE display_result

  !This routine gathers all of the data onto processor zero. It's ugly and
  !isn't a part of this course. THIS IS NOT A GOOD SOLUTION TO THIS PROBLEM!
  SUBROUTINE gather_to_zero

    INTEGER :: ierr
    REAL, DIMENSION(0:nx+1, 0:ny+1) :: red

    values = 0.0
    red = 0.0

    !Copy values for local part of array into copy of global array
    values(x_cell_min_local:x_cell_max_local, &
        y_cell_min_local:y_cell_max_local) = &
        values_local(1:nx_local, 1:ny_local)

    !Use MPI_Reduce to get globally correct global array
    CALL MPI_Reduce(values, red, (nx+2) * (ny+2), MPI_REAL, &
        MPI_SUM, 0, cart_comm, ierr)
    values = red

  END SUBROUTINE gather_to_zero

  !Wrapper around MPI_Type_create_subarray and MPI_Type_commit
  SUBROUTINE create_single_type(sizes, subsizes, starts, newtype)

    INTEGER, DIMENSION(2), INTENT(IN) :: sizes, subsizes, starts
    INTEGER, INTENT(INOUT) :: newtype
    INTEGER :: ierr

    CALL MPI_Type_create_subarray(2, sizes, subsizes, starts, &
        MPI_ORDER_FORTRAN, MPI_REAL, newtype, ierr)
    CALL MPI_Type_commit(newtype, ierr)

  END SUBROUTINE create_single_type

  !Create the types used in sending and receiving data
  SUBROUTINE create_types

    INTEGER, DIMENSION(2) :: sizes, subsizes, starts

    !Always the same sizes. This is local to each CPU
    !So use nx_local, not nx
    sizes = (/nx_local+2, ny_local+2/)
    !Same subsizes for all sends and receives in x direction
    subsizes = (/1, ny_local/)
    !All operations in x direction
    starts = (/0, 1/)
    CALL create_single_type(sizes, subsizes, starts, type_x_dir)

    !Same subsizes for all sends and receives in y direction
    subsizes = (/nx_local, 1/)
    starts = (/1, 0/)
    CALL create_single_type(sizes, subsizes, starts, type_y_dir)

  END SUBROUTINE create_types

  !Routine the applies interprocessor boundary conditions
  !Leaves true boundaries alone because they have MPI_PROC_NULL
  !for x_min_rank, x_max_rank, y_min_rank, y_max_rank (depending on edge)
  !This makes that part of MPI_Sendrecv a null operation
  SUBROUTINE bcs()

    INTEGER :: ierr
    INTEGER(KIND=MPI_ADDRESS_KIND) :: offset

    !Note that all offsets are in multiples of size of real (4 bytes)
    !Because I told MPI that the displacement was the size of a real
    !in MPI_Win_create

    CALL MPI_Win_fence(MPI_MODE_NOSTORE, window, ierr)

    offset = INT(1, MPI_ADDRESS_KIND) !Getting from (1, 0)
    !Inserting into (nx_local+1, 0)
    CALL MPI_Get(values_local(nx_local+1, 0), 1, type_x_dir, x_max_rank, &
        offset, 1, type_x_dir, window, ierr)

    offset = INT(nx_local, MPI_ADDRESS_KIND) !Getting from (nx_local, 0)
    !Inserting into (0,0)
    CALL MPI_Get(values_local(0, 0), 1, type_x_dir, x_min_rank, &
        offset, 1, type_x_dir, window, ierr)

    offset = INT(1, MPI_ADDRESS_KIND) * &
             INT(nx_local+2, MPI_ADDRESS_KIND) !Getting from (0, 1)
             !Multiply by length in x to get offset
    !Inserting into (0, ny_local + 1)
    CALL MPI_Get(values_local(0, ny_local + 1), 1, type_y_dir, y_max_rank, &
        offset, 1, type_y_dir, window, ierr)

    !Now y direction
    offset = INT(ny_local, MPI_ADDRESS_KIND) * & 
             INT(nx_local+2, MPI_ADDRESS_KIND) !Getting from (0, ny_local)
             !Multiply by length in x to get offset
    !Inserting into (0, 0)
    CALL MPI_Get(values_local(0, 0), 1, type_y_dir, y_min_rank, &
        offset, 1, type_y_dir, window, ierr)

    CALL MPI_Win_fence(MPI_MODE_NOSTORE, window, ierr)


  END SUBROUTINE bcs

  !Routine to set up the MPI system
  SUBROUTINE setup_mpi

    LOGICAL, DIMENSION(2) :: periods
    INTEGER :: ierr, ix, iy, size_real
    INTEGER(KIND=MPI_ADDRESS_KIND) :: size

    periods = .FALSE.

    CALL MPI_Init(ierr)

    CALL MPI_Comm_size(MPI_COMM_WORLD, nproc, ierr)
    CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

    !In general you'd want to deal with decompositions that don't match to your
    !number of cells. Here, we restrict the code to a number of processors
    !that smoothly subdivides the number of cells
    IF (nproc /= 1 .AND. nproc /=2 .AND. nproc /=4 .AND. nproc /= 8 .AND. &
        nproc /= 16) THEN

      IF (rank == 0) THEN
        PRINT *,'Demo code only works on 1, 2, 4, 8 or 16 processors'
        CALL MPI_Abort(MPI_COMM_WORLD, 2, ierr)
      END IF
    END IF

    CALL MPI_Barrier(MPI_COMM_WORLD, ierr)
    CALL MPI_Dims_create(nproc, 2, nprocs, ierr)

    IF (rank == 0) THEN
      PRINT *,'Processor decomposition is ', nprocs
    ENDIF
    CALL MPI_Barrier(MPI_COMM_WORLD, ierr)

    !Divide the global size (nx x ny) per processor
    nx_local = nx / nprocs(1)
    ny_local = ny / nprocs(2)

    CALL MPI_Cart_create(MPI_COMM_WORLD, 2, nprocs, periods, .TRUE., &
        cart_comm, ierr)

    !Rank in new communicator might be different
    CALL MPI_Comm_rank(cart_comm, rank, ierr)

    !Get the rank of the neighbouring processors in Cartesian communicator
    CALL MPI_Cart_shift(cart_comm, 0, 1, x_min_rank, x_max_rank, ierr)
    CALL MPI_Cart_shift(cart_comm, 1, 1, y_min_rank, y_max_rank, ierr)

    !Get my coordinates in Cartesian communicator
    CALL MPI_Cart_coords(cart_comm, rank, 2, coordinates, ierr)

    !Calculate what fraction of the global array this processor has
    x_cell_min_local = nx_local * coordinates(1) + 1
    x_cell_max_local = nx_local * (coordinates(1) + 1)
    y_cell_min_local = ny_local * coordinates(2) + 1
    y_cell_max_local = ny_local * (coordinates(2) + 1)

    CALL create_types

    ALLOCATE(values_local(0:nx_local+1, 0:ny_local+1))
    ALLOCATE(temp_local(0:nx_local+1, 0:ny_local+1))

    !In C would just use sizeof()
    !In Fortran2008, could use C_SIZEOF
    !NOTE! This routine returns the size of AN ELEMENT
    !Not the size of an array if you give it an array
    CALL MPI_Sizeof(values_local, size_real, ierr)
    !Window is whole of values_local
    size = INT(nx_local+2, MPI_ADDRESS_KIND) * &
        INT(ny_local+2, MPI_ADDRESS_KIND) * &
        INT(size_real, MPI_ADDRESS_KIND)

    CALL MPI_Win_create(values_local, size, size_real, MPI_INFO_NULL, &
        MPI_COMM_WORLD, window, ierr)

  END SUBROUTINE setup_mpi

END MODULE display

PROGRAM serial

  USE display

  IMPLICIT NONE

  INTEGER :: ix, iy, icycle, ierr

  CALL setup_mpi

  !This applies global boundaries to all edges
  !They will be overwritten using MPI when needed
  values_local = 0.0
  values_local(0,:) = 1.0
  values_local(nx_local+1,:) = 10.0
  values_local(:,0) = 1.0
  values_local(:, ny_local+1) = 10.0

  values = 5.5

  IF (rank == 0) THEN
    PRINT *,'Please press a key to start iterating'
    READ(*,*)
  END IF
  CALL MPI_Barrier(MPI_COMM_WORLD, ierr)

  !Gather everything on rank 0 for display
  CALL gather_to_zero
  IF (rank == 0) THEN
    CALL display_result(values)
    PRINT *,'Please press a key to advance'
    READ(*,*)
  END IF
  CALL MPI_Barrier(MPI_COMM_WORLD, ierr)
  !Now iterate
  DO icycle = 1, 500

    !Operate on the local variables just the same as the global ones in serial
    DO iy = 1, ny_local
      DO ix = 1, nx_local
        temp_local(ix,iy) = 0.25 * (values_local(ix+1,iy) + &
             values_local(ix,iy+1) + values_local(ix-1,iy) + &
             values_local(ix,iy-1))
      END DO
    END DO
    values_local(1:nx_local,1:ny_local) = temp_local(1:nx_local,1:ny_local)

    !Now apply the interprocessor boundary conditions
    CALL bcs()

    !And output by gathering on rank 0
    IF (MOD(icycle,50) == 0) THEN
      CALL gather_to_zero
      IF (rank == 0) THEN
        CALL display_result(values)
        PRINT *,'Please press a key to advance'
        READ (*,*)
      END IF
    END IF
  END DO

  CALL MPI_Finalize(ierr)

END PROGRAM serial
