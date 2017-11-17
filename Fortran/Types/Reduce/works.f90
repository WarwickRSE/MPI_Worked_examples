MODULE subs

  USE iso_c_binding
  USE mpi
  IMPLICIT NONE

  INTEGER, PARAMETER :: nitems = 100

  CONTAINS

  !Simple adder that works for this specific case
  SUBROUTINE adder(newvals, accumvals, nels, datatype)

    INTEGER, DIMENSION(nitems * nels), INTENT(IN) :: newvals
    INTEGER, DIMENSION(nitems * nels), INTENT(INOUT) :: accumvals
    INTEGER, INTENT(IN) :: nels
    INTEGER, INTENT(IN) :: datatype

    accumvals = accumvals + newvals

  END SUBROUTINE adder


  !More complex adder
  SUBROUTINE adder_f2008(newvals, accumvals, nels, datatype)

    TYPE(C_PTR), VALUE :: newvals, accumvals
    INTEGER, INTENT(IN) :: nels
    INTEGER, INTENT(IN) :: datatype
    INTEGER, POINTER, DIMENSION(:) :: nv2, av2
    INTEGER(KIND=MPI_ADDRESS_KIND) :: lb, extents, els, start
    INTEGER :: ierr

    !Find the extents of the datatype
    CALL MPI_Type_get_extent(datatype, lb, extents, ierr)
    !Convert it into numbers of integers (F2008 C_SIZEOF function)
    start = lb / C_SIZEOF(nels)
    els = extents / C_SIZEOF(nels)

    !Behind the scenes newvals and accumvals are C void* pointers
    !Convert them to Fortran Integer pointers (F2003 C_F_POINTER function)
    CALL C_F_POINTER(newvals, nv2, SHAPE=[els])
    CALL C_F_POINTER(accumvals, av2, SHAPE=[els])

    !Add together the elements
    av2 = av2 + nv2

  END SUBROUTINE adder_f2008

END MODULE subs

PROGRAM type

  USE mpi
  USE subs
  IMPLICIT NONE

  INTEGER, PARAMETER :: tag = 100

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

  !Create the custom addition operator
  CALL MPI_Op_create(adder_f2008, .TRUE., custom_sum, ierr)

  CALL MPI_Reduce(values, values_recv, 1, contig_type, custom_sum, 0, &
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
