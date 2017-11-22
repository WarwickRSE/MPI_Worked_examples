MODULE display

  IMPLICIT NONE

  CONTAINS

  !This routine displays the output. It isn't a part of this course. Annotation
  !is only for general interest
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

END MODULE display

PROGRAM serial

  USE display

  IMPLICIT NONE

  INTEGER, PARAMETER :: nx = 20, ny = 20
  REAL, DIMENSION(0:nx+1, 0:ny+1) :: values, temp_values
  INTEGER :: ix, iy, icycle

  values = 5.5
  values(0,:) = 1.0
  values(nx+1,:) = 10.0
  values(:,0) = 1.0
  values(:, ny+1) = 10.0

  CALL display_result(values)
  PRINT *,'Please press a key to advance'
  READ(*,*)
  DO icycle = 1, 500
    DO iy = 1, ny
      DO ix = 1, nx
        temp_values(ix,iy) = 0.25 * (values(ix+1,iy) + &
             values(ix,iy+1) + values(ix-1,iy) + values(ix,iy-1))
      END DO
    END DO
    values(1:nx,1:ny) = temp_values(1:nx,1:ny)
    IF (MOD(icycle,50) == 0) THEN
      CALL display_result(values)
      PRINT *,'Please press a key to advance'
      READ (*,*)
    ENDIF
  END DO


END PROGRAM serial
