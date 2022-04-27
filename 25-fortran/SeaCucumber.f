      PROGRAM MAIN

C*********************************************************************72
C  Advent of Code 2021
C  --- Day 25: Sea Cucumber ---
C
C  data.txt - 137x139 grid
C  example.txt - 9x10 grid

      IMPLICIT NONE

      CHARACTER INF_N*128 ! Input file name
      INTEGER INF_H ! Handle for the input file
      CHARACTER*255 GRID(255), EAST(255)
      LOGICAL MOVED
      INTEGER I, J, N, M, NEXT, PART_1, IARGC
      PARAMETER (INF_H = 10)

C  Check if the input file name is available - if not, then exit
      I = IARGC()
      IF (I .LT. 1) THEN
          WRITE(*, '(A)') 'ERROR: An input file is required!'
          GOTO 9999
      END IF

C  Read input from data file, stop if end-of-file is reached
      CALL GETARG(1, INF_N)
      OPEN(INF_H, FILE=INF_N, STATUS='OLD')
      I = 0
      N = 0
      DO WHILE (I .EQ. 0)
          N = N + 1
          READ(INF_H, *, IOSTAT=I) GRID(N)
      END DO
      IF (I .NE. 0) THEN ! Decrement line count if there was an error
          N = N - 1
      END IF
      M = INDEX(GRID(1), ' ') - 1
      CLOSE(INF_H)

C  Part 1 solution
      MOVED = .TRUE.
      PART_1 = 0
      DO WHILE (MOVED .EQV. .TRUE.)
          MOVED = .FALSE.
          DO I = 1, N
              DO J = 1, M
                  EAST(I)(J:J) = GRID(I)(J:J)
              END DO
          END DO
          DO I = 1, N ! East facing
              DO J = 1, M
                  IF (J .LT. M) THEN
                      NEXT = J + 1
                  ELSE
                      NEXT = 1
                  END IF
                  IF ((GRID(I)(J:J) .EQ. '>') .AND.
     2                (GRID(I)(NEXT:NEXT) .EQ. '.')) THEN
                      MOVED = .TRUE.
                      EAST(I)(J:J) = '.'
                      EAST(I)(NEXT:NEXT) = '>'
                  END IF
              END DO
          END DO
          DO I = 1, N
              DO J = 1, M
                  GRID(I)(J:J) = EAST(I)(J:J)
              END DO
          END DO
          DO I = 1, N ! South facing
              DO J = 1, M
                  IF (I .LT. N) THEN
                      NEXT = I + 1
                  ELSE
                      NEXT = 1
                  END IF
                  IF ((EAST(I)(J:J) .EQ. 'v') .AND.
     2                (EAST(NEXT)(J:J) .EQ. '.')) THEN
                      MOVED = .TRUE.
                      GRID(I)(J:J) = '.'
                      GRID(NEXT)(J:J) = 'v'
                  END IF
              END DO
          END DO
          PART_1 = PART_1 + 1
      END DO
      WRITE(*, '(A, I12.1)') 'Part 1: ', PART_1

 9999 STOP
      END
