      PROGRAM MAIN

C*********************************************************************72
C  Advent of Code 2021
C  --- Day 5: Hydrothermal Venture ---
C
C  data.txt - 500 lines of input, max coordinate is 989
C  example.txt - 10 lines of input, max coordinate is 9

      IMPLICIT NONE

      CHARACTER INF_N*128 ! Input file name
      INTEGER INF_H ! Handle for the input file
      INTEGER COORDS(4, 500), OFLOOR(0:999, 0:999)
      INTEGER I, J, A, N, FROM_X, FROM_Y, TO_X, TO_Y, DIFF_X, DIFF_Y
      INTEGER PART_1, PART_2
      PARAMETER (INF_H = 10)

C  Check if the input file name is available - if not, then exit
      I = IARGC()
      IF (I.LT.1) THEN
          WRITE(*, '(A)') 'ERROR: An input file is required!'
          GOTO 9999
      END IF

C  Read input from data file, stop if end-of-file is reached
      CALL GETARG(1, INF_N)
      OPEN(INF_H, FILE=INF_N, STATUS='OLD')
      I = 0
      N = 0
      DO WHILE (I.EQ.0)
          N = N + 1
          READ(INF_H, *, IOSTAT=I) COORDS(1, N), COORDS(2, N),
     2                             COORDS(3, N), COORDS(4, N)
      END DO
      IF (I.NE.0) THEN ! Decrement the line count if there was an error
          N = N - 1
      END IF
      CLOSE(INF_H)

C  Initialize OFLOOR array
      DO I = 0, 999
          DO J = 0, 999
              OFLOOR(J, I) = 0
          END DO
      END DO

C  Process horizontal & vertical line descriptors
      DO I = 1, N
          FROM_X = COORDS(1, I)
          FROM_Y = COORDS(2, I)
          TO_X = COORDS(3, I)
          TO_Y = COORDS(4, I)
          IF (FROM_X.EQ.TO_X) THEN
              DO J = FROM_Y, TO_Y ! Mark line from top to bottom
                  OFLOOR(FROM_X, J) = OFLOOR(FROM_X, J) + 1
              END DO
              DO J = TO_Y, FROM_Y ! Mark line from bottom to top
                  OFLOOR(FROM_X, J) = OFLOOR(FROM_X, J) + 1
              END DO
          END IF
          IF (FROM_Y.EQ.TO_Y) THEN
              DO J = FROM_X, TO_X ! Mark line from left to right
                  OFLOOR(J, FROM_Y) = OFLOOR(J, FROM_Y) + 1
              END DO
              DO J = TO_X, FROM_X ! Mark line from right to left
                  OFLOOR(J, FROM_Y) = OFLOOR(J, FROM_Y) + 1
              END DO
          END IF
      END DO

C  Part 1 solution
      PART_1 = 0
      DO I = 0, 999
          DO J = 0, 999
              IF (OFLOOR(J, I).GE.2) THEN
                  PART_1 = PART_1 + 1
              END IF
          END DO
      END DO
      WRITE(*, '(A, I8.1)') 'Part 1: ', PART_1

C  Process diagonal (Y = AX + B, where ABS(A) = 1) line descriptors
      DO I = 1, N
          FROM_X = COORDS(1, I)
          FROM_Y = COORDS(2, I)
          TO_X = COORDS(3, I)
          TO_Y = COORDS(4, I)
          DIFF_X = TO_X - FROM_X
          DIFF_Y = TO_Y - FROM_Y
          IF (ABS(DIFF_X).EQ.ABS(DIFF_Y)) THEN ! ABS(A) = 1
              IF (FROM_X.GT.TO_X) THEN ! Flip coords if FROM_X > TO_X
                  CALL ISWAP(FROM_X, TO_X)
                  CALL ISWAP(FROM_Y, TO_Y)
              END IF
              A = SIGN(1, DIFF_X * DIFF_Y)
              DO J = 0, ABS(DIFF_X)
                  OFLOOR(FROM_X+J, FROM_Y+A*J) =
     2                OFLOOR(FROM_X+J, FROM_Y+A*J) + 1
              END DO
          END IF
      END DO

C  Part 2 solution
      PART_2 = 0
      DO I = 0, 999
          DO J = 0, 999
              IF (OFLOOR(J, I).GE.2) THEN
                  PART_2 = PART_2 + 1
              END IF
          END DO
      END DO
      WRITE(*, '(A, I8.1)') 'Part 2: ', PART_2

 9999 STOP
      END

      SUBROUTINE ISWAP(A, B)
          INTEGER A, B
          INTEGER TEMP

          TEMP = A
          A = B
          B = TEMP
          RETURN
      END
