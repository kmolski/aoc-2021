      PROGRAM MAIN

C*********************************************************************72
C  Advent of Code 2021
C  --- Day 5: Hydrothermal Venture ---
C
C  data.txt - 500 lines of input, max coordinate is 989
C  example.txt - 10 lines of input, max coordinate is 9

      IMPLICIT NONE

      CHARACTER FILENAME*128
      INTEGER INF_H ! Handle for the input file
      INTEGER COORDS(4, 500), OFLOOR(1000, 1000)
      INTEGER I, J, N, FROM_X, FROM_Y, TO_X, TO_Y
      PARAMETER (INF_H = 10)

C  Check if the input file name is available - if not, then exit
      I = IARGC()
      IF (I.LT.1) THEN
          WRITE(*, '(A)') 'ERROR: An input file is required!'
          GOTO 9999
      END IF

C  Read input from data file, stop if end-of-file is reached
      CALL GETARG(1, FILENAME)
      OPEN(INF_H, FILE=FILENAME, STATUS='OLD')
      I = 0
      N = 0
      DO WHILE (I.EQ.0)
          N = N + 1
          READ(INF_H, *, IOSTAT=I) COORDS(1, N), COORDS(2, N),
     2                             COORDS(3, N), COORDS(4, N)
          IF (I.NE.0) THEN
              N = N - 1
          END IF
      END DO
      CLOSE(INF_H)



 9999 STOP
      END
