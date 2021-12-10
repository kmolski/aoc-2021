      PROGRAM MAIN

C*********************************************************************72
C  Advent of Code 2021
C  --- Day 10: Syntax Scoring ---
C
C  data.txt - 90 lines of input, max length is 109
C  example.txt - 10 lines of input, max length is 24

      IMPLICIT NONE

      CHARACTER*128 INF_N! Input file name
      INTEGER INF_H ! Handle for the input file
      CHARACTER*128 LINES(128)
      CHARACTER CSTACK(128)
      INTEGER*8 SCORES(128)
      INTEGER I, J, N, L_LEN, CS_TOP
      INTEGER*8 M, PART_1, PART_2
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
          READ(INF_H, *, IOSTAT=I) LINES(N)
      END DO
      IF (I .NE. 0) THEN ! Decrement line count if there was an error
          N = N - 1
      END IF
      CLOSE(INF_H)

C  Part 1 solution & 2 scores
      PART_1 = 0
      M = 0
      DO I = 1, N
          CS_TOP = 0
          L_LEN = LEN(LINES(I))
          DO J = 1, L_LEN
              IF (LINES(I)(J:J) .EQ. '(' .OR. 
     2              LINES(I)(J:J) .EQ. '[' .OR.
     3              LINES(I)(J:J) .EQ. '{' .OR. 
     4              LINES(I)(J:J) .EQ. '<') THEN ! Push char to stack
                  CS_TOP = CS_TOP + 1
                  CSTACK(CS_TOP) = LINES(I)(J:J)
              ELSE IF (LINES(I)(J:J) .EQ. ')') THEN
                  IF (CSTACK(CS_TOP) .EQ. '(') THEN
                      CS_TOP = CS_TOP - 1
                  ELSE
                      PART_1 = PART_1 + 3
                      GO TO 10
                  END IF
              ELSE IF (LINES(I)(J:J) .EQ. ']') THEN
                  IF (CSTACK(CS_TOP) .EQ. '[') THEN
                      CS_TOP = CS_TOP - 1
                  ELSE
                      PART_1 = PART_1 + 57
                      GO TO 10
                  END IF
              ELSE IF (LINES(I)(J:J) .EQ. '}') THEN
                  IF (CSTACK(CS_TOP) .EQ. '{') THEN
                      CS_TOP = CS_TOP - 1
                  ELSE
                      PART_1 = PART_1 + 1197
                      GO TO 10
                  END IF
              ELSE IF (LINES(I)(J:J) .EQ. '>') THEN
                  IF (CSTACK(CS_TOP) .EQ. '<') THEN
                      CS_TOP = CS_TOP - 1
                  ELSE
                      PART_1 = PART_1 + 25137
                      GO TO 10
                  END IF
              END IF
          END DO

          PART_2 = 0
          DO J = 0, CS_TOP - 1
              IF (CSTACK(CS_TOP - J) .EQ. '(') THEN
                  PART_2 = (PART_2 * 5) + 1
              ELSE IF (CSTACK(CS_TOP - J) .EQ. '[') THEN
                  PART_2 = (PART_2 * 5) + 2
              ELSE IF (CSTACK(CS_TOP - J) .EQ. '{') THEN
                  PART_2 = (PART_2 * 5) + 3
              ELSE IF (CSTACK(CS_TOP - J) .EQ. '<') THEN
                  PART_2 = (PART_2 * 5) + 4
              END IF
          END DO
          M = M + 1
          SCORES(M) = PART_2
   10 END DO
      WRITE(*, '(A, I10.1)') 'Part 1: ', PART_1

C  Part 2 solution
      CALL ISORT(SCORES, M)
      IF (MOD(M, 2) .EQ. 0) THEN ! Calculate mean value
          PART_2 = (SCORES(M / 2) + SCORES(M / 2 + 1)) / 2
      ELSE
          PART_2 = SCORES((M + 1) / 2) 
      END IF

      WRITE(*, '(A, I10.1)') 'Part 2: ', PART_2

 9999 STOP
      END

      SUBROUTINE ISORT(ARR, A_SIZE)

          IMPLICIT NONE

          INTEGER*8 ARR(A_SIZE), A_SIZE, I, J, T

          DO I = 2, A_SIZE
              T = ARR(I)
              J = I
   20         J = J - 1
              IF (J .EQ. 0) GO TO 30
              IF (ARR(J) .LE. T) GO TO 30
              ARR(J + 1) = ARR(J)
              GO TO 20
   30         ARR(J + 1) = T
          END DO
          RETURN
      END
