      PROGRAM MAIN

C*********************************************************************72
C  Advent of Code 2021
C  --- Day 16: Packet Decoder ---
C
C  data.txt - 666 bytes of input * 2 hex digits = 1332 chars
C  example.txt - 13 bytes of input * 2 hex digits = 26 chars

      IMPLICIT NONE

      CHARACTER INF_N*128 ! Input file name
      INTEGER INF_H ! Handle for the input file
      INTEGER BITS(8192)
      INTEGER I, BYTE, NBITS, NPKT, MAXPKT, PART_1, IARGC
      INTEGER VERID(1024), TYPEID(1024), PARENT(1024)
      INTEGER*8 VALUE(1024)
      PARAMETER (INF_H = 10, MAXPKT = 1024)

C  Check if the input file name is available - if not, then exit
      I = IARGC()
      IF (I .LT. 1) THEN
          WRITE(*, '(A)') 'ERROR: An input file is required!'
          GOTO 9999
      END IF

C  Read single hex number line into the BITS array
      CALL GETARG(1, INF_N)
      OPEN(INF_H, FILE=INF_N, STATUS='OLD')

      DO NBITS = 0, 8192, 8 ! Until end-of-line
          READ(INF_H, '(Z2 $)', IOSTAT=I) BYTE
          IF (I .NE. 0) THEN ! End of string
              GO TO 10
          END IF
          DO I = 1, 8 ! Reverse to get MSB first
              BITS(NBITS + I) = IBITS(BYTE, 8 - I, 1)
          END DO
      END DO
   10 CLOSE(INF_H)

      IF (I .NE. 0) THEN ! Decrement NBITS if there was an error
          NBITS = NBITS - 8
      END IF

C  Part 1 solution
      NPKT = 0
      CALL PARSE(0, BITS, I, NBITS, NPKT, VERID, TYPEID, PARENT, VALUE)
      PART_1 = 0
      DO I = 1, NPKT
          PART_1 = PART_1 + VERID(I)
      END DO
      WRITE(*, '(A, I12.1)') 'Part 1: ', PART_1

C  Part 2 solution
      DO I = NPKT, 2, -1
          CALL EVAL(I, NPKT, TYPEID, PARENT, VALUE)
      END DO
      WRITE(*, '(A, I12.1)') 'Part 2: ', VALUE(1)

 9999 STOP
      END

      RECURSIVE SUBROUTINE PARSE(PID, BITS, FIN, NBITS,
     2                           NPKT, VERID, TYPEID, PARENT, VALUE)
          IMPLICIT NONE
          INTEGER BITS(NBITS), NBITS, PID, FIN, NPKT, BITNUM
          INTEGER VERID(1024), TYPEID(1024), PARENT(1024)
          INTEGER*8 VALUE(1024), LIT
          INTEGER I, J, K, L, ID

          NPKT = NPKT + 1
          ID = NPKT
          VERID(ID) = BITNUM(BITS(1:3), 3)
          TYPEID(ID) = BITNUM(BITS(4:6), 3)
          PARENT(ID) = PID
          FIN = FIN + 6

          IF (TYPEID(ID) .EQ. 4) THEN ! Literal type
              LIT = 0
              DO I = 7, NBITS, 5
                  J = BITNUM(BITS(I:I), 1)
                  LIT = (LIT * 16) + BITNUM(BITS(I + 1: I + 4), 4)
                  FIN = FIN + 5
                  IF (J .EQ. 0) THEN
                      VALUE(ID) = LIT
                      GO TO 10
                  END IF
              END DO
          ELSE ! Operator type
              FIN = FIN + 1
              VALUE(ID) = -1
              IF (BITNUM(BITS(7:7), 1) .EQ. 0) THEN ! Length in bits
                  I = BITNUM(BITS(8:22), 15) 
                  FIN = FIN + 15
                  K = 0
                  DO WHILE (K .LT. I)
                      J = FIN
                      CALL PARSE(ID, BITS(23 + K:), FIN, I,
     2                           NPKT, VERID, TYPEID, PARENT, VALUE)
                      K = K + (FIN - J)
                  END DO
              ELSE ! Subpacket count
                  I = BITNUM(BITS(8:18), 11)
                  FIN = FIN + 11
                  L = 0
                  DO K = 1, I
                      J = FIN
                      CALL PARSE(ID, BITS(19 + L:), FIN, NBITS,
     2                           NPKT, VERID, TYPEID, PARENT, VALUE)
                      L = L + (FIN - J)
                  END DO
              END IF
          END IF
   10     RETURN
      END

      SUBROUTINE EVAL(ID, NPKT, TYPEID, PID, VALUE)
          IMPLICIT NONE
          INTEGER ID, NPKT, TYPEID(NPKT), PID(NPKT)
          INTEGER*8 VALUE(NPKT), MIN64, MAX64
          INTEGER PARENT

          PARENT = PID(ID)
          IF (VALUE(PARENT) .EQ. -1) THEN
              VALUE(PARENT) = VALUE(ID)
          ELSE IF (TYPEID(PARENT) .EQ. 0) THEN ! Sum packet
              VALUE(PARENT) = VALUE(PARENT) + VALUE(ID)
          ELSE IF (TYPEID(PARENT) .EQ. 1) THEN ! Product
              VALUE(PARENT) = VALUE(PARENT) * VALUE(ID)
          ELSE IF (TYPEID(PARENT) .EQ. 2) THEN ! Minimum
              VALUE(PARENT) = MIN64(VALUE(PARENT), VALUE(ID))
          ELSE IF (TYPEID(PARENT) .EQ. 3) THEN ! Maximum
              VALUE(PARENT) = MAX64(VALUE(PARENT), VALUE(ID))
          ELSE IF (TYPEID(PARENT) .EQ. 5) THEN ! Greater than
              IF (VALUE(ID) .GT. VALUE(PARENT)) THEN
                  VALUE(PARENT) = 1
              ELSE
                  VALUE(PARENT) = 0
              END IF
          ELSE IF (TYPEID(PARENT) .EQ. 6) THEN ! Less than
              IF (VALUE(ID) .LT. VALUE(PARENT)) THEN
                  VALUE(PARENT) = 1
              ELSE
                  VALUE(PARENT) = 0
              END IF
          ELSE IF (TYPEID(PARENT) .EQ. 7) THEN ! Equal to
              IF (VALUE(ID) .EQ. VALUE(PARENT)) THEN
                  VALUE(PARENT) = 1
              ELSE
                  VALUE(PARENT) = 0
              END IF
          END IF
          RETURN
      END

      INTEGER*8 FUNCTION MIN64(A, B)
          IMPLICIT NONE
          INTEGER*8 A, B

          IF (A .LT. B) THEN
              MIN64 = A
          ELSE
              MIN64 = B
          END IF
          RETURN
      END

      INTEGER*8 FUNCTION MAX64(A, B)
          IMPLICIT NONE
          INTEGER*8 A, B

          IF (A .GT. B) THEN
              MAX64 = A
          ELSE
              MAX64 = B
          END IF
          RETURN
      END

      INTEGER FUNCTION BITNUM(BITS, NBITS)
          IMPLICIT NONE
          INTEGER I, NBITS, BITS(NBITS)

          BITNUM = 0
          DO I = 1, NBITS
               BITNUM = (BITNUM * 2) + BITS(I)
          END DO
          RETURN
      END
