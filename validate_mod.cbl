       IDENTIFICATION DIVISION.
       PROGRAM-ID. VALIDATE-MOD.
       AUTHOR. JOHN.

       DATA DIVISION.
       LINKAGE SECTION.

       01 LK-ACC-ID        PIC X(10).
       01 LK-AMOUNT        PIC 9(9).
       01 LK-RESULT        PIC X(1).

       PROCEDURE DIVISION USING LK-ACC-ID LK-AMOUNT LK-RESULT.

           MOVE 'Y' TO LK-RESULT

           IF LK-ACC-ID = SPACES
               MOVE 'N' TO LK-RESULT
           END-IF

           IF LK-AMOUNT = 0
               MOVE 'N' TO LK-RESULT
           END-IF

           IF LK-AMOUNT > 500000
               MOVE 'N' TO LK-RESULT
           END-IF

           GOBACK.
