       IDENTIFICATION DIVISION.
       PROGRAM-ID. CTRL-BREAK.
       AUTHOR. JOHN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 TABLE-DATA.
          05 TXN-RECORD OCCURS 200 TIMES.
             10 BRANCH-ID   PIC X(10).
             10 TXN-AMT     PIC 9(9).

       01 TXN-COUNT          PIC 9(3) VALUE 0.
       01 I                  PIC 9(3) VALUE 0.

       01 PREV-BRANCH        PIC X(10) VALUE SPACES.
       01 GROUP-TOTAL        PIC 9(12) VALUE 0.
       01 GRAND-TOTAL        PIC 9(12) VALUE 0.
       01 PRINT-VAL          PIC Z(11)9.

       PROCEDURE DIVISION.

           DISPLAY "ENTER NUMBER OF TRANSACTIONS (SORTED BY BRANCH):"
           ACCEPT TXN-COUNT

           IF TXN-COUNT > 200
               DISPLAY "TOO MANY TRANSACTIONS"
               GOBACK
           END-IF

           PERFORM VARYING I FROM 1 BY 1
               UNTIL I > TXN-COUNT

               DISPLAY "ENTER BRANCH ID:"
               ACCEPT BRANCH-ID(I)
               DISPLAY "ENTER AMOUNT:"
               ACCEPT TXN-AMT(I)

           END-PERFORM

           DISPLAY " "
           DISPLAY "--- BRANCH WISE REPORT ---"

           MOVE BRANCH-ID(1) TO PREV-BRANCH

           PERFORM VARYING I FROM 1 BY 1
               UNTIL I > TXN-COUNT

               IF BRANCH-ID(I) NOT = PREV-BRANCH
                   MOVE GROUP-TOTAL TO PRINT-VAL
                   DISPLAY "BRANCH " FUNCTION TRIM(PREV-BRANCH)
                           " TOTAL: " FUNCTION TRIM(PRINT-VAL)
                   MOVE 0 TO GROUP-TOTAL
                   MOVE BRANCH-ID(I) TO PREV-BRANCH
               END-IF

               ADD TXN-AMT(I) TO GROUP-TOTAL
               ADD TXN-AMT(I) TO GRAND-TOTAL

           END-PERFORM

           MOVE GROUP-TOTAL TO PRINT-VAL
           DISPLAY "BRANCH " FUNCTION TRIM(PREV-BRANCH)
                   " TOTAL: " FUNCTION TRIM(PRINT-VAL)

           MOVE GRAND-TOTAL TO PRINT-VAL
           DISPLAY "GRAND TOTAL: " FUNCTION TRIM(PRINT-VAL)

           STOP RUN.
