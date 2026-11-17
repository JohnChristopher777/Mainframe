       IDENTIFICATION DIVISION.
       PROGRAM-ID. MODULAR-APP.
       AUTHOR. JOHN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 TABLE-DATA.
          05 TXN-RECORD OCCURS 100 TIMES.
             10 TXN-ACC       PIC X(10).
             10 TXN-AMT       PIC 9(9).

       01 TXN-COUNT            PIC 9(3) VALUE 0.
       01 I                    PIC 9(3) VALUE 0.
       01 VALID-RESULT         PIC X(1) VALUE 'N'.
       01 ACCEPT-COUNT         PIC 9(3) VALUE 0.
       01 REJECT-COUNT         PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.

           DISPLAY "ENTER NUMBER OF TRANSACTIONS:"
           ACCEPT TXN-COUNT

           IF TXN-COUNT > 100
               DISPLAY "TOO MANY TRANSACTIONS"
               GOBACK
           END-IF

           PERFORM VARYING I FROM 1 BY 1
               UNTIL I > TXN-COUNT

               DISPLAY "ENTER ACCOUNT ID:"
               ACCEPT TXN-ACC(I)
               DISPLAY "ENTER AMOUNT:"
               ACCEPT TXN-AMT(I)

           END-PERFORM

           DISPLAY " "
           DISPLAY "--- MODULAR VALIDATION RESULTS ---"

           PERFORM VARYING I FROM 1 BY 1
               UNTIL I > TXN-COUNT

               MOVE 'N' TO VALID-RESULT

               CALL 'VALIDATE-MOD' USING TXN-ACC(I)
                                         TXN-AMT(I)
                                         VALID-RESULT

               IF VALID-RESULT = 'Y'
                   ADD 1 TO ACCEPT-COUNT
                   DISPLAY FUNCTION TRIM(TXN-ACC(I))
                           " ACCEPTED"
               ELSE
                   ADD 1 TO REJECT-COUNT
                   DISPLAY FUNCTION TRIM(TXN-ACC(I))
                           " REJECTED"
               END-IF

           END-PERFORM

           DISPLAY " "
           DISPLAY "ACCEPTED: " ACCEPT-COUNT
           DISPLAY "REJECTED: " REJECT-COUNT

           STOP RUN.
