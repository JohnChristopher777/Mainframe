       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEDUP-ENGINE.
       AUTHOR. JOHN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 TABLE-DATA.
          05 TXN-RECORD OCCURS 200 TIMES.
             10 TXN-ID      PIC X(10).
             10 TXN-AMT     PIC 9(9).

       01 POSTED-TABLE.
          05 POSTED-ID OCCURS 200 TIMES PIC X(10).

       01 TXN-COUNT          PIC 9(3) VALUE 0.
       01 POSTED-COUNT       PIC 9(3) VALUE 0.
       01 I                  PIC 9(3) VALUE 0.
       01 J                  PIC 9(3) VALUE 0.

       01 FOUND              PIC X VALUE 'N'.
       01 ACCEPT-TOTAL       PIC 9(12) VALUE 0.
       01 PRINT-VAL          PIC Z(11)9.

       PROCEDURE DIVISION.

           DISPLAY "ENTER NUMBER OF ALREADY POSTED IDs:"
           ACCEPT POSTED-COUNT

           IF POSTED-COUNT > 200
               DISPLAY "TOO MANY POSTED IDs"
               GOBACK
           END-IF

           PERFORM VARYING I FROM 1 BY 1
               UNTIL I > POSTED-COUNT

               DISPLAY "ENTER POSTED ID:"
               ACCEPT POSTED-ID(I)

           END-PERFORM

           DISPLAY "ENTER NUMBER OF INCOMING TRANSACTIONS:"
           ACCEPT TXN-COUNT

           IF TXN-COUNT > 200
               DISPLAY "TOO MANY TRANSACTIONS"
               GOBACK
           END-IF

           PERFORM VARYING I FROM 1 BY 1
               UNTIL I > TXN-COUNT

               DISPLAY "ENTER TXN ID:"
               ACCEPT TXN-ID(I)
               DISPLAY "ENTER AMOUNT:"
               ACCEPT TXN-AMT(I)

           END-PERFORM

           DISPLAY " "
           DISPLAY "--- DEDUPLICATION RESULTS ---"

           PERFORM VARYING I FROM 1 BY 1
               UNTIL I > TXN-COUNT

               MOVE 'N' TO FOUND

               PERFORM VARYING J FROM 1 BY 1
                   UNTIL J > POSTED-COUNT

                   IF TXN-ID(I) = POSTED-ID(J)
                       MOVE 'Y' TO FOUND
                   END-IF

               END-PERFORM

               IF FOUND = 'Y'
                   DISPLAY FUNCTION TRIM(TXN-ID(I))
                           " REJECTED: DUPLICATE"
               ELSE
                   ADD TXN-AMT(I) TO ACCEPT-TOTAL
                   ADD 1 TO POSTED-COUNT
                   MOVE TXN-ID(I) TO POSTED-ID(POSTED-COUNT)
                   DISPLAY FUNCTION TRIM(TXN-ID(I)) " ACCEPTED"
               END-IF

           END-PERFORM

           MOVE ACCEPT-TOTAL TO PRINT-VAL
           DISPLAY "ACCEPTED TOTAL: " FUNCTION TRIM(PRINT-VAL)

           STOP RUN.
