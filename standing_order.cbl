       IDENTIFICATION DIVISION.
       PROGRAM-ID. STANDING-ORDER.
       AUTHOR. JOHN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 TABLE-DATA.
          05 ACC-RECORD OCCURS 100 TIMES.
             10 ACC-ID      PIC X(10).
             10 ACC-BAL     PIC 9(7).

       01 ACC-COUNT         PIC 9(3) VALUE 0.
       01 ORDER-COUNT       PIC 9(3) VALUE 0.
       01 I                 PIC 9(3) VALUE 0.
       01 J                 PIC 9(3) VALUE 0.

       01 ORDER-ID          PIC X(10).
       01 TARGET-ACC        PIC X(10).
       01 ORDER-AMT         PIC 9(7).

       01 FOUND             PIC X VALUE 'N'.
       01 SUCCESS-TOTAL     PIC 9(9) VALUE 0.
       01 PRINT-TOTAL       PIC Z(8)9.

       PROCEDURE DIVISION.

           DISPLAY "ENTER NUMBER OF ACCOUNTS:"
           ACCEPT ACC-COUNT

           IF ACC-COUNT > 100
               DISPLAY "TOO MANY ACCOUNTS"
               GOBACK
           END-IF

           PERFORM VARYING I FROM 1 BY 1
               UNTIL I > ACC-COUNT

               DISPLAY "ENTER ACCOUNT ID AND BALANCE:"
               ACCEPT ACC-ID(I)
               ACCEPT ACC-BAL(I)

           END-PERFORM

           DISPLAY "ENTER NUMBER OF ORDERS:"
           ACCEPT ORDER-COUNT

           IF ORDER-COUNT > 100
               DISPLAY "TOO MANY ORDERS"
               GOBACK
           END-IF

           PERFORM VARYING J FROM 1 BY 1
               UNTIL J > ORDER-COUNT

               DISPLAY "ENTER ORDER ID, ACCOUNT ID, AMOUNT:"
               ACCEPT ORDER-ID
               ACCEPT TARGET-ACC
               ACCEPT ORDER-AMT

               MOVE 'N' TO FOUND

               PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > ACC-COUNT

                   IF TARGET-ACC = ACC-ID(I)
                       MOVE 'Y' TO FOUND

                       IF ORDER-AMT <= 0
                           DISPLAY FUNCTION TRIM(ORDER-ID)
                                   " REJECTED: INVALID AMOUNT"
                       ELSE
                           IF ACC-BAL(I) >= ORDER-AMT
                               SUBTRACT ORDER-AMT FROM ACC-BAL(I)
                               ADD ORDER-AMT TO SUCCESS-TOTAL
                               DISPLAY FUNCTION TRIM(ORDER-ID)
                                       " SUCCESS"
                           ELSE
                               DISPLAY FUNCTION TRIM(ORDER-ID)
                                       " REJECTED: INSUFFICIENT FUNDS"
                           END-IF
                       END-IF

                   END-IF

               END-PERFORM

               IF FOUND = 'N'
                   DISPLAY FUNCTION TRIM(ORDER-ID)
                           " REJECTED: ACCOUNT NOT FOUND"
               END-IF

           END-PERFORM

           MOVE SUCCESS-TOTAL TO PRINT-TOTAL
           DISPLAY "Successful total: " FUNCTION TRIM(PRINT-TOTAL)

           STOP RUN.
