       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IDEMPOTENCY.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01 TABLE-DATA.
          05 TABLE-ID PIC X(10) OCCURS 100 TIMES.
       
       01 TABLE-COUNT PIC 9(3) VALUE 0.
       01 TRX-COUNT PIC 9(3) VALUE 0.
       01 I PIC 9(3) VALUE 0.
       01 J PIC 9(3) VALUE 0.
       
       01 TRX-ID PIC X(10).
       01 ACC-ID PIC X(10).
       01 AMT PIC 9(7).
       
       01 FOUND PIC X VALUE 'N'.
       01 TOTAL PIC 9(9) VALUE 0.
       01 PRINT-TOTAL PIC Z(8)9.
       
       PROCEDURE DIVISION.
       
           DISPLAY "HOW MANY PREVIOUSLY POSTED IDS?"
           ACCEPT TABLE-COUNT
       
           IF TABLE-COUNT > 100
               DISPLAY "TOO MANY IDS"
               GOBACK
           END-IF
       
           PERFORM VARYING I FROM 1 BY 1
               UNTIL I > TABLE-COUNT
       
               DISPLAY "ENTER PREVIOUSLY POSTED ID:"
               ACCEPT TABLE-ID(I)
       
           END-PERFORM
       
           DISPLAY "HOW MANY NEW TRANSACTIONS?"
           ACCEPT TRX-COUNT
       
           PERFORM VARYING J FROM 1 BY 1
               UNTIL J > TRX-COUNT
       
               DISPLAY "ENTER TRANSACTION ID:"
               ACCEPT TRX-ID
       
               DISPLAY "ENTER ACCOUNT ID:"
               ACCEPT ACC-ID
       
               DISPLAY "ENTER AMOUNT:"
               ACCEPT AMT
       
               MOVE 'N' TO FOUND
       
               PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > TABLE-COUNT
       
                   IF TRX-ID = TABLE-ID(I)
                       MOVE 'Y' TO FOUND
                   END-IF
       
               END-PERFORM
       
               IF FOUND = 'Y'
                   DISPLAY TRX-ID " ALREADY POSTED"
               ELSE
                   ADD AMT TO TOTAL
       
                   ADD 1 TO TABLE-COUNT
                   MOVE TRX-ID TO TABLE-ID(TABLE-COUNT)
       
                   DISPLAY TRX-ID " POSTED"
               END-IF
       
           END-PERFORM
       
           MOVE TOTAL TO PRINT-TOTAL
       
           DISPLAY "FINAL POSTED TOTAL: "
               FUNCTION TRIM(PRINT-TOTAL)
       
           GOBACK.