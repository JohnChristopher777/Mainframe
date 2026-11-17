       IDENTIFICATION DIVISION.
       PROGRAM-ID. EOD-LEDGER.
       AUTHOR. JOHN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 TABLE-DATA.
          05 TXN-RECORD OCCURS 200 TIMES.
             10 TXN-ID        PIC X(10).
             10 TXN-TYPE      PIC X(1).
             10 TXN-AMT       PIC 9(9).

       01 TXN-COUNT            PIC 9(3)  VALUE 0.
       01 REJECT-COUNT         PIC 9(3)  VALUE 0.
       01 I                    PIC 9(3)  VALUE 0.

       01 TOTAL-CREDIT         PIC 9(12) VALUE 0.
       01 TOTAL-DEBIT          PIC 9(12) VALUE 0.
       01 NET-POSITION         PIC S9(12) VALUE 0.
       01 PRINT-VAL            PIC Z(11)9.
       01 PRINT-NET            PIC -Z(11)9.

       PROCEDURE DIVISION.

           DISPLAY "ENTER NUMBER OF TRANSACTIONS:"
           ACCEPT TXN-COUNT

           IF TXN-COUNT > 200
               DISPLAY "TOO MANY TRANSACTIONS"
               GOBACK
           END-IF

           PERFORM VARYING I FROM 1 BY 1
               UNTIL I > TXN-COUNT

               DISPLAY "ENTER TXN ID:"
               ACCEPT TXN-ID(I)
               DISPLAY "ENTER TYPE (C=CREDIT D=DEBIT):"
               ACCEPT TXN-TYPE(I)
               DISPLAY "ENTER AMOUNT:"
               ACCEPT TXN-AMT(I)

           END-PERFORM

           PERFORM VARYING I FROM 1 BY 1
               UNTIL I > TXN-COUNT

               EVALUATE TXN-TYPE(I)
                   WHEN 'C'
                       ADD TXN-AMT(I) TO TOTAL-CREDIT
                   WHEN 'D'
                       ADD TXN-AMT(I) TO TOTAL-DEBIT
                   WHEN OTHER
                       ADD 1 TO REJECT-COUNT
                       DISPLAY FUNCTION TRIM(TXN-ID(I))
                               " REJECTED: INVALID TYPE"
               END-EVALUATE

           END-PERFORM

           COMPUTE NET-POSITION = TOTAL-CREDIT - TOTAL-DEBIT

           DISPLAY " "
           DISPLAY "--- END OF DAY LEDGER ---"
           MOVE TOTAL-CREDIT TO PRINT-VAL
           DISPLAY "TOTAL CREDITS   : " FUNCTION TRIM(PRINT-VAL)
           MOVE TOTAL-DEBIT TO PRINT-VAL
           DISPLAY "TOTAL DEBITS    : " FUNCTION TRIM(PRINT-VAL)
           MOVE NET-POSITION TO PRINT-NET
           DISPLAY "NET POSITION    : " FUNCTION TRIM(PRINT-NET)
           DISPLAY "REJECTED RECORDS: " REJECT-COUNT

           IF NET-POSITION >= 0
               DISPLAY "STATUS: SURPLUS"
           ELSE
               DISPLAY "STATUS: DEFICIT"
           END-IF

           STOP RUN.
