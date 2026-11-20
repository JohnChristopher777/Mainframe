       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRANSFER.
       AUTHOR. JOHN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 SRC-BAL              PIC 9(9)  VALUE 0.
       01 DST-BAL              PIC 9(9)  VALUE 0.
       01 TXN-AMT              PIC 9(9)  VALUE 0.
       01 DAILY-USED           PIC 9(9)  VALUE 0.
       01 DAILY-LIMIT          PIC 9(9)  VALUE 500000.
       01 DAILY-REMAINING      PIC 9(9)  VALUE 0.

       01 TEMP-SRC             PIC 9(9)  VALUE 0.
       01 TEMP-DST             PIC 9(9)  VALUE 0.
       01 PRINT-VAL            PIC Z(8)9.

       PROCEDURE DIVISION.

           DISPLAY "ENTER SOURCE BALANCE:"
           ACCEPT SRC-BAL

           DISPLAY "ENTER DESTINATION BALANCE:"
           ACCEPT DST-BAL

           DISPLAY "ENTER TRANSFER AMOUNT:"
           ACCEPT TXN-AMT

           DISPLAY "ENTER AMOUNT ALREADY TRANSFERRED TODAY:"
           ACCEPT DAILY-USED

           IF TXN-AMT <= 0
               DISPLAY "STATUS: REJECT_INVALID_AMOUNT"
               DISPLAY "SOURCE BAL   : " SRC-BAL
               DISPLAY "DEST BAL     : " DST-BAL
               GOBACK
           END-IF

           IF TXN-AMT > SRC-BAL
               DISPLAY "STATUS: REJECT_INSUFFICIENT_FUNDS"
               DISPLAY "SOURCE BAL   : " SRC-BAL
               DISPLAY "DEST BAL     : " DST-BAL
               GOBACK
           END-IF

           COMPUTE DAILY-REMAINING = DAILY-LIMIT - DAILY-USED

           IF TXN-AMT > DAILY-REMAINING
               DISPLAY "STATUS: REJECT_DAILY_LIMIT_EXCEEDED"
               MOVE DAILY-REMAINING TO PRINT-VAL
               DISPLAY "DAILY REMAINING: " FUNCTION TRIM(PRINT-VAL)
               DISPLAY "SOURCE BAL   : " SRC-BAL
               DISPLAY "DEST BAL     : " DST-BAL
               GOBACK
           END-IF

           COMPUTE TEMP-SRC = SRC-BAL - TXN-AMT
           COMPUTE TEMP-DST = DST-BAL + TXN-AMT

           MOVE TEMP-SRC TO SRC-BAL
           MOVE TEMP-DST TO DST-BAL

           DISPLAY "STATUS: SUCCESS"
           MOVE SRC-BAL TO PRINT-VAL
           DISPLAY "SOURCE BAL   : " FUNCTION TRIM(PRINT-VAL)
           MOVE DST-BAL TO PRINT-VAL
           DISPLAY "DEST BAL     : " FUNCTION TRIM(PRINT-VAL)

           STOP RUN.
