       IDENTIFICATION DIVISION.
       PROGRAM-ID. BATCH-RECON.
       AUTHOR. JOHN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 HEADER-DATA.
          05 BATCH-ID         PIC X(10).
          05 EXP-COUNT        PIC 9(5).
          05 EXP-TOTAL        PIC 9(12).

       01 TABLE-DATA.
          05 TXN-RECORD OCCURS 500 TIMES.
             10 TXN-ID        PIC X(10).
             10 TXN-AMT       PIC 9(9).

       01 ACT-COUNT           PIC 9(5)  VALUE 0.
       01 ACT-TOTAL           PIC 9(12) VALUE 0.
       01 VARIANCE            PIC S9(12) VALUE 0.
       01 COUNT-DIFF          PIC S9(5) VALUE 0.
       01 I                   PIC 9(5)  VALUE 0.
       01 PRINT-VAL           PIC Z(11)9.
       01 PRINT-VAR           PIC -Z(11)9.
       01 PRINT-CNT           PIC -Z(4)9.

       PROCEDURE DIVISION.

           DISPLAY "ENTER BATCH ID:"
           ACCEPT BATCH-ID
           DISPLAY "ENTER EXPECTED COUNT:"
           ACCEPT EXP-COUNT
           DISPLAY "ENTER EXPECTED TOTAL:"
           ACCEPT EXP-TOTAL

           DISPLAY "ENTER NUMBER OF TRANSACTIONS:"
           ACCEPT ACT-COUNT

           IF ACT-COUNT > 500
               DISPLAY "TOO MANY TRANSACTIONS"
               GOBACK
           END-IF

           PERFORM VARYING I FROM 1 BY 1
               UNTIL I > ACT-COUNT

               DISPLAY "ENTER TXN ID:"
               ACCEPT TXN-ID(I)
               DISPLAY "ENTER AMOUNT:"
               ACCEPT TXN-AMT(I)
               ADD TXN-AMT(I) TO ACT-TOTAL

           END-PERFORM

           COMPUTE VARIANCE   = ACT-TOTAL - EXP-TOTAL
           COMPUTE COUNT-DIFF = ACT-COUNT - EXP-COUNT

           DISPLAY " "
           DISPLAY "BATCH RECONCILIATION: "
                   FUNCTION TRIM(BATCH-ID) " ---"
           MOVE EXP-COUNT TO PRINT-VAL
           DISPLAY "EXPECTED COUNT  : " FUNCTION TRIM(PRINT-VAL)
           MOVE ACT-COUNT TO PRINT-VAL
           DISPLAY "ACTUAL COUNT    : " FUNCTION TRIM(PRINT-VAL)
           MOVE COUNT-DIFF TO PRINT-CNT
           DISPLAY "COUNT DIFF      : " FUNCTION TRIM(PRINT-CNT)
           MOVE EXP-TOTAL TO PRINT-VAL
           DISPLAY "EXPECTED TOTAL  : " FUNCTION TRIM(PRINT-VAL)
           MOVE ACT-TOTAL TO PRINT-VAL
           DISPLAY "ACTUAL TOTAL    : " FUNCTION TRIM(PRINT-VAL)
           MOVE VARIANCE TO PRINT-VAR
           DISPLAY "AMOUNT VARIANCE : " FUNCTION TRIM(PRINT-VAR)

           IF ACT-COUNT = EXP-COUNT AND VARIANCE = 0
               DISPLAY "BATCH STATUS: BALANCED"
           ELSE
               DISPLAY "BATCH STATUS: OUT OF BALANCE"
           END-IF

           STOP RUN.
