       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHKPT-RESTART.
       AUTHOR. JOHN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 TABLE-DATA.
          05 TXN-RECORD OCCURS 200 TIMES.
             10 TXN-ID         PIC X(10).
             10 TXN-AMT        PIC 9(9).

       01 TXN-COUNT            PIC 9(3)  VALUE 0.
       01 RESTART-FROM         PIC 9(3)  VALUE 0.
       01 SAVED-TOTAL          PIC 9(12) VALUE 0.
       01 I                    PIC 9(3)  VALUE 0.

       01 CHKPT-EVERY          PIC 9(3)  VALUE 3.
       01 PROCESSED            PIC 9(3)  VALUE 0.
       01 START-REC            PIC 9(3)  VALUE 0.
       01 DISPLAY-REC          PIC 9(3)  VALUE 0.
       01 RUN-TOTAL            PIC 9(12) VALUE 0.
       01 PRINT-VAL            PIC Z(11)9.

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
               DISPLAY "ENTER AMOUNT:"
               ACCEPT TXN-AMT(I)

           END-PERFORM

           DISPLAY "RESTART FROM RECORD (0 = FRESH START):"
           ACCEPT RESTART-FROM

           IF RESTART-FROM > TXN-COUNT
               DISPLAY "RESTART POSITION EXCEEDS TOTAL COUNT"
               GOBACK
           END-IF

           DISPLAY "ENTER SAVED TOTAL FROM CHECKPOINT (0 = FRESH):"
           ACCEPT SAVED-TOTAL

           MOVE SAVED-TOTAL TO RUN-TOTAL

           DISPLAY " "
           IF RESTART-FROM > 0
               COMPUTE DISPLAY-REC = RESTART-FROM + 1
               MOVE SAVED-TOTAL TO PRINT-VAL
               DISPLAY "RESUMING FROM RECORD " DISPLAY-REC
               DISPLAY "RESTORING TOTAL: " FUNCTION TRIM(PRINT-VAL)
           ELSE
               DISPLAY "STARTING FRESH RUN"
           END-IF

           COMPUTE START-REC = RESTART-FROM + 1

           PERFORM VARYING I FROM START-REC BY 1
               UNTIL I > TXN-COUNT

               ADD TXN-AMT(I) TO RUN-TOTAL
               ADD 1 TO PROCESSED

               DISPLAY "PROCESSED: " FUNCTION TRIM(TXN-ID(I))

               IF FUNCTION MOD(PROCESSED, CHKPT-EVERY) = 0
                   MOVE RUN-TOTAL TO PRINT-VAL
                   DISPLAY "** CHECKPOINT AT RECORD " I
                           " TOTAL=" FUNCTION TRIM(PRINT-VAL) " **"
               END-IF

           END-PERFORM

           MOVE RUN-TOTAL TO PRINT-VAL
           DISPLAY " "
           DISPLAY "RECORDS PROCESSED: " PROCESSED
           DISPLAY "RUN TOTAL: " FUNCTION TRIM(PRINT-VAL)
           DISPLAY "RUN COMPLETE"

           STOP RUN.
