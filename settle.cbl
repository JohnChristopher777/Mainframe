       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATM-SETTLE.
       AUTHOR. JOHN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 TABLE-DATA.
          05 ATM-RECORD OCCURS 50 TIMES.
             10 ATM-ID        PIC X(10).
             10 OPEN-CASH     PIC 9(9).
             10 DISPENSED     PIC 9(9).

       01 ATM-COUNT           PIC 9(3)  VALUE 0.
       01 I                   PIC 9(3)  VALUE 0.

       01 CLOSING-BAL         PIC 9(9)  VALUE 0.
       01 TOTAL-DISPENSED     PIC 9(12) VALUE 0.
       01 PRINT-BAL           PIC Z(8)9.
       01 PRINT-DISP          PIC Z(11)9.

       PROCEDURE DIVISION.

           DISPLAY "ENTER NUMBER OF ATMs:"
           ACCEPT ATM-COUNT

           IF ATM-COUNT > 50
               DISPLAY "TOO MANY ATMs"
               GOBACK
           END-IF

           PERFORM VARYING I FROM 1 BY 1
               UNTIL I > ATM-COUNT

               DISPLAY "ENTER ATM ID:"
               ACCEPT ATM-ID(I)
               DISPLAY "ENTER OPENING CASH:"
               ACCEPT OPEN-CASH(I)
               DISPLAY "ENTER TOTAL DISPENSED:"
               ACCEPT DISPENSED(I)

           END-PERFORM

           DISPLAY " "
           DISPLAY "ATM SETTLEMENT REPORT"

           PERFORM VARYING I FROM 1 BY 1
               UNTIL I > ATM-COUNT

               IF DISPENSED(I) > OPEN-CASH(I)
                   DISPLAY "ERROR: DISPENSED EXCEEDS OPENING CASH"
               ELSE
                   COMPUTE CLOSING-BAL =
                       OPEN-CASH(I) - DISPENSED(I)

                   MOVE CLOSING-BAL TO PRINT-BAL
                   DISPLAY "CLOSING: " FUNCTION TRIM(PRINT-BAL)

                   ADD DISPENSED(I) TO TOTAL-DISPENSED
               END-IF

           END-PERFORM

           MOVE TOTAL-DISPENSED TO PRINT-DISP
           DISPLAY "TOTAL DISPENSED TODAY: " FUNCTION TRIM(PRINT-DISP)

           STOP RUN.
