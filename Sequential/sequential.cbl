       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEQ.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       
           SELECT TXN-FILE ASSIGN TO "TXNIN"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       
       FD TXN-FILE
           RECORD CONTAINS 80 CHARACTERS.
       
       01 TXN-RECORD.
          05 ACC-ID   PIC X(4).
          05 ACC-NAME PIC X(10).
          05 AMT      PIC 9(5).
          05 FILLER   PIC X(61).
       
       WORKING-STORAGE SECTION.
       
       01 WS-STATUS PIC XX.
       01 WS-EOF PIC X VALUE "N".
       
       PROCEDURE DIVISION.
       
       MAIN-PARA.
       
           OPEN INPUT TXN-FILE
       
           IF WS-STATUS NOT = "00"
               DISPLAY "OPEN ERROR: " WS-STATUS
               STOP RUN
           END-IF
       
           DISPLAY "ACCOUNT TRANSACTION DETAILS"
       
           PERFORM UNTIL WS-EOF = "Y"
       
               READ TXN-FILE
                   AT END
                       MOVE "Y" TO WS-EOF
                   NOT AT END
                       DISPLAY ACC-ID " | "
                               FUNCTION TRIM(ACC-NAME) " | "
                               AMT
               END-READ
       
           END-PERFORM
       
           CLOSE TXN-FILE
       
           DISPLAY "PROCESSING COMPLETED"
       
           STOP RUN.
       