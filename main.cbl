       IDENTIFICATION DIVISION.
       PROGRAM-ID. DORM-BPIM-TRACKER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
      * FD statements go here if using files

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Declare variables here, for example:
       01  WS-MESSAGE     PIC X(20) VALUE "HELLO WORLD".

       PROCEDURE DIVISION.
           DISPLAY "THIS IS A TEST".
           DISPLAY WS-MESSAGE.
           STOP RUN.
