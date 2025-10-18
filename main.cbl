       IDENTIFICATION DIVISION.
       PROGRAM-ID. DORM-BPIM-TRACKER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
      * FD statements go here if using files

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Declare variables here, for example:
       COPY "billing-payment.cpy".
       01  OS-NAME        PIC X(50).
       01  CLEAR-COMMAND  PIC X(10).
       01  USER-CHOICE     PIC 9.
       01  TEST-PRINT      PIC X(10).

       PROCEDURE DIVISION.
           PERFORM MAIN-MENU
           STOP RUN.

      *===================
      *FUNCTION: MAIN MENU
      *=================== 
       MAIN-MENU.
           PERFORM UNTIL USER-CHOICE = 4
           PERFORM CLEAR-SCREEN
           
           DISPLAY "==========================="
           DISPLAY "         MAIN MENU         "
           DISPLAY "==========================="
           DISPLAY "1 - ADD STUDENT"
           DISPLAY "2 - VIEW STUDENTS"
           DISPLAY "3 - DELETE STUDENTS"
           DISPLAY "4 - EXIT"
           DISPLAY "ENTER CHOICE (1 - 4): "
           ACCEPT USER-CHOICE

           EVALUATE USER-CHOICE
               WHEN 1
                   PERFORM CLEAR-SCREEN
                   DISPLAY "YOU CHOSE TO ADD STUDENT"
                   DISPLAY "PLEASE PRESS ENTER TO EXIT"
                   ACCEPT TEST-PRINT
                   DISPLAY TEST-PRINT
    
               WHEN 2
                   PERFORM CLEAR-SCREEN
                   DISPLAY "YOU CHOSE TO VIEW STUDENTS"
                   DISPLAY "PLEASE PRESS ENTER TO EXIT"
                   ACCEPT TEST-PRINT
                   DISPLAY TEST-PRINT
    
               WHEN 3
                   PERFORM CLEAR-SCREEN
                   DISPLAY "YOU CHOSE TO DELETE STUDENTS"
                   DISPLAY "PLEASE PRESS ENTER TO EXIT"
                   ACCEPT TEST-PRINT
                   DISPLAY TEST-PRINT
    
               WHEN 4
                   DISPLAY "EXITING PROGRAM..."
    
               WHEN OTHER 
                   DISPLAY "INVALID CHOICE. TRY AGAIN"
           END-EVALUATE

           END-PERFORM
           EXIT PARAGRAPH.

      *======================
      *FUNCTION: CLEAR-SCREEN
      *======================
        CLEAR-SCREEN.
           ACCEPT OS-NAME FROM ENVIRONMENT "OS"
           IF OS-NAME = "Windows_NT"
               MOVE "cls" TO CLEAR-COMMAND
           ELSE
               MOVE "clear" TO CLEAR-COMMAND
           END-IF

           CALL "SYSTEM" USING CLEAR-COMMAND
           
           EXIT PARAGRAPH.
