       IDENTIFICATION DIVISION.
       PROGRAM-ID. DORM-BPIM-TRACKER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
      * FD statements go here if using files

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Declare variables here, for example:
       COPY "billing-payment.cpy".
       01  USER-CHOICE     PIC 9.
       01  TEST-PRINT      PIC X(10).

       PROCEDURE DIVISION.

           

       MAIN-PARA.
           PERFORM UNTIL USER-CHOICE = 4
           
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
                   DISPLAY "YOU CHOSE TO ADD STUDENT"
                   DISPLAY "PLEASE ENTER SOMETHING"
                   ACCEPT TEST-PRINT
                   DISPLAY TEST-PRINT
    
               WHEN 2
                   DISPLAY "YOU CHOSE TO VIEW STUDENTS"
    
               WHEN 3
                   DISPLAY "YOU CHOSE TO DELETE STUDENTS"
    
               WHEN 4
                   DISPLAY "EXITING PROGRAM..."
    
               WHEN OTHER 
                   DISPLAY "INVALID CHOICE. TRY AGAIN"
           END-EVALUATE

           END-PERFORM
           
           STOP RUN.
