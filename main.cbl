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
                   PERFORM ADD-STUDENT
    
               WHEN 2
                   PERFORM CLEAR-SCREEN
                   PERFORM VIEW-STUDENTS
    
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

      *=====================
      *FUNCTION: ADD-STUDENT
      *=====================
       ADD-STUDENT.
           DISPLAY "YOU CHOSE TO ADD STUDENT"

           DISPLAY "HOW MANY STUDENTS YOU WANT TO ENTER?"
-              "(MAX 100): " WITH NO ADVANCING
           ACCEPT STUDENT-COUNT

           IF STUDENT-COUNT > 100
               MOVE 100 TO STUDENT-COUNT
               DISPLAY "LIMIT EXCEEDED. ONLY 100 STUDENTS PLEASE"
           END-IF

           PERFORM UNTIL STUDENT-COUNTER > STUDENT-COUNT
               DISPLAY "==========================="
               DISPLAY " Entering Student #" STUDENT-COUNTER
               DISPLAY "==========================="
               DISPLAY "Name: " ACCEPT SI-NAME (STUDENT-COUNTER)
               DISPLAY "Age: " ACCEPT SI-AGE (STUDENT-COUNTER)
               DISPLAY "Gender: " ACCEPT SI-GENDER (STUDENT-COUNTER)
               DISPLAY "Religion: " ACCEPT SI-RELIGION (STUDENT-COUNTER)
               ADD 1 TO STUDENT-COUNTER
           END-PERFORM

           DISPLAY "PLEASE PRESS ENTER TO EXIT"
           ACCEPT TEST-PRINT
           DISPLAY TEST-PRINT

           EXIT PARAGRAPH. 

      *=======================
      *FUNCTION: VIEW-STUDENTS
      *=======================
       VIEW-STUDENTS.
           DISPLAY "YOU CHOSE TO VIEW STUDENTS"
           DISPLAY " "
           DISPLAY "=====STUDENT LIST====="
           MOVE 1 TO STUDENT-COUNTER

           PERFORM UNTIL STUDENT-COUNTER > STUDENT-COUNT
               DISPLAY "Student #" STUDENT-COUNTER
               DISPLAY "Name: " SI-NAME (STUDENT-COUNTER)
               DISPLAY "Age: " SI-AGE (STUDENT-COUNTER)
               DISPLAY "Gender: " SI-GENDER (STUDENT-COUNTER)
               DISPLAY "Religion: " SI-RELIGION (STUDENT-COUNTER)
               DISPLAY "-------------------------"
               ADD 1 TO STUDENT-COUNTER
           END-PERFORM

           DISPLAY "PLEASE PRESS ENTER TO EXIT"
           ACCEPT TEST-PRINT
           DISPLAY TEST-PRINT

           EXIT PARAGRAPH.
      
