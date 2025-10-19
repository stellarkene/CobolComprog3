       IDENTIFICATION DIVISION.
       PROGRAM-ID. DORM-BPIM-TRACKER.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
               FILE-CONTROL.
           SELECT STUDENT-FILE ASSIGN TO "students.dat"
-              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
           COPY "student-info.cpy".

       WORKING-STORAGE SECTION.
      * Declare variables here, for example:
           COPY "billing-payment-vars.cpy".
       
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

           DISPLAY "How many students to add?"
           ACCEPT STUDENT-COUNT
           OPEN EXTEND STUDENT-FILE

           PERFORM VARYING S_C FROM 1 BY 1 UNTIL S_C > STUDENT-COUNT
               DISPLAY "==============="
               DISPLAY "STUDENT #" S_C
               DISPLAY "==============="
               DISPLAY "Name: " ACCEPT SI-NAME
               DISPLAY "Age: " ACCEPT SI-AGE
               DISPLAY "Gender: " ACCEPT SI-GENDER
               DISPLAY "Religion: " ACCEPT SI-RELIGION
               WRITE STUDENT-RECORD
               DISPLAY " "
           END-PERFORM


           CLOSE STUDENT-FILE
           
           DISPLAY "Students saved successfully!"
           ACCEPT OMITTED

           EXIT PARAGRAPH. 

      *=======================
      *FUNCTION: VIEW-STUDENTS
      *=======================
       VIEW-STUDENTS.
           
           OPEN INPUT STUDENT-FILE
           MOVE 1 TO S_C
                  PERFORM UNTIL EOF = 'Y'
           READ STUDENT-FILE
               AT END
                   MOVE 'Y' TO EOF
               NOT AT END
                   DISPLAY "==============="
                   DISPLAY "STUDENT #" S_C
                   DISPLAY "==============="
                   DISPLAY "Name: " SI-NAME
                   DISPLAY "Age: " SI-AGE
                   DISPLAY "Gender: " SI-GENDER
                   DISPLAY "Religion: " SI-RELIGION
                   ADD 1 TO S_C
                   DISPLAY " "
           END-READ
           END-PERFORM

           CLOSE STUDENT-FILE
           DISPLAY "Data loaded into memory!"
           ACCEPT OMITTED

           EXIT PARAGRAPH.
      
