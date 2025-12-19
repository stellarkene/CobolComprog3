       IDENTIFICATION DIVISION.
       PROGRAM-ID. DORM-BPIM-TRACKER.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
               FILE-CONTROL.
           SELECT STUDENT-FILE ASSIGN TO "students.dat"
-              ORGANIZATION IS LINE SEQUENTIAL.

           SELECT TEMP-STUDENT-FILE ASSIGN TO "temp.dat"
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
           PERFORM UNTIL USER-CHOICE = 5
           PERFORM CLEAR-SCREEN
           
           DISPLAY "==========================="
           DISPLAY "         MAIN MENU         "
           DISPLAY "==========================="
           DISPLAY "1 - ADD STUDENT"
           DISPLAY "2 - VIEW STUDENTS"
           DISPLAY "3 - EDIT STUDENT INFO"
           DISPLAY "4 - DELETE STUDENTS"
           DISPLAY "5 - EXIT"
           DISPLAY "ENTER CHOICE (1 - 5): "
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
                   PERFORM EDIT-STUDENTS
    
               WHEN 4
                   PERFORM CLEAR-SCREEN
                   PERFORM DELETE-STUDENTS
    
               WHEN 5
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

      *=======================
      *FUNCTION: EXIT-PROMT
      *=======================
       EXIT-PROMT.

           DISPLAY "Press enter to exit."
           ACCEPT OMITTED

           EXIT PARAGRAPH.

      *=====================
      *FUNCTION: ADD-STUDENT
      *=====================
       ADD-STUDENT.
           DISPLAY "YOU CHOSE TO ADD STUDENT"
           DISPLAY "ADD STUDENT? (Y/y): "
           ACCEPT WS-ADD-FLAG
           INSPECT WS-ADD-FLAG CONVERTING 'y' TO 'Y'

               IF WS-ADD-FLAG = "Y"
                OPEN EXTEND STUDENT-FILE
        
                PERFORM UNTIL WS-ADD-FLAG = "N"
                    DISPLAY "==============="
                    DISPLAY "ADD STUDENT"
                    DISPLAY "==============="
                    DISPLAY "Name: " ACCEPT WS-NAME
                    DISPLAY "Age: " ACCEPT WS-AGE
                    DISPLAY "Gender: " ACCEPT WS-GENDER
                    DISPLAY "Contact Number: " ACCEPT WS-CONTACT-NUM
                    DISPLAY "Religion: " ACCEPT WS-RELIGION
                    DISPLAY "Assign Room: " ACCEPT WS-ROOM-NUM
                    DISPLAY "Rent Amount: " ACCEPT WS-RENT-AMOUNT
        
                    MOVE WS-NAME TO SI-NAME
                    MOVE WS-AGE TO SI-AGE
                    MOVE WS-GENDER TO SI-GENDER
                    MOVE WS-CONTACT-NUM TO SI-CONTACT-NUM
                    MOVE WS-RELIGION TO SI-RELIGION
                    MOVE WS-ROOM-NUM TO SI-ROOM-NUM
                    MOVE WS-RENT-AMOUNT TO SI-RENT-AMOUNT
        
                    WRITE STUDENT-RECORD
        
                    DISPLAY "ADD ANOTHER? (Y/N):"
                    ACCEPT WS-ADD-FLAG
                    INSPECT WS-ADD-FLAG CONVERTING 'y' TO 'Y'
                    INSPECT WS-ADD-FLAG CONVERTING 'n' TO 'N'
                END-PERFORM
        
                CLOSE STUDENT-FILE
               END-IF
        
           DISPLAY "Students saved successfully!"
           PERFORM EXIT-PROMT
       EXIT PARAGRAPH.


      *=======================
      *FUNCTION: VIEW-STUDENTS
      *=======================
       VIEW-STUDENTS.
           
           OPEN INPUT STUDENT-FILE
           MOVE 1 TO S_C
           MOVE "N" TO EOF
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
                   DISPLAY "Contact Number: " SI-CONTACT-NUM
                   DISPLAY "Religion: " SI-RELIGION
                   DISPLAY "Assigned Room: " SI-ROOM-NUM
                   DISPLAY "Rent Amount: " SI-RENT-AMOUNT
                   ADD 1 TO S_C
                   DISPLAY " "
           END-READ
           END-PERFORM

           CLOSE STUDENT-FILE
           DISPLAY "Data loaded into memory!"
           
           PERFORM EXIT-PROMT

           EXIT PARAGRAPH.

      *=======================
      *FUNCTION: EDIT-STUDENTS
      *=======================
       EDIT-STUDENTS.
           DISPLAY "YOU CHOSE TO EDIT STUDENTS"
           
           DISPLAY "Enter a name of the student to edit: "
           ACCEPT SEARCH-NAME

           OPEN INPUT STUDENT-FILE
               OUTPUT TEMP-STUDENT-FILE

           MOVE "N" TO EOF

           PERFORM UNTIL EOF = "Y"
               READ STUDENT-FILE
                   AT END
                       MOVE "Y" TO EOF

                   NOT AT END
                       IF SI-NAME = SEARCH-NAME
                           DISPLAY "Editing student: " SI-NAME

                       DISPLAY "Edit Name (keep empty to unchange)"
                           ACCEPT TEMP-NAME
                           IF TEMP-NAME NOT = SPACES
                               MOVE TEMP-NAME TO SI-NAME
                           END-IF

                       DISPLAY "Edit age (keep empty to unchange)"
                           ACCEPT TEMP-AGE
                           IF TEMP-AGE NOT = SPACES
                               MOVE TEMP-AGE TO SI-AGE
                           END-IF

                       DISPLAY "Edit Gender (keep empty to unchange)"
                           ACCEPT TEMP-GENDER
                           IF TEMP-GENDER NOT = SPACES
                               MOVE TEMP-GENDER TO SI-GENDER
                           END-IF

                       DISPLAY "Edit Contact (keep empty to unchange)"
                           ACCEPT TEMP-CONTACT-NUM
                           IF TEMP-CONTACT-NUM NOT = SPACES
                               MOVE TEMP-CONTACT-NUM TO SI-CONTACT-NUM
                           END-IF

                       DISPLAY "Edit Religion (keep empty to unchange)"
                           ACCEPT TEMP-RELIGION
                           IF TEMP-RELIGION NOT = SPACES
                               MOVE TEMP-RELIGION TO SI-RELIGION
                           END-IF

                       DISPLAY "Edit Room (keep empty to unchange)"
                           ACCEPT TEMP-ROOM-NUM
                           IF TEMP-ROOM-NUM NOT = SPACES
                               MOVE TEMP-ROOM-NUM TO SI-ROOM-NUM
                           END-IF

                       DISPLAY "Edit Rent (keep empty to unchange)"
                           ACCEPT TEMP-RENT-AMOUNT
                           IF TEMP-RENT-AMOUNT NOT = SPACES
                               MOVE TEMP-RENT-AMOUNT TO SI-RENT-AMOUNT
                           END-IF
                       END-IF

                   WRITE TEMP-STUDENT-RECORD FROM STUDENT-RECORD

               END-READ
           END-PERFORM

           CLOSE STUDENT-FILE TEMP-STUDENT-FILE

           ACCEPT OS-NAME FROM ENVIRONMENT "OS"

           IF OS-NAME = "Windows_NT"
               CALL "SYSTEM" USING "del students.dat"
               CALL "SYSTEM" USING "rename temp.dat students.dat"

           ELSE
               CALL "SYSTEM" USING "rm students.dat"
               CALL "SYSTEM" USING "mv temp.dat students.dat"

           END-IF

           PERFORM EXIT-PROMT
           EXIT PARAGRAPH.

      
      *=========================
      *FUNCTION: DELETE-STUDENTS
      *=========================
       DELETE-STUDENTS.
           DISPLAY "YOU CHOSE TO DELETE STUDENTS"

           DISPLAY "PLEASE ENTER THE NAME OF THE STUDENT TO DELETE"
           ACCEPT SEARCH-NAME

           OPEN INPUT STUDENT-FILE
               OUTPUT TEMP-STUDENT-FILE
           
               MOVE "N" TO EOF
                   
               PERFORM UNTIL EOF = "Y"
                   READ STUDENT-FILE
                       AT END 
                            MOVE "Y" TO EOF

                       NOT AT END
                       IF SI-NAME = SEARCH-NAME
                           DISPLAY "DELETING STUDENT: " SI-NAME
                       ELSE
                           WRITE TEMP-STUDENT-RECORD FROM STUDENT-RECORD
                       END-IF

                   END-READ
               END-PERFORM

           CLOSE STUDENT-FILE TEMP-STUDENT-FILE

           ACCEPT OS-NAME FROM ENVIRONMENT "OS"

           IF OS-NAME = "Windows_NT"
               CALL "SYSTEM" USING "del students.dat"
               CALL "SYSTEM" USING "rename temp.dat students.dat"

           ELSE
               CALL "SYSTEM" USING "rm students.dat"
               CALL "SYSTEM" USING "mv temp.dat students.dat"

           END-IF
           
           PERFORM EXIT-PROMT
           EXIT PARAGRAPH.
