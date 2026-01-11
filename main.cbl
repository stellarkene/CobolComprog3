       IDENTIFICATION DIVISION.
       PROGRAM-ID. DORM-BPIM-TRACKER.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
               FILE-CONTROL.
           SELECT STUDENT-FILE ASSIGN TO "students.dat"
-              ORGANIZATION IS LINE SEQUENTIAL.

           SELECT TEMP-STUDENT-FILE ASSIGN TO "temp.dat"
-              ORGANIZATION IS LINE SEQUENTIAL.

           SELECT DORM-FILE ASSIGN TO "dorm.dat"
-              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
      *STUDENT RECORD
           FD  STUDENT-FILE.
       01  STUDENT-RECORD.
           05  SI-NAME                             PIC X(50).
           05  SI-AGE                              PIC 9(2).
           05  SI-GENDER                           PIC X(15).
           05  SI-CONTACT-NUM                      PIC X(12).
           05  SI-ROOM-NUM                         PIC X(20).
           05  SI-RENT-AMOUNT                      PIC X(6).
           05  SI-ELECTRIC-BILL                    PIC X(7).
           05  SI-WIFI-BILL                        PIC X(7).
           05  SI-PAYMENT-STATUS                   PIC X(10).

      *TEMP STUDENT RECORD
       FD  TEMP-STUDENT-FILE.
       01  TEMP-STUDENT-RECORD.
           05  TEMP-NAME                           PIC X(50).
           05  TEMP-AGE                            PIC 9(2).
           05  TEMP-GENDER                         PIC X(15).         
           05  TEMP-CONTACT-NUM                    PIC X(12).
           05  TEMP-ROOM-NUM                       PIC X(20).
           05  TEMP-RENT-AMOUNT                    PIC X(6).
           05  TEMP-ELECTRIC-BILL                  PIC X(7).
           05  TEMP-WIFI-BILL                      PIC X(7).
           05  TEMP-STATUS                         PIC X(10).

      *DORM FILE
           FD DORM-FILE.
       01  DORM-RECORD.
           05  DI-FLOOR                            PIC X(2).
           05  DI-ROOM-NUM                         PIC X(6).
           05  DI-RENT-AMOUNT                      PIC X(6).
           05  DI-ELECTRICITY                      PIC X(7).
           05  DI-WIFI                             PIC X(7).
           05  DI-STATUS                           PIC X(10).
           05  DI-ID                               PIC X(10).
           05  DI-DATE-PAID                        PIC X(8).

      *WS
       WORKING-STORAGE SECTION.
      *UTIL
       01  UTIL-OS-NAME                            PIC X(50).
       01  UTIL-CLEAR-COMMAND                      PIC X(10).
       01  UTIL-SM-CHOICE                          PIC 9.
       01  UTIL-MM-CHOICE                          PIC 9.
       01  UTIL-DM-CHOICE                          PIC 9.
       01  UTIL-SEARCH-NAME                        PIC X(50).
       01  UTIL-EOF                                PIC X VALUE 'N'.
       01  UTIL-EDIT-AGAIN                         PIC X VALUE "Y".
       01  UTIL-EDIT-FOUND                         PIC X VALUE "N".
       01  UTIL-DELETE-CHOICE                      PIC X VALUE "Y".
       01  UTIL-DELETE-FOUND                        PIC X VALUE "N".


      *WS STUDENT
       01  WS-STUDENT-ID                           PIC X(10).
       01  WS-NAME                                 PIC X(50).
       01  WS-AGE                                  PIC 9(2).
       01  WS-GENDER                               PIC X(15).
       01  WS-CONTACT-NUM                          PIC X(12).
       01  WS-ASSIGNED-DORM-ID                     PIC X(10).
       01  WS-RENT-AMOUNT-PAID                     PIC X(6).
       01  WS-ELECTRIC-BILL                        PIC 9(4)V99.
       01  WS-WIFI-BILL                            PIC 9(4)V99.
       01  WS-PAYMENT-STATUS                       PIC X(10).

      *WS DORM
       01  WS-DORM-FLOOR                           PIC X(2).
       01  WS-DORM-ROOM-NUM                        PIC X(6).
       01  WS-DORM-RENT-AMOUNT                     PIC X(6).
       01  WS-DORM-ELECTRICITY                     PIC 9(4)V99.
       01  WS-DORM-WIFI                            PIC 9(4)V99.
       01  WS-DORM-STATUS                          PIC X(10).
       01  WS-DORM-ID                              PIC X(10).
       01  WS-DORM-DATE-PAID                       PIC X(8).
       
      *WS ADD
       01  WS-ADD-FLAG                             PIC X(2).

       PROCEDURE DIVISION.
           PERFORM MAIN-MENU.
           STOP RUN.


      *============================
      *FUNCTION: MAIN MENU
      *============================
       MAIN-MENU.
           PERFORM UNTIL UTIL-MM-CHOICE = 5
           PERFORM CLEAR-SCREEN
           DISPLAY "==========================="
           DISPLAY "         MAIN MENU         "
           DISPLAY "==========================="
           DISPLAY "1 - STUDENT MANAGEMENT"
           DISPLAY "2 - DORM MANAGEMENT"
           DISPLAY "3 - RECORD PAYMENT"
           DISPLAY "4 - REPORTS"
           DISPLAY "5 - EXIT"

           DISPLAY "ENTER CHOICE (1 - 5): "
           ACCEPT UTIL-MM-CHOICE

           EVALUATE UTIL-MM-CHOICE
               WHEN 1
                   PERFORM STUDENT-MANAGEMENT

               WHEN 2
                   PERFORM DORM-MANAGEMENT
               WHEN 3
                   DISPLAY "PAYMENT MANAGEMENT"
               WHEN 4
                   DISPLAY "REPORT MENU"
               WHEN 5
                   DISPLAY "EXITING..."
                   PERFORM EXIT-PROMT
               WHEN OTHER
                   DISPLAY "INVALID INPUT, PLEASE TRY AGAIN"
                   PERFORM EXIT-PROMT
           END-EVALUATE

           END-PERFORM
           PERFORM EXIT-PROMT
           EXIT PARAGRAPH.

      *============================
      *FUNCTION: DORM MANAGEMENT
      *============================
       DORM-MANAGEMENT.
           PERFORM UNTIL UTIL-DM-CHOICE = 5
           PERFORM CLEAR-SCREEN

           DISPLAY "==========================="
           DISPLAY "      DORM MANAGEMENT      "
           DISPLAY "==========================="
           DISPLAY "1 - ADD DORM"
           DISPLAY "2 - VIEW DORMS"
           DISPLAY "3 - EDIT DORM INFO"
           DISPLAY "4 - DELETE DORMS"
           DISPLAY "5 - EXIT"

           DISPLAY "ENTER CHOICE (1 - 5): "
           ACCEPT UTIL-DM-CHOICE

           EVALUATE UTIL-DM-CHOICE
               WHEN 1
                   PERFORM CLEAR-SCREEN
                   PERFORM ADD-DORM
               WHEN 2
                   PERFORM CLEAR-SCREEN
                   PERFORM VIEW-DORMS

               WHEN 3
                   PERFORM CLEAR-SCREEN
                   
               WHEN 4
                   PERFORM CLEAR-SCREEN

               WHEN 5
                   PERFORM CLEAR-SCREEN

               WHEN OTHER
                   DISPLAY "INVALID INPUT, TRY AGAIN"
                   PERFORM EXIT-PROMT
           END-EVALUATE
           
           END-PERFORM
           PERFORM EXIT-PROMT
           EXIT PARAGRAPH.

      *============================
      *FUNCTION: ADD DORM 
      *============================
       ADD-DORM.
           DISPLAY "YOU CHOSE TO ADD DORM"
           DISPLAY "ADD DORM? (Y/N)"
           ACCEPT WS-ADD-FLAG

           PERFORM CONVERT-FLAG

           IF WS-ADD-FLAG = "Y"
               OPEN EXTEND DORM-FILE
               PERFORM UNTIL WS-ADD-FLAG = "N"
                   DISPLAY "PLEASE ENTER DORM ROOM FLOOR: "
                   ACCEPT WS-DORM-FLOOR

                   DISPLAY "PLEASE ENTER DORM ROOM NUMBER: "
                   ACCEPT WS-DORM-ROOM-NUM

                   DISPLAY "PLEASE ENTER RENT AMOUNT: "
                   ACCEPT WS-DORM-RENT-AMOUNT

                   MOVE WS-DORM-FLOOR TO DI-FLOOR
                   MOVE WS-DORM-ROOM-NUM TO DI-ROOM-NUM
                   MOVE WS-DORM-RENT-AMOUNT TO DI-RENT-AMOUNT

                   WRITE DORM-RECORD

                   DISPLAY "ADD ANOTHER? (Y/N):"
                   ACCEPT WS-ADD-FLAG
                   PERFORM CONVERT-FLAG
               END-PERFORM
               CLOSE DORM-FILE
           END-IF

           PERFORM EXIT-PROMT
           EXIT PARAGRAPH.

      *============================
      *FUNCTION: ADD DORM 
      *============================
       VIEW-DORMS.
              
           OPEN INPUT DORM-FILE
       
           MOVE "N" TO UTIL-EOF
       
           PERFORM UNTIL UTIL-EOF = "Y"
               READ DORM-FILE
                   AT END
                       MOVE "Y" TO UTIL-EOF
                   NOT AT END
                       DISPLAY "========================"
                       DISPLAY "DORM ID: " DI-ID
                       DISPLAY "========================"
                       DISPLAY "Floor: " DI-FLOOR
                       DISPLAY "Room Number: " DI-ROOM-NUM
                       DISPLAY "Rent Amount: " DI-RENT-AMOUNT
                       DISPLAY "Electricity: " DI-ELECTRICITY
                       DISPLAY "WiFi: " DI-WIFI
                       DISPLAY "Status: " DI-STATUS
                       DISPLAY "Date Paid: " DI-DATE-PAID
                       DISPLAY SPACE
               END-READ
           END-PERFORM
       
           CLOSE DORM-FILE
       
           PERFORM EXIT-PROMT
           EXIT PARAGRAPH.

      *============================
      *FUNCTION: STUDENT MANAGEMENT
      *============================
       STUDENT-MANAGEMENT.
           PERFORM UNTIL UTIL-SM-CHOICE = 5
           PERFORM CLEAR-SCREEN
           
           DISPLAY "==========================="
           DISPLAY "    STUDNENT MANAGEMENT    "
           DISPLAY "==========================="
           DISPLAY "1 - ADD STUDENT"
           DISPLAY "2 - VIEW STUDENTS"
           DISPLAY "3 - EDIT STUDENT INFO"
           DISPLAY "4 - DELETE STUDENTS"
           DISPLAY "5 - EXIT"

           DISPLAY "ENTER CHOICE (1 - 5): "
           ACCEPT UTIL-SM-CHOICE

           EVALUATE UTIL-SM-CHOICE
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
                   PERFORM EXIT-PROMT
    
               WHEN OTHER 
                   DISPLAY "INVALID CHOICE. TRY AGAIN"
           END-EVALUATE

           END-PERFORM
           EXIT PARAGRAPH.

      *======================
      *FUNCTION: CLEAR-SCREEN
      *======================
       CLEAR-SCREEN.
           ACCEPT UTIL-OS-NAME FROM ENVIRONMENT "OS"
           IF UTIL-OS-NAME = "Windows_NT"
               MOVE "cls" TO UTIL-CLEAR-COMMAND
           ELSE
               MOVE "clear" TO UTIL-CLEAR-COMMAND
           END-IF

           CALL "SYSTEM" USING UTIL-CLEAR-COMMAND
           
           EXIT PARAGRAPH.

      *=======================
      *FUNCTION: EXIT-PROMT
      *=======================
       EXIT-PROMT.

           DISPLAY "Press enter to proceed."
           ACCEPT OMITTED

           EXIT PARAGRAPH.

      *=====================
      *FUNCTION: ADD-STUDENT
      *=====================
       ADD-STUDENT.
           DISPLAY "YOU CHOSE TO ADD STUDENT"
           DISPLAY "ADD STUDENT? (Y/N): "
           ACCEPT WS-ADD-FLAG
           PERFORM CONVERT-FLAG

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
                    DISPLAY "Assign Room: " ACCEPT WS-ASSIGNED-DORM-ID
                    DISPLAY "Rent Amount: " ACCEPT WS-RENT-AMOUNT-PAID
        
                    MOVE WS-NAME TO SI-NAME
                    MOVE WS-AGE TO SI-AGE
                    MOVE WS-GENDER TO SI-GENDER
                    MOVE WS-CONTACT-NUM TO SI-CONTACT-NUM
                    MOVE WS-ASSIGNED-DORM-ID TO SI-ROOM-NUM
                    MOVE WS-RENT-AMOUNT-PAID TO SI-RENT-AMOUNT
        
                    WRITE STUDENT-RECORD
        
                    DISPLAY "ADD ANOTHER? (Y/N):"
                    ACCEPT WS-ADD-FLAG
                    PERFORM CONVERT-FLAG

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
           
           MOVE "N" TO UTIL-EOF
                  PERFORM UNTIL UTIL-EOF = 'Y'
           READ STUDENT-FILE
               AT END
                   MOVE 'Y' TO UTIL-EOF
               NOT AT END
                   DISPLAY "==============="
                   DISPLAY "STUDENT #" SI-NAME
                   DISPLAY "==============="
                   DISPLAY "Name: " SI-NAME
                   DISPLAY "Age: " SI-AGE
                   DISPLAY "Gender: " SI-GENDER
                   DISPLAY "Contact Number: " SI-CONTACT-NUM
                   DISPLAY "Assigned Room: " SI-ROOM-NUM
                   DISPLAY "Rent Amount: " SI-RENT-AMOUNT
                   
                   DISPLAY SPACE
           END-READ
           END-PERFORM

           CLOSE STUDENT-FILE
           
           PERFORM EXIT-PROMT

           EXIT PARAGRAPH.

      *=======================
      *FUNCTION: EDIT-STUDENTS
      *=======================
       EDIT-STUDENTS.
           DISPLAY "YOU CHOSE TO EDIT STUDENTS"
       
           MOVE "Y" TO UTIL-EDIT-AGAIN
       
           PERFORM UNTIL UTIL-EDIT-AGAIN = "N"
       
               MOVE "N" TO UTIL-EDIT-FOUND
       
               DISPLAY "Enter name of the student to edit: "
               ACCEPT UTIL-SEARCH-NAME
       
               OPEN INPUT STUDENT-FILE
                    OUTPUT TEMP-STUDENT-FILE
       
               MOVE "N" TO UTIL-EOF
       
               PERFORM UNTIL UTIL-EOF = "Y"
                   READ STUDENT-FILE
                       AT END
                           MOVE "Y" TO UTIL-EOF
       
                       NOT AT END
                       IF SI-NAME = UTIL-SEARCH-NAME
                           MOVE "Y" TO UTIL-EDIT-FOUND
                           DISPLAY "Editing student: " SI-NAME
       
                           DISPLAY "Edit Name"
                           DISPLAY "(keep empty to unchange)"
                           ACCEPT TEMP-NAME
                           IF TEMP-NAME NOT = SPACES
                               MOVE TEMP-NAME TO SI-NAME
                           END-IF
       
                           DISPLAY "Edit Age"
                           DISPLAY "(keep empty to unchange)"
                           ACCEPT TEMP-AGE
                           IF TEMP-AGE NOT = SPACES
                               MOVE TEMP-AGE TO SI-AGE
                           END-IF
       
                           DISPLAY "Edit Gender"
                           DISPLAY "(keep empty to unchange)"
                           ACCEPT TEMP-GENDER
                           IF TEMP-GENDER NOT = SPACES
                               MOVE TEMP-GENDER TO SI-GENDER
                           END-IF
       
                           DISPLAY "Edit Contact"
                           DISPLAY "(keep empty to unchange)"
                           ACCEPT TEMP-CONTACT-NUM
                           IF TEMP-CONTACT-NUM NOT = SPACES
                               MOVE TEMP-CONTACT-NUM TO SI-CONTACT-NUM
                           END-IF
       
                           DISPLAY "Edit Room"
                           DISPLAY "(keep empty to unchange)"
                           ACCEPT TEMP-ROOM-NUM
                           IF TEMP-ROOM-NUM NOT = SPACES
                               MOVE TEMP-ROOM-NUM TO SI-ROOM-NUM
                           END-IF
       
                           DISPLAY "Edit Rent"
                           DISPLAY "(keep empty to unchange)"
                           ACCEPT TEMP-RENT-AMOUNT
                           IF TEMP-RENT-AMOUNT NOT = SPACES
                               MOVE TEMP-RENT-AMOUNT TO SI-RENT-AMOUNT
                           END-IF
                       END-IF
       
                       WRITE TEMP-STUDENT-RECORD FROM STUDENT-RECORD
                   END-READ
               END-PERFORM
       
               CLOSE STUDENT-FILE TEMP-STUDENT-FILE
       
               PERFORM SAVE-STUDENT-RECORD
       
               IF UTIL-EDIT-FOUND = "N"
                   DISPLAY "Student not found."
               ELSE
                   DISPLAY "Student record updated."
               END-IF
       
               DISPLAY "Edit another student? (Y/N): "
               ACCEPT UTIL-EDIT-AGAIN
       
           END-PERFORM
       
           PERFORM EXIT-PROMT
           EXIT PARAGRAPH.
       
             
      *=========================
      *FUNCTION: DELETE-STUDENTS
      *=========================
       DELETE-STUDENTS.
           MOVE "Y" TO UTIL-DELETE-CHOICE

           PERFORM UNTIL UTIL-DELETE-CHOICE = "N"

               DISPLAY "YOU CHOSE TO DELETE STUDENTS"
               DISPLAY "PLEASE ENTER THE NAME OF THE STUDENT TO DELETE:"
               ACCEPT UTIL-SEARCH-NAME

               MOVE "N" TO UTIL-EOF
               MOVE "N" TO UTIL-DELETE-FOUND

               OPEN INPUT STUDENT-FILE
                    OUTPUT TEMP-STUDENT-FILE

               PERFORM UNTIL UTIL-EOF = "Y"
               READ STUDENT-FILE
                   AT END
                       MOVE "Y" TO UTIL-EOF
                   NOT AT END
                       IF SI-NAME = UTIL-SEARCH-NAME
                           DISPLAY "DELETING STUDENT: " SI-NAME
                           MOVE "Y" TO UTIL-DELETE-FOUND
                       ELSE
                           WRITE TEMP-STUDENT-RECORD FROM STUDENT-RECORD
                       END-IF
               END-READ
               END-PERFORM

               CLOSE STUDENT-FILE TEMP-STUDENT-FILE

               IF UTIL-DELETE-FOUND = "Y"
                   PERFORM SAVE-STUDENT-RECORD
                   DISPLAY "STUDENT SUCCESSFULLY DELETED."
               ELSE
                   DISPLAY "STUDENT NOT FOUND."
               END-IF

               DISPLAY "DELETE ANOTHER STUDENT? (Y/N): "
               ACCEPT UTIL-DELETE-CHOICE

               IF UTIL-DELETE-CHOICE NOT = "Y"
                   MOVE "N" TO UTIL-DELETE-CHOICE
               END-IF

           END-PERFORM

           PERFORM EXIT-PROMT
           EXIT PARAGRAPH.


      *============================
      *FUNCTION: CONVERT FLAG 
      *============================
       CONVERT-FLAG.
           INSPECT WS-ADD-FLAG CONVERTING 'y' TO 'Y'
           INSPECT WS-ADD-FLAG CONVERTING 'n' TO 'N'
           EXIT PARAGRAPH.

       
      *============================
      *FUNCTION: SAVE STUDENT RECORD 
      *============================
       SAVE-STUDENT-RECORD.
           ACCEPT UTIL-OS-NAME FROM ENVIRONMENT "OS"

           IF UTIL-OS-NAME = "Windows_NT"
               CALL "SYSTEM" USING "del students.dat"
               CALL "SYSTEM" USING "rename temp.dat students.dat"

           ELSE
               CALL "SYSTEM" USING "rm students.dat"
               CALL "SYSTEM" USING "mv temp.dat students.dat"

           END-IF
           EXIT PARAGRAPH.
           