       IDENTIFICATION DIVISION.
       PROGRAM-ID. DORM-BPIM-TRACKER.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
               FILE-CONTROL.
           SELECT TENANT-FILE ASSIGN TO "tenants.dat"
-              ORGANIZATION IS LINE SEQUENTIAL.

           SELECT TEMP-TENANT-FILE ASSIGN TO "temp.dat"
-              ORGANIZATION IS LINE SEQUENTIAL.

           SELECT DORM-FILE ASSIGN TO "dorm.dat"
-                 ORGANIZATION IS INDEXED
-                 ACCESS MODE IS DYNAMIC
-                 RECORD KEY IS DI-ID
-                 FILE STATUS IS WS-DORM-FILE-STATUS.

           SELECT HISTORY-FILE ASSIGN TO "history.dat"
-              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

      *TENANT RECORD
           FD  TENANT-FILE.
       01  TENANT-RECORD.
           05  SI-NAME                             PIC X(50).
           05  SI-AGE                              PIC 9(2).
           05  SI-GENDER                           PIC X(15).
           05  SI-CONTACT-NUM                      PIC X(12).
           05  SI-ASSIGNED-D-ID                    PIC X(20).
           

      *TEMP TENANT RECORD
       FD  TEMP-TENANT-FILE.
       01  TEMP-TENANT-RECORD.
           05  SI-ID                               PIC X(10).
           05  TEMP-NAME                           PIC X(50).
           05  TEMP-AGE                            PIC 9(2).
           05  TEMP-GENDER                         PIC X(15).         
           05  TEMP-CONTACT-NUM                    PIC X(12).
           05  TEMP-ASSIGNED-D-ID                  PIC X(20).

      *DORM FILE
           FD DORM-FILE.
       01  DORM-RECORD.
           05  DI-ID                               PIC X(10).
           05  DI-FLOOR                            PIC X(2).
           05  DI-ROOM-NUM                         PIC X(3).
           05  DI-RENT-AMOUNT                      PIC X(6).
           05  DI-RENT-DUE                         PIC X(10).
           05  DI-RENT-LAST-PAID                   PIC X(10).
           05  DI-RENT-PAID-AMOUNT                 PIC X(6).    
           05  DI-ELECTRICITY-AMT                  PIC X(7).
           05  DI-ELECTRICITY-DUE                  PIC X(10).
           05  DI-ELECTRICITY-LAST                 PIC X(10).
           05  DI-ELECTRICITY-PAID-AMT             PIC X(7).
           05  DI-WATER-AMT                        PIC X(7).
           05  DI-WATER-DUE                        PIC X(10).
           05  DI-WATER-LAST-PAID                  PIC X(10).
           05  DI-WATER-PAID-AMT                   PIC X(7).
           05  DI-STATUS                           PIC X(10).

      *PAYMENT HISTORY RECORD
       FD  HISTORY-FILE.
       01  PAYMENT-HISTORY-RECORD.
           05  PH-TRANSACTION-ID          PIC X(15).
           05  PH-DORM-ID                 PIC X(10).
           05  PH-PAYMENT-TYPE            PIC X(15). 
           05  PH-AMOUNT-DUE              PIC X(7).
           05  PH-AMOUNT-PAID             PIC X(7).
           05  PH-PAYMENT-DATE            PIC X(10). 
           05  PH-DUE-DATE                PIC X(10). 
           05  PH-TIMESTAMP               PIC X(19). 
           05  PH-NOTES                   PIC X(100).


      *WS
       WORKING-STORAGE SECTION.
      *UTIL
       01  UTIL-OS-NAME                            PIC X(50).
       01  UTIL-CLEAR-COMMAND                      PIC X(10).
       01  UTIL-SM-CHOICE                          PIC 9.
       01  UTIL-MM-CHOICE                          PIC 9.
       01  UTIL-DM-CHOICE                          PIC 9.
       01  UTIL-PH-CHOICE                          PIC 9.
       01  UTIL-SEARCH-NAME                        PIC X(50).
       01  UTIL-EOF                                PIC X VALUE 'N'.
       01  UTIL-EDIT-AGAIN                         PIC X VALUE "Y".
       01  UTIL-EDIT-FOUND                         PIC X VALUE "N".
       01  UTIL-DELETE-CHOICE                      PIC X VALUE "Y".
       01  UTIL-DELETE-FOUND                       PIC X VALUE "N".
       01  UTIL-FLOOR-N                            PIC 99.
       01  UTIL-ROOM-N                             PIC 999.
       01  UTIL-SEARCH-DORM-ID                     PIC X(10). 
       01  UTIL-DELETE-AGAIN                       PIC X VALUE "N".
       01  UTIL-CONFIRM-DELETE                     PIC X VALUE "N". 
       01  UTIL-PM-CHOICE                          PIC X.             




      *WS TENANT
       01  WS-TENANT-ID                           PIC X(10).
       01  WS-NAME                                 PIC X(50).
       01  WS-AGE                                  PIC 9(2).
       01  WS-GENDER                               PIC X(15).
       01  WS-CONTACT-NUM                          PIC X(12).
       01  WS-ASSIGNED-D-ID                        PIC X(10).
       01  WS-RENT-AMOUNT-PAID                     PIC X(6).
       

      *WS DORM
       01  WS-DORM-ID                              PIC X(10).
       01  WS-DORM-RENT-AMOUNT                     PIC X(6).
       01  WS-DORM-ELECTRICITY                     PIC 9(4)V99.
       01  WS-DORM-WATER                           PIC 9(4)V99.
       01  WS-DORM-STATUS                          PIC X(10).
       01  WS-DORM-DATE-PAID                       PIC X(10).
       01  WS-VALID-ROOM-FLAG                      PIC X VALUE "N".
       01  WS-DORM-FILE-STATUS                     PIC XX.

      *WS TEMP DORM
       01  TEMP-FLOOR                              PIC X(2).
       01  TEMP-ROOM-NUM                           PIC X(3).
       01  TEMP-RENT-AMOUNT                        PIC X(6).
       01  TEMP-RENT-DUE                           PIC X(10).
       01  TEMP-RENT-LAST-PAID                     PIC X(10).
       01  TEMP-ELECTRICITY-AMT                    PIC X(7).
       01  TEMP-ELECTRICITY-DUE                    PIC X(10).
       01  TEMP-ELECTRICITY-LAST                   PIC X(10).
       01  TEMP-WATER-AMT                          PIC X(7).
       01  TEMP-WATER-DUE                          PIC X(10).
       01  TEMP-WATER-LAST-PAID                    PIC X(10).
       01  TEMP-STATUS                             PIC X(10).
       01  TEMP-RENT-PAID                          PIC X(6).
       01  TEMP-ELECTRICITY-PAID                   PIC X(7).
       01  TEMP-WATER-PAID                         PIC X(7).
       
      *PAYMENT
       01  WS-CONFIRM-PAYMENT                      PIC X.
       
       
      *WS ADD
       01  WS-ADD-FLAG                             PIC X(2).
       01  WS-AVAILABLE-ROOM-COUNT                 PIC 9(4) VALUE 0.
       01  WS-CANCEL-FLAG                          PIC X VALUE "N".
       
      *PAYMENT HISTORY
       01  WS-PAYMENT-HISTORY-COUNT                PIC 9(8) VALUE 0.
       01  WS-TRANSACTION-ID                       PIC X(30).
       01  WS-TRANSACTION-COUNTER                  PIC 9(8) VALUE 1000.
       01  WS-CURRENT-DATE                         PIC X(10).
       01  WS-CURRENT-TIME                         PIC X(8).
       01  WS-TIMESTAMP                            PIC X(19).      
       01  WS-PAYMENT-TYPE                         PIC X(15).
       01  WS-AMOUNT-DUE                           PIC X(7).
       01  WS-AMOUNT-PAID                          PIC X(7).
       01  WS-PAYMENT-DATE-LOG                     PIC X(10).
       01  WS-DUE-DATE-LOG                         PIC X(10).
       01  WS-DORM-ID-TABLE.
           05  WS-DORM-ID-ENTRY                    OCCURS 100 TIMES.
               10  WS-STORED-DORM-ID               PIC X(10).
       01  WS-DORM-ID-COUNT                        PIC 9(3) VALUE 0.
       01  WS-DORM-INDEX                           PIC 9(3).
       01  WS-DORM-FOUND                           PIC X VALUE "N".
       

       PROCEDURE DIVISION.
           PERFORM MAIN-MENU.
           STOP RUN.


      *============================
      *FUNCTION: MAIN MENU
      *============================
       MAIN-MENU.
           
           PERFORM UNTIL UTIL-MM-CHOICE = 5
           PERFORM CLEAR-SCREEN
           MOVE 0 TO UTIL-MM-CHOICE
           DISPLAY "==========================="
           DISPLAY "         MAIN MENU         "
           DISPLAY "==========================="
           DISPLAY "1 - TENANT MANAGEMENT"
           DISPLAY "2 - DORM MANAGEMENT"
           DISPLAY "3 - RECORD PAYMENT"
           DISPLAY "4 - PAYMENT HISTORY"
           DISPLAY "5 - EXIT"

           DISPLAY "ENTER CHOICE (1 - 5): "
           ACCEPT UTIL-MM-CHOICE

           EVALUATE UTIL-MM-CHOICE
               WHEN 1
                   MOVE 0 TO UTIL-SM-CHOICE
                   PERFORM TENANT-MANAGEMENT

               WHEN 2
                   MOVE 0 TO UTIL-DM-CHOICE
                   PERFORM DORM-MANAGEMENT
               WHEN 3
                   MOVE 0 TO UTIL-PM-CHOICE
                   PERFORM PAYMENT-MANAGEMENT
               WHEN 4
                   MOVE 0 TO UTIL-PH-CHOICE
                   PERFORM VIEW-PAYMENT-HISTORY
               WHEN 5
                   DISPLAY "EXITING..."
                   PERFORM EXIT-PROMT
               WHEN OTHER
                   DISPLAY "INVALID INPUT, PLEASE TRY AGAIN"
                   PERFORM EXIT-PROMT
           END-EVALUATE

           END-PERFORM
           EXIT PARAGRAPH.

      *============================
      *FUNCTION: VIEW PAYMENT HISTORY
      *============================
       VIEW-PAYMENT-HISTORY.
           PERFORM UNTIL UTIL-PH-CHOICE = 3
           PERFORM CLEAR-SCREEN

           DISPLAY "==============================="
           DISPLAY "   PAYMENT HISTORY SEARCH"
           DISPLAY "==============================="
           DISPLAY "1 - View all payments"
           DISPLAY "2 - View payments by Dorm ID"
           DISPLAY "3 - Back to main menu"

           DISPLAY "Enter choice(1 - 3): "
           ACCEPT UTIL-PH-CHOICE
           
           EVALUATE UTIL-PH-CHOICE
               WHEN 1
                   PERFORM CLEAR-SCREEN
                   PERFORM DISPLAY-ALL-PAYMENT-HISTORY
               WHEN 2
                   PERFORM CLEAR-SCREEN
                   PERFORM DISPLAY-DORM-PAYMENT-HISTORY
               WHEN 3
                   DISPLAY "EXITING PAYMENT HISTORY..."
                   PERFORM EXIT-PROMT
               WHEN OTHER 
                   DISPLAY "INVALID INPUT "
                   PERFORM EXIT-PROMT
           END-EVALUATE
           
           END-PERFORM
           
           EXIT PARAGRAPH.
       
      *=====================================
      *FUNCTION: DISPLAY ALL PAYMENT HISTORY
      *=====================================
       DISPLAY-ALL-PAYMENT-HISTORY.
           OPEN INPUT HISTORY-FILE
           
           MOVE "N" TO UTIL-EOF
           MOVE 0 TO WS-PAYMENT-HISTORY-COUNT
           
           DISPLAY " "
           DISPLAY "================================================"
           DISPLAY "         ALL PAYMENT TRANSACTIONS"
           DISPLAY "================================================"
           DISPLAY " "
           
           PERFORM UNTIL UTIL-EOF = "Y"
               READ HISTORY-FILE
               AT END
                   MOVE "Y" TO UTIL-EOF
               NOT AT END
                   ADD 1 TO WS-PAYMENT-HISTORY-COUNT
                   DISPLAY "Transaction ID: " PH-TRANSACTION-ID
                   DISPLAY "  Dorm ID     : " PH-DORM-ID
                   DISPLAY "  Type        : " PH-PAYMENT-TYPE
                   DISPLAY "  Amount Due  : " PH-AMOUNT-DUE
                   DISPLAY "  Amount Paid : " PH-AMOUNT-PAID
                   DISPLAY "  Payment Date: " PH-PAYMENT-DATE
                   DISPLAY "  Due Date    : " PH-DUE-DATE
                   DISPLAY "  Recorded    : " PH-TIMESTAMP
                   DISPLAY "  Notes       : " PH-NOTES
                   DISPLAY "-------------------------------------------"
               END-READ
           END-PERFORM
           
           CLOSE HISTORY-FILE
           
           DISPLAY " "
           DISPLAY "Total transactions: " WS-PAYMENT-HISTORY-COUNT
           DISPLAY " "
           PERFORM EXIT-PROMT
           EXIT PARAGRAPH.
       
      *=======================================
      *FUNCTION: DISPLAY DORM PAYMENT HISTORY
      *=======================================
       DISPLAY-DORM-PAYMENT-HISTORY.
           PERFORM CLEAR-SCREEN
    
           DISPLAY " "
           DISPLAY "================================================"
           DISPLAY "   AVAILABLE DORM IDS (WITH PAYMENT HISTORY)"
           DISPLAY "================================================"
           DISPLAY " "
           
           *> First, display all unique dorm IDs from history
           OPEN INPUT HISTORY-FILE
           
           MOVE "N" TO UTIL-EOF
           MOVE 0 TO WS-DORM-ID-COUNT
           
           *> Read through file and collect unique dorm IDs
           PERFORM UNTIL UTIL-EOF = "Y"
               READ HISTORY-FILE
                   AT END
                       MOVE "Y" TO UTIL-EOF
                   NOT AT END
                       *> Check if dorm ID already in table
                       MOVE "N" TO WS-DORM-FOUND
                       PERFORM VARYING WS-DORM-INDEX FROM 1 BY 1
                           UNTIL WS-DORM-INDEX > WS-DORM-ID-COUNT
                           IF WS-STORED-DORM-ID(WS-DORM-INDEX) = 
                              PH-DORM-ID
                               MOVE "Y" TO WS-DORM-FOUND
                           END-IF
                       END-PERFORM
                       
                       *> If not found, add it
                       IF WS-DORM-FOUND = "N"
                           IF WS-DORM-ID-COUNT < 100
                               ADD 1 TO WS-DORM-ID-COUNT
                               MOVE PH-DORM-ID TO 
                                   WS-STORED-DORM-ID(WS-DORM-ID-COUNT)
                           END-IF
                       END-IF
               END-READ
           END-PERFORM
           
           CLOSE HISTORY-FILE
           
           *> Display the unique list
           IF WS-DORM-ID-COUNT = 0
               DISPLAY "No dorms with payment history found."
               DISPLAY " "
               PERFORM EXIT-PROMT
               EXIT PARAGRAPH
           END-IF
           
           PERFORM VARYING WS-DORM-INDEX FROM 1 BY 1
               UNTIL WS-DORM-INDEX > WS-DORM-ID-COUNT
               DISPLAY "  " WS-STORED-DORM-ID(WS-DORM-INDEX)
           END-PERFORM
           
           DISPLAY " "
           DISPLAY "Total dorms with history: " WS-DORM-ID-COUNT
           DISPLAY "================================================"
           DISPLAY " "
           
           *> Now prompt user to enter dorm ID
           DISPLAY "Enter Dorm ID to view payments: "
           ACCEPT UTIL-SEARCH-DORM-ID
           
           *> Validate that entered dorm ID exists in history
           MOVE "N" TO WS-DORM-FOUND
           PERFORM VARYING WS-DORM-INDEX FROM 1 BY 1
               UNTIL WS-DORM-INDEX > WS-DORM-ID-COUNT
               IF WS-STORED-DORM-ID(WS-DORM-INDEX) = 
                  UTIL-SEARCH-DORM-ID
                   MOVE "Y" TO WS-DORM-FOUND
               END-IF
           END-PERFORM
           
           IF WS-DORM-FOUND = "N"
               DISPLAY "ERROR: Dorm ID " UTIL-SEARCH-DORM-ID 
                       " not found in payment history."
               DISPLAY " "
               PERFORM EXIT-PROMT
               EXIT PARAGRAPH
           END-IF
           
           *> Display detailed history for selected dorm
           OPEN INPUT HISTORY-FILE
           
           MOVE "N" TO UTIL-EOF
           MOVE 0 TO WS-PAYMENT-HISTORY-COUNT
           
           DISPLAY " "
           DISPLAY "================================================"
           DISPLAY "  PAYMENT HISTORY FOR DORM: " UTIL-SEARCH-DORM-ID
           DISPLAY "================================================"
           DISPLAY " "
           
           PERFORM UNTIL UTIL-EOF = "Y"
               READ HISTORY-FILE
                   AT END
                       MOVE "Y" TO UTIL-EOF
                   NOT AT END
                       IF PH-DORM-ID = UTIL-SEARCH-DORM-ID
                           ADD 1 TO WS-PAYMENT-HISTORY-COUNT
                           DISPLAY "Transaction ID: " PH-TRANSACTION-ID
                           DISPLAY "  Type        : " PH-PAYMENT-TYPE
                           DISPLAY "  Amount Due  : " PH-AMOUNT-DUE
                           DISPLAY "  Amount Paid : " PH-AMOUNT-PAID
                           DISPLAY "  Payment Date: " PH-PAYMENT-DATE
                           DISPLAY "  Due Date    : " PH-DUE-DATE
                           DISPLAY "  Recorded    : " PH-TIMESTAMP
                           DISPLAY "-----------------------------------"
                       END-IF
               END-READ
           END-PERFORM
           
           CLOSE HISTORY-FILE
           
           DISPLAY " "
           DISPLAY "Total transactions for this dorm: " 
                   WS-PAYMENT-HISTORY-COUNT
           DISPLAY " "
           PERFORM EXIT-PROMT
           EXIT PARAGRAPH.
       
       
       
      *============================
      *FUNCTION: PAYMENT MANAGEMENT
      *============================
       PAYMENT-MANAGEMENT.
           PERFORM UNTIL UTIL-PM-CHOICE = 4
           PERFORM CLEAR-SCREEN
           
           DISPLAY "=============================="
           DISPLAY "    PAYMENT MANAGEMENT"
           DISPLAY "=============================="
           DISPLAY "1. PAY RENT"
           DISPLAY "2. PAY ELECTRICITY"
           DISPLAY "3. PAY WATER"
           DISPLAY "4. BACK TO MAIN MENU"
           DISPLAY "=============================="
           DISPLAY "Enter your choice: "
           ACCEPT UTIL-PM-CHOICE
           
           EVALUATE UTIL-PM-CHOICE
               WHEN 1
                   PERFORM CLEAR-SCREEN
                   PERFORM PAY-RENT
               WHEN 2
                   PERFORM CLEAR-SCREEN
                   PERFORM PAY-ELECTRICITY
               WHEN 3
                   PERFORM CLEAR-SCREEN
                   PERFORM PAY-WATER
               WHEN 4
                   DISPLAY "Returning to main menu..."
                   PERFORM EXIT-PROMT
               WHEN OTHER
                   DISPLAY "INVALID CHOICE"
                   PERFORM EXIT-PROMT
           END-EVALUATE
           END-PERFORM
           EXIT PARAGRAPH.
      *============================
      *FUNCTION: PAY RENT
      *============================
       PAY-RENT.
           DISPLAY "==============================="
           DISPLAY "        PAY RENT"
           DISPLAY "==============================="
           
           *> Display all occupied dorms
           DISPLAY " "
           DISPLAY "OCCUPIED DORMS:"
           DISPLAY "---------------------------------------"
           OPEN INPUT DORM-FILE
           
           MOVE LOW-VALUES TO DI-ID
           START DORM-FILE KEY >= DI-ID
               INVALID KEY
                   DISPLAY "NO DORMS IN SYSTEM"
           END-START
   
           PERFORM UNTIL WS-DORM-FILE-STATUS NOT = "00"
               READ DORM-FILE NEXT
                   AT END
                       CONTINUE
                   NOT AT END
                       IF DI-STATUS = "OCCUPIED"
                           DISPLAY "ID: " DI-ID 
                                   " | Last Amount Due: " DI-RENT-AMOUNT
                                   " | Due: " DI-RENT-DUE
                                   " | Last Paid: " DI-RENT-LAST-PAID
                                   " | Amt Paid: " DI-RENT-PAID-AMOUNT
                       END-IF
               END-READ
           END-PERFORM
           
           CLOSE DORM-FILE
           DISPLAY "---------------------------------------"
           DISPLAY " "
           
           DISPLAY "Enter Dorm ID (or EXIT to cancel): "
           ACCEPT UTIL-SEARCH-DORM-ID
           
           *> Convert to uppercase
           INSPECT UTIL-SEARCH-DORM-ID 
               CONVERTING "abcdefghijklmnopqrstuvwxyz"
               TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
           
           IF UTIL-SEARCH-DORM-ID = "EXIT"
               DISPLAY "Payment cancelled."
               PERFORM EXIT-PROMT
           ELSE
               OPEN I-O DORM-FILE
               
               MOVE UTIL-SEARCH-DORM-ID TO DI-ID
               READ DORM-FILE
               INVALID KEY
                   DISPLAY "ERROR: DORM NOT FOUND."
               NOT INVALID KEY
                   DISPLAY "Last Amount Due: " DI-RENT-AMOUNT
                   DISPLAY "Last Due Date: " DI-RENT-DUE
                   DISPLAY " "
                   
                   DISPLAY "Enter NEW amount due for this month: "
                   ACCEPT TEMP-RENT-AMOUNT
                   MOVE TEMP-RENT-AMOUNT TO DI-RENT-AMOUNT
                   
                   DISPLAY "Enter amount paid: "
                   ACCEPT TEMP-RENT-PAID
                   
                   DISPLAY "Enter payment date (YYYY-MM-DD): "
                   MOVE SPACES TO WS-DORM-DATE-PAID
                   ACCEPT WS-DORM-DATE-PAID
                   
                   DISPLAY "Enter next due date (YYYY-MM-DD): "
                   MOVE SPACES TO DI-RENT-DUE
                   ACCEPT DI-RENT-DUE
                   
                   DISPLAY " "
                   DISPLAY "SUMMARY:"
                   DISPLAY "  Amount Due: " DI-RENT-AMOUNT
                   DISPLAY "  Amount Paid: " TEMP-RENT-PAID
                   DISPLAY "  Payment Date: " WS-DORM-DATE-PAID
                   DISPLAY "  Next Due Date: " DI-RENT-DUE
                   DISPLAY " "
                   DISPLAY "Confirm rent payment (Y/N): "
                   ACCEPT WS-CONFIRM-PAYMENT
                   INSPECT WS-CONFIRM-PAYMENT 
                       CONVERTING "abcdefghijklmnopqrstuvwxyz"
                       TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                       
                   IF WS-CONFIRM-PAYMENT = "Y"
                       MOVE WS-DORM-DATE-PAID TO DI-RENT-LAST-PAID
                       MOVE TEMP-RENT-PAID TO DI-RENT-PAID-AMOUNT
                       
                       *> Set working storage variables FIRST
                       MOVE UTIL-SEARCH-DORM-ID TO WS-DORM-ID
                       MOVE "RENT" TO WS-PAYMENT-TYPE
                       MOVE TEMP-RENT-AMOUNT TO WS-AMOUNT-DUE
                       MOVE TEMP-RENT-PAID TO WS-AMOUNT-PAID
                       MOVE WS-DORM-DATE-PAID TO WS-PAYMENT-DATE-LOG
                       MOVE DI-RENT-DUE TO WS-DUE-DATE-LOG
                       
                       REWRITE DORM-RECORD
                           INVALID KEY
                               DISPLAY "ERROR: Could not update."
                           NOT INVALID KEY
                               *> NOW call the logging function
                               PERFORM LOG-PAYMENT-HISTORY
                               
                               DISPLAY "Rent payment recorded!"
                       END-REWRITE
                   ELSE
                       DISPLAY "Payment cancelled."
                   END-IF
                   
               END-READ
               
               CLOSE DORM-FILE
           END-IF
           
           PERFORM EXIT-PROMT
           EXIT PARAGRAPH.
      
      *============================
      *FUNCTION: PAY ELECTRICITY
      *============================
           PAY-ELECTRICITY.
           DISPLAY "==============================="
           DISPLAY "      PAY ELECTRICITY"
           DISPLAY "==============================="
           
           *> Display all occupied dorms
           DISPLAY " "
           DISPLAY "OCCUPIED DORMS:"
           DISPLAY "---------------------------------------"
           OPEN INPUT DORM-FILE
           
           MOVE LOW-VALUES TO DI-ID
           START DORM-FILE KEY >= DI-ID
               INVALID KEY
                   DISPLAY "NO DORMS IN SYSTEM"
           END-START
   
           PERFORM UNTIL WS-DORM-FILE-STATUS NOT = "00"
               READ DORM-FILE NEXT
                   AT END
                       CONTINUE
                   NOT AT END
                       IF DI-STATUS = "OCCUPIED"
                           DISPLAY "ID: " DI-ID 
                                   " | Last Amount Due: " 
                                   DI-ELECTRICITY-AMT
                                   " | Due: " DI-ELECTRICITY-DUE
                                   " | Last Paid: " 
                                   DI-ELECTRICITY-LAST
                                   " | Amt Paid: " 
                                   DI-ELECTRICITY-PAID-AMT
                       END-IF
               END-READ
           END-PERFORM
           
           CLOSE DORM-FILE
           DISPLAY "---------------------------------------"
           DISPLAY " "
           
           DISPLAY "Enter Dorm ID (or EXIT to cancel): "
           ACCEPT UTIL-SEARCH-DORM-ID
           
           *> Convert to uppercase
           INSPECT UTIL-SEARCH-DORM-ID 
               CONVERTING "abcdefghijklmnopqrstuvwxyz"
               TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
           
           IF UTIL-SEARCH-DORM-ID = "EXIT"
               DISPLAY "Payment cancelled."
               PERFORM EXIT-PROMT
           ELSE
               OPEN I-O DORM-FILE
               
               MOVE UTIL-SEARCH-DORM-ID TO DI-ID
               READ DORM-FILE
                   INVALID KEY
                       DISPLAY "ERROR: DORM NOT FOUND."
                   NOT INVALID KEY
                       DISPLAY "Last Amount Due: " DI-ELECTRICITY-AMT
                       DISPLAY "Last Due Date: " DI-ELECTRICITY-DUE
                       DISPLAY " "
                       
                       DISPLAY "Enter NEW electricity bill amount: "
                       ACCEPT TEMP-ELECTRICITY-AMT
                       MOVE TEMP-ELECTRICITY-AMT TO DI-ELECTRICITY-AMT
                       
                       DISPLAY "Enter amount paid: "
                       ACCEPT TEMP-ELECTRICITY-PAID
                       
                       DISPLAY "Enter payment date (YYYY-MM-DD): "
                       MOVE SPACES TO WS-DORM-DATE-PAID
                       ACCEPT WS-DORM-DATE-PAID
                       
                       DISPLAY "Enter next due date (YYYY-MM-DD): "
                       MOVE SPACES TO DI-ELECTRICITY-DUE
                       ACCEPT DI-ELECTRICITY-DUE
                       
                       DISPLAY " "
                       DISPLAY "SUMMARY:"
                       DISPLAY "  Amount Due: " DI-ELECTRICITY-AMT
                       DISPLAY "  Amount Paid: " TEMP-ELECTRICITY-PAID
                       DISPLAY "  Payment Date: " WS-DORM-DATE-PAID
                       DISPLAY "  Next Due Date: " DI-ELECTRICITY-DUE
                       DISPLAY " "
                       DISPLAY "Confirm payment (Y/N): "
                       ACCEPT WS-CONFIRM-PAYMENT
                       INSPECT WS-CONFIRM-PAYMENT 
                           CONVERTING "abcdefghijklmnopqrstuvwxyz"
                           TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                       
                       IF WS-CONFIRM-PAYMENT = "Y"
                           MOVE WS-DORM-DATE-PAID 
-                              TO DI-ELECTRICITY-LAST
                           MOVE TEMP-ELECTRICITY-PAID 
-                              TO DI-ELECTRICITY-PAID-AMT
                           
                           *> Set working storage variables FIRST
                           MOVE UTIL-SEARCH-DORM-ID TO WS-DORM-ID
                           MOVE "ELECTRICITY" TO WS-PAYMENT-TYPE
                           MOVE TEMP-ELECTRICITY-AMT TO WS-AMOUNT-DUE
                           MOVE TEMP-ELECTRICITY-PAID TO WS-AMOUNT-PAID
                           MOVE WS-DORM-DATE-PAID TO WS-PAYMENT-DATE-LOG
                           MOVE DI-ELECTRICITY-DUE TO WS-DUE-DATE-LOG
                           
                           REWRITE DORM-RECORD
                           INVALID KEY
                               DISPLAY "ERROR: Could not update."
                           NOT INVALID KEY
                               PERFORM LOG-PAYMENT-HISTORY
                               
                               DISPLAY "Electricity payment recorded!"
                           END-REWRITE
                       ELSE
                           DISPLAY "Payment cancelled."
                       END-IF
                       
               END-READ
               
               CLOSE DORM-FILE
           END-IF
           
           PERFORM EXIT-PROMT
           EXIT PARAGRAPH.
       
      *============================
      *FUNCTION: PAY WATER
      *============================
           PAY-WATER.
           DISPLAY "==============================="
           DISPLAY "        PAY WATER"
           DISPLAY "==============================="
           
           DISPLAY " "
           DISPLAY "OCCUPIED DORMS:"
           DISPLAY "---------------------------------------"
           OPEN INPUT DORM-FILE
           
           MOVE LOW-VALUES TO DI-ID
           START DORM-FILE KEY >= DI-ID
               INVALID KEY
                   DISPLAY "NO DORMS IN SYSTEM"
           END-START
   
           PERFORM UNTIL WS-DORM-FILE-STATUS NOT = "00"
               READ DORM-FILE NEXT
                   AT END
                       CONTINUE
                   NOT AT END
                       IF DI-STATUS = "OCCUPIED"
                           DISPLAY "ID: " DI-ID 
                                   " | Last Amount Due: " DI-WATER-AMT
                                   " | Due: " DI-WATER-DUE
                                   " | Last Paid: " DI-WATER-LAST-PAID
                                   " | Amt Paid: " DI-WATER-PAID-AMT
                       END-IF
               END-READ
           END-PERFORM
           
           CLOSE DORM-FILE
           DISPLAY "---------------------------------------"
           DISPLAY " "
           
           DISPLAY "Enter Dorm ID (or EXIT to cancel): "
           ACCEPT UTIL-SEARCH-DORM-ID
           
           INSPECT UTIL-SEARCH-DORM-ID 
               CONVERTING "abcdefghijklmnopqrstuvwxyz"
               TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
           
           IF UTIL-SEARCH-DORM-ID = "EXIT"
               DISPLAY "Payment cancelled."
               PERFORM EXIT-PROMT
           ELSE
               OPEN I-O DORM-FILE
               
               MOVE UTIL-SEARCH-DORM-ID TO DI-ID
               READ DORM-FILE
                   INVALID KEY
                       DISPLAY "ERROR: DORM NOT FOUND."
                   NOT INVALID KEY
                       DISPLAY "Last Amount Due: " DI-WATER-AMT
                       DISPLAY "Last Due Date: " DI-WATER-DUE
                       DISPLAY " "
                       
                       DISPLAY "Enter NEW WATER bill amount: "
                       ACCEPT TEMP-WATER-AMT
                       MOVE TEMP-WATER-AMT TO DI-WATER-AMT
                       
                       DISPLAY "Enter amount paid: "
                       ACCEPT TEMP-WATER-PAID
                       
                       DISPLAY "Enter payment date (YYYY-MM-DD): "
                       MOVE SPACES TO WS-DORM-DATE-PAID
                       ACCEPT WS-DORM-DATE-PAID
                       
                       DISPLAY "Enter next due date (YYYY-MM-DD): "
                       MOVE SPACES TO DI-WATER-DUE
                       ACCEPT DI-WATER-DUE
                       
                       DISPLAY " "
                       DISPLAY "SUMMARY:"
                       DISPLAY "  Amount Due: " DI-WATER-AMT
                       DISPLAY "  Amount Paid: " TEMP-WATER-PAID
                       DISPLAY "  Payment Date: " WS-DORM-DATE-PAID
                       DISPLAY "  Next Due Date: " DI-WATER-DUE
                       DISPLAY " "
                       DISPLAY "Confirm payment (Y/N): "
                       ACCEPT WS-CONFIRM-PAYMENT
                       INSPECT WS-CONFIRM-PAYMENT 
                           CONVERTING "abcdefghijklmnopqrstuvwxyz"
                           TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                       
                    IF WS-CONFIRM-PAYMENT = "Y"
                       MOVE WS-DORM-DATE-PAID TO DI-WATER-LAST-PAID
                       MOVE TEMP-WATER-PAID TO DI-WATER-PAID-AMT
                       
                       *> Set working storage variables FIRST
                       MOVE UTIL-SEARCH-DORM-ID TO WS-DORM-ID
                       MOVE "WATER" TO WS-PAYMENT-TYPE
                       MOVE TEMP-WATER-AMT TO WS-AMOUNT-DUE
                       MOVE TEMP-WATER-PAID TO WS-AMOUNT-PAID
                       MOVE WS-DORM-DATE-PAID TO WS-PAYMENT-DATE-LOG
                       MOVE DI-WATER-DUE TO WS-DUE-DATE-LOG
                       
                       REWRITE DORM-RECORD
                           INVALID KEY
                               DISPLAY "ERROR: Could not update."
                           NOT INVALID KEY
                               PERFORM LOG-PAYMENT-HISTORY
                               
                               DISPLAY "Water payment recorded!"
                       END-REWRITE
                   ELSE
                       DISPLAY "Payment cancelled."
                   END-IF                 
                   
               END-READ
               
               CLOSE DORM-FILE
           END-IF
           
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
                   PERFORM EDIT-DORM
                   
               WHEN 4
                   PERFORM CLEAR-SCREEN
                   PERFORM DELETE-DORM

               WHEN 5
                   DISPLAY "EXITING DORM MANAGEMENT..."
                   PERFORM EXIT-PROMT

               WHEN OTHER
                   DISPLAY "INVALID INPUT, TRY AGAIN"
                   PERFORM EXIT-PROMT
           END-EVALUATE
           
           END-PERFORM
           EXIT PARAGRAPH.

      *============================
      *FUNCTION: ADD DORM 
      *============================
       ADD-DORM.
           DISPLAY "YOU CHOSE TO ADD DORM"
           
           
           DISPLAY "ADD DORM? (Y/N): "
           ACCEPT WS-ADD-FLAG
           PERFORM CONVERT-FLAG
       
           
           PERFORM UNTIL WS-ADD-FLAG = "Y" OR WS-ADD-FLAG = "N"
               DISPLAY "INVALID INPUT. PLEASE ENTER Y OR N: "
               ACCEPT WS-ADD-FLAG
               PERFORM CONVERT-FLAG
           END-PERFORM
       
           IF WS-ADD-FLAG = "Y"
               OPEN I-O DORM-FILE   
       
               PERFORM UNTIL WS-ADD-FLAG = "N"
       
                   DISPLAY "PLEASE ENTER DORM ROOM FLOOR (e.g., 01): "
                   ACCEPT UTIL-FLOOR-N
       
                   DISPLAY "PLEASE ENTER DORM ROOM NUMBER (e.g., 001): "
                   ACCEPT UTIL-ROOM-N
       
                   DISPLAY "PLEASE ENTER RENT AMOUNT: "
                   ACCEPT WS-DORM-RENT-AMOUNT
       
                   
                   *> Generate unique ID (Format: F01-R001)
                   STRING "F" DELIMITED BY SIZE
                          UTIL-FLOOR-N DELIMITED BY SIZE
                          "-R" DELIMITED BY SIZE
                          UTIL-ROOM-N DELIMITED BY SIZE
                          INTO WS-DORM-ID
                   END-STRING
       
                   
                   *> Check if ID already exists
                   MOVE WS-DORM-ID TO DI-ID
                   READ DORM-FILE
                       INVALID KEY
                           *> Room doesn't exist, proceed with write
                           MOVE WS-DORM-ID          TO DI-ID
                           MOVE UTIL-FLOOR-N        TO DI-FLOOR
                           MOVE UTIL-ROOM-N         TO DI-ROOM-NUM
                           MOVE WS-DORM-RENT-AMOUNT TO DI-RENT-AMOUNT
                           MOVE "UNOCCUPIED"        TO DI-STATUS
       
                           WRITE DORM-RECORD
                               INVALID KEY
                                   DISPLAY "ERROR WRITING RECORD"
                               NOT INVALID KEY
                                   DISPLAY "DORM " WS-DORM-ID 
                                           " ADDED SUCCESSFULLY!"
                           END-WRITE
                       NOT INVALID KEY
                           *> Room already exists
                           DISPLAY "ERROR: DORM " WS-DORM-ID 
                                   " ALREADY EXISTS!"
                   END-READ
       
                   
                   DISPLAY "ADD ANOTHER? (Y/N):"
                   ACCEPT WS-ADD-FLAG
                   PERFORM CONVERT-FLAG
       
                   
                   PERFORM UNTIL WS-ADD-FLAG = "Y" OR WS-ADD-FLAG = "N"
                       DISPLAY "INVALID INPUT. PLEASE ENTER Y OR N: "
                       ACCEPT WS-ADD-FLAG
                       PERFORM CONVERT-FLAG
                   END-PERFORM
       
               END-PERFORM
       
               CLOSE DORM-FILE
           END-IF
       
           DISPLAY "Dorm records saved successfully!"
           PERFORM EXIT-PROMT
           EXIT PARAGRAPH.


      *============================
      *FUNCTION: VIEW DORMS 
      *============================
       VIEW-DORMS.
              
           OPEN INPUT DORM-FILE
       
           MOVE "N" TO UTIL-EOF
       
           PERFORM UNTIL UTIL-EOF = "Y"
               READ DORM-FILE
               AT END
                   MOVE "Y" TO UTIL-EOF
               NOT AT END
                   DISPLAY "======================================="
                   DISPLAY "           Dorm ID: " DI-ID
                   DISPLAY "======================================="
                   DISPLAY "Floor          : " DI-FLOOR
                   DISPLAY "Room Number    : " DI-ROOM-NUM
                   DISPLAY "Status         : " DI-STATUS
                   DISPLAY "---------------------------------------"
                   
                   DISPLAY "RENT"
                   DISPLAY "  Amount Due   : " DI-RENT-AMOUNT
                   DISPLAY "  Due Date     : " DI-RENT-DUE
                   DISPLAY "  Amount Paid  : " DI-RENT-PAID-AMOUNT
                   DISPLAY "  Date Paid    : " DI-RENT-LAST-PAID
                   DISPLAY "---------------------------------------"
                   
                   DISPLAY "ELECTRICITY"
                   DISPLAY "  Amount Due   : " DI-ELECTRICITY-AMT
                   DISPLAY "  Due Date     : " DI-ELECTRICITY-DUE
                   DISPLAY "  Amount Paid  : " DI-ELECTRICITY-PAID-AMT
                   DISPLAY "  Date Paid    : " DI-ELECTRICITY-LAST
                   DISPLAY "---------------------------------------"
                   
                   DISPLAY "WATER"
                   DISPLAY "  Amount Due   : " DI-WATER-AMT
                   DISPLAY "  Due Date     : " DI-WATER-DUE
                   DISPLAY "  Amount Paid  : " DI-WATER-PAID-AMT
                   DISPLAY "  Date Paid    : " DI-WATER-LAST-PAID
                   DISPLAY "======================================="
                   
                   DISPLAY SPACE
               END-READ
           END-PERFORM
       
           CLOSE DORM-FILE
       
           PERFORM EXIT-PROMT
           EXIT PARAGRAPH.
       
      *============================
      *FUNCTION: EDIT DORMS 
      *============================
       EDIT-DORM.
           DISPLAY "YOU CHOSE TO EDIT DORM"
       
           MOVE "Y" TO UTIL-EDIT-AGAIN
       
           PERFORM UNTIL UTIL-EDIT-AGAIN = "N"
               PERFORM SHOW-DORM-ID
       
               DISPLAY "Enter Dorm ID to edit (e.g., F01-R001): "
               ACCEPT UTIL-SEARCH-DORM-ID
       
               OPEN I-O DORM-FILE
       
               *> Try to read the dorm record
               MOVE UTIL-SEARCH-DORM-ID TO DI-ID
               READ DORM-FILE
                   INVALID KEY
                   DISPLAY "ERROR: DORM " UTIL-SEARCH-DORM-ID
                           " NOT FOUND."
                   MOVE "N" TO UTIL-EDIT-FOUND
                   NOT INVALID KEY
                   MOVE "Y" TO UTIL-EDIT-FOUND
                   DISPLAY "Editing dorm: " DI-ID
                   DISPLAY "Current Floor: " DI-FLOOR
                   DISPLAY "Current Room: " DI-ROOM-NUM
                   DISPLAY "Current Rent: " DI-RENT-AMOUNT
                   DISPLAY "Current Status: " DI-STATUS
                   DISPLAY " "
       
                   
                   DISPLAY "Edit Floor (keep empty to unchanged): "
                   ACCEPT TEMP-FLOOR
                   IF TEMP-FLOOR NOT = SPACES
                       MOVE TEMP-FLOOR TO DI-FLOOR
                   END-IF
       
                   
                   DISPLAY "Edit Room Number "
                           "(keep empty to unchanged): "
                   ACCEPT TEMP-ROOM-NUM
                   IF TEMP-ROOM-NUM NOT = SPACES
                       MOVE TEMP-ROOM-NUM TO DI-ROOM-NUM
                   END-IF
       
                   
                   DISPLAY "Edit Rent Amount "
                           "(keep empty to unchanged): "
                   ACCEPT TEMP-RENT-AMOUNT
                   IF TEMP-RENT-AMOUNT NOT = SPACES
                       MOVE TEMP-RENT-AMOUNT TO DI-RENT-AMOUNT
                   END-IF
       
                   
                   DISPLAY "Edit Rent Due Date "
                           "(keep empty to unchanged): "
                   ACCEPT TEMP-RENT-DUE
                   IF TEMP-RENT-DUE NOT = SPACES
                       MOVE TEMP-RENT-DUE TO DI-RENT-DUE
                   END-IF
       
                   
                   DISPLAY "Edit Last Rent Paid "
                           "(keep empty to unchanged): "
                   ACCEPT TEMP-RENT-LAST-PAID
                   IF TEMP-RENT-LAST-PAID NOT = SPACES
                       MOVE TEMP-RENT-LAST-PAID 
                            TO DI-RENT-LAST-PAID
                   END-IF
       
                   
                   DISPLAY "Edit Electricity Amount "
                           "(keep empty to unchanged): "
                   ACCEPT TEMP-ELECTRICITY-AMT
                   IF TEMP-ELECTRICITY-AMT NOT = SPACES
                       MOVE TEMP-ELECTRICITY-AMT 
                            TO DI-ELECTRICITY-AMT
                   END-IF
       
                   
                   DISPLAY "Edit Electricity Due "
                           "(keep empty to unchanged): "
                   ACCEPT TEMP-ELECTRICITY-DUE
                   IF TEMP-ELECTRICITY-DUE NOT = SPACES
                       MOVE TEMP-ELECTRICITY-DUE 
                            TO DI-ELECTRICITY-DUE
                   END-IF
       
                   
                   DISPLAY "Edit Last Electricity Paid "
                           "(keep empty to unchanged): "
                   ACCEPT TEMP-ELECTRICITY-LAST
                   IF TEMP-ELECTRICITY-LAST NOT = SPACES
                       MOVE TEMP-ELECTRICITY-LAST 
                            TO DI-ELECTRICITY-LAST
                   END-IF
       
                   
                   DISPLAY "Edit WATER Amount "
                           "(keep empty to unchanged): "
                   ACCEPT TEMP-WATER-AMT
                   IF TEMP-WATER-AMT NOT = SPACES
                       MOVE TEMP-WATER-AMT TO DI-WATER-AMT
                   END-IF
       
                   
                   DISPLAY "Edit WATER Due "
                           "(keep empty to unchanged): "
                   ACCEPT TEMP-WATER-DUE
                   IF TEMP-WATER-DUE NOT = SPACES
                       MOVE TEMP-WATER-DUE TO DI-WATER-DUE
                   END-IF
       
                   
                   DISPLAY "Edit Last WATER Paid "
                           "(keep empty to unchanged): "
                   ACCEPT TEMP-WATER-LAST-PAID
                   IF TEMP-WATER-LAST-PAID NOT = SPACES
                       MOVE TEMP-WATER-LAST-PAID 
                            TO DI-WATER-LAST-PAID
                   END-IF
       
                   
                   DISPLAY "Edit Status (OCCUPIED/UNOCCUPIED) "
                           "(keep empty to unchanged): "
                   ACCEPT TEMP-STATUS
                   IF TEMP-STATUS NOT = SPACES
                       INSPECT TEMP-STATUS 
                           CONVERTING "abcdefghijklmnopqrstuvwxyz"
                           TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                       IF TEMP-STATUS = "OCCUPIED" OR
                          TEMP-STATUS = "UNOCCUPIED"
                           MOVE TEMP-STATUS TO DI-STATUS
                       ELSE
                           DISPLAY "Invalid status. "
                                   "Keeping current value."
                       END-IF
                   END-IF
       
                   
                   REWRITE DORM-RECORD
                       INVALID KEY
                           DISPLAY "ERROR: Could not update dorm."
                       NOT INVALID KEY
                           DISPLAY "Dorm record updated successfully!"
                   END-REWRITE
               END-READ
       
               CLOSE DORM-FILE
       
               IF UTIL-EDIT-FOUND = "N"
                   DISPLAY "Dorm not found."
               END-IF
       
               DISPLAY "Edit another dorm? (Y/N): "
               ACCEPT UTIL-EDIT-AGAIN
               PERFORM CONVERT-FLAG-EDIT
       
           END-PERFORM
       
           PERFORM EXIT-PROMT
           EXIT PARAGRAPH.

      *============================
      *FUNCTION: DELETE DORMS 
      *============================
       DELETE-DORM.
           DISPLAY "YOU CHOSE TO DELETE DORM"
       
           MOVE "Y" TO UTIL-DELETE-AGAIN
       
           PERFORM UNTIL UTIL-DELETE-AGAIN = "N"
               PERFORM SHOW-DORM-ID
       
               DISPLAY "Enter Dorm ID to delete (e.g., F01-R001): "
               ACCEPT UTIL-SEARCH-DORM-ID
       
               OPEN I-O DORM-FILE
       
               
               MOVE UTIL-SEARCH-DORM-ID TO DI-ID
               READ DORM-FILE
                   INVALID KEY
                       DISPLAY "ERROR: DORM " UTIL-SEARCH-DORM-ID
                               " NOT FOUND."
                       MOVE "N" TO UTIL-DELETE-FOUND
                   NOT INVALID KEY
                       MOVE "Y" TO UTIL-DELETE-FOUND
                       
                       
                       DISPLAY "Found Dorm:"
                       DISPLAY "  ID: " DI-ID
                       DISPLAY "  Floor: " DI-FLOOR
                       DISPLAY "  Room: " DI-ROOM-NUM
                       DISPLAY "  Rent: " DI-RENT-AMOUNT
                       DISPLAY "  Status: " DI-STATUS
                       DISPLAY " "
       
                       
                       IF DI-STATUS = "OCCUPIED"
                           DISPLAY "WARNING: This room is currently "
                                   "OCCUPIED!"
                           DISPLAY "Are you sure you want to delete? "
                                   "(Y/N): "
                           ACCEPT UTIL-CONFIRM-DELETE
                           PERFORM CONVERT-FLAG-DELETE
                       ELSE
                           DISPLAY "Confirm deletion (Y/N): "
                           ACCEPT UTIL-CONFIRM-DELETE
                           PERFORM CONVERT-FLAG-DELETE
                       END-IF
       
                       
                       IF UTIL-CONFIRM-DELETE = "Y"
                           DELETE DORM-FILE
                               INVALID KEY
                                   DISPLAY "ERROR: Could not delete "
                                           "dorm."
                               NOT INVALID KEY
                                   DISPLAY "Dorm " UTIL-SEARCH-DORM-ID
                                           " deleted successfully!"
                           END-DELETE
                       ELSE
                           DISPLAY "Deletion cancelled."
                       END-IF
               END-READ
       
               CLOSE DORM-FILE
       
               IF UTIL-DELETE-FOUND = "N"
                   DISPLAY "Dorm not found."
               END-IF
       
               DISPLAY "Delete another dorm? (Y/N): "
               ACCEPT UTIL-DELETE-AGAIN
               PERFORM CONVERT-FLAG-DELETE-AGAIN
       
           END-PERFORM
       
           PERFORM EXIT-PROMT
           EXIT PARAGRAPH.

      *============================
      *FUNCTION: TENANT MANAGEMENT
      *============================
       TENANT-MANAGEMENT.
           PERFORM UNTIL UTIL-SM-CHOICE = 5
           PERFORM CLEAR-SCREEN
           
           DISPLAY "==========================="
           DISPLAY "    STUDNENT MANAGEMENT    "
           DISPLAY "==========================="
           DISPLAY "1 - ADD TENANT"
           DISPLAY "2 - VIEW TENANTS"
           DISPLAY "3 - EDIT TENANT INFO"
           DISPLAY "4 - DELETE TENANTS"
           DISPLAY "5 - EXIT"

           DISPLAY "ENTER CHOICE (1 - 5): "
           ACCEPT UTIL-SM-CHOICE

           EVALUATE UTIL-SM-CHOICE
               WHEN 1
                   PERFORM CLEAR-SCREEN
                   PERFORM ADD-TENANT
    
               WHEN 2
                   PERFORM CLEAR-SCREEN
                   PERFORM VIEW-TENANTS

               WHEN 3
                   PERFORM CLEAR-SCREEN
                   PERFORM EDIT-TENANTS
    
               WHEN 4
                   PERFORM CLEAR-SCREEN
                   PERFORM DELETE-TENANTS
    
               WHEN 5
                   DISPLAY "EXITING TENANT MANAGEMENT..."
                   PERFORM EXIT-PROMT
    
               WHEN OTHER 
                   DISPLAY "INVALID CHOICE. TRY AGAIN"
           END-EVALUATE

           END-PERFORM
           EXIT PARAGRAPH.

      

      *=====================
      *FUNCTION: ADD-TENANT
      *=====================
       ADD-TENANT.
           DISPLAY "YOU CHOSE TO ADD TENANT"
           DISPLAY "ADD TENANT? (Y/N): "
           ACCEPT WS-ADD-FLAG
           PERFORM CONVERT-FLAG
       
           IF WS-ADD-FLAG = "Y"
               *> Open files once
               OPEN EXTEND TENANT-FILE *>Line sequential
               OPEN I-O DORM-FILE   *> Indexed
       
               PERFORM UNTIL WS-ADD-FLAG = "N"
       
                   DISPLAY "==============="
                   DISPLAY "ADD TENANT"
                   DISPLAY "==============="
       
                   
                   DISPLAY " "
                   DISPLAY "AVAILABLE ROOMS:"
                   DISPLAY "----------------"
                   MOVE LOW-VALUES TO DI-ID
                   MOVE 0 TO WS-AVAILABLE-ROOM-COUNT
                   START DORM-FILE KEY >= DI-ID
                       INVALID KEY
                           DISPLAY "NO ROOMS IN SYSTEM"
                   END-START
       
                   PERFORM UNTIL WS-DORM-FILE-STATUS NOT = "00"
                       READ DORM-FILE NEXT
                           AT END
                               CONTINUE
                           NOT AT END
                               IF DI-STATUS = "UNOCCUPIED"
                                   DISPLAY "ROOM: " DI-ID 
                                           " | FLOOR: " DI-FLOOR
                                           " | RENT: " DI-RENT-AMOUNT
                                   ADD 1 TO WS-AVAILABLE-ROOM-COUNT
                               END-IF
                       END-READ
                   END-PERFORM
                   DISPLAY "----------------"
                   DISPLAY " "
       
                   *> Check if there are available rooms
                   IF WS-AVAILABLE-ROOM-COUNT = 0
                       DISPLAY "NO AVAILABLE ROOMS. "
                               "CANNOT ADD TENANT."
                       MOVE "N" TO WS-ADD-FLAG
                   ELSE
                       
                       *> Get TENANT information
                       DISPLAY "Name: " WITH NO ADVANCING
                       ACCEPT WS-NAME
                       DISPLAY "Age: " WITH NO ADVANCING
                       ACCEPT WS-AGE
                       DISPLAY "Gender: " WITH NO ADVANCING
                       ACCEPT WS-GENDER
                       DISPLAY "Contact Number: " WITH NO ADVANCING
                       ACCEPT WS-CONTACT-NUM
           
                       
                       *> Room assignment with validation
                       MOVE "N" TO WS-VALID-ROOM-FLAG
                       MOVE "N" TO WS-CANCEL-FLAG
                       PERFORM UNTIL WS-VALID-ROOM-FLAG = "Y"
                           DISPLAY "Assign Room (type EXIT to cancel): " 
                                   WITH NO ADVANCING
                           ACCEPT WS-ASSIGNED-D-ID
                           
                           
                           INSPECT WS-ASSIGNED-D-ID 
                               CONVERTING "abcdefghijklmnopqrstuvwxyz"
                               TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                           
                           
                           IF WS-ASSIGNED-D-ID = "EXIT"
                               DISPLAY "TENANT ADDITION CANCELLED."
                               MOVE "Y" TO WS-VALID-ROOM-FLAG
                               MOVE "Y" TO WS-CANCEL-FLAG
                           ELSE
                               
                               MOVE WS-ASSIGNED-D-ID TO DI-ID
                               READ DORM-FILE
                               INVALID KEY
                                   DISPLAY "ERROR: ROOM " 
                                           WS-ASSIGNED-D-ID
                                           " DOES NOT EXIST. "
                                           "TRY AGAIN."
                               NOT INVALID KEY
                                   IF DI-STATUS = "OCCUPIED"
                                       DISPLAY "ERROR: ROOM " 
                                               WS-ASSIGNED-D-ID
                                               " IS ALREADY "
                                               "OCCUPIED. TRY AGAIN."
                                   ELSE
                                       MOVE "Y" TO 
                                            WS-VALID-ROOM-FLAG
                                       MOVE DI-RENT-AMOUNT 
                                            TO WS-RENT-AMOUNT-PAID
                                   END-IF
                               END-READ
                           END-IF
                       END-PERFORM
           
                       *> Only write TENANT if not cancelled
                       IF WS-CANCEL-FLAG = "N"
                           
                           MOVE WS-NAME             TO SI-NAME
                           MOVE WS-AGE              TO SI-AGE
                           MOVE WS-GENDER           TO SI-GENDER
                           MOVE WS-CONTACT-NUM      TO SI-CONTACT-NUM
                           MOVE WS-ASSIGNED-D-ID    TO SI-ASSIGNED-D-ID
                          
               
                           WRITE TENANT-RECORD
               
                           
                           MOVE WS-ASSIGNED-D-ID TO DI-ID
                           READ DORM-FILE
                           INVALID KEY
                               DISPLAY "ERROR UPDATING DORM STATUS"
                           NOT INVALID KEY
                               MOVE "OCCUPIED" TO DI-STATUS
                               REWRITE DORM-RECORD
                                   INVALID KEY
                                       DISPLAY "ERROR REWRITING "
                                               "DORM RECORD"
                                   NOT INVALID KEY
                                       DISPLAY "TENANT ADDED AND "
                                               "ROOM " 
                                               WS-ASSIGNED-D-ID 
                                               " MARKED AS OCCUPIED!"
                               END-REWRITE
                           END-READ
               
                           DISPLAY "ADD ANOTHER? (Y/N):"
                           ACCEPT WS-ADD-FLAG
                           PERFORM CONVERT-FLAG
                       ELSE
                           MOVE "N" TO WS-ADD-FLAG
                       END-IF
                   END-IF
       
               END-PERFORM
       
               CLOSE TENANT-FILE
               CLOSE DORM-FILE
       
           END-IF
       
           PERFORM EXIT-PROMT
           EXIT PARAGRAPH.
       
       
       
       

      *=======================
      *FUNCTION: VIEW-TENANTS
      *=======================
       VIEW-TENANTS.
           
           OPEN INPUT TENANT-FILE
           
           MOVE "N" TO UTIL-EOF
                  PERFORM UNTIL UTIL-EOF = 'Y'
           READ TENANT-FILE
               AT END
                   MOVE 'Y' TO UTIL-EOF
               NOT AT END
                   DISPLAY "==============="
                   DISPLAY "TENANT ID: " SI-NAME *> NO TENANT ID YET
                   DISPLAY "==============="
                   DISPLAY "Name: " SI-NAME
                   DISPLAY "Age: " SI-AGE
                   DISPLAY "Gender: " SI-GENDER
                   DISPLAY "Contact Number: " SI-CONTACT-NUM
                   DISPLAY "Assigned Room: " SI-ASSIGNED-D-ID
                   
                   DISPLAY SPACE
           END-READ
           END-PERFORM

           CLOSE TENANT-FILE
           
           PERFORM EXIT-PROMT

           EXIT PARAGRAPH.

      *=======================
      *FUNCTION: EDIT-TENANTS
      *=======================
       EDIT-TENANTS.
           DISPLAY "YOU CHOSE TO EDIT TENANTS"
       
           MOVE "Y" TO UTIL-EDIT-AGAIN
       
           PERFORM UNTIL UTIL-EDIT-AGAIN = "N"
       
               MOVE "N" TO UTIL-EDIT-FOUND
       
               DISPLAY "Enter name of the TENANT to edit: "
               ACCEPT UTIL-SEARCH-NAME
       
               OPEN INPUT TENANT-FILE
                    OUTPUT TEMP-TENANT-FILE
       
               MOVE "N" TO UTIL-EOF
       
               PERFORM UNTIL UTIL-EOF = "Y"
                   READ TENANT-FILE
                       AT END
                           MOVE "Y" TO UTIL-EOF
       
                       NOT AT END
                       IF SI-NAME = UTIL-SEARCH-NAME
                           MOVE "Y" TO UTIL-EDIT-FOUND
                           DISPLAY "Editing TENANT: " SI-NAME
       
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
                           ACCEPT TEMP-ASSIGNED-D-ID
                           IF TEMP-ASSIGNED-D-ID NOT = SPACES
                               MOVE TEMP-ASSIGNED-D-ID 
-                                  TO SI-ASSIGNED-D-ID
                           END-IF
       
                       END-IF
       
                       WRITE TEMP-TENANT-RECORD FROM TENANT-RECORD
                   END-READ
               END-PERFORM
       
               CLOSE TENANT-FILE TEMP-TENANT-FILE
       
               PERFORM SAVE-TENANT-RECORD
       
               IF UTIL-EDIT-FOUND = "N"
                   DISPLAY "TENANT not found."
               ELSE
                   DISPLAY "TENANT record updated."
               END-IF
       
               DISPLAY "Edit another TENANT? (Y/N): "
               ACCEPT UTIL-EDIT-AGAIN
       
           END-PERFORM
       
           PERFORM EXIT-PROMT
           EXIT PARAGRAPH.
       
             
      *=========================
      *FUNCTION: DELETE-TENANTS
      *=========================
       DELETE-TENANTS.
           MOVE "Y" TO UTIL-DELETE-CHOICE

           PERFORM UNTIL UTIL-DELETE-CHOICE = "N"

               DISPLAY "YOU CHOSE TO DELETE TENANTS"
               DISPLAY "PLEASE ENTER THE NAME OF THE TENANT TO DELETE:"
               ACCEPT UTIL-SEARCH-NAME

               MOVE "N" TO UTIL-EOF
               MOVE "N" TO UTIL-DELETE-FOUND

               OPEN INPUT TENANT-FILE
                    OUTPUT TEMP-TENANT-FILE

               PERFORM UNTIL UTIL-EOF = "Y"
               READ TENANT-FILE
                   AT END
                       MOVE "Y" TO UTIL-EOF
                   NOT AT END
                       IF SI-NAME = UTIL-SEARCH-NAME
                           DISPLAY "DELETING TENANT: " SI-NAME
                           MOVE "Y" TO UTIL-DELETE-FOUND
                       ELSE
                           WRITE TEMP-TENANT-RECORD FROM TENANT-RECORD
                       END-IF
               END-READ
               END-PERFORM

               CLOSE TENANT-FILE TEMP-TENANT-FILE

               IF UTIL-DELETE-FOUND = "Y"
                   PERFORM SAVE-TENANT-RECORD
                   DISPLAY "TENANT SUCCESSFULLY DELETED."
               ELSE
                   DISPLAY "TENANT NOT FOUND."
               END-IF

               DISPLAY "DELETE ANOTHER TENANT? (Y/N): "
               ACCEPT UTIL-DELETE-CHOICE

               IF UTIL-DELETE-CHOICE NOT = "Y"
                   MOVE "N" TO UTIL-DELETE-CHOICE
               END-IF

           END-PERFORM

           PERFORM EXIT-PROMT
           EXIT PARAGRAPH.
      *============================
      *FUNCTION: GENERATE TRANSACTION ID
      *============================
       GENERATE-TRANSACTION-ID.
           ADD 1 TO WS-TRANSACTION-COUNTER
           
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           
           *> STRING with explicit delimiters to avoid spaces
           STRING WS-DORM-ID DELIMITED BY "  "
                  "-" DELIMITED BY SIZE
                  WS-CURRENT-DATE DELIMITED BY SIZE
                  "-" DELIMITED BY SIZE
                  WS-TRANSACTION-COUNTER DELIMITED BY SIZE
                  INTO WS-TRANSACTION-ID
           END-STRING
           
           EXIT PARAGRAPH.
       
      *============================
      *FUNCTION: LOG PAYMENT HISTORY
      *============================
       LOG-PAYMENT-HISTORY.
           PERFORM GENERATE-TRANSACTION-ID
           
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           ACCEPT WS-CURRENT-TIME FROM TIME
           
           *> Format timestamp as YYYY-MM-DD HH:MM:SS
           STRING WS-CURRENT-DATE(1:4) DELIMITED BY SIZE
                  "-" DELIMITED BY SIZE
                  WS-CURRENT-DATE(5:2) DELIMITED BY SIZE
                  "-" DELIMITED BY SIZE
                  WS-CURRENT-DATE(7:2) DELIMITED BY SIZE
                  " " DELIMITED BY SIZE
                  WS-CURRENT-TIME(1:2) DELIMITED BY SIZE
                  ":" DELIMITED BY SIZE
                  WS-CURRENT-TIME(3:2) DELIMITED BY SIZE
                  ":" DELIMITED BY SIZE
                  WS-CURRENT-TIME(5:2) DELIMITED BY SIZE
                  INTO WS-TIMESTAMP
           END-STRING
           
           *> DEBUG: Display what we're about to write
           DISPLAY "DEBUG - Logging Payment:"
           DISPLAY "  Transaction ID: " WS-TRANSACTION-ID
           DISPLAY "  Dorm ID: " WS-DORM-ID
           DISPLAY "  Payment Type: " WS-PAYMENT-TYPE
           DISPLAY "  Amount Due: " WS-AMOUNT-DUE
           DISPLAY "  Amount Paid: " WS-AMOUNT-PAID
           
           *> Set all fields in the record
           MOVE WS-TRANSACTION-ID TO PH-TRANSACTION-ID
           MOVE WS-DORM-ID TO PH-DORM-ID
           MOVE WS-PAYMENT-TYPE TO PH-PAYMENT-TYPE
           MOVE WS-AMOUNT-DUE TO PH-AMOUNT-DUE
           MOVE WS-AMOUNT-PAID TO PH-AMOUNT-PAID
           MOVE WS-PAYMENT-DATE-LOG TO PH-PAYMENT-DATE
           MOVE WS-DUE-DATE-LOG TO PH-DUE-DATE
           MOVE WS-TIMESTAMP TO PH-TIMESTAMP
           MOVE SPACES TO PH-NOTES
           
           OPEN EXTEND HISTORY-FILE
           
           WRITE PAYMENT-HISTORY-RECORD
           
           DISPLAY "Payment logged to history - Transaction ID: " 
                   WS-TRANSACTION-ID
           
           CLOSE HISTORY-FILE
           
           EXIT PARAGRAPH.
                  
      *============================
      *FUNCTION: DISPLAY ALL DORM ID
      *============================
       SHOW-DORM-ID.
               DISPLAY " "
               DISPLAY "ALL DORM ROOMS:"
               DISPLAY "----------------------------------------"
               OPEN INPUT DORM-FILE
               
               MOVE LOW-VALUES TO DI-ID
               START DORM-FILE KEY >= DI-ID
                   INVALID KEY
                       DISPLAY "NO DORMS IN SYSTEM"
               END-START
       
               PERFORM UNTIL WS-DORM-FILE-STATUS NOT = "00"
                   READ DORM-FILE NEXT
                       AT END
                           CONTINUE
                       NOT AT END
                           DISPLAY "ID: " DI-ID 
                                   " | Floor: " DI-FLOOR
                                   " | Room: " DI-ROOM-NUM
                                   " | Status: " DI-STATUS
                   END-READ
               END-PERFORM
               
               CLOSE DORM-FILE
               DISPLAY "----------------------------------------"
               DISPLAY " "
               EXIT PARAGRAPH.
      *============================
      *FUNCTION: UTILITIES CONVERT ADD FLAG 
      *============================
       CONVERT-FLAG.
           INSPECT WS-ADD-FLAG CONVERTING 'y' TO 'Y'
           INSPECT WS-ADD-FLAG CONVERTING 'n' TO 'N'
           EXIT PARAGRAPH.

       
      *============================
      *FUNCTION: UTILITIES OS SAVE TENANT RECORD 
      *============================
       SAVE-TENANT-RECORD.
           ACCEPT UTIL-OS-NAME FROM ENVIRONMENT "OS"

           IF UTIL-OS-NAME = "Windows_NT"
               CALL "SYSTEM" USING "del TENANTs.dat"
               CALL "SYSTEM" USING "rename temp.dat TENANTs.dat"

           ELSE
               CALL "SYSTEM" USING "rm TENANTs.dat"
               CALL "SYSTEM" USING "mv temp.dat TENANTs.dat"

           END-IF
           EXIT PARAGRAPH.
           
      *================================
      *FUNCTION: UTILITIES CONVERT EDIT 
      *================================
       CONVERT-FLAG-EDIT.
           INSPECT UTIL-EDIT-AGAIN 
               CONVERTING "abcdefghijklmnopqrstuvwxyz"
               TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
           
           PERFORM UNTIL UTIL-EDIT-AGAIN = "Y" 
                      OR UTIL-EDIT-AGAIN = "N"
               DISPLAY "INVALID INPUT. PLEASE ENTER Y OR N: "
               ACCEPT UTIL-EDIT-AGAIN
               INSPECT UTIL-EDIT-AGAIN 
                   CONVERTING "abcdefghijklmnopqrstuvwxyz"
                   TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
           END-PERFORM
           EXIT PARAGRAPH.
      *================================
      *FUNCTION: UTILITIES CONVERT DELETE 
      *================================
       CONVERT-FLAG-DELETE.
           INSPECT UTIL-CONFIRM-DELETE 
               CONVERTING "abcdefghijklmnopqrstuvwxyz"
               TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
           
           PERFORM UNTIL UTIL-CONFIRM-DELETE = "Y" 
                      OR UTIL-CONFIRM-DELETE = "N"
               DISPLAY "INVALID INPUT. PLEASE ENTER Y OR N: "
               ACCEPT UTIL-CONFIRM-DELETE
               INSPECT UTIL-CONFIRM-DELETE 
                   CONVERTING "abcdefghijklmnopqrstuvwxyz"
                   TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
           END-PERFORM
           EXIT PARAGRAPH.
       
      *================================
      *FUNCTION: UTILITIES CONVERT DELETE AGAIN 
      *================================
       CONVERT-FLAG-DELETE-AGAIN.
           INSPECT UTIL-DELETE-AGAIN 
               CONVERTING "abcdefghijklmnopqrstuvwxyz"
               TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
           
           PERFORM UNTIL UTIL-DELETE-AGAIN = "Y" 
                      OR UTIL-DELETE-AGAIN = "N"
               DISPLAY "INVALID INPUT. PLEASE ENTER Y OR N: "
               ACCEPT UTIL-DELETE-AGAIN
               INSPECT UTIL-DELETE-AGAIN 
                   CONVERTING "abcdefghijklmnopqrstuvwxyz"
                   TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
           END-PERFORM
           EXIT PARAGRAPH.
       
      *======================
      *FUNCTION: UTILITIES CLEAR-SCREEN
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
      *FUNCTION: UTILITIES EXIT-PROMT
      *=======================
       EXIT-PROMT.

           DISPLAY "Press enter to proceed."
           ACCEPT OMITTED

           EXIT PARAGRAPH.
