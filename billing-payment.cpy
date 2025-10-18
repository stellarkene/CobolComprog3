       01  STUDENT-COUNT           PIC 9(2) VALUE 0.
       01  STUDENT-COUNTER         PIC 9(2) VALUE 1.
       
       01  STUDENTS.    
           05  STUDENT-INFO OCCURS 100 TIMES.
               10 SI-NAME          PIC X(50).
               10 SI-AGE           PIC Z(2).
               10 SI-GENDER        PIC X(15).
               10 SI-RELIGION      PIC X(20).

      
