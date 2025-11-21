       FD  STUDENT-FILE.
       01  STUDENT-RECORD.
           05  SI-NAME                 PIC X(50).
           05  SI-AGE                  PIC 9(2).
           05  SI-GENDER               PIC X(15).
           05  SI-CONTACT-NUM          PIC X(12).
           05  SI-RELIGION             PIC X(20).
           05  SI-ROOM-NUM             PIC X(20).
           05  SI-RENT-AMOUNT          PIC ZZZ,ZZZ.


       FD  TEMP-STUDENT-FILE.
       01  TEMP-STUDENT-RECORD.
           05  TEMP-NAME               PIC X(50).
           05  TEMP-AGE                PIC 9(2).
           05  TEMP-GENDER             PIC X(15).         
           05  TEMP-CONTACT-NUM        PIC X(12).
           05  TEMP-RELIGION           PIC X(20).
           05  TEMP-ROOM-NUM           PIC X(20).
           05  TEMP-RENT-AMOUNT        PIC ZZZ,ZZZ.        
           