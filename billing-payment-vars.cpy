       01  STUDENT-COUNT     PIC 9(2).
       01  S_C               PIC 9(2).

       01  STUDENTS.
           05  SI-NAME-ARR       OCCURS 50 TIMES PIC X(50).
           05  SI-AGE-ARR        OCCURS 50 TIMES PIC 9(2).
           05  SI-GENDER-ARR     OCCURS 50 TIMES PIC X(15).
           05  SI-RELIGION-ARR   OCCURS 50 TIMES PIC X(20).

       01  EOF                PIC X VALUE 'N'.
