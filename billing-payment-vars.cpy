       
      *add module
       01  WS-ADD-FLAG             PIC X(2).

      *view module
       01  S_C                     PIC 9(2).

      *edit module
       01  SEARCH-NAME             PIC X(50).

      *working storage run time memory
       01  WS-NAME                 PIC X(50).
       01  WS-AGE                  PIC 9(2).
       01  WS-GENDER               PIC X(15).
       01  WS-CONTACT-NUM          PIC X(12).
       01  WS-RELIGION             PIC X(20).
       01  WS-ROOM-NUM             PIC X(20).
       01  WS-RENT-AMOUNT          PIC X(6).
       
      *end of file check
       01  EOF                     PIC X VALUE 'N'.
