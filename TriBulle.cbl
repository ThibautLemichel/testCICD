       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRIBULLE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 ARRAY-SIZE        PIC 9(4) VALUE 9998.
       01 I                 PIC 9(4).
       01 J                 PIC 9(4).
       01 RANDOM-NUMBERS.
          05 RANDOM-NUMBER OCCURS 9998 TIMES
                            PIC 9(3).
          05 TEMP-NUMBER    PIC 9(3).
       01 MIN-NUMBER        PIC 9(3) VALUE 000.                             
       01 MAX-NUMBER        PIC 9(3) VALUE 999.   

       PROCEDURE DIVISION.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-SIZE
                   COMPUTE RANDOM-NUMBER(I) =(FUNCTION RANDOM) *
                      (MAX-NUMBER - MIN-NUMBER + 1) +
                      MIN-NUMBER
           END-PERFORM.

           DISPLAY "Tableau non trié :"
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-SIZE 
                   DISPLAY RANDOM-NUMBER(I)
           END-PERFORM.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-SIZE
                   PERFORM VARYING J FROM I BY 1 UNTIL J > ARRAY-SIZE
                           IF RANDOM-NUMBER(I) > RANDOM-NUMBER(J)
                              MOVE RANDOM-NUMBER(I) TO TEMP-NUMBER
                              MOVE RANDOM-NUMBER(J) TO RANDOM-NUMBER(I)
                              MOVE TEMP-NUMBER TO RANDOM-NUMBER(J)
                           END-IF
                   END-PERFORM
           END-PERFORM.

           DISPLAY "Tableau trié dans l'ordre croissant :"
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ARRAY-SIZE
                   DISPLAY RANDOM-NUMBER(I)
           END-PERFORM.

           STOP RUN.