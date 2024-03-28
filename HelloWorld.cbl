       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLOWORLD.

       DATA DIVISION. 
       WORKING-STORAGE SECTION. 
       01 VAR1       PIC 9(3).
          88 MAJEUR           VALUE 18.

       PROCEDURE DIVISION.

           DISPLAY "Entrer valeur a :".

           ACCEPT VAR1.
           DISPLAY "Valeur de a : " VAR1.
      *     DISPLAY "Est majeur ? " MAJEUR.

           STOP RUN.