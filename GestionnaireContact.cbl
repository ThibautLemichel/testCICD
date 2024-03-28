       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONTACTMANAGER.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CONTACT-RECORD.
          05 NOM            PIC X(30).
          05 PHONE          PIC X(15).
          05 EMAIL          PIC X(50).
       
       01 CONTACTS.
          05 CONTACT-ENTRY OCCURS 100 TIMES.
             10 NOM         PIC X(30).
             10 PHONE       PIC X(15).
             10 EMAIL       PIC X(50).

       01 I                 PIC 9(3).
       01 NEW-EMAIL         PIC X(50).
       01 NEW-PHONE         PIC X(15).
       01 USER-CHOICE       PIC 9(1).
       01 SEARCH-NAME       PIC X(30).
       01 UPDATE-NAME       PIC X(30).
       01 CONTACT-COUNT     PIC 9(3)  VALUE 0.
       
       PROCEDURE DIVISION.
       MAIN-ROUTINE.
           DISPLAY "Contact Manager".
           PERFORM UNTIL USER-CHOICE = '4'
                   DISPLAY "1. Add Contact"
                   DISPLAY "2. Search Contact"
                   DISPLAY "3. Update Contact"
                   DISPLAY "4. Display Contacts"
                   DISPLAY "5. Exit"
                   ACCEPT USER-CHOICE
                   IF USER-CHOICE = '1' THEN
                      PERFORM ADD-CONTACT
                   ELSE
                      IF USER-CHOICE = '2' THEN
                         PERFORM SEARCH-CONTACT
                      ELSE
                         IF USER-CHOICE = '3' THEN
                            PERFORM UPDATE-CONTACT
                         ELSE
                            IF USER-CHOICE = '4' THEN
                               PERFORM DISPLAY-CONTACT
                            ELSE 
                               IF USER-CHOICE = '5' THEN
                                  DISPLAY
                                     "Exiting Contact Manager."
                               ELSE
                                  DISPLAY
                                  "Choix non correcte, un autre chose?"
                                  STOP RUN
                               END-IF
           END-PERFORM.

           STOP RUN.
       
       ADD-CONTACT.
           DISPLAY "Enter the contact's name: ".
           ACCEPT NOM OF CONTACT-RECORD.
           DISPLAY "Enter the contact's phone number: ".
           ACCEPT PHONE OF CONTACT-RECORD.
           DISPLAY "Enter the contact's email address: ".
           ACCEPT EMAIL OF CONTACT-RECORD.
           ADD 1 TO CONTACT-COUNT.
           MOVE CONTACT-RECORD TO CONTACT-ENTRY(CONTACT-COUNT).
           DISPLAY "Contact successfully added: " NOM OF CONTACT-RECORD.
              
       SEARCH-CONTACT.
           DISPLAY "Enter the contact's name to search: ".
           ACCEPT SEARCH-NAME.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 100
                   IF NOM OF CONTACT-ENTRY(I) = SEARCH-NAME
                      DISPLAY "Contact found: "
                              NOM OF CONTACT-ENTRY(I)
                              " Phone: "
                              PHONE OF CONTACT-ENTRY(I)
                              " Email: "
                              EMAIL OF CONTACT-ENTRY(I)
                      EXIT PERFORM
                   END-IF
           END-PERFORM.
           IF I > 100
              DISPLAY "Contact with name '" SEARCH-NAME "' not found."
           END-IF.
       
       UPDATE-CONTACT.
           DISPLAY "Enter the contact's name to update: ".
           ACCEPT UPDATE-NAME.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 100
                   IF (NOM OF CONTACT-ENTRY(I)) = UPDATE-NAME
                      DISPLAY "Contact found: "
                              NOM OF CONTACT-ENTRY(I)
                              " Phone: "
                              PHONE OF CONTACT-ENTRY(I)
                              " Email: "
                              EMAIL OF CONTACT-ENTRY(I)
                      DISPLAY "Enter the new phone number: "
                      ACCEPT NEW-PHONE
                      DISPLAY "Enter the new email address: "
                      ACCEPT NEW-EMAIL
                      MOVE NEW-PHONE TO PHONE OF CONTACT-ENTRY
                         (I)
                      MOVE NEW-EMAIL TO EMAIL OF CONTACT-ENTRY
                         (I)
                      DISPLAY "Contact "
                              NOM OF CONTACT-ENTRY
                         (I)
                      EXIT PERFORM
                   END-IF
           END-PERFORM.
           IF I > 100
              DISPLAY "Contact with name '" UPDATE-NAME "' not found."
           END-IF.
       
       DISPLAY-CONTACT.
           DISPLAY "Contact List: ".
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > CONTACT-COUNT 
                   DISPLAY NOM OF CONTACT-ENTRY(I)
                           " Phone: "
                           PHONE OF CONTACT-ENTRY(I)
                           " Email: "
                           EMAIL OF CONTACT-ENTRY(I)
           END-PERFORM.