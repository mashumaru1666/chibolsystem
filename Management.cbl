      ******************************************************************
      * Author:CHIBOL
      * Date:01/27/25
      * COURSE YEAR & SECTION: BSIT 2-1
      * MAIN-MENU
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. finalchibol.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EventFile
           ASSIGN TO
           "C:\Users\kal\OneDrive\Documents\event.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FILE-STATUS.

           SELECT LocationFile
           ASSIGN TO
           "C:\Users\kal\OneDrive\Documents\location.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FILE-STATUS.

           SELECT TEMP-LOCATION-FILE
           ASSIGN TO
           "C:\Users\kal\OneDrive\Documents\templocation.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FILE-STATUS.

           SELECT TEMP-FILE
           ASSIGN TO
           "C:\Users\kal\OneDrive\Documents\tempfile.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FILE-STATUS.

           SELECT UserFile
           ASSIGN TO
           "C:\Users\kal\OneDrive\Documents\users.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FILE-STATUS.

           SELECT AdminFile
           ASSIGN TO
           "C:\Users\kal\OneDrive\Documents\admins.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FILE-STATUS.

           SELECT VIEW-MY-BOOKINGS-FILE
           ASSIGN TO
           "C:\Users\kal\OneDrive\Documents\viewmybookings.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FILE-STATUS.



       DATA DIVISION.
       FILE SECTION.
       FD EventFile.
       01 EVENTData.
               02 EVENTNAME   PIC X(20).
               02 EVENTDATE   PIC X(10).
               02 ORGANIZER   PIC X(20).
               02 LOCATION    PIC X(20).
               02 TICKET-PRICE PIC 9(5).
               02 AVAILABLE-SEATS PIC 9(2).

       FD LocationFile.
       01 LocationRecord.
           05 LOCATION-NAME PIC X(15).
           05 LOCATION-PRICE        PIC 9(5).
           05 LOCATION-PRICE-FIELD PIC X(10).

       FD TEMP-LOCATION-FILE.
       01 TEMP-LOCATION-RECORD.
           02 TEMP-LOCATION-NUMBER PIC 9(2).
           02 TEMP-LOCATION-NAME PIC X(50).

       FD TEMP-FILE.
       01 TEMP-RECORD.
               02 EVENTNAME2   PIC X(30).
               02 EVENTDATE2  PIC  X(10).
               02 ORGANIZER2   PIC X(30).
               02 LOCATION2    PIC X(50).
               02 TICKET-PRICE2 PIC X(30).
               02 AVAILABLE-SEATS2 PIC 9(2).

       FD UserFile.
       01 UserRecord.
              02 ATTENDEE-STORED-USERNAME PIC X(30).
              02 ATTENDEE-STORED-PASSWORD PIC X(30).
              02 ATTENDEE-STORED-USER-TYPE PIC X(1).

       FD AdminFile.
       01 AdminRecord.
               02 ADMIN-STORED-USERNAME PIC X(30).
               02 ADMIN-STORED-PASSWORD PIC X(30).
               02 ADMIN-STORED-USER-TYPE PIC X(1).
               FD VIEW-MY-BOOKINGS-FILE.

       01 MyBookingsRecord.
               02 BOOKED-ATTENDEE-USERNAME    PIC X(30).
               02 BOOKED-EVENTNAME            PIC X(50).
               02 BOOKED-EVENTDATE            PIC X(10).
               02 BOOKED-LOCATION             PIC X(20).
               02 BOOKED-TP                   PIC X(30).
               02 BOOKED-STB                  PIC X(4).
               02 BOOKED-ORGANIZER            PIC X(20).


       WORKING-STORAGE SECTION.
       *> =================== USER INPUT ===================
       01 ENTER          PIC X(30).
       01 CHOICE PIC 9(2) VALUE 99.
       01 RES PIC S9(9)V9(9).
       01 END-IN PIC X.
       01 SPACE-COUNT             PIC 9(2) VALUE 5.
       01 EOF            PIC 9 VALUE 0.
       01 AGE            PIC 9(5).
       01 WS-PAUSE       PIC X(1).
       01 SEARCH-NAME    PIC X(50).
       01 DELETE-NAME    PIC X(50).
       01 FILE-STATUS    PIC XX.
       01 WS-CURRENT-DATE.
               05 CUR-YEAR    PIC 9(4).
               05 CUR-MONTH   PIC 9(2).
               05 CUR-DAY     PIC 9(2).
       01 WS-EVENT-DATE.
               05 EVENT-YEAR  PIC 9(4).
               05 EVENT-MONTH PIC 9(2).
               05 EVENT-DAY   PIC 9(2).
       01 TEMP-YEAR       PIC 9(4).
       01 EVENT-FOUND    PIC 9 VALUE 0.
       01 WS-CONFIRM     PIC X(1).
       01 USERNAME       PIC X(30).
       01 PASSWORDS       PIC X(30).
       01 NEW-USERNAME   PIC X(30).
       01 NEW-PASSWORD   PIC X(30).
       01 NEW-USER-TYPE  PIC X(1).
       01 USER-TYPE      PIC X(1).
       01 USER-ROLE      PIC X(5).
       01 VALID-USER     PIC X VALUE 'N'.

       01 NEW-NAME       PIC X(50).
       01 NEW-ORGANIZER  PIC X(50).
       01 NEW-DATE       PIC X(10).
       01 NEW-LOCATION   PIC X(50).
       01 NEW-TICKET-PRICE PIC X(30).
       01 NEW-AVAILABLE-SEATS PIC X(30).

       01 ENCRYPTED-PASSWORD    PIC X(30).
       01 CHAR-INDEX            PIC 9(2).
       01 TEMP-CHAR             PIC 9(3) VALUE 0.
       01 SHIFT-VALUE           PIC 9 VALUE 3.

       01 LOCATION-CHOICE       PIC 9(2).
       01 LOCATION-NUMBER       PIC X(100).
       01 OLD-PRICE             PIC X(10).
       01 NEW-PRICE             PIC X(10).
       01 LOCATION-INDEX        PIC 9(2) VALUE 1.
       01 NEW-LOCATIONS         PIC X(50).
       01 TEMP-LOCATION         PIC X(50).
       01 SEATS-TO-BUY          PIC 9(4) VALUE 0.
       01 BOOKED-SEATS                PIC 9(5) VALUE ZEROES.
       01 CURRENT-USER-NAME             PIC X(20) VALUE SPACES.
       01 NO-RECORD-FOUND              PIC 9 VALUE 0.
       01 BALANCE               PIC 9(5) VALUE 0.
       01 TOTAL-COST PIC 9(5) VALUE 0.
       01 CHANGE PIC 9(4) VALUE 0.
       01 TICKET-PRICEU PIC 9(4).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INTRO.
           PERFORM MAIN-MENU UNTIL CHOICE = 0.
           STOP RUN.

       INTRO.
               CALL "SYSTEM" USING "CLS"
              PERFORM CLEAR-SCREEN
              PERFORM CLEAR-SCREEN
              PERFORM BETTER DISPLAY "   ".
              PERFORM BETTER DISPLAY "   ".
              PERFORM BETTER DISPLAY "   ".
              PERFORM BETTER DISPLAY "   ".
              PERFORM BETTER DISPLAY "   ".
              PERFORM BETTER DISPLAY "   ".

       perform MID
       DISPLAY "   TTTTT   EEEEE    AAAAA   M   M      CCCC    H   H III  "
      -     " BBBBB   OOO   L      !!".
       perform MID
       DISPLAY "     T     E       A     A  MM MM      C       H   H  I    "
      -         " B    B O   O  L      !!".
       perform MID
       DISPLAY "     T     EEEE    AAAAAAA  M M M      C       HHHHH  I    "
      -         " BBBBB  O   O  L      !!".
       perform MID
       DISPLAY "     T     E       A     A  M   M      C       H   H  I    "
      -         " B    B O   O  L      ".
       perform MID
       DISPLAY "     T     EEEEE   A     A  M   M      CCCC    H   H III   "
      -         " BBBBB   OOO   LLLLL  !!".
       PERFORM MID DISPLAY "   ".
       PERFORM MID DISPLAY "   ".
       PERFORM MID DISPLAY "   ".

       perform MID display"press any key to continue..." .
       accept  END-IN.

       MAIN-MENU.
           CALL "SYSTEM" USING 'CLS'
              PERFORM CLEAR-SCREEN
              PERFORM CLEAR-SCREEN



            CALL "SYSTEM" USING 'CLS'
               PERFORM CLEAR-SCREEN.
               PERFORM CLEAR-SCREEN
               PERFORM PRINT-BLANK-LINES
              PERFORM BETTER DISPLAY
           "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           perform better
           DISPLAY "|                                               |".
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |".
           perform better
           DISPLAY "|   *        EVENT MANAGEMENT SYSTEM        *   |".
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |".
           perform better
           DISPLAY "|                                               |".
           perform better
           DISPLAY"=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           perform better
           DISPLAY "|    [01] LOG IN                                |".
           perform better
           DISPLAY "|    [02] SIGN UP                               |".
           perform better
           DISPLAY "|    [00] EXIT PROGRAM                          |".
           perform better
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="

            PERFORM BETTER DISPLAY ' '.
            PERFORM BETTER DISPLAY 'INPUT:' WITH NO ADVANCING.
                   ACCEPT CHOICE.
               PERFORM CLEAR-SCREEN.

               EVALUATE TRUE
                   WHEN CHOICE = 1
                       PERFORM ADMIN-OR-GUEST-SIGN-IN UNTIL CHOICE = 0
                       SET CHOICE TO 3
                   WHEN CHOICE = 2
                       PERFORM SIGN-UP UNTIL CHOICE = 0
                       SET CHOICE TO 3
                   WHEN CHOICE = 0
                       PERFORM ENDRUN
                       PERFORM CLEAR-SCREEN
                       STOP RUN
                   WHEN OTHER
                       PERFORM BETTER DISPLAY 'INVALID INPUT'
                        PERFORM CLEAR-SCREEN.
               PERFORM CLEAR-SCREEN
               PERFORM PRINT-BLANK-LINES
                       PERFORM ENDRUN.


       ADMIN-OR-GUEST-SIGN-IN.
              CALL "SYSTEM" USING 'CLS'
              PERFORM CLEAR-SCREEN
              PERFORM CLEAR-SCREEN
               PERFORM BETTER DISPLAY "   ".
              PERFORM BETTER DISPLAY "   ".
              PERFORM BETTER DISPLAY "   ".
           PERFORM BETTER DISPLAY
           "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           perform better
           DISPLAY "|                                               |".
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |".
           perform better
           DISPLAY "|   *            ADMIN OR GUEST?            *   |".
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |".
           perform better
           DISPLAY "|                                               |".
           perform better
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           perform better
           DISPLAY "|    [01] ADMIN                                 |".
           perform better
           DISPLAY "|    [02] GUEST                                 |".
           perform better
           DISPLAY "|    [00] EXIT                                  |".
           perform better
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="

            PERFORM BETTER
            DISPLAY 'ENTER YOUR CHOICE:' WITH NO ADVANCING.
            ACCEPT CHOICE

               PERFORM CLEAR-SCREEN.

               EVALUATE TRUE
                   WHEN CHOICE = 1
                       PERFORM ADMIN-SIGN-IN
                   WHEN CHOICE = 2
                       PERFORM GUEST-SIGN-IN
                   WHEN CHOICE = 0
                       PERFORM MAIN-MENU
                       PERFORM CLEAR-SCREEN
                       STOP RUN
                   WHEN OTHER
                       PERFORM BETTER DISPLAY 'INVALID INPUT'
                       PERFORM ENDRUN
                       PERFORM CLEAR-SCREEN
                   END-EVALUATE.

       SIGN-UP-MENU.
              CALL "SYSTEM" USING 'CLS'
              PERFORM CLEAR-SCREEN
              PERFORM SIGN-UP.


       LOG-IN-MENU.
              CALL "SYSTEM" USING 'CLS'
              PERFORM CLEAR-SCREEN
              PERFORM CLEAR-SCREEN
              PERFORM PRINT-BLANK-LINES
              PERFORM BETTER DISPLAY "-------------------------------"
              PERFORM BETTER DISPLAY "[     ADMIN OR GUEST?         ]"
              PERFORM BETTER DISPLAY "[-----------------------------]"
              PERFORM BETTER DISPLAY "[  [1]. ADMIN                 ]"
              PERFORM BETTER DISPLAY "[  [2]. GUEST                 ]"
              PERFORM BETTER DISPLAY "[  [0]. EXIT                  ]"
              PERFORM BETTER DISPLAY "[-----------------------------]"
              PERFORM BETTER DISPLAY "[ ENTER YOUR CHOICE:          ]"
              PERFORM BETTER DISPLAY "-------------------------------"
               PERFORM BETTER DISPLAY 'INPUT: ' WITH NO ADVANCING.
                   ACCEPT CHOICE.
               PERFORM CLEAR-SCREEN.

               EVALUATE TRUE
                   WHEN CHOICE = 1
                       PERFORM ADMIN-SIGN-IN
                   WHEN CHOICE = 2
                       PERFORM GUEST-SIGN-IN
                   WHEN CHOICE = 0
                       PERFORM ENDRUN
                       PERFORM CLEAR-SCREEN
                   WHEN OTHER
                       PERFORM BETTER DISPLAY 'INVALID INPUT'
                       PERFORM ENDRUN
                       PERFORM CLEAR-SCREEN
                   END-EVALUATE.

       ADMIN-SIGN-IN.
            CALL "SYSTEM" USING "CLS"
            MOVE " " TO USERNAME
            MOVE " " TO PASSWORDS
            MOVE " " TO USER-TYPE
            PERFORM CLEAR-SCREEN
            PERFORM CLEAR-SCREEN
            PERFORM BETTER DISPLAY "   ".
            PERFORM BETTER DISPLAY "   ".
            PERFORM BETTER DISPLAY "   ".
             PERFORM BETTER DISPLAY
           "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           perform better
           DISPLAY "|                                               |".
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |".
           perform better
           DISPLAY "|   *              ADMIN LOG-IN             *   |".
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |".
           perform better
           DISPLAY "|                                               |".
           perform better
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
            PERFORM BETTER DISPLAY "   ".
            PERFORM BETTER DISPLAY "   ".
            PERFORM BETTER DISPLAY "   ".
            PERFORM BETTER
            DISPLAY "Enter Username:"
            PERFORM BETTER ACCEPT USERNAME
            PERFORM BETTER DISPLAY "Enter Password:"
            PERFORM BETTER ACCEPT PASSWORDS
            PERFORM BETTER display
           'Enter user-type (A for Admin, G for Guest):'
            PERFORM BETTER ACCEPT USER-TYPE
            MOVE FUNCTION UPPER-CASE(user-type) TO user-type
            ACCEPT END-IN
            perform ADMIN-VERIFY-CREDENTIALS.

       GUEST-SIGN-IN.
            CALL "SYSTEM" USING "CLS"
            MOVE " " TO USERNAME
            MOVE " " TO PASSWORDS
            MOVE " " TO USER-TYPE
            PERFORM CLEAR-SCREEN
            PERFORM CLEAR-SCREEN
            PERFORM BETTER DISPLAY "   ".
            PERFORM BETTER DISPLAY "   ".
            PERFORM BETTER DISPLAY "   ".
             PERFORM BETTER DISPLAY
           "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           perform better
           DISPLAY "|                                               |".
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |".
           perform better
           DISPLAY "|   *              GUEST LOG-IN             *   |".
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |".
           perform better
           DISPLAY "|                                               |".
           perform better
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
            PERFORM BETTER DISPLAY "   ".
            PERFORM BETTER DISPLAY "   ".
            PERFORM BETTER DISPLAY "   ".
            PERFORM BETTER DISPLAY 'Enter Username: '
            PERFORM BETTER ACCEPT USERNAME
            PERFORM BETTER DISPLAY 'Enter Password: '
            PERFORM BETTER ACCEPT PASSWORDS
            PERFORM BETTER
            DISPLAY 'Enter user-type (A for Admin, G for Guest):'
            PERFORM BETTER ACCEPT USER-TYPE
            MOVE FUNCTION UPPER-CASE(user-type) TO user-type
            ACCEPT END-IN
            perform GUEST-VERIFY-CREDENTIALS.

       ADMIN-VERIFY-CREDENTIALS.
           CLOSE AdminFile
           OPEN INPUT AdminFile
           MOVE 0 TO EOF
           MOVE 'N' TO VALID-USER

           PERFORM UNTIL EOF = 1 OR VALID-USER = 'Y'
               READ AdminFile
                   AT END
                       MOVE 1 TO EOF
                   NOT AT END
                       PERFORM ENCRYPT-PASSWORD
                       IF USERNAME = ADMIN-STORED-USERNAME AND
                          ENCRYPTED-PASSWORD = ADMIN-STORED-PASSWORD AND
                          USER-TYPE = ADMIN-STORED-USER-TYPE
                           MOVE 'Y' TO VALID-USER
                           PERFORM BETTER DISPLAY "Log in successful."
                           ACCEPT END-IN
                           PERFORM ADMIN-FEATURES
                       END-IF
               END-READ
           END-PERFORM
           CLOSE AdminFile
           IF VALID-USER = 'N'
               PERFORM BETTER
               DISPLAY 'Invalid credentials, please try again.'
               ACCEPT END-IN
           END-IF.

       GUEST-VERIFY-CREDENTIALS.
           CLOSE UserFile
           OPEN INPUT UserFile
           MOVE 0 TO EOF
           MOVE 'N' TO VALID-USER

           PERFORM UNTIL EOF = 1 OR VALID-USER = 'Y'
               READ UserFile
                   AT END
                       MOVE 1 TO EOF
                   NOT AT END
                       PERFORM ENCRYPT-PASSWORD
                       IF USERNAME = ATTENDEE-STORED-USERNAME AND
                           ENCRYPTED-PASSWORD = ATTENDEE-STORED-PASSWORD
                           AND
                           USER-TYPE = ATTENDEE-STORED-USER-TYPE
                           MOVE 'Y' TO VALID-USER
                           PERFORM BETTER DISPLAY "Log in successful."
                           ACCEPT END-IN
                           PERFORM GUEST-FEATURES
                       END-IF
           END-PERFORM
           CLOSE UserFile
           IF VALID-USER = 'N'
               PERFORM BETTER
               DISPLAY 'Invalid credentials, please try again.'
               ACCEPT END-IN
           END-IF.



       GUEST-SAVE-CREDENTIALS.
           CALL "SYSTEM" USING "CLS"
           PERFORM CLEAR-SCREEN
           PERFORM CLEAR-SCREEN
           PERFORM PRINT-BLANK-LINES
           PERFORM BETTER DISPLAY
           "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           PERFORM BETTER DISPLAY "Encrypting Password...".
           PERFORM ENCRYPT-PASSWORD.
           OPEN EXTEND UserFile
               IF FILE-STATUS = "30"
                   OPEN OUTPUT UserFile
               END-IF
           MOVE NEW-USERNAME TO ATTENDEE-STORED-USERNAME
           MOVE ENCRYPTED-PASSWORD TO ATTENDEE-STORED-PASSWORD
           MOVE NEW-USER-TYPE TO ATTENDEE-STORED-USER-TYPE
           WRITE UserRecord
           CLOSE UserFile
           PERFORM BETTER DISPLAY 'Sign-up successful, please sign in.'
           ACCEPT OMITTED
           PERFORM GUEST-SIGN-IN.


       ADMIN-SAVE-CREDENTIALS.
           CALL "SYSTEM" USING "CLS".
           PERFORM CLEAR-SCREEN
           PERFORM CLEAR-SCREEN
           PERFORM PRINT-BLANK-LINES
           PERFORM BETTER DISPLAY
           "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           PERFORM BETTER DISPLAY "Encrypting Password...".
           PERFORM ENCRYPT-PASSWORD.
           OPEN EXTEND AdminFile
               IF FILE-STATUS = "30"
                   OPEN OUTPUT AdminFile
               END-IF
           MOVE NEW-USERNAME TO ADMIN-STORED-USERNAME
           MOVE ENCRYPTED-PASSWORD TO ADMIN-STORED-PASSWORD
           MOVE NEW-USER-TYPE TO ADMIN-STORED-USER-TYPE
           WRITE AdminRecord
           CLOSE AdminFile
           PERFORM BETTER DISPLAY 'Sign-up successful, please sign in.'
           ACCEPT OMITTED
           PERFORM ADMIN-SIGN-IN.

       ADMIN-FEATURES.
               PERFORM UNTIL CHOICE = 8
               CALL "SYSTEM" USING 'CLS'
               MOVE 0 TO CHOICE
               PERFORM CLEAR-SCREEN
               PERFORM PRINT-BLANK-LINES
              PERFORM BETTER DISPLAY
           "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           perform better
           DISPLAY "|                                               |"
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |"
           perform better
           DISPLAY "|   *              ADMIN MENU               *   |"
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |"
           perform better
           DISPLAY "|                                               |"
           perform better
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           perform better
           DISPLAY "|    [01] REGISTER-EVENT                        |"
           perform better
           DISPLAY "|    [02] VIEW-EVENTS                           |"
           perform better
           DISPLAY "|    [03] SEARCH-EVENTS                         |"
           perform better
           DISPLAY "|    [04] DELETE-EVENTS                         |"
           perform better
           DISPLAY "|    [05] MODIFY-LOCATION                       |"
           perform better
           DISPLAY "|    [06] EDIT-EVENTS                           |"
           perform better
           DISPLAY "|    [07] VIEW-REPORTS                          |"
           perform better
           DISPLAY "|    [08] LOG-OUT                               |"
           perform better
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="


           PERFORM BETTER DISPLAY ' '
            PERFORM BETTER
            DISPLAY '   ENTER YOUR CHOICE:' WITH NO ADVANCING
            ACCEPT CHOICE

               EVALUATE CHOICE
                   WHEN 1
                       PERFORM REGISTER-EVENT
                       PERFORM CLEAR-SCREEN
                   WHEN 2
                       PERFORM VIEW-EVENTS
                       PERFORM CLEAR-SCREEN
                   WHEN 3
                       PERFORM SEARCH-EVENT2
                       PERFORM CLEAR-SCREEN
                   WHEN 4
                       PERFORM DELETE-EVENT
                       PERFORM CLEAR-SCREEN
                   WHEN 5
                       PERFORM MODIFY-LOCATION
                       PERFORM CLEAR-SCREEN
                   WHEN 6
                       PERFORM EDIT-EVENT
                       PERFORM CLEAR-SCREEN
                   WHEN 7
                       PERFORM VIEW-REPORTS
                       PERFORM CLEAR-SCREEN
                   WHEN 8
                       PERFORM MAIN-MENU
                   WHEN OTHER
                       PERFORM BETTER
                       DISPLAY 'Invalid choice, please try again.'
               END-EVALUATE
           END-PERFORM.


       GUEST-FEATURES.
           PERFORM UNTIL CHOICE = 5
            CALL "SYSTEM" USING 'CLS'
            MOVE 0 TO CHOICE
            PERFORM CLEAR-SCREEN
            PERFORM PRINT-BLANK-LINES
            PERFORM BETTER
            DISPLAY
            "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           perform better
           DISPLAY "|                                               |"
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |"
           perform better
           DISPLAY "|   *              GUEST MENU               *   |"
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |"
           perform better
           DISPLAY "|                                               |"
           perform better
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           perform better
           DISPLAY "|    [01] BOOK-EVENTS                           |"
           perform better
           DISPLAY "|    [02] SEARCH-EVENTS                         |"
           perform better
           DISPLAY "|    [03] VIEW-MY-BOOKINGS                      |"
           perform better
           DISPLAY "|    [04] LOG-OUT                               |"
           perform better
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           PERFORM BETTER DISPLAY ' '
            PERFORM BETTER
            DISPLAY '   ENTER YOUR CHOICE:' WITH NO ADVANCING
            ACCEPT CHOICE

            EVALUATE CHOICE
                WHEN 1
                        PERFORM BOOK-EVENTS
                        PERFORM CLEAR-SCREEN
                WHEN 2
                        PERFORM SEARCH-EVENT
                        PERFORM CLEAR-SCREEN
                WHEN 3
                        PERFORM VIEW-MY-BOOKINGS
                WHEN 4
                        PERFORM MAIN-MENU
                WHEN OTHER
                    PERFORM BETTER DISPLAY 'Invalid choice, '
                    'please try again.'
            END-EVALUATE
           END-PERFORM.


       ENCRYPT-PASSWORD.
           MOVE SPACES TO ENCRYPTED-PASSWORD.
           PERFORM VARYING CHAR-INDEX FROM 1 BY 1 UNTIL
           CHAR-INDEX > LENGTH OF PASSWORDS
               MOVE PASSWORDS(CHAR-INDEX:1) TO TEMP-CHAR
               IF TEMP-CHAR NOT = SPACE
                   COMPUTE TEMP-CHAR =
                   FUNCTION MOD((FUNCTION ORD(TEMP-CHAR) + SHIFT-VALUE),
                   256)
                   MOVE TEMP-CHAR TO ENCRYPTED-PASSWORD(CHAR-INDEX:1)
               ELSE
                   MOVE SPACE TO ENCRYPTED-PASSWORD(CHAR-INDEX:1)
               END-IF
           end-perform.


       SIGN-UP.
           MOVE " " TO NEW-USER-TYPE
            CALL "SYSTEM" USING "CLS"
            PERFORM CLEAR-SCREEN
            PERFORM CLEAR-SCREEN
           PERFORM BETTER DISPLAY "   ".
            PERFORM BETTER DISPLAY "   ".
            PERFORM BETTER DISPLAY "   ".
            PERFORM BETTER DISPLAY
           "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           perform better
           DISPLAY "|                                               |"
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |"
           perform better
           DISPLAY "|   *                 SIGN UP               *   |"
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |"
           perform better
           DISPLAY "|                                               |"
           perform better
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
            PERFORM BETTER DISPLAY 'Enter New Username:'
            PERFORM BETTER ACCEPT NEW-USERNAME
            PERFORM BETTER DISPLAY 'Enter New Password:'
            PERFORM BETTER ACCEPT NEW-PASSWORD
            PERFORM BETTER
            DISPLAY 'Enter user-type(A for Admin | G for Guest):'
            PERFORM BETTER ACCEPT NEW-USER-TYPE
            MOVE FUNCTION UPPER-CASE(NEW-USER-TYPE) TO NEW-USER-TYPE

            IF NEW-USER-TYPE = 'A'
               PERFORM ADMIN-SAVE-CREDENTIALS
               ACCEPT END-IN
            ELSE IF NEW-USER-TYPE = 'G'
               PERFORM GUEST-SAVE-CREDENTIALS
               ACCEPT END-IN
            PERFORM CHECK-USER-TYPE.

       CHECK-USER-TYPE.
            MOVE " " TO USER-ROLE
            IF USER-TYPE = 'A'
                MOVE 'ADMIN' TO USER-ROLE
                PERFORM ADMIN-FEATURES
            ELSE IF USER-TYPE = 'G'
                MOVE 'USER' TO USER-ROLE
                PERFORM GUEST-FEATURES
            END-IF.


       FEATURES.
           MOVE " " TO USER-ROLE
           IF USER-ROLE = 'ADMIN'
               PERFORM GUEST-FEATURES
           ELSE IF USER-ROLE = 'GUEST'
               PERFORM ADMIN-FEATURES
           EXIT.


       REGISTER-EVENT.
           CALL "SYSTEM" USING 'CLS'
           PERFORM CLEAR-SCREEN
            PERFORM PRINT-BLANK-LINES
            PERFORM PRINT-BLANK-LINES
           PERFORM BETTER DISPLAY "ENTER YOUR AGE: " WITH NO ADVANCING
           ACCEPT AGE
           IF AGE >= 18 THEN
               DISPLAY " "
               PERFORM BETTER
               DISPLAY "You are Eligible to Register an Event."
               DISPLAY " "
               PERFORM ENDRUN
               CALL "SYSTEM" USING 'CLS'
                PERFORM CLEAR-SCREEN
                PERFORM PRINT-BLANK-LINES
               PERFORM BETTER DISPLAY "ENTER EVENT NAME: "
               WITH NO ADVANCING
               ACCEPT EVENTNAME

               PERFORM BETTER DISPLAY "ENTER EVENT DATE (MM/DD/YY): "
                WITH NO ADVANCING
               ACCEPT EVENTDATE

               MOVE FUNCTION CURRENT-DATE (1:4) TO CUR-YEAR
               MOVE FUNCTION CURRENT-DATE (5:2) TO CUR-MONTH
               MOVE FUNCTION CURRENT-DATE (7:2) TO CUR-DAY

               MOVE EVENTDATE (7:2) TO TEMP-YEAR
               ADD 2000 TO TEMP-YEAR
               MOVE TEMP-YEAR TO EVENT-YEAR

               MOVE EVENTDATE (1:2) TO EVENT-MONTH
               MOVE EVENTDATE (4:2) TO EVENT-DAY

           IF EVENT-YEAR NUMERIC AND EVENT-MONTH NUMERIC AND
           EVENT-DAY NUMERIC THEN
                   IF EVENT-YEAR > CUR-YEAR OR (EVENT-YEAR = CUR-YEAR
                   AND EVENT-MONTH > CUR-MONTH) OR (EVENT-YEAR =
           CUR-YEAR AND EVENT-MONTH = CUR-MONTH AND EVENT-DAY >=
           CUR-DAY) THEN
                       PERFORM BETTER DISPLAY "ENTER ORGANIZER NAME: "
                       WITH NO ADVANCING ACCEPT ORGANIZER
                       perform better
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
                       PERFORM BETTER DISPLAY " "
                       PERFORM BETTER DISPLAY "SELECT EVENT LOCATION: "
                       PERFORM BETTER
                       DISPLAY "Select location for this event:"
                       PERFORM DISPLAY-LOCATION-OPTIONS
                        perform better
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
                       PERFORM BETTER DISPLAY "Admin choice: "
                       WITH NO ADVANCING
                       ACCEPT LOCATION-CHOICE
                       PERFORM GET-LOCATION

                       PERFORM BETTER DISPLAY "Location selected: "
                        LOCATION-NAME
                       PERFORM BETTER DISPLAY " "
                       PERFORM BETTER DISPLAY "ENTER TICKET PRICE: P"
                       WITH NO ADVANCING
                       ACCEPT TICKET-PRICE

                       PERFORM BETTER
                       DISPLAY "ENTER NUMBER OF AVAILABLE SEATS: "
                       WITH NO ADVANCING
                       ACCEPT AVAILABLE-SEATS

                       MOVE 0 TO BALANCE
                       PERFORM BETTER DISPLAY "You are about to rent "
                       LOCATION-NAME " for a total price of P "
                       LOCATION-PRICE "."

                       PERFORM BETTER
                       DISPLAY "HOW MUCH ARE YOU GOING TO PAY?: "
                       WITH NO ADVANCING ACCEPT BALANCE
                       ACCEPT OMITTED

                           IF BALANCE >= location-price THEN

                               COMPUTE CHANGE = BALANCE -
                               LOCATION-PRICE
                               DISPLAY " "
                               PERFORM BETTER
                               DISPLAY "Payment accepted."
                               PERFORM BETTER
                               DISPLAY "Your change is P" CHANGE "."
                               PERFORM BETTER
                               DISPLAY "Enjoy your event!"
                               ACCEPT OMITTED
                               DISPLAY " "
                           ELSE
                               DISPLAY " "
                               PERFORM BETTER
                               DISPLAY "Insufficient Balance."
                               ACCEPT OMITTED
                               PERFORM ADMIN-FEATURES
                               DISPLAY " "
                           END-IF


                       OPEN EXTEND EventFile
                       IF FILE-STATUS = "30"
                           OPEN OUTPUT EventFile
                       END-IF
                       IF FILE-STATUS = "00"
                           WRITE EVENTData
                           IF FILE-STATUS = "00"
           CALL "SYSTEM" USING 'CLS'
           DISPLAY " "
           DISPLAY " "
           DISPLAY " "
           DISPLAY " "
           DISPLAY " "
           DISPLAY " "
           DISPLAY " "
           perform better
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           perform better
           DISPLAY "|                 EVENT REGISTERED!             |"
           perform better
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           DISPLAY " "

           PERFORM BETTER DISPLAY "YOUR EVENT: " EVENTNAME
           PERFORM BETTER DISPLAY "HAS BEEN REGISTERED! "
           DISPLAY " "
           PERFORM BETTER
           DISPLAY "This Event has been successfully registered at "
           LOCATION-NAME
           PERFORM BETTER
           DISPLAY"with a Rental Price of P " LOCATION-PRICE
           DISPLAY " "

           PERFORM BETTER DISPLAY"Enjoy your event ^^!"

                           ELSE
                               PERFORM BETTER
                               DISPLAY "Error writing to EventFile: "
                                FILE-STATUS
                           END-IF
                       ELSE
                           PERFORM BETTER
                           DISPLAY "Error opening EventFile: "
                           FILE-STATUS
                       END-IF
                       CLOSE EventFile
                   ELSE
                       PERFORM BETTER
                    DISPLAY "Error: Event date must not be in the past."
                   END-IF
               ELSE
                   PERFORM BETTER
                   DISPLAY "Error: Invalid event date format."
               END-IF
           ELSE
               PERFORM BETTER
           DISPLAY "You must be at least 18 years old to book an event."
           END-IF
           DISPLAY " "
           PERFORM ENDRUN
           PERFORM ADMIN-FEATURES.

       MODIFY-LOCATION.
           CALL "SYSTEM" USING 'CLS'
           PERFORM CLEAR-SCREEN
           PERFORM CLEAR-SCREEN
           PERFORM PRINT-BLANK-LINES
           PERFORM BETTER DISPLAY
           "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           perform better
           DISPLAY "|                                               |"
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |"
           perform better
           DISPLAY "|   *           MODIFY LOCATIONS            *   |"
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |"
           perform better
           DISPLAY "|                                               |"
           perform better
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           perform better
           DISPLAY "|    [01] ADD LOCATION                          |"
           perform better
           DISPLAY "|    [02] EDIT LOCATION                         |"
           perform better
           DISPLAY "|    [03] DELETE LOCATION                       |"
           perform better
           DISPLAY "|    [04] VIEW LOCATION                         |"
           perform better
           DISPLAY "|    [05] EDIT RENTAL PRICE                     |"
           perform better
           DISPLAY "|    [06] BACK                                  |"
           perform better
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="

           PERFORM BETTER DISPLAY "Enter your choice: "
           WITH NO ADVANCING
           ACCEPT LOCATION-CHOICE

           EVALUATE LOCATION-CHOICE
               WHEN 1
                   PERFORM ADD-LOCATION
               WHEN 2
                   PERFORM EDIT-LOCATION
               WHEN 3
                   PERFORM DELETE-LOCATION
               WHEN 4
                   PERFORM VIEW-LOCATIONS
               WHEN 5
                   PERFORM EDIT-RENTAL-PRICE
               WHEN 6
                   PERFORM ADMIN-FEATURES
               WHEN OTHER
                   PERFORM BETTER
                   DISPLAY "Invalid choice. No changes made."
           END-EVALUATE.


       ADD-LOCATION.
           CALL "SYSTEM" USING 'CLS'
           PERFORM CLEAR-SCREEN
           PERFORM PRINT-BLANK-LINES
           PERFORM BETTER DISPLAY "   ".
            PERFORM BETTER DISPLAY "   ".
            PERFORM BETTER DISPLAY "   ".
            PERFORM BETTER DISPLAY
           "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           perform better
           DISPLAY "|                                               |"
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |"
           perform better
           DISPLAY "|   *            ADD NEW LOCATION           *   |"
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |"
           perform better
           DISPLAY "|                                               |"
           perform better
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           PERFORM BETTER DISPLAY " "
           PERFORM BETTER DISPLAY " "
           PERFORM BETTER DISPLAY "Enter new location name: "
           WITH NO ADVANCING
           ACCEPT NEW-LOCATIONS

           PERFORM BETTER DISPLAY "Enter rental price: P"
           WITH NO ADVANCING
           ACCEPT LOCATION-PRICE

           IF NEW-LOCATIONS = SPACES THEN
               PERFORM BETTER
           DISPLAY " Please enter a valid location name. "
               PERFORM ENDRUN
               PERFORM ADD-LOCATION
           ELSE
               OPEN EXTEND LocationFile
               IF FILE-STATUS = 30
                   OPEN OUTPUT LocationFile
               END-IF
               MOVE NEW-LOCATIONS TO LOCATION-NAME
               MOVE LOCATION-PRICE TO LOCATION-PRICE-FIELD
               WRITE LocationRecord
               CLOSE LocationFile

               PERFORM BETTER DISPLAY "Location added successfully!"
               PERFORM BETTER DISPLAY " "

               PERFORM BETTER
               DISPLAY "Do you want to input more? (Y/N): "
               WITH NO ADVANCING
               ACCEPT WS-CONFIRM

               IF WS-CONFIRM = 'Y' OR WS-CONFIRM = 'y'
                   PERFORM ADD-LOCATION
               ELSE
                   PERFORM MODIFY-LOCATION
               END-IF
           END-IF.



       EDIT-LOCATION.
           CALL "SYSTEM" USING 'CLS'
           PERFORM CLEAR-SCREEN
           PERFORM PRINT-BLANK-LINES
            PERFORM BETTER DISPLAY "   "
            PERFORM BETTER DISPLAY "   "
            PERFORM BETTER DISPLAY "   "
            PERFORM BETTER DISPLAY
           "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           perform better
           DISPLAY "|                                               |"
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |"
           perform better
           DISPLAY "|   *             EDIT LOCATION             *   |"
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |"
           perform better
           DISPLAY "|                                               |"
           perform better
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           PERFORM DISPLAY-LOCATION-OPTIONS
           PERFORM BETTER DISPLAY "Admin choice: " WITH NO ADVANCING
           ACCEPT LOCATION-CHOICE

             IF LOCATION-CHOICE = SPACES THEN
               PERFORM BETTER DISPLAY "Please make a valid choice."
               PERFORM ENDRUN
               PERFORM EDIT-LOCATION
           ELSE
               OPEN INPUT LocationFile
               MOVE 0 TO EOF
               MOVE 1 TO LOCATION-INDEX
               MOVE " " TO NEW-LOCATION
               PERFORM UNTIL EOF = 1
                   READ LocationFile
                       AT END
                           MOVE 1 TO EOF
                       NOT AT END
                           IF LOCATION-CHOICE = LOCATION-INDEX THEN
                               MOVE LOCATION-NAME TO NEW-LOCATION
                               EXIT PERFORM
                           END-IF
                           ADD 1 TO LOCATION-INDEX
                   END-READ
               END-PERFORM
               CLOSE LocationFile

               PERFORM BETTER DISPLAY "Location to edit: " NEW-LOCATION

               OPEN INPUT LocationFile
               OPEN OUTPUT TEMP-LOCATION-FILE
               MOVE 0 TO EOF
               MOVE 0 TO EVENT-FOUND
               PERFORM UNTIL EOF = 1
                   READ LocationFile
                       AT END
                           MOVE 1 TO EOF
                       NOT AT END
                           IF LOCATION-NAME = NEW-LOCATION THEN
                               PERFORM BETTER
                               DISPLAY "Enter new location name: "
                                WITH NO ADVANCING
                               ACCEPT NEW-LOCATION
                               MOVE NEW-LOCATION TO LOCATION-NAME
                               MOVE 1 TO EVENT-FOUND
                           END-IF
                       WRITE TEMP-LOCATION-RECORD FROM LocationRecord
                   END-READ
               END-PERFORM
               CLOSE LocationFile
               CLOSE TEMP-LOCATION-FILE

               IF EVENT-FOUND = 0 THEN
                   PERFORM BETTER DISPLAY
                    "No location found with that name."
               ELSE
                   OPEN INPUT TEMP-LOCATION-FILE
                   OPEN OUTPUT LocationFile
                   MOVE 0 TO EOF
                   PERFORM UNTIL EOF = 1
                       READ TEMP-LOCATION-FILE
                           AT END
                               MOVE 1 TO EOF
                           NOT AT END
                               WRITE LocationRecord FROM
                               TEMP-LOCATION-RECORD
                       END-READ
                   END-PERFORM
                   CLOSE TEMP-LOCATION-FILE
                   CLOSE LocationFile
                   PERFORM BETTER
                    DISPLAY "Location updated successfully!"
               END-IF.

            DISPLAY " "
           PERFORM ENDRUN
               PERFORM MODIFY-LOCATION.
       EDIT-RENTAL-PRICE.
           CALL "SYSTEM" USING 'CLS'
           PERFORM CLEAR-SCREEN
           PERFORM PRINT-BLANK-LINES
           PERFORM BETTER DISPLAY "   "
            PERFORM BETTER DISPLAY "   "
            PERFORM BETTER DISPLAY "   "
            PERFORM BETTER DISPLAY
           "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           perform better
           DISPLAY "|                                               |"
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |"
           perform better
           DISPLAY "|   *      EDIT LOCATION RENTAL PRICE       *   |"
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |"
           perform better
           DISPLAY "|                                               |"
           perform better
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           PERFORM DISPLAY-LOCATION-OPTIONS
           PERFORM BETTER DISPLAY "Admin choice: " WITH NO ADVANCING
           ACCEPT LOCATION-CHOICE

           IF LOCATION-CHOICE = SPACES THEN
               PERFORM BETTER DISPLAY "Please make a valid choice."
               PERFORM ENDRUN
               PERFORM EDIT-RENTAL-PRICE
           ELSE
               OPEN INPUT LocationFile
               MOVE 0 TO EOF
               MOVE 1 TO LOCATION-INDEX
               MOVE " " TO NEW-LOCATION
               PERFORM UNTIL EOF = 1
                   READ LocationFile
                       AT END
                           MOVE 1 TO EOF
                       NOT AT END
                           IF LOCATION-CHOICE = LOCATION-INDEX THEN
                               MOVE LOCATION-NAME TO NEW-LOCATION
                               MOVE LOCATION-PRICE-FIELD TO OLD-PRICE
                               EXIT PERFORM
                           END-IF
                           ADD 1 TO LOCATION-INDEX
                   END-READ
               END-PERFORM
               CLOSE LocationFile

               PERFORM BETTER DISPLAY "Location to edit price: "
                NEW-LOCATION
               PERFORM BETTER DISPLAY "Current rental price: P"
               OLD-PRICE
            perform better
            DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
             PERFORM BETTER DISPLAY "   "
               PERFORM BETTER DISPLAY "Enter new rental price: P"
               WITH NO ADVANCING
               ACCEPT NEW-PRICE

               OPEN INPUT LocationFile
               OPEN OUTPUT TEMP-LOCATION-FILE
               MOVE 0 TO EOF
               MOVE 0 TO EVENT-FOUND
               PERFORM UNTIL EOF = 1
                   READ LocationFile
                       AT END
                           MOVE 1 TO EOF
                       NOT AT END
                           IF LOCATION-NAME = NEW-LOCATION THEN
                               MOVE NEW-PRICE TO LOCATION-PRICE-FIELD
                               MOVE 1 TO EVENT-FOUND
                           END-IF
                       WRITE TEMP-LOCATION-RECORD FROM LocationRecord
                   END-READ
               END-PERFORM
               CLOSE LocationFile
               CLOSE TEMP-LOCATION-FILE

               IF EVENT-FOUND = 0 THEN
                   PERFORM BETTER DISPLAY
                    "No location found with that name."
               ELSE
                   OPEN INPUT TEMP-LOCATION-FILE
                   OPEN OUTPUT LocationFile
                   MOVE 0 TO EOF
                   PERFORM UNTIL EOF = 1
                       READ TEMP-LOCATION-FILE
                           AT END
                               MOVE 1 TO EOF
                           NOT AT END
                               WRITE LocationRecord FROM
                               TEMP-LOCATION-RECORD
                       END-READ
                   END-PERFORM
                   CLOSE TEMP-LOCATION-FILE
                   CLOSE LocationFile
                   PERFORM BETTER
                    DISPLAY "Rental price updated successfully!"
               END-IF.
           DISPLAY " "
           PERFORM ENDRUN
           PERFORM MODIFY-LOCATION.

           PERFORM DISPLAY-LOCATIONS-PARA.

       DELETE-LOCATION.
           CALL "SYSTEM" USING 'CLS'
           PERFORM CLEAR-SCREEN
           PERFORM PRINT-BLANK-LINES

            PERFORM BETTER DISPLAY "   "
            PERFORM BETTER DISPLAY "   "
            PERFORM BETTER DISPLAY "   "
            PERFORM BETTER DISPLAY
           "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           perform better
           DISPLAY "|                                               |"
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |"
           perform better
           DISPLAY "|   *             DELETE LOCATION           *   |"
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |"
           perform better
           DISPLAY "|                                               |"
           perform better
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="

           PERFORM BETTER DISPLAY "     Select location to delete:   "
           PERFORM BETTER DISPLAY "  "
           PERFORM DISPLAY-LOCATION-OPTIONS
           PERFORM BETTER DISPLAY
           "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           PERFORM BETTER DISPLAY "Admin choice: " WITH NO ADVANCING
           ACCEPT LOCATION-CHOICE


           IF LOCATION-CHOICE = SPACES THEN
               PERFORM BETTER DISPLAY "Please make a valid choice."
               PERFORM DELETE-LOCATION
           ELSE
               OPEN INPUT LocationFile
               MOVE 0 TO EOF
               MOVE 1 TO LOCATION-INDEX
               MOVE " " TO NEW-LOCATION
               PERFORM UNTIL EOF = 1
                   READ LocationFile
                       AT END
                           MOVE 1 TO EOF
                       NOT AT END
                           IF LOCATION-CHOICE = LOCATION-INDEX THEN
                               MOVE LOCATION-NAME TO NEW-LOCATION
                               EXIT PERFORM
                           END-IF
                           ADD 1 TO LOCATION-INDEX
                   END-READ
               END-PERFORM
               CLOSE LocationFile

               PERFORM BETTER DISPLAY "Location to delete: "
               NEW-LOCATION

               OPEN INPUT LocationFile
               OPEN OUTPUT TEMP-LOCATION-FILE
               MOVE 0 TO EOF
               MOVE 0 TO EVENT-FOUND
               PERFORM UNTIL EOF = 1
                   READ LocationFile
                       AT END
                           MOVE 1 TO EOF
                       NOT AT END
                           IF LOCATION-NAME NOT = NEW-LOCATION THEN
                               WRITE TEMP-LOCATION-RECORD FROM
                                LocationRecord
                           ELSE
                               MOVE 1 TO EVENT-FOUND
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE LocationFile
               CLOSE TEMP-LOCATION-FILE

               IF EVENT-FOUND = 0 THEN
                   PERFORM BETTER
                   DISPLAY "No location found with that name."
               ELSE
                   OPEN INPUT TEMP-LOCATION-FILE
                   OPEN OUTPUT LocationFile
                   MOVE 0 TO EOF
                   PERFORM UNTIL EOF = 1
                       READ TEMP-LOCATION-FILE
                           AT END
                               MOVE 1 TO EOF
                           NOT AT END
                               WRITE LocationRecord FROM
                                TEMP-LOCATION-RECORD
                       END-READ
                   END-PERFORM
                   CLOSE TEMP-LOCATION-FILE
                   CLOSE LocationFile
               PERFORM BETTER DISPLAY "Location deleted successfully!"
               END-IF.
             DISPLAY " "
             PERFORM ENDRUN
               PERFORM MODIFY-LOCATION.
       VIEW-LOCATIONS.
           CALL "SYSTEM" USING 'CLS'
           PERFORM CLEAR-SCREEN
           PERFORM PRINT-BLANK-LINES

            PERFORM BETTER DISPLAY "   "
            PERFORM BETTER DISPLAY "   "
            PERFORM BETTER DISPLAY "   "
            PERFORM BETTER DISPLAY
           "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           perform better
           DISPLAY "|                                               |"
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |"
           perform better
           DISPLAY "|   *             VIEW LOCATION             *   |"
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |"
           perform better
           DISPLAY "|                                               |"
           perform better
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="

             PERFORM BETTER DISPLAY "   "
           PERFORM BETTER DISPLAY "            Available Locations:"
           PERFORM BETTER DISPLAY " "
           OPEN INPUT LocationFile
           MOVE 0 TO EOF
           PERFORM UNTIL EOF = 1
               READ LocationFile
                   AT END
                       MOVE 1 TO EOF
                   NOT AT END
                       PERFORM BETTER DISPLAY "Location: " LOCATION-NAME
                       PERFORM BETTER DISPLAY "Price: P"
                       LOCATION-PRICE-FIELD
           perform better
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
                       PERFORM BETTER DISPLAY " "
               END-READ
           END-PERFORM
           CLOSE LocationFile

           PERFORM ENDRUN
           PERFORM MODIFY-LOCATION.

       DISPLAY-LOCATIONS-PARA.
           CALL "SYSTEM" USING 'CLS'
           PERFORM CLEAR-SCREEN
           PERFORM PRINT-BLANK-LINES
            PERFORM BETTER DISPLAY "   "
            PERFORM BETTER DISPLAY "   "
            PERFORM BETTER DISPLAY "   "
            PERFORM BETTER DISPLAY
           "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           perform better
           DISPLAY "|                                               |"
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |"
           perform better
           DISPLAY "|   *            AVAILABLE LOCATION         *   |"
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |"
           perform better
           DISPLAY "|                                               |"
           perform better
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="


           PERFORM BETTER DISPLAY " "

           OPEN INPUT LocationFile
           MOVE 0 TO EOF
           PERFORM UNTIL EOF = 1
               READ LocationFile
                   AT END
                       MOVE 1 TO EOF
                   NOT AT END
                       DISPLAY "                   "
           DISPLAY ">" LOCATION-NAME " | Price: P" LOCATION-PRICE-FIELD
                       PERFORM BETTER DISPLAY " "
               END-READ
           END-PERFORM
           CLOSE LocationFile.

       DISPLAY-LOCATION-OPTIONS.
           OPEN INPUT LocationFile
           MOVE 0 TO EOF
           MOVE 1 TO LOCATION-INDEX
           PERFORM UNTIL EOF = 1
               READ LocationFile
                   AT END
                       MOVE 1 TO EOF
                   NOT AT END
                       PERFORM BETTER
                       DISPLAY "[" LOCATION-INDEX "] " LOCATION-NAME
                        " [Price: P" LOCATION-PRICE-FIELD "]"
                       ADD 1 TO LOCATION-INDEX
               END-READ
           END-PERFORM
           CLOSE LocationFile.

       GET-LOCATION.
           OPEN INPUT LocationFile
           MOVE 0 TO EOF
           MOVE 1 TO LOCATION-INDEX
           PERFORM UNTIL EOF = 1
               READ LocationFile
                   AT END
                       MOVE 1 TO EOF
                   NOT AT END
                       IF LOCATION-CHOICE = LOCATION-INDEX THEN
                           MOVE LOCATION-NAME TO LOCATION
                           MOVE LOCATION-PRICE-FIELD TO LOCATION-PRICE
                           EXIT PERFORM
                       END-IF
                       ADD 1 TO LOCATION-INDEX
               END-READ
           END-PERFORM
           CLOSE LocationFile.

       BOOK-EVENTS.
           CALL "SYSTEM" USING "CLS"
           PERFORM CLEAR-SCREEN
           PERFORM BETTER DISPLAY "   ".
            PERFORM BETTER DISPLAY "   ".
            PERFORM BETTER DISPLAY "   ".
            PERFORM BETTER DISPLAY
            "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           perform better
           DISPLAY "|                                               |"
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |"
           perform better
           DISPLAY "|   *              BOOK EVENTS              *   |"
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |"
           perform better
           DISPLAY "|                                               |"
           perform better
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           PERFORM BETTER DISPLAY "Available Events:"

           OPEN INPUT EventFile
           MOVE 0 TO EVENT-FOUND
           MOVE 0 TO EOF

           PERFORM UNTIL EOF = 1
               READ EventFile
                   AT END
                       MOVE 1 TO EOF
                   NOT AT END
                       MOVE 1 TO EVENT-FOUND
                       PERFORM BETTER DISPLAY
                           "Event Name: " EVENTNAME
                       PERFORM BETTER DISPLAY
                           "Date: " EVENTDATE
                       PERFORM BETTER DISPLAY
                           "Organizer: " ORGANIZER
                       PERFORM BETTER DISPLAY
                           "Location: " LOCATION
                       PERFORM BETTER DISPLAY
                           "Ticket Price: " TICKET-PRICE
                       PERFORM BETTER DISPLAY
                           "Available Seats: " AVAILABLE-SEATS
           perform better
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="

           END-PERFORM

           CLOSE EventFile

           IF EVENT-FOUND = 0 THEN
             PERFORM BETTER DISPLAY "No events are currently available."
               ACCEPT WS-PAUSE
               EXIT PARAGRAPH
           END-IF

           PERFORM BETTER
           DISPLAY "Enter the name of the event you want to book: "
           WITH NO ADVANCING

           ACCEPT SEARCH-NAME

           INSPECT SEARCH-NAME REPLACING TRAILING SPACES BY SPACE
           MOVE FUNCTION UPPER-CASE(SEARCH-NAME) TO SEARCH-NAME

           OPEN INPUT EventFile
           MOVE 0 TO EOF
           MOVE 0 TO EVENT-FOUND

           PERFORM UNTIL EOF = 1
               READ EventFile
                   AT END
                       MOVE 1 TO EOF
                   NOT AT END
                       INSPECT EVENTNAME REPLACING TRAILING SPACES BY
                       SPACE
                       MOVE FUNCTION UPPER-CASE(EVENTNAME) TO EVENTNAME
                       IF EVENTNAME = SEARCH-NAME THEN
                           MOVE 1 TO EVENT-FOUND
           CALL "SYSTEM" USING "CLS"
           PERFORM BETTER DISPLAY "   "
            PERFORM BETTER DISPLAY "   "
            PERFORM BETTER DISPLAY "   "
            PERFORM BETTER DISPLAY
            "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           perform better
           DISPLAY "|                                               |"
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |"
           perform better
           DISPLAY "|   *              BOOK EVENTS              *   |"
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |"
           perform better
           DISPLAY "|                                               |"
           perform better
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           PERFORM BETTER DISPLAY "   "
           perform better
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
                           PERFORM BETTER DISPLAY
                               "Event Name: " EVENTNAME
                           PERFORM BETTER DISPLAY
                               "Date: " EVENTDATE
                           PERFORM BETTER DISPLAY
                               "Organizer: " ORGANIZER
                           PERFORM BETTER DISPLAY
                               "Location: " LOCATION
                           PERFORM BETTER DISPLAY
                               "Ticket Price: " TICKET-PRICE
                           PERFORM BETTER DISPLAY
                               "Available Seats: " AVAILABLE-SEATS
           perform better
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
                           PERFORM BETTER DISPLAY " "
                           PERFORM BETTER
           DISPLAY "How many seats would you like to purchase?: " WITH
           NO ADVANCING ACCEPT SEATS-TO-BUY
           PERFORM BETTER
           DISPLAY "How much is are you going to pay?: " WITH NO
           ADVANCING ACCEPT BALANCE
           INSPECT AVAILABLE-SEATS REPLACING ALL SPACES BY ZERO
           MOVE FUNCTION NUMVAL(AVAILABLE-SEATS) TO AVAILABLE-SEATS
            IF SEATS-TO-BUY NUMERIC AND SEATS-TO-BUY > 0 THEN
                COMPUTE TOTAL-COST = TICKET-PRICE * SEATS-TO-BUY
                IF SEATS-TO-BUY > AVAILABLE-SEATS THEN
                    PERFORM BETTER DISPLAY "Not enough seats available."
                    ELSE IF TOTAL-COST > BALANCE THEN
                       PERFORM BETTER
                       DISPLAY "Insufficient balance."
                    ELSE
                        PERFORM BETTER
                        DISPLAY "You are about to purchase "
                        SEATS-TO-BUY " seat(s) for a total of P"
                        TOTAL-COST "."
                        PERFORM BETTER
                        DISPLAY "Confirm purchase (Y/N): "
                        WITH NO ADVANCING
                        ACCEPT WS-CONFIRM
                            INSPECT WS-CONFIRM REPLACING ALL
                            SPACES BY SPACE
                           IF WS-CONFIRM = 'Y' OR WS-CONFIRM = 'y' THEN
                               COMPUTE CHANGE =
                               BALANCE - TOTAL-COST
                               MOVE USERNAME TO ATTENDEE-STORED-USERNAME
                               MOVE EVENTNAME TO EVENTNAME2
                               MOVE EVENTDATE TO EVENTDATE2
                               MOVE ORGANIZER TO ORGANIZER2
                               MOVE LOCATION TO LOCATION2
                               MOVE TICKET-PRICE TO TICKET-PRICE2
                               MOVE AVAILABLE-SEATS TO AVAILABLE-SEATS2
                               WRITE TEMP-RECORD

                               OPEN EXTEND VIEW-MY-BOOKINGS-FILE
                               IF FILE-STATUS = 30
                                   OPEN OUTPUT VIEW-MY-BOOKINGS-FILE
                               END-IF

                               MOVE ATTENDEE-STORED-USERNAME TO
                               BOOKED-ATTENDEE-USERNAME
                               MOVE EVENTNAME TO BOOKED-EVENTNAME
                               MOVE EVENTDATE TO BOOKED-EVENTDATE
                               MOVE LOCATION TO BOOKED-LOCATION
                               MOVE ORGANIZER TO BOOKED-ORGANIZER
                               MOVE SEATS-TO-BUY TO BOOKED-STB
                               MOVE TICKET-PRICE TO BOOKED-TP

                               WRITE MyBookingsRecord
                               CLOSE VIEW-MY-BOOKINGS-FILE

                               SUBTRACT SEATS-TO-BUY FROM
                               AVAILABLE-SEATS GIVING AVAILABLE-SEATS
                               OPEN INPUT EventFile
                               OPEN OUTPUT TEMP-FILE

                                PERFORM UNTIL EOF = 1
                                   READ EventFile
                                       AT END
                                           MOVE 1 TO EOF
                                       NOT AT END
                                           IF EVENTNAME = SEARCH-NAME
                                           THEN
                                               MOVE AVAILABLE-SEATS TO
                                               AVAILABLE-SEATS2
                                           END-IF
                                           WRITE TEMP-RECORD
                               END-PERFORM

                               CLOSE EventFile
                               CLOSE TEMP-FILE


                               PERFORM BETTER DISPLAY
                               "Purchase successful! Remaining seats: "
                               AVAILABLE-SEATS
                               REWRITE AVAILABLE-SEATS
                               DISPLAY "Purchase successful!
      -                        " Remaining balance: $" CHANGE
                           ELSE
                                PERFORM BETTER
                                DISPLAY "Purchase canceled."
                            END-IF
                        END-IF
                    ELSE
                        PERFORM BETTER
                        DISPLAY "Invalid number of seats."
                    END-IF

                    EXIT PERFORM
                END-IF
           END-READ
           END-PERFORM

           IF EVENT-FOUND = 0 THEN
               PERFORM BETTER DISPLAY "The specified event
      -        " was not found."
           END-IF

           CLOSE EventFile

           ACCEPT WS-PAUSE
           PERFORM GUEST-FEATURES.

       VIEW-EVENTS.
           OPEN INPUT EventFile
           IF FILE-STATUS NOT = "00"
               DISPLAY "Error opening file: " FILE-STATUS
               PERFORM ENDRUN
           END-IF
           CALL "SYSTEM" USING 'CLS'
           PERFORM CLEAR-SCREEN
           PERFORM CLEAR-SCREEN

           PERFORM BETTER DISPLAY "   "
            PERFORM BETTER DISPLAY "   "
            PERFORM BETTER DISPLAY "   "

            PERFORM BETTER DISPLAY
            "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           perform better
           DISPLAY "|                                               |"
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |"
           perform better
           DISPLAY "|   *         CHIBOL EVENT BOOKINGS:        *   |"
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |"
           perform better
           DISPLAY "|                                               |"
           perform better
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="

           PERFORM BETTER DISPLAY "   "
            PERFORM BETTER DISPLAY "   "
           MOVE 0 TO EOF
           PERFORM UNTIL EOF = 1
               READ EventFile
                   AT END
                       MOVE 1 TO EOF
                   NOT AT END

                       PERFORM BETTER
                       DISPLAY "  Event Name: " EVENTNAME
                       PERFORM BETTER
                       DISPLAY "  Date: " EVENTDATE
                       PERFORM BETTER
                       DISPLAY "  Organizer: " ORGANIZER
                       PERFORM BETTER
                       DISPLAY "  Location: " LOCATION
                       PERFORM BETTER
                       DISPLAY "  Ticket Price: P" TICKET-PRICE
                       PERFORM BETTER
                       DISPLAY "  Available Seats: " AVAILABLE-SEATS
                       PERFORM BETTER
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
               END-READ
           END-PERFORM
           CLOSE EventFile
           perform better
           DISPLAY "Press ENTER to return to the menu."
           ACCEPT WS-PAUSE
           PERFORM ADMIN-FEATURES.

       SEARCH-EVENT.
           CALL "SYSTEM" USING 'CLS'
           PERFORM CLEAR-SCREEN
           PERFORM CLEAR-SCREEN
           PERFORM PRINT-BLANK-LINES

            PERFORM BETTER DISPLAY "   ".
            PERFORM BETTER DISPLAY "   ".
            PERFORM BETTER DISPLAY "   ".

            PERFORM BETTER DISPLAY
            "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           perform better
           DISPLAY "|                                               |"
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |"
           perform better
           DISPLAY "|   *           SEARCH FOR AN EVENT         *   |"
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |"
           perform better
           DISPLAY "|                                               |"
           perform better
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="

           PERFORM BETTER
           DISPLAY "ENTER EVENT NAME TO SEARCH: " WITH NO ADVANCING
           ACCEPT SEARCH-NAME

           MOVE FUNCTION UPPER-CASE(SEARCH-NAME) TO SEARCH-NAME

           OPEN INPUT EventFile
           MOVE 0 TO EOF
           MOVE 0 TO EVENT-FOUND

           PERFORM UNTIL EOF = 1
               READ EventFile
                   AT END
                       MOVE 1 TO EOF
                   NOT AT END
                       MOVE FUNCTION UPPER-CASE(EVENTNAME) TO EVENTNAME
                       IF EVENTNAME = SEARCH-NAME
                          MOVE 1 TO EVENT-FOUND
                          PERFORM BETTER
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
                           PERFORM BETTER DISPLAY "Event Found!"
                           PERFORM BETTER
                           DISPLAY "Event Name: " EVENTNAME
                           PERFORM BETTER DISPLAY "Date: " EVENTDATE
                           PERFORM BETTER
                           DISPLAY "Organizer: " ORGANIZER
                           PERFORM BETTER DISPLAY "Location: " LOCATION
                           PERFORM BETTER
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
                       END-IF
               END-READ
           END-PERFORM

           IF EVENT-FOUND = 0 THEN
               PERFORM BETTER DISPLAY "No event found with that name."
           END-IF

           CLOSE EventFile
           PERFORM BETTER DISPLAY "Press ENTER to return to the menu."
           ACCEPT WS-PAUSE
           GO TO GUEST-FEATURES.

       SEARCH-EVENT2.
           CALL "SYSTEM" USING 'CLS'
           PERFORM CLEAR-SCREEN
           PERFORM CLEAR-SCREEN
           PERFORM PRINT-BLANK-LINES

           PERFORM BETTER DISPLAY "   ".
            PERFORM BETTER DISPLAY "   ".
            PERFORM BETTER DISPLAY "   ".

            PERFORM BETTER DISPLAY
            "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           perform better
           DISPLAY "|                                               |"
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |"
           perform better
           DISPLAY "|   *           SEARCH FOR AN EVENT         *   |"
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |"
           perform better
           DISPLAY "|                                               |"
           perform better
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           PERFORM BETTER
           DISPLAY "ENTER EVENT NAME TO SEARCH: " WITH NO ADVANCING
           ACCEPT SEARCH-NAME

           MOVE FUNCTION UPPER-CASE(SEARCH-NAME) TO SEARCH-NAME

           OPEN INPUT EventFile
           MOVE 0 TO EOF
           MOVE 0 TO EVENT-FOUND

           PERFORM UNTIL EOF = 1
               READ EventFile
                   AT END
                       MOVE 1 TO EOF
                   NOT AT END
                       MOVE FUNCTION UPPER-CASE(EVENTNAME) TO EVENTNAME
                       IF EVENTNAME = SEARCH-NAME
                           MOVE 1 TO EVENT-FOUND
                           PERFORM BETTER
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
                           PERFORM BETTER DISPLAY "Event Found!"
                           PERFORM BETTER
                           DISPLAY "Event Name: " EVENTNAME
                           PERFORM BETTER DISPLAY "Date: " EVENTDATE
                           PERFORM BETTER
                           DISPLAY "Organizer: " ORGANIZER
                           PERFORM BETTER DISPLAY "Location: " LOCATION
                           PERFORM BETTER
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
                       END-IF
               END-READ
           END-PERFORM

           IF EVENT-FOUND = 0 THEN
               PERFORM BETTER DISPLAY "No event found with that name."
           END-IF

           CLOSE EventFile
           PERFORM BETTER DISPLAY "Press ENTER to return to the menu."
           ACCEPT WS-PAUSE
           GO TO ADMIN-FEATURES.

       DELETE-EVENT.
           CALL "SYSTEM" USING "CLS"
           PERFORM CLEAR-SCREEN
           PERFORM CLEAR-SCREEN
           PERFORM PRINT-BLANK-LINES

           PERFORM BETTER DISPLAY "   ".
            PERFORM BETTER DISPLAY "   ".
            PERFORM BETTER DISPLAY "   ".

            PERFORM BETTER DISPLAY
             "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           perform better
           DISPLAY "|                                               |"
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |"
           perform better
           DISPLAY "|   *            DELETE AN EVENT            *   |"
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |"
           perform better
           DISPLAY "|                                               |"
           perform better
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           PERFORM BETTER DISPLAY "ENTER EVENT NAME TO DELETE: "
           WITH NO ADVANCING
           ACCEPT DELETE-NAME

            MOVE FUNCTION UPPER-CASE(DELETE-NAME) TO DELETE-NAME

           OPEN INPUT EventFile
           OPEN OUTPUT TEMP-FILE

           MOVE 0 TO EOF
           MOVE 0 TO EVENT-FOUND

           PERFORM UNTIL EOF = 1
           READ EventFile
             AT END
               MOVE 1 TO EOF
             NOT AT END
              MOVE FUNCTION UPPER-CASE(EVENTNAME) TO EVENTNAME
                IF EVENTNAME = DELETE-NAME
                  PERFORM BETTER
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
                  PERFORM BETTER DISPLAY " Event Found!"
                  PERFORM BETTER
                  DISPLAY " Event Name: " EVENTNAME
                  PERFORM BETTER DISPLAY " Date: " EVENTDATE
                  PERFORM BETTER
                  DISPLAY " Organizer: " ORGANIZER
                  PERFORM BETTER DISPLAY " Location: " LOCATION
                  PERFORM BETTER
                  DISPLAY " Ticket Price: " TICKET-PRICE
                  PERFORM BETTER
                  DISPLAY " Available Seats: " AVAILABLE-SEATS
                  PERFORM BETTER
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
                  DISPLAY " "
                  PERFORM BETTER
                  DISPLAY "Are you sure you want to delete this event"
                  perform better
                  DISPLAY "(Y/N)?" WITH NO ADVANCING
                  ACCEPT WS-CONFIRM
                  IF WS-CONFIRM = "Y" OR WS-CONFIRM = "y"
                      MOVE 1 TO EVENT-FOUND
                      PERFORM BETTER
                      DISPLAY "Event deleted successfully!"
                   ELSE
                       WRITE TEMP-RECORD FROM EVENTData
                   END-IF
               ELSE
                   WRITE TEMP-RECORD FROM EVENTData
               END-IF
             END-READ
           END-PERFORM

           CLOSE EventFile
           CLOSE TEMP-FILE

            IF EVENT-FOUND = 0
             DISPLAY " "
             perform better
             DISPLAY "No event found with that name."
            ELSE
                perform better
             DISPLAY "Replacing the original file..."

             OPEN INPUT TEMP-FILE
             OPEN OUTPUT EventFile

             MOVE 0 TO EOF
             PERFORM UNTIL EOF = 1
                 READ TEMP-FILE
                     AT END
                         MOVE 1 TO EOF
                     NOT AT END
                         WRITE EVENTData FROM TEMP-RECORD
                 END-READ
             END-PERFORM

             CLOSE TEMP-FILE
             CLOSE EventFile

           PERFORM BETTER DISPLAY "Original file updated successfully!"
           END-IF

           DISPLAY " "
           PERFORM BETTER DISPLAY "Press ENTER to return to the menu."
           ACCEPT WS-PAUSE
           PERFORM ADMIN-FEATURES.


       EDIT-EVENT.
           CALL "SYSTEM" USING "CLS"
           PERFORM CLEAR-SCREEN
           PERFORM CLEAR-SCREEN
           PERFORM PRINT-BLANK-LINES


           PERFORM BETTER DISPLAY
             "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           perform better
           DISPLAY "|                                               |"
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |"
           perform better
           DISPLAY "|   *            EDIT AN EVENT              *   |"
           perform better
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |"
           perform better
           DISPLAY "|                                               |"
           perform better
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="

           PERFORM BETTER
           DISPLAY "ENTER EVENT NAME TO EDIT: " WITH NO ADVANCING
           ACCEPT SEARCH-NAME

           OPEN INPUT EventFile
           OPEN OUTPUT TEMP-FILE

           MOVE 0 TO EOF
           MOVE 0 TO EVENT-FOUND

           PERFORM UNTIL EOF = 1
             READ EventFile INTO EVENTData
               AT END
                 MOVE 1 TO EOF
               NOT AT END
                 IF EVENTNAME = SEARCH-NAME
                    DISPLAY " "
          PERFORM BETTER
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           PERFORM BETTER DISPLAY "Event Found!        "
           PERFORM BETTER DISPLAY "   "
           PERFORM BETTER DISPLAY "Event Name: " EVENTNAME
           PERFORM BETTER DISPLAY "Date: " EVENTDATE
           PERFORM BETTER DISPLAY "Organizer: " ORGANIZER
           PERFORM BETTER DISPLAY "Location: " LOCATION
           PERFORM BETTER DISPLAY "Ticket Price: " TICKET-PRICE
           PERFORM BETTER DISPLAY "Available Seats: " AVAILABLE-SEATS
           PERFORM BETTER
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           PERFORM BETTER DISPLAY " "
           PERFORM BETTER
           DISPLAY "Choose a field to edit:" WITH NO ADVANCING
           PERFORM BETTER DISPLAY "   "
           PERFORM BETTER DISPLAY "1: Edit Event Name"
           PERFORM BETTER DISPLAY "2: Edit Event Date"
           PERFORM BETTER DISPLAY "3: Edit Organizer"
           PERFORM BETTER DISPLAY "4: Edit Ticket Price"
           PERFORM BETTER DISPLAY "5: Edit Available Seats"
           PERFORM BETTER
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
                    DISPLAY " "
                    PERFORM BETTER
                    DISPLAY "Enter your choice: " WITH NO ADVANCING
                    ACCEPT CHOICE

                    EVALUATE CHOICE
                         WHEN 1
            perform better
            DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
            perform better
            DISPLAY "Enter new Event Name: " WITH NO ADVANCING
                             ACCEPT NEW-NAME
                             MOVE NEW-NAME TO EVENTNAME

                         WHEN 2
            perform better
            DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
            PERFORM BETTER
            DISPLAY "Enter new Event Date: " WITH NO ADVANCING
                             ACCEPT NEW-DATE
                             MOVE NEW-DATE TO EVENTDATE

                         WHEN 3
            perform better
            DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
             PERFORM BETTER
             DISPLAY "Enter new Organizer: " WITH NO ADVANCING
                             ACCEPT NEW-ORGANIZER
                             MOVE NEW-ORGANIZER TO ORGANIZER

                         WHEN 4
            perform better
            DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
            PERFORM BETTER
            DISPLAY "Enter new Ticket Price: " WITH NO ADVANCING
                             ACCEPT NEW-TICKET-PRICE
                             MOVE NEW-TICKET-PRICE TO TICKET-PRICE

                         WHEN 5
            perform better
            DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
            PERFORM BETTER
            DISPLAY "Enter new Available Seats: " WITH NO ADVANCING
                             ACCEPT NEW-AVAILABLE-SEATS
                             MOVE NEW-AVAILABLE-SEATS TO AVAILABLE-SEATS

                         WHEN OTHER
                             DISPLAY "Invalid choice. No changes made."
                     END-EVALUATE

                MOVE 1 TO EVENT-FOUND
                END-IF

                WRITE TEMP-RECORD FROM EVENTData
              END-READ
             END-PERFORM

              CLOSE EventFile
            CLOSE TEMP-FILE

            IF EVENT-FOUND = 0
             PERFORM BETTER
             DISPLAY "No event found with the specified name."
            ELSE
            PERFORM BETTER DISPLAY "Updating original file."
            OPEN INPUT TEMP-FILE
            OPEN OUTPUT EventFile

            MOVE 0 TO EOF
            PERFORM UNTIL EOF = 1
            READ TEMP-FILE INTO EVENTData
                AT END
                    MOVE 1 TO EOF
                NOT AT END
                    WRITE EVENTData
                END-READ
             END-PERFORM

              CLOSE TEMP-FILE
             CLOSE EventFile

            PERFORM BETTER DISPLAY "Event updated successfully!"
            END-IF

             PERFORM BETTER
             DISPLAY "Press ENTER to return to the main menu."
             ACCEPT WS-PAUSE
            PERFORM ADMIN-FEATURES.

       VIEW-MY-BOOKINGS.
           CALL "SYSTEM" USING 'CLS'
           PERFORM CLEAR-SCREEN
           PERFORM PRINT-BLANK-LINES
           PERFORM BETTER DISPLAY "   "
           PERFORM BETTER DISPLAY "   "
           PERFORM BETTER DISPLAY "   "

           PERFORM BETTER DISPLAY
           "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           PERFORM BETTER
           DISPLAY "|                                               |"
           PERFORM BETTER
           DISPLAY "|       VIEW YOUR BOOKINGS RECEIPTS             |"
           PERFORM BETTER
           DISPLAY "|                                               |"
           PERFORM BETTER
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="

           OPEN INPUT VIEW-MY-BOOKINGS-FILE
           IF FILE-STATUS NOT = "00"
               DISPLAY "Error opening file: " FILE-STATUS
               PERFORM ENDRUN
           END-IF

           MOVE 0 TO EOF
           PERFORM UNTIL EOF = 1
               READ VIEW-MY-BOOKINGS-FILE
                   AT END
                       MOVE 1 TO EOF
                   NOT AT END
                       IF BOOKED-ATTENDEE-USERNAME = USERNAME
                       MOVE 0 TO NO-RECORD-FOUND
                       PERFORM BETTER
                       DISPLAY "  Booking Receipt: "
                       PERFORM BETTER
                       DISPLAY "  Event Name: " BOOKED-EVENTNAME
                       PERFORM BETTER
                       DISPLAY "  Date: " BOOKED-EVENTDATE
                       PERFORM BETTER
                       DISPLAY "  Organizer: " BOOKED-ORGANIZER
                       PERFORM BETTER
                       DISPLAY "  Location: " BOOKED-LOCATION
                       PERFORM BETTER
                       DISPLAY "  Ticket Price: P" BOOKED-TP
                       PERFORM BETTER
                       DISPLAY "  Seats Booked: " BOOKED-STB
                       PERFORM BETTER
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
               END-READ
           END-PERFORM
           CLOSE VIEW-MY-BOOKINGS-FILE

           PERFORM BETTER DISPLAY "Press ENTER to return to the menu."
           ACCEPT WS-PAUSE
           PERFORM GUEST-FEATURES.

       VIEW-REPORTS.
         CALL "SYSTEM" USING 'CLS'
           PERFORM CLEAR-SCREEN
           PERFORM PRINT-BLANK-LINES

           PERFORM BETTER DISPLAY "   "
           PERFORM BETTER DISPLAY "   "
           PERFORM BETTER DISPLAY "   "

           PERFORM BETTER DISPLAY
           "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           PERFORM BETTER
           DISPLAY "|                                               |"
           PERFORM BETTER
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |"
           PERFORM BETTER
           DISPLAY "|   *       VIEW BOOKING REPORTS            *   |"
           PERFORM BETTER
           DISPLAY "|   * * * * * * * * * * * * * * * * * * * * *   |"
           PERFORM BETTER
           DISPLAY "|                                               |"
           PERFORM BETTER
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="

           OPEN INPUT VIEW-MY-BOOKINGS-FILE
           MOVE 0 TO EOF
           PERFORM UNTIL EOF = 1
           READ VIEW-MY-BOOKINGS-FILE INTO MyBookingsRecord
               AT END
                   MOVE 1 TO EOF
               NOT AT END
                       PERFORM BETTER
                       DISPLAY "Attendee: " BOOKED-ATTENDEE-USERNAME
                       PERFORM BETTER
                       DISPLAY "  Event Name: " BOOKED-EVENTNAME
                       PERFORM BETTER
                       DISPLAY "  Date: " BOOKED-EVENTDATE
                       PERFORM BETTER
                       DISPLAY "  Organizer: " BOOKED-ORGANIZER
                       PERFORM BETTER
                       DISPLAY "  Location: " BOOKED-LOCATION
                       PERFORM BETTER
                       DISPLAY "  Ticket Price: P" BOOKED-TP
                       PERFORM BETTER
                       DISPLAY "  Seats Booked: " BOOKED-STB
                       PERFORM BETTER
           DISPLAY "=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*="
           END-READ
           END-PERFORM
           CLOSE VIEW-MY-BOOKINGS-FILE

           PERFORM BETTER DISPLAY "Press ENTER to return to the menu."
           ACCEPT WS-PAUSE
           PERFORM ADMIN-FEATURES.

       ENDRUN.
               PERFORM BETTER DISPLAY 'PRESS ENTER TO CONTINUE...'
               WITH NO ADVANCING.
               ACCEPT END-IN.

       CLEAR-SCREEN.
               DISPLAY ' '.
               DISPLAY '                     ===========================
      -    '======================================================='.

       BETTER.
               DISPLAY '                                     '
               WITH NO ADVANCING.
       MID.
               DISPLAY '                      '
               WITH NO ADVANCING.



       PRINT-BLANK-LINES.
               PERFORM VARYING SPACE-COUNT FROM 1 BY 1 UNTIL
               SPACE-COUNT > 3
               DISPLAY ' '
              END-PERFORM.
            END PROGRAM finalchibol.
