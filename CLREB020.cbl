00001  IDENTIFICATION DIVISION.
00002  PROGRAM-ID. CLREB020.
00003  AUTHOR. RUSS DOBER - RON URBANIAK.
00004  DATE-WRITTEN. SEP 25, 1978.
00005  DATE-COMPILED.
00006 *REMARKS. THIS PROGRAM WILL EDIT AND LIST EQUALIZATION
00007 *         FACTORS AND PUT THEM OUT ON A DISK FILE.
00008      SKIP3
00009  ENVIRONMENT DIVISION.
00010  CONFIGURATION SECTION.
00011  SOURCE-COMPUTER. IBM-370.
00012  OBJECT-COMPUTER. IBM-370.
00013  INPUT-OUTPUT SECTION.
00014  FILE-CONTROL.
00015      SELECT CARD-FILE ASSIGN TO UT-S-CARDS.
00016      SELECT PRINT-FILE ASSIGN TO UT-S-PRINT.
00017      SELECT FACTOR-FILE ASSIGN TO UT-S-FACTOR.
00018      SKIP3
00019  DATA DIVISION.
00020  FILE SECTION.
00021  FD  CARD-FILE
00022      RECORD CONTAINS 80 CHARACTERS
00023      BLOCK CONTAINS 0 RECORDS
00024      RECORDING MODE IS F
00025      LABEL RECORDS ARE STANDARD
00026      DATA RECORD IS CARD-REC.
00027  01  CARD-REC.
00028      05  CARD.
00029          10 CD-YR          PIC XX.
00030          10 CD-QUAD        PIC X.
00031            88 VALID-QUAD         VALUE '1' THRU '4'.
00032      05 CD-FACTOR.
00033         10 CD-FT1      PIC X.
00034         10 CD-FT4      PIC X(4).
00035      05 CD-FACTOR-RD REDEFINES CD-FACTOR PIC 9V9999.
00036      05 CD-FILLER      PIC X(72).
00037  SKIP1
00038  FD  PRINT-FILE
00039      LABEL RECORDS ARE STANDARD
00040      RECORDING MODE IS F
00041      RECORD CONTAINS 133 CHARACTERS
00042      BLOCK CONTAINS 0 RECORDS
00043      DATA RECORD IS PRINT-REC.
00044  01  PRINT-REC         PIC X(133).
00045      SKIP1
00046  FD  FACTOR-FILE
00047      RECORD CONTAINS 21 CHARACTERS
00048      BLOCK CONTAINS 0 RECORDS
00049      RECORDING MODE IS F
00050      LABEL RECORDS ARE STANDARD
00051      DATA RECORD IS FACTOR-REC.
00052  01  FACTOR-REC.
00053      05  FT-TAXYR      PIC 99.
00054      05  FT-QUAD       PIC 9.
00055      05  FT-EQFACT     PIC 9V9999.
00056      05  FILLER        PIC X(13).
00057      SKIP2
00058 ****************  WORKING STORAGE STARTS HERE  *******************
00059      SKIP1
00060  WORKING-STORAGE SECTION.
00061  77  LINE-CNT          PIC S999       VALUE +60      COMP-3.
00062  77  ERR-MESG          PIC X(11)      VALUE 'NOT NUMERIC'.
00063  77  IN-CNT            PIC S999       VALUE +0       COMP-3.
00064  77  OUT-CNT           PIC S999       VALUE +0       COMP-3.
00065  77  ERROR-CNT         PIC S999       VALUE +0       COMP-3.
00066  77  PAGE-CNT          PIC S999       VALUE +0       COMP-3.
00067  77  CARD-EOF          PIC 9          VALUE 0.
00068      88 END-OF-CARD-FILE              VALUE 1.
00069  77  SEQ-CHECK         PIC X.
00070      88  SEQ-ERROR                    VALUE 'E'.
00071  01  WORK-AREA.
00072      05 WORK-FACTOR.
00073         10 WK-FACT1    PIC X.
00074         10 FILLER      PIC X          VALUE '.'.
00075         10 WK-FACT4    PIC X(4).
00076      05  PREV-CARD.
00077          10 PREV-YR    PIC XX         VALUE LOW-VALUE.
00078          10 PREV-QUAD  PIC X          VALUE LOW-VALUE.
00079      05 DATE-LINE.
00080          10 FILLER     PIC X(11)      VALUE SPACES.
00081          10 DATE-DT    PIC X(8).
00082      05 HDG-LINE.
00083          10 FILLER     PIC X(11)      VALUE SPACES.
00084          10 FILLER     PIC X(40)      VALUE 'CLREB020'.
00085          10 FILLER     PIC X(70)      VALUE 'OFFICE  OF  THE COUNT
00086 -        'Y  CLERK'.
00087          10 FILLER     PIC X(6)       VALUE 'PAGE  '.
00088          10 HDG-PG     PIC ZZ9.
00089      05  TTL-LINE.
00090          10 FILLER     PIC X(51)      VALUE SPACES.
00091          10 FILLER     PIC X(25)      VALUE 'EQUALIZATION      FAC
00092 -        'TORS'.
00093      05  TTL-LINE2.
00094          10 FILLER     PIC X(51)      VALUE SPACES.
00095          10 FILLER     PIC X(25)      VALUE 'YEAR      QUAD     FA
00096 -        'CTOR'.
00097      05  WORK-LINE.
00098          10 FILLER     PIC X(52)      VALUE SPACES.
00099          10 WK-YR      PIC XX.
00100          10 FILLER     PIC X(8)       VALUE SPACES.
00101          10 WK-QUAD    PIC X.
00102          10 FILLER     PIC X(7)       VALUE SPACES.
00103          10 WK-FACT    PIC X(6).
00104          10 FILLER     PIC X(5)       VALUE SPACES.
00105          10 WK-MESG    PIC X(11)      VALUE SPACES.
00106  SKIP2
00107 *****************  WORKING STORAGE ENDS HERE  ********************
00108  SKIP3
00109  PROCEDURE DIVISION.
00110  010-BEGIN.
00111      OPEN    INPUT CARD-FILE
00112              OUTPUT PRINT-FILE
00113                     FACTOR-FILE
00114      MOVE    CURRENT-DATE TO DATE-DT
00115      PERFORM 020-MAIN-LINE THRU 020-EXIT
00116              UNTIL END-OF-CARD-FILE
00117               OR   SEQ-ERROR
00118      DISPLAY 'NO. OF INPUT RECORDS  = ' IN-CNT
00119      DISPLAY 'NO. OF OUTPUT RECORDS = ' OUT-CNT
00120      DISPLAY 'NO. OF ERROR RECORDS  = ' ERROR-CNT
00121      CLOSE   CARD-FILE
00122              PRINT-FILE
00123              FACTOR-FILE
00124      STOP RUN.
00125      SKIP3
00126  020-MAIN-LINE.
00127      PERFORM 030-READ-CARD THRU 030-READ-EXIT
00128      IF      NOT END-OF-CARD-FILE
00129        AND   NOT SEQ-ERROR
00130              IF     CD-YR NUMERIC
00131                AND  CD-YR GREATER THAN 0
00132                AND  CD-FACTOR NUMERIC
00133                AND  CD-FACTOR GREATER THAN 0
00134                AND  VALID-QUAD
00135                     PERFORM 040-CREATE-FACTOR THRU 040-EXIT
00136                     PERFORM 050-WRITE THRU 050-EXIT
00137              ELSE
00138                     ADD +1 TO ERROR-CNT
00139                     MOVE ERR-MESG TO WK-MESG
00140                     PERFORM 050-WRITE THRU 050-EXIT.
00141      SKIP1
00142  020-EXIT.
00143      EXIT.
00144      SKIP3
00145  030-READ-CARD.
00146      READ    CARD-FILE
00147              AT END MOVE 1 TO CARD-EOF.
00148      IF      NOT END-OF-CARD-FILE
00149              IF   CARD LESS THAN PREV-CARD
00150                   MOVE 16 TO RETURN-CODE
00151                   DISPLAY 'CARDS OUT OF SEQUENCE'
00152                   DISPLAY 'CURRENT CARD ' CARD
00153                   DISPLAY 'PREVIOUS CARD ' PREV-CARD
00154                   MOVE 'E' TO SEQ-CHECK
00155              ELSE
00156                   MOVE CARD TO PREV-CARD
00157                   ADD +1 TO IN-CNT.
00158      SKIP1
00159  030-READ-EXIT.
00160      EXIT.
00161      SKIP3
00162  040-CREATE-FACTOR.
00163      MOVE    SPACES TO FACTOR-REC
00164      MOVE    CD-YR      TO  FT-TAXYR
00165      MOVE    CD-QUAD    TO  FT-QUAD
00166      MOVE    CD-FACTOR-RD  TO  FT-EQFACT
00167      WRITE   FACTOR-REC
00168                   ADD +1 TO OUT-CNT.
00169      SKIP1
00170  040-EXIT.
00171      EXIT.
00172      SKIP3
00173  050-WRITE.
00174      IF      LINE-CNT GREATER THAN +55
00175              PERFORM 060-HDG-ROUTINE THRU 060-EXIT.
00176      MOVE    CD-YR      TO  WK-YR
00177      MOVE    CD-QUAD    TO  WK-QUAD
00178      MOVE    CD-FT1     TO  WK-FACT1
00179      MOVE    CD-FT4     TO  WK-FACT4
00180      MOVE    WORK-FACTOR TO WK-FACT
00181      WRITE   PRINT-REC FROM WORK-LINE
00182              AFTER ADVANCING 2
00183      MOVE    SPACES TO WK-MESG
00184      ADD     +2 TO LINE-CNT.
00185      SKIP1
00186  050-EXIT.
00187      EXIT.
00188      SKIP3
00189  060-HDG-ROUTINE.
00190      ADD     +1 TO PAGE-CNT
00191      MOVE    PAGE-CNT TO HDG-PG
00192      WRITE   PRINT-REC FROM DATE-LINE
00193              AFTER PAGE
00194      WRITE   PRINT-REC FROM HDG-LINE
00195              AFTER ADVANCING 2
00196      WRITE   PRINT-REC FROM TTL-LINE
00197              AFTER ADVANCING 2
00198      WRITE   PRINT-REC FROM TTL-LINE2
00199              AFTER ADVANCING 3
00200      MOVE    +8 TO LINE-CNT.
00201      SKIP1
00202  060-EXIT.
00203      EXIT.