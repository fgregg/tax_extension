00001  IDENTIFICATION DIVISION.
00002  PROGRAM-ID. ASREA010.
00003  AUTHOR.     GEORGE A ZINTAK.
00004 *REMARKS. C O P Y   O F   A S R E A 0 1 0  FOR  T E S T I N G !!!
00005 *REMARKS.
00006 *             THIS PROGRAM MATCHES THE TAX CODE
00007 *            MASTER WITH THE SUMMARY TAPE FILE
00008 *            AND LIST PROPERTY NUMBERS WITHOUT
00009 *            A MATCH.
00010  ENVIRONMENT DIVISION.
00011  INPUT-OUTPUT SECTION.
00012  FILE-CONTROL.
00013      SELECT MASTER  ASSIGN TO UT-S-MASTER.
00014      SELECT  TAXCODE ASSIGN TO DA-I-TAXCODE
00015              ACCESS MODE IS RANDOM
00016              ORGANIZATION IS INDEXED
00017              RECORD KEY IS TR-TAX-CODE.
00018      SELECT  PRNTFILE ASSIGN TO UT-S-PRNTFILE.
00019  DATA DIVISION.
00020  FILE SECTION.
00021  FD  MASTER
00022      LABEL RECORDS ARE STANDARD
00023      BLOCK CONTAINS 0 CHARACTERS
00024      RECORDING MODE IS S.
00025  01  M-REC.
00026  COPY ASREASRD01.
00027  FD  TAXCODE
00028      LABEL RECORDS ARE STANDARD
00029      RECORD CONTAINS 127 CHARACTERS
00030      BLOCK CONTAINS 4 RECORDS
00031      DATA RECORD IS TAXCODE-REC.
00032  01  TAXCODE-REC.
00033      05  TR-TAX-CODE             PIC 9(5)        COMP-3.
00034      05  FILLER                  PIC X(124).
00035  FD  PRNTFILE
00036      RECORDING MODE IS F
00037      LABEL RECORDS ARE OMITTED
00038      RECORD CONTAINS 133 CHARACTERS
00039      BLOCK  CONTAINS 0   RECORDS
00040      DATA RECORD IS PRINT-LINE.
00041  01  PRINT-LINE                  PIC X(133).
00042  WORKING-STORAGE SECTION.
00043  77  77-LN-CNT                   PIC 99           VALUE 88.
00044  77  77-M-REC                    PIC 9(7)         VALUE ZERO.
00045  77  77-ERROR-REC                PIC 9(7)         VALUE ZERO.
00046  77  77-X                        PIC 9          VALUE ZERO.
00047  77  CURRENT-DATE                PIC 99/99/9(4).
00048  77  CURRENT-TIME                PIC 99,99,99.
00049
00050  01  MDCY-OR-CYMD    PIC S9(5)V9(4)  VALUE +10000.0001.
00051  01  CURRENT-DATE-RETURN             VALUE SPACES.
00052      05  SYSTEM-DATE PIC 9(8).
00053      05  SYSTEM-TIME PIC 9(6).
00054      05  FILLER      PIC X(7).
00055
00056  01  UNPK-TXCD PIC 9(5).
00057  01  TXCD-UNPK REDEFINES UNPK-TXCD.
00058      05 M-TOWN PIC 99.
00059      05 FILLER PIC XXX.
00060  01  WS-CORE.
00061      02  HEADING-LINE-1.
00062        05  FILLER                PIC X(46)        VALUE SPACE.
00063      05  HEAD-CENT     PIC XX              VALUE ZERO.
00064      05  HEAD-YEAR     PIC XX              VALUE ZERO.
00065      05  FILLER        PIC X(38)           VALUE
00066          '  INVALID TAXCODES ON MASTER RECORDS '.
00067        05  FILLER                PIC X(17)        VALUE SPACE.
00068        05  HL1-DATE              PIC X(10)        VALUE SPACE.
00069        05  FILLER                PIC X(6)         VALUE SPACE.
00070        05  FILLER                PIC X(8)         VALUE
00071              'ASREA010'.
00072      02  HEADING-LINE-2.
00073        05  FILLER                PIC X(46)        VALUE SPACE.
00074        05  FILLER                PIC X(40)        VALUE
00075              'VOL       PROPERTY                   TAX'.
00076      02  HEADING-LINE-3.
00077        05  FILLER                PIC X(46)        VALUE SPACE.
00078        05  FILLER                PIC X(40)        VALUE
00079              'NO.         NO.                     CODE'.
00080      02  DETAIL-LINE.
00081        05  FILLER                PIC X(46)        VALUE SPACE.
00082        05  DL-VOL                PIC 999          VALUE ZERO.
00083        05  FILLER                PIC X(3)         VALUE SPACE.
00084        05  DL-PROP               PIC X(18)        VALUE SPACE.
00085        05  FILLER                PIC X(12)        VALUE SPACE.
00086        05  DL-TAX-CODE           PIC ZZZZ9       VALUE ZERO.
00087      02  TOTAL-LINE.
00088        05  FILLER                PIC X(40)        VALUE SPACE.
00089        05  TL-TYPE               PIC X(37)        VALUE SPACE.
00090        05  FILLER                PIC X(3)         VALUE SPACE.
00091        05  TL-TOT                PIC Z,ZZZ,ZZ9    VALUE ZERO.
00092      02  WS-TAXCODE-KEY      PIC 9(5) VALUE 0 COMP-3.
00093      02  WS-PROP                 PIC 9(14)        VALUE ZERO.
00094      02  WS-PROP-NO REDEFINES WS-PROP.
00095        05  WS-AREA               PIC 99.
00096        05  WS-SUBAREA            PIC 99.
00097        05  WS-BLOCK              PIC 999.
00098        05  WS-PARCEL             PIC 999.
00099        05  WS-UNIT               PIC 9999.
00100      02  EDIT-PROP.
00101        05  EP-AREA               PIC 99           VALUE ZERO.
00102        05  FILLER                PIC X            VALUE '-'.
00103        05  EP-SUBAREA            PIC 99           VALUE ZERO.
00104        05  FILLER                PIC X            VALUE '-'.
00105        05  EP-BLOCK              PIC 999          VALUE ZERO.
00106        05  FILLER                PIC X            VALUE '-'.
00107        05  EP-PARCEL             PIC 999          VALUE ZERO.
00108        05  FILLER                PIC X            VALUE '-'.
00109        05  EP-UNIT               PIC 9999         VALUE ZERO.
00110  LINKAGE SECTION.
00111  01  LNKAGE.
00112      03 PARM-LENGTH    PIC S99   COMP.
00113      03  PARM-YEAR     PIC XX.
00114      03 TOWN-NO PIC XX.
00115      03 TOWN-NUM REDEFINES TOWN-NO PIC 99.
00116         88 COUNTRY VALUE 10 THRU 39.
00117         88 CITY VALUE 70 THRU 77.
00118  PROCEDURE DIVISION USING LNKAGE.
00119      MOVE    PARM-YEAR TO HEAD-YEAR.
00120      IF HEAD-YEAR > '60'
00121         MOVE '19' TO HEAD-CENT
00122      ELSE
00123         MOVE '20' TO HEAD-CENT
00124      END-IF
00125      IF TOWN-NO IS NUMERIC NEXT SENTENCE
00126      ELSE
00127          DISPLAY 'PARM TOWN MISSING, JOB ENDS ' TOWN-NO
00128                   GO TO 190-EOJ.
00129      IF COUNTRY OR CITY NEXT SENTENCE
00130      ELSE
00131          DISPLAY 'PARM TOWN INVALID, JOB ENDS ' TOWN-NO
00132                   GO TO 190-EOJ.
00133  100-OPEN-FILES.
00134      OPEN    INPUT MASTER.
00135      OPEN    INPUT TAXCODE.
00136      OPEN    OUTPUT PRNTFILE.
00137      GO TO 110-GET-DATE.
00138
00139  105-CHECK-TOWN.
00140      MOVE    M-TXCD TO UNPK-TXCD.
00141      IF      M-TOWN = TOWN-NUM NEXT SENTENCE
00142      ELSE
00143          PERFORM 150-WRITE-ERROR THRU 155-EXIT.
00144  105-EXIT. EXIT.
00145
00146  110-GET-DATE.
00147      MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-RETURN.
00148      COMPUTE CURRENT-DATE = SYSTEM-DATE * MDCY-OR-CYMD.
00149      MOVE    CURRENT-DATE TO HL1-DATE.
00150      PERFORM 160-HEADING THRU 170-EXIT.
00151  120-FIRST-REC.
00152      READ    MASTER  AT END GO TO 190-EOJ.
00153      IF      M-VOL = 999 GO TO 120-FIRST-REC.
00154      PERFORM 105-CHECK-TOWN THRU 105-EXIT.
00155      ADD     1 TO 77-M-REC.
00156      PERFORM 130-READ-TAX THRU 133-EXIT.
00157  130-READ-TAX.
00158      MOVE    M-TXCD TO TR-TAX-CODE.
00159      READ    TAXCODE INVALID KEY PERFORM 150-WRITE-ERROR
00160              THRU 155-EXIT.
00161      GO TO 120-FIRST-REC.
00162  133-EXIT.
00163      EXIT.
00164  150-WRITE-ERROR.
00165      MOVE    1 TO 77-X.
00166      IF      77-LN-CNT GREATER THAN 55
00167              PERFORM 160-HEADING THRU 170-EXIT.
00168      MOVE    M-VOL TO DL-VOL.
00169      MOVE    M-PROP TO WS-PROP.
00170      MOVE    WS-AREA    TO EP-AREA.
00171      MOVE    WS-SUBAREA TO EP-SUBAREA.
00172      MOVE    WS-BLOCK   TO EP-BLOCK.
00173      MOVE    WS-PARCEL  TO EP-PARCEL.
00174      MOVE    WS-UNIT    TO EP-UNIT.
00175      MOVE    EDIT-PROP  TO DL-PROP.
00176      MOVE    M-TXCD TO DL-TAX-CODE.
00177      MOVE DETAIL-LINE TO PRINT-LINE.
00178      WRITE   PRINT-LINE AFTER ADVANCING 2 LINES.
00179      ADD     2 TO 77-LN-CNT.
00180      ADD     1 TO 77-ERROR-REC.
00181  155-EXIT.
00182      EXIT.
00183  160-HEADING.
00184      MOVE    HEADING-LINE-1 TO PRINT-LINE.
00185      WRITE   PRINT-LINE AFTER ADVANCING PAGE.
00186      MOVE    HEADING-LINE-2 TO PRINT-LINE.
00187      WRITE   PRINT-LINE AFTER ADVANCING 3 LINES.
00188      MOVE    HEADING-LINE-3 TO PRINT-LINE.
00189      WRITE   PRINT-LINE AFTER ADVANCING 1 LINE.
00190      MOVE    ZERO TO 77-LN-CNT.
00191  170-EXIT.
00192      EXIT.
00193  190-EOJ.
00194      MOVE    'TOTAL NO. OF INPUT MASTER RECORDS'
00195              TO TL-TYPE.
00196      MOVE    77-M-REC TO TL-TOT.
00197      MOVE    TOTAL-LINE TO PRINT-LINE.
00198      WRITE   PRINT-LINE AFTER ADVANCING 3 LINES.
00199      MOVE    'TOTAL NO. OF UNMATCHED MASTER RECORDS'
00200              TO TL-TYPE.
00201      MOVE    77-ERROR-REC TO TL-TOT.
00202      MOVE    TOTAL-LINE TO PRINT-LINE.
00203      WRITE   PRINT-LINE AFTER ADVANCING 2 LINES.
00204      CLOSE   MASTER TAXCODE PRNTFILE.
00205      STOP RUN.
