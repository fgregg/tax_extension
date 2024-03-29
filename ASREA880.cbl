00001  IDENTIFICATION DIVISION.
00002  PROGRAM-ID. ASREA880.
00003  AUTHOR. RUGIS-MOLIS.
00004  DATE-WRITTEN. JUNE 14,1979.
00005  DATE-COMPILED.
00006 *REMARKS. THE PURPOSE OF THIS PROGRAM IS TO UPDATE THE
00007 *         HOMEOWNERS EXEMPTION FILE WITH TAXCODE VALUES.
00008      SKIP2
00009  ENVIRONMENT DIVISION.
00010  CONFIGURATION SECTION.
00011  SOURCE-COMPUTER. IBM-370.
00012  OBJECT-COMPUTER. IBM-370.
00013  INPUT-OUTPUT SECTION.
00014  FILE-CONTROL.
00015      SELECT EQVAL-FILE  ASSIGN TO UT-S-EQVAL.
00016      SELECT MASTER-IN ASSIGN TO UT-S-MAST.
00017      SELECT OUTPUT-FILE ASSIGN TO UT-S-OUT.
00018      SELECT PRINT-FILE  ASSIGN TO UT-S-PRINT.
00019      SKIP2
00020  DATA DIVISION.
00021  FILE SECTION.
00022      SKIP1
00023  FD  EQVAL-FILE
00024      BLOCK  CONTAINS   0 RECORDS
00025      RECORD CONTAINS 130 CHARACTERS
00026      RECORDING  MODE  IS F
00027      LABEL  RECORDS  ARE STANDARD.
00028      SKIP1
00029  01  EQVAL-REC               PIC X(130).
00030  FD  MASTER-IN
00031      BLOCK CONTAINS 0 CHARACTERS
00032      RECORD CONTAINS 122 TO 18706 CHARACTERS
00033      LABEL RECORDS ARE STANDARD
00034      RECORDING MODE IS S
00035      DATA RECORD IS MIN-REC.
00036  01  MIN-REC.
00037  COPY ASREASRD01.
00038      SKIP1
00039  FD  OUTPUT-FILE
00040      BLOCK  CONTAINS   0 RECORDS
00041      RECORD CONTAINS 130 CHARACTERS
00042      RECORDING MODE  IS F
00043      LABEL  RECORDS  ARE STANDARD.
00044      SKIP1
00045  01  OUT-REC                 PIC X(130).
00046      SKIP1
00047  FD  PRINT-FILE
00048      BLOCK  CONTAINS   0 RECORDS
00049      RECORD CONTAINS 133 CHARACTERS
00050      RECORDING  MODE  IS F
00051      LABEL  RECORDS  ARE STANDARD.
00052      SKIP1
00053  01  PRINT-REC               PIC X(133).
00054      SKIP2
00055  WORKING-STORAGE SECTION.
00056      SKIP1
00057  77  PAGE-CNT        COMP-3  PIC S9(5)   VALUE +0.
00058  77  LINE-CNT        COMP-3  PIC S9(3)   VALUE +58.
00059  77  EQVAL-IN-CNT    COMP-3  PIC S9(7)   VALUE +0.
00060  77  EQVAL-OUT-CNT   COMP-3  PIC S9(7)   VALUE +0.
00061  77  MAST-IN-CNT     COMP-3  PIC S9(7)   VALUE +0.
00062  77  NO-MATCH-CNT    COMP-3  PIC S9(7)   VALUE +0.
00063  77  NON-RES-CNT     COMP-3  PIC S9(7)   VALUE +0.
00064  77  EQVAL-EOF               PIC X      VALUE 'N'.
00065      88  END-OF-EQVAL-FILE              VALUE 'Y'.
00066  77  MAST-EOF                PIC X      VALUE 'N'.
00067      88  END-OF-MAST-FILE               VALUE 'Y'.
00068  77  MSG                     PIC X(23)  VALUE SPACES.
00069      SKIP1
00070  01  UNPK-TXCD           PIC 9(5).
00071      SKIP1
00072  01  FILLER REDEFINES UNPK-TXCD.
00073      05  TOWN            PIC 99.
00074      05  FILLER          PIC 999.
00075  01  CLASS-BREAK         PIC 9(3).
00076      SKIP1
00077  01  FILLER REDEFINES CLASS-BREAK.
00078      05  MAJ-CLS         PIC 9.
00079          88  CLS-2                  VALUE 1 THRU 6.
00080      05  MIN-CLS         PIC 99.
00081          88  CLS-BYPASS  VALUE 00 39 40 41 50.
00082      SKIP1
00083  01  HOME-REC.
00084  COPY HOMEXERD01.
00085      SKIP1
00086  01  WORK-AREA.
00087      SKIP1
00088      05  WS-EQVAL-REC-KEY.
00089          10  WS-EQTOWN       PIC 99.
00090          10  WS-EQVOL        PIC 9(3)              COMP-3.
00091          10  WS-EQPROP       PIC 9(15)             COMP-3.
00092          10  WS-EQTXTYP      PIC X.
00093      SKIP1
00094      05  WS-MAST-REC-KEY.
00095          10  WS-MSTOWN       PIC 99.
00096          10  WS-MSVOL        PIC 9(3)              COMP-3.
00097          10  WS-MSPROP       PIC 9(15)             COMP-3.
00098          10  WS-MSTXTYP      PIC X.
00099      SKIP1
00100      05  CURR-EQVAL.
00101          10  CE-TOWN         PIC 99.
00102          10  CE-VOL          PIC 9(3)              COMP-3.
00103          10  CE-PROP         PIC 9(15)             COMP-3.
00104          10  CE-TXTYP        PIC X.
00105      SKIP1
00106      05  CURR-MAST.
00107          10  CM-TOWN         PIC 99.
00108          10  CM-VOL          PIC 9(3)              COMP-3.
00109          10  CM-PROP         PIC 9(15)             COMP-3.
00110          10  CM-TXTYP        PIC X.
00111      SKIP1
00112      05  SAVE-EQVAL.
00113          10  SE-TOWN         PIC 99.
00114          10  SE-VOL          PIC 9(3)   VALUE 0    COMP-3.
00115          10  SE-PROP         PIC 9(15)  VALUE 0    COMP-3.
00116          10  SE-TXTYP        PIC X      VALUE SPACE.
00117      SKIP1
00118      05  SAVE-MAST.
00119          10  SM-TOWN         PIC 99.
00120          10  SM-VOL          PIC 9(3)   VALUE 0    COMP-3.
00121          10  SM-PROP         PIC 9(15)  VALUE 0    COMP-3.
00122          10  SM-TXTYP        PIC X      VALUE SPACE.
00123      SKIP1
00124      05  HEAD-A.
00125          10  FILLER          PIC X(3)   VALUE SPACES.
00126          10  HD-DATE         PIC X(8).
00127          10  FILLER          PIC X(44)  VALUE SPACES.
00128          10  FILLER          PIC X(22)  VALUE 'OFFICE OF THE ASSES
00129 -            'SOR'.
00130          10  FILLER          PIC X(44)  VALUE SPACES.
00131          10  FILLER          PIC X(5)   VALUE 'PAGE '.
00132          10  HD-PAGE         PIC ZZ,ZZ9.
00133      SKIP1
00134      05  HEAD-B.
00135          10  FILLER          PIC X(3)   VALUE SPACES.
00136          10  FILLER          PIC X(8)   VALUE 'ASREA880'.
00137          10  FILLER          PIC X(31)  VALUE SPACES.
00138          10  FILLER          PIC X(48)  VALUE 'HOMEOWNERS HOMESTEA
00139 -            'D TAXCODE UPDATE ERROR REPORT'.
00140      SKIP1
00141      05  HEAD-C.
00142          10  FILLER          PIC X(3)   VALUE SPACES.
00143          10  FILLER          PIC X(16)  VALUE 'TOWN   VOL'.
00144          10  FILLER          PIC X(20)  VALUE 'PROPERTY NUMBER'.
00145          10  FILLER          PIC X(10)  VALUE ' TAX TYPE '.
00146          10  FILLER          PIC X(11)  VALUE 'TAXCODE'.
00147          10  FILLER          PIC X(5)   VALUE 'CLASS'.
00148      SKIP1
00149      05  DETAIL-LINE.
00150          10  FILLER          PIC X(4)   VALUE SPACES.
00151          10  D-TOWN          PIC 99.
00152          10  FILLER          PIC X(4)   VALUE SPACES.
00153          10  D-VOL           PIC 999.
00154          10  FILLER          PIC X(4)   VALUE SPACES.
00155          10  D-PROP          PIC 99,99,999,999,9999.
00156          10  FILLER          PIC X(10)  VALUE SPACES.
00157          10  D-TXTYP         PIC X      VALUE SPACE.
00158          10  FILLER          PIC X(4)   VALUE SPACES.
00159          10  D-TXCDE         PIC 99999.
00160          10  FILLER          PIC X(6)   VALUE SPACES.
00161          10  D-CLS           PIC 9,99.
00162          10  FILLER          PIC X(6)   VALUE SPACES.
00163          10  D-MSG           PIC X(23).
00164      SKIP1
00165      05  TOTAL-LINE-1.
00166          10  FILLER          PIC X(3)   VALUE SPACES.
00167          10  FILLER          PIC X(35)  VALUE 'TOTAL EQVALUE RECOR
00168 -            'DS READ'.
00169          10  TOT-EQVAL-IN    PIC Z,ZZZ,ZZ9.
00170      SKIP1
00171      05  TOTAL-LINE-2.
00172          10  FILLER          PIC X(3)   VALUE SPACES.
00173          10  FILLER          PIC X(35)  VALUE 'TOTAL EQVALUE RECOR
00174 -            'DS WRITTEN'.
00175          10  TOT-EQVAL-OUT   PIC Z,ZZZ,ZZ9.
00176      SKIP1
00177      05  TOTAL-LINE-3.
00178          10  FILLER          PIC X(3)   VALUE SPACES.
00179          10  FILLER          PIC X(35)  VALUE 'TOTAL MASTER RECORD
00180 -            'S READ'.
00181          10  TOT-MAST-IN     PIC Z,ZZZ,ZZ9.
00182      SKIP1
00183      05  TOTAL-LINE-4.
00184          10  FILLER          PIC X(3)   VALUE SPACES.
00185          10  FILLER          PIC X(35)  VALUE 'TOTAL NO-MATCHING E
00186 -            'QVALUE RECORDS'.
00187          10  TOT-NOMATCH     PIC Z,ZZZ,ZZ9.
00188      SKIP1
00189      05  TOTAL-LINE-5.
00190          10  FILLER      PIC X(3)   VALUE SPACES.
00191          10  FILLER      PIC X(35)  VALUE 'TOTAL NON-RESIDENTIAL R
00192 -            'ECORDS'.
00193          10  TOT-NON-RES PIC ZZZ,ZZ9.
00194      EJECT
00195  PROCEDURE DIVISION.
00196      SKIP1
00197  A010-HOUSEKEEPING.
00198      OPEN INPUT EQVAL-FILE MASTER-IN
00199          OUTPUT OUTPUT-FILE PRINT-FILE.
00200      MOVE CURRENT-DATE TO HD-DATE.
00201      PERFORM A030-READ-EQVAL-FILE THRU A030-EXIT.
00202      PERFORM A040-READ-MASTER-FILE THRU A040-EXIT.
00203      PERFORM A020-MAINLINE THRU A020-EXIT
00204          UNTIL END-OF-EQVAL-FILE AND END-OF-MAST-FILE.
00205      IF LINE-CNT GREATER THAN +50
00206          PERFORM B100-HEADING THRU B100-EXIT.
00207      MOVE EQVAL-IN-CNT  TO TOT-EQVAL-IN.
00208      MOVE EQVAL-OUT-CNT TO TOT-EQVAL-OUT.
00209      MOVE MAST-IN-CNT   TO TOT-MAST-IN.
00210      MOVE NO-MATCH-CNT  TO TOT-NOMATCH.
00211      MOVE NON-RES-CNT   TO TOT-NON-RES.
00212      WRITE PRINT-REC FROM TOTAL-LINE-1 AFTER ADVANCING 3.
00213      WRITE PRINT-REC FROM TOTAL-LINE-2 AFTER ADVANCING 1.
00214      WRITE PRINT-REC FROM TOTAL-LINE-3 AFTER ADVANCING 1.
00215      WRITE PRINT-REC FROM TOTAL-LINE-4 AFTER ADVANCING 1.
00216      WRITE PRINT-REC FROM TOTAL-LINE-5 AFTER ADVANCING 1.
00217      DISPLAY TOTAL-LINE-1.
00218      DISPLAY TOTAL-LINE-2.
00219      DISPLAY TOTAL-LINE-3.
00220      DISPLAY TOTAL-LINE-4.
00221      DISPLAY TOTAL-LINE-5.
00222      CLOSE EQVAL-FILE MASTER-IN OUTPUT-FILE PRINT-FILE.
00223      STOP RUN.
00224      SKIP1
00225  A020-MAINLINE.
00226      IF WS-EQVAL-REC-KEY EQUAL WS-MAST-REC-KEY
00227          PERFORM A050-WRITE-REC THRU A050-EXIT
00228          PERFORM A030-READ-EQVAL-FILE THRU A030-EXIT
00229          PERFORM A040-READ-MASTER-FILE THRU A040-EXIT
00230      ELSE
00231          IF WS-EQVAL-REC-KEY LESS THAN WS-MAST-REC-KEY
00232              MOVE 'PARCEL NO-LONGER EXISTS' TO MSG
00233              ADD +1 TO NO-MATCH-CNT
00234              PERFORM A070-PRINT THRU A070-EXIT
00235              PERFORM A030-READ-EQVAL-FILE THRU A030-EXIT
00236          ELSE
00237              PERFORM A040-READ-MASTER-FILE THRU A040-EXIT.
00238  A020-EXIT. EXIT.
00239      SKIP1
00240  A030-READ-EQVAL-FILE.
00241      READ EQVAL-FILE INTO HOME-REC AT END
00242          MOVE HIGH-VALUES TO WS-EQVAL-REC-KEY
00243          MOVE 'Y' TO EQVAL-EOF.
00244      IF NOT END-OF-EQVAL-FILE
00245          MOVE HM-TXCD TO UNPK-TXCD
00246          MOVE TOWN    TO CE-TOWN
00247          MOVE HM-VOL  TO CE-VOL
00248          MOVE HM-PROP TO CE-PROP
00249          MOVE HM-TXTYP TO CE-TXTYP
00250              IF CURR-EQVAL LESS THAN SAVE-EQVAL
00251                  MOVE 16 TO RETURN-CODE
00252                  MOVE 'Y' TO EQVAL-EOF MAST-EOF
00253                  DISPLAY 'EQVAL FILE SEQUENCE ERROR'
00254                  DISPLAY 'PREVIOUS RECORD ' SE-TOWN SPACE SE-VOL
00255                                             SPACE SE-PROP SPACE
00256                                             SE-TXTYP
00257                  DISPLAY 'CURRENT RECORD ' CE-TOWN SPACE CE-VOL
00258                                            SPACE CE-PROP SPACE
00259                                            CE-TXTYP
00260              ELSE
00261                  MOVE CURR-EQVAL TO SAVE-EQVAL
00262                  MOVE TOWN    TO WS-EQTOWN
00263                  MOVE HM-VOL  TO WS-EQVOL
00264                  MOVE HM-PROP TO WS-EQPROP
00265                  MOVE HM-TXTYP TO WS-EQTXTYP
00266                  ADD +1 TO EQVAL-IN-CNT.
00267  A030-EXIT. EXIT.
00268      SKIP1
00269  A040-READ-MASTER-FILE.
00270      READ MASTER-IN AT END
00271          MOVE HIGH-VALUES TO WS-MAST-REC-KEY
00272          MOVE 'Y' TO MAST-EOF.
00273      IF NOT END-OF-MAST-FILE
00274          MOVE M-CLS  TO CLASS-BREAK
00275          MOVE M-TXCD TO UNPK-TXCD
00276          MOVE TOWN   TO CM-TOWN
00277          MOVE M-VOL  TO CM-VOL
00278          MOVE M-PROP TO CM-PROP
00279          MOVE M-TXTYP TO CM-TXTYP
00280          IF CURR-MAST LESS THAN SAVE-MAST
00281              MOVE 16 TO RETURN-CODE
00282              MOVE 'Y' TO EQVAL-EOF MAST-EOF
00283              DISPLAY 'MASTER FILE SEQUENCE ERROR'
00284              DISPLAY 'PREVIOUS RECORD ' SM-TOWN SPACE SM-VOL
00285                                         SPACE SM-PROP SPACE
00286                                         SM-TXTYP
00287              DISPLAY 'CURRENT RECORD ' CM-TOWN SPACE CM-VOL
00288                                        SPACE CM-PROP SPACE
00289                                        CM-TXTYP
00290          ELSE
00291              MOVE CURR-MAST TO SAVE-MAST
00292              MOVE TOWN   TO WS-MSTOWN
00293              MOVE M-VOL  TO WS-MSVOL
00294              MOVE M-PROP TO WS-MSPROP
00295              MOVE M-TXTYP TO WS-MSTXTYP
00296              ADD +1 TO MAST-IN-CNT.
00297  A040-EXIT. EXIT.
00298      SKIP1
00299  A050-WRITE-REC.
00300      IF CLS-2 AND NOT CLS-BYPASS
00301          MOVE M-TXCD TO HM-TXCD
00302          MOVE M-CLS TO HM-CLS
00303          MOVE M-STAT3 TO HM-CLERK-CLS
00304          MOVE M-TXTYP TO HM-TXTYP
00305          WRITE OUT-REC FROM HOME-REC
00306          ADD +1 TO EQVAL-OUT-CNT
00307      ELSE
00308          MOVE 'NON-RESIDENTIAL PARCEL' TO MSG
00309          ADD +1 TO NON-RES-CNT
00310          PERFORM A070-PRINT THRU A070-EXIT.
00311  A050-EXIT. EXIT.
00312      SKIP1
00313  A070-PRINT.
00314      IF LINE-CNT GREATER THAN +57
00315          PERFORM B100-HEADING THRU B100-EXIT.
00316      MOVE HM-TXCD TO UNPK-TXCD.
00317      MOVE TOWN    TO D-TOWN.
00318      MOVE HM-VOL  TO D-VOL.
00319      MOVE HM-PROP TO D-PROP.
00320      INSPECT D-PROP REPLACING ALL ',' BY '-'.
00321      MOVE HM-TXTYP TO D-TXTYP.
00322      MOVE HM-TXCD TO D-TXCDE.
00323      MOVE HM-CLS  TO D-CLS.
00324      INSPECT D-CLS REPLACING ALL ',' BY '-'.
00325      MOVE MSG     TO D-MSG.
00326      WRITE PRINT-REC FROM DETAIL-LINE AFTER ADVANCING 2.
00327      ADD +2 TO LINE-CNT.
00328  A070-EXIT. EXIT.
00329      SKIP1
00330  B100-HEADING.
00331      ADD +1 TO PAGE-CNT.
00332      MOVE PAGE-CNT TO HD-PAGE.
00333      WRITE PRINT-REC FROM HEAD-A AFTER PAGE.
00334      WRITE PRINT-REC FROM HEAD-B AFTER ADVANCING 2.
00335      WRITE PRINT-REC FROM HEAD-C AFTER ADVANCING 2.
00336      MOVE +5 TO LINE-CNT.
00337  B100-EXIT. EXIT.
