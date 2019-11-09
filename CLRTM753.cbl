00001  IDENTIFICATION DIVISION.
00002  PROGRAM-ID.    CLRTM753.
00003 *****************************************************************
00004 *AUTHOR.        BROWN/MOLIS.                                    *
00005 *DATE-WRITTEN.  09/95.                                          *
00006 *REMARKS.       CLRTM753 COMPUTES THE DISCONNECTED AND ANNEXED  *
00007 *                        AGENCY VALUATION.                      *
00008 *****************************************************************
00009 *****************************************************************
00010 *           PROGRAM WORK REQUEST (MODIFICATION)                 *
00011 *                                                               *
00012 * PROGRAMMER: DPSILVA                                           *
00013 * DATE: 09/02/99                                                *
00014 * REQUEST LETTER NAME: CL071799.001                             *
00015 *****************************************************************
00016 *                MODIFICATION DESCRIPTION                       *
00017 *                                                               *
00018 * MODIFIED PROGRAM CLRTM753 FOR YEAR 2000.                      *
00019 *                                                               *
00020 * 1. IN WORKING STORAGE:                                        *
00021 *   A). EXPANDED H1-CURR-DATE FROM 8 TO 10 AND CHANGED THE      *
00022 *       PROGRAM TO DISPLAY THE CURRENT DATE ALONG WITH CENTURY  *
00023 *       IN THIS FIELD.                                          *
00024 *****************************************************************
00025  ENVIRONMENT DIVISION.
00026  INPUT-OUTPUT SECTION.
00027  FILE-CONTROL.
00028      SELECT PRIOR-AGCY-MAST ASSIGN TO UT-S-PRIORAGY.
00029      SELECT CURR-AGCY-MAST  ASSIGN TO UT-S-CURRAGY.
00030      SELECT CLDISANXFILE    ASSIGN TO UT-S-DISANXAG.
00031      SELECT PRINT-FILE      ASSIGN TO UT-S-PRINTFLE.
00032  EJECT
00033  DATA DIVISION.
00034  FILE SECTION.
00035  SKIP2
00036  COPY CLASRAGYF3.
00037  EJECT
00038  COPY CLASRAGYF2.
00039  EJECT
00040  COPY CLDISANXF1.
00041  EJECT
00042  COPY PRINTFILE.
00043  EJECT
00044  WORKING-STORAGE SECTION.
00045  77  PRIOR-AGCY-REC-CTR     PIC 9(8)  VALUE 0.
00046  77  CURR-AGCY-REC-CTR      PIC 9(8)  VALUE 0.
00047  77  DIS-ANX-AGCY-REC-CTR   PIC 9(8)  VALUE 0.
00048  77  DISCONNECT-OUT-TOTAL   PIC 9(8)  VALUE 0.
00049  77  ANNEXATION-OUT-TOTAL   PIC 9(8)  VALUE 0.
00050  77  LINE-CTR               PIC S9(4) VALUE 99.
00051  77  PAGE-CTR               PIC S9(4) VALUE 0.
00052  77  SUB1                   PIC S9(4) VALUE 0.
00053  77  PRIOR-AGCY-MAST-EOF-SW PIC 9     VALUE ZEROES.
00054      88 PRIOR-AGCY-MAST-EOF           VALUE 1.
00055  77  CURR-AGCY-MAST-EOF-SW  PIC 9     VALUE ZEROES.
00056      88 CURR-AGCY-MAST-EOF            VALUE 1.
00057  77  SEARCH-END-SW          PIC 9     VALUE ZEROES.
00058      88 SEARCH-END                    VALUE 1.
00059  77  PREV-PRIOR-KEY         PIC 9(15) VALUE 0 PACKED-DECIMAL.
00060  77  PREV-CURR-KEY          PIC 9(15) VALUE 0 PACKED-DECIMAL.
00061  77  SAVE-KEY               PIC 9(15) VALUE 0 PACKED-DECIMAL.
00062  77  CURR-AGCY-NOTFND       PIC X(48) VALUE
00063      'NO MATCHING CURRENT YEAR RECORD FOR DIVISION NO.'.
00064  77  PRIOR-AGCY-NOTFND      PIC X(46) VALUE
00065      'NO MATCHING PRIOR YEAR RECORD FOR DIVISION NO.'.
00066
00067  01  FILLER.
00068      03 MDY-DATE.
00069         05 MO               PIC 99.
00070         05 FILLER           PIC X     VALUE '/'.
00071         05 DA               PIC 99.
00072         05 FILLER           PIC X     VALUE '/'.
00073         05 YR               PIC 9999.
00074      03 YMD-DATE.
00075         05 YR               PIC 9999.
00076         05 MO               PIC 99.
00077         05 DA               PIC 99.
00078
00079      03 HEADING1.
00080         05 FILLER           PIC XX    VALUE SPACES.
00081         05 H1-CURR-DATE     PIC X(10).
00082         05 FILLER           PIC X(40) VALUE SPACES.
00083         05 FILLER           PIC X(71) VALUE
00084            'OFFICE OF THE COOK COUNTY CLERK'.
00085         05 FILLER           PIC X(5)  VALUE 'PAGE '.
00086         05 H1-PAGE          PIC Z,ZZ9.
00087
00088      03 HEADING2.
00089         05 FILLER           PIC XX    VALUE SPACES.
00090         05 FILLER           PIC X(43) VALUE 'CLRTM753'.
00091         05 FILLER           PIC X(40) VALUE
00092            'DISCONNECTED/ANNEXED AGENCY ERROR REPORT'.
00093
00094      03 HEADING3.
00095         05 FILLER           PIC X(5)  VALUE SPACES.
00096         05 FILLER           PIC X(16) VALUE 'DIVISION'.
00097         05 FILLER           PIC X(19) VALUE 'PERMANENT INDEX    '.
00098         05 FILLER           PIC X(12) VALUE 'TAX      TAX'.
00099
00100      03 HEADING4.
00101         05 FILLER           PIC X(6)  VALUE SPACES.
00102         05 FILLER           PIC X(19) VALUE 'NUMBER'.
00103         05 FILLER           PIC X(15) VALUE 'NUMBER'.
00104         05 FILLER           PIC X(13) VALUE 'TYPE     CODE'.
00105
00106      03 DETAIL-LINE.
00107         05 FILLER           PIC XX    VALUE SPACES.
00108         05 D-DIVNO          PIC 9(14).
00109         05 FILLER           PIC XXX   VALUE SPACES.
00110         05 D-PROP           PIC 99B99B999B999B9(4).
00111         05 FILLER           PIC X(5)  VALUE SPACES.
00112         05 D-TXTYP          PIC 9.
00113         05 FILLER           PIC X(5)  VALUE SPACES.
00114         05 D-TXCD           PIC 9(5).
00115         05 FILLER           PIC X(4)  VALUE SPACES.
00116         05 D-MESSG          PIC X(48).
00117
00118      03 TOTAL-LINE.
00119         05 FILLER           PIC XX    VALUE SPACES.
00120         05 T-MESSG          PIC X(34).
00121         05 T-TOTAL          PIC Z,ZZZ,ZZ9.
00122
00123      03 PRIOR-AGCY-TABLE.
00124         05 PRIOR-AGCY-TAB   OCCURS 40 TIMES
00125                             INDEXED BY PRIOR-INDEX.
00126            07 PAT-AGCY      PIC 9(9)               PACKED-DECIMAL.
00127         05 PRIOR-TOT-EV     PIC 9(13)              PACKED-DECIMAL.
00128
00129      03 CURR-AGCY-TABLE.
00130         05 CURR-AGCY-TAB    OCCURS 40 TIMES
00131                             INDEXED BY CURR-INDEX.
00132            07 CAT-AGCY      PIC 9(9)               PACKED-DECIMAL.
00133         05 CURR-TOT-EV      PIC 9(13)              PACKED-DECIMAL.
00134
00135      03 WS-DIV-NO.
00136         05  WS-DIV-1ST-6    PIC 9(6).
00137         05  WS-DIV-LAST-9   PIC 9(9).
00138      EJECT
00139  PROCEDURE DIVISION.
00140  0000-MAINLINE.
00141      PERFORM 1000-INITIALIZATION.
00142      PERFORM 2000-PROCESS
00143              UNTIL (PRIOR-AGCY-MAST-EOF AND CURR-AGCY-MAST-EOF)
00144              OR RETURN-CODE EQUAL 16.
00145      PERFORM 9000-FINALIZATION.
00146      STOP RUN.
00147  SKIP2
00148  1000-INITIALIZATION.
00149      OPEN INPUT  PRIOR-AGCY-MAST    CURR-AGCY-MAST
00150           OUTPUT CLDISANXFILE       PRINT-FILE.
00151 *    ACCEPT YMD-DATE FROM DATE.
00152      MOVE FUNCTION CURRENT-DATE(1:8) TO YMD-DATE.
00153      MOVE CORRESPONDING YMD-DATE TO MDY-DATE.
00154      MOVE MDY-DATE TO H1-CURR-DATE.
00155      PERFORM 2200-READ-PRIOR-AGCY-MAST.
00156      PERFORM 2300-READ-CURR-AGCY-MAST.
00157  SKIP2
00158  1100-INITIALIZE-TABLES.
00159      MOVE ZERO TO PRIOR-TOT-EV.
00160      MOVE ZERO TO CURR-TOT-EV.
00161      PERFORM 1200-MOVE-ZERO VARYING PRIOR-INDEX FROM 1 BY 1
00162              UNTIL PRIOR-INDEX GREATER 40.
00163  SKIP2
00164  1200-MOVE-ZERO.
00165      MOVE ZERO TO PAT-AGCY (PRIOR-INDEX).
00166      MOVE ZERO TO CAT-AGCY (PRIOR-INDEX).
00167  EJECT
00168  2000-PROCESS.
00169      IF PA-DIVNO EQUAL CA-DIVNO
00170         MOVE PA-DIVNO TO SAVE-KEY
00171         PERFORM 1100-INITIALIZE-TABLES
00172         PERFORM 3100-LOAD-OLD-AGENCIES
00173            UNTIL PA-DIVNO NOT EQUAL SAVE-KEY
00174            OR PRIOR-AGCY-MAST-EOF
00175         PERFORM 3200-LOAD-NEW-AGENCIES
00176            UNTIL CA-DIVNO NOT EQUAL SAVE-KEY
00177            OR CURR-AGCY-MAST-EOF
00178         PERFORM 5000-CREATE-DISC-ANX-RECORD
00179      ELSE
00180         IF PA-DIVNO LESS CA-DIVNO
00181            MOVE CURR-AGCY-NOTFND TO D-MESSG
00182            MOVE PA-DIVNO TO D-DIVNO
00183            MOVE PA-PROP TO D-PROP
00184            MOVE PA-TXTYP TO D-TXTYP
00185            MOVE PA-TXCD TO D-TXCD
00186            PERFORM 7000-FORMAT-DETAIL-LINE
00187            PERFORM 2200-READ-PRIOR-AGCY-MAST
00188         ELSE
00189            MOVE PRIOR-AGCY-NOTFND TO D-MESSG
00190            MOVE CA-DIVNO TO D-DIVNO
00191            MOVE CA-PROP TO D-PROP
00192            MOVE CA-TXTYP TO D-TXTYP
00193            MOVE CA-TXCD TO D-TXCD
00194            PERFORM 7000-FORMAT-DETAIL-LINE
00195            PERFORM 2300-READ-CURR-AGCY-MAST.
00196  EJECT
00197  2200-READ-PRIOR-AGCY-MAST.
00198      READ PRIOR-AGCY-MAST AT END
00199         MOVE 1 TO PRIOR-AGCY-MAST-EOF-SW
00200         MOVE 999999999999999 TO PA-DIVNO.
00201      IF NOT PRIOR-AGCY-MAST-EOF
00202         ADD 1 TO PRIOR-AGCY-REC-CTR
00203         IF PA-DIVNO < PREV-PRIOR-KEY
00204            DISPLAY 'PRIOR-AGCY-MAST IS OUT OF SEQUENCE'
00205            DISPLAY 'CURRENT KEY IS   ' PA-DIVNO
00206            DISPLAY 'PREVIOUS KEY IS  ' PREV-PRIOR-KEY
00207            MOVE 16 TO RETURN-CODE
00208         ELSE
00209            MOVE PA-DIVNO TO PREV-PRIOR-KEY.
00210  SKIP2
00211  2300-READ-CURR-AGCY-MAST.
00212      READ CURR-AGCY-MAST AT END
00213         MOVE 1 TO CURR-AGCY-MAST-EOF-SW
00214         MOVE 999999999999999 TO CA-DIVNO.
00215      IF NOT CURR-AGCY-MAST-EOF
00216         ADD 1 TO CURR-AGCY-REC-CTR
00217         IF CA-DIVNO < PREV-CURR-KEY
00218            DISPLAY 'CURR-AGCY-MAST IS OUT OF SEQUENCE'
00219            DISPLAY 'CURRENT KEY IS   ' CA-DIVNO
00220            DISPLAY 'PREVIOUS KEY IS  ' PREV-CURR-KEY
00221            MOVE 16 TO RETURN-CODE
00222         ELSE
00223            MOVE CA-DIVNO TO PREV-CURR-KEY.
00224  EJECT
00225  3100-LOAD-OLD-AGENCIES.
00226      ADD PA-EQUL-VAL TO PRIOR-TOT-EV.
00227      SET PRIOR-INDEX TO 1.
00228      PERFORM 3150-COMPARE-OLD-AGENCIES
00229              VARYING SUB1 FROM 1 BY 1
00230              UNTIL PRIOR-INDEX GREATER 40 OR PA-AGENCY(SUB1) = 0.
00231      PERFORM 2200-READ-PRIOR-AGCY-MAST.
00232  SKIP2
00233  3150-COMPARE-OLD-AGENCIES.
00234      SET PRIOR-INDEX TO 1.
00235      SEARCH PRIOR-AGCY-TAB AT END
00236         PERFORM 3175-LOAD-AGENCY
00237      WHEN PA-AGENCY (SUB1) = PAT-AGCY (PRIOR-INDEX)
00238         NEXT SENTENCE
00239      END-SEARCH.
00240  SKIP2
00241  3175-LOAD-AGENCY.
00242      SET PRIOR-INDEX TO 1.
00243      SEARCH PRIOR-AGCY-TAB
00244      WHEN PAT-AGCY (PRIOR-INDEX) = 0
00245         MOVE PA-AGENCY (SUB1) TO PAT-AGCY (PRIOR-INDEX)
00246      END-SEARCH.
00247  EJECT
00248  3200-LOAD-NEW-AGENCIES.
00249      ADD CA-EQUL-VAL TO CURR-TOT-EV.
00250      SET CURR-INDEX TO 1.
00251      PERFORM 3250-COMPARE-NEW-AGENCIES
00252              VARYING SUB1 FROM 1 BY 1
00253              UNTIL CURR-INDEX GREATER 40 OR CA-AGENCY (SUB1) = 0.
00254      PERFORM 2300-READ-CURR-AGCY-MAST.
00255  SKIP2
00256  3250-COMPARE-NEW-AGENCIES.
00257      SET CURR-INDEX TO 1.
00258      SEARCH CURR-AGCY-TAB AT END
00259         PERFORM 3275-LOAD-AGENCY
00260      WHEN CA-AGENCY (SUB1) = CAT-AGCY (CURR-INDEX)
00261           NEXT SENTENCE
00262      END-SEARCH.
00263  SKIP2
00264  3275-LOAD-AGENCY.
00265      SET CURR-INDEX TO 1.
00266      SEARCH CURR-AGCY-TAB
00267      WHEN CAT-AGCY (CURR-INDEX) = 0
00268         MOVE CA-AGENCY (SUB1) TO CAT-AGCY (CURR-INDEX)
00269      END-SEARCH.
00270  EJECT
00271  5000-CREATE-DISC-ANX-RECORD.
00272      MOVE SPACES TO CLDISANX-REC.
00273      MOVE SAVE-KEY TO DA-DIVNO.
00274      MOVE PRIOR-TOT-EV TO DA-TOTPREV.
00275      MOVE CURR-TOT-EV TO DA-TOTCUEV.
00276      MOVE 0 TO DA-SEGCTR.
00277      MOVE SAVE-KEY TO WS-DIV-NO.
00278 *    IF (WS-DIV-1ST-6 = 0) OR (PRIOR-TOT-EV = 0)
00279 *       OR (CURR-TOT-EV = 0)
00280         MOVE +100.0 TO DA-PCTCHG.
00281 *    ELSE
00282 *       COMPUTE DA-PCTCHG ROUNDED = CURR-TOT-EV / PRIOR-TOT-EV
00283 *    END-IF.
00284  SKIP1
00285      PERFORM 5100-OLD-AGENCY-LOOKUP
00286              VARYING PRIOR-INDEX FROM 1 BY 1
00287              UNTIL PRIOR-INDEX GREATER 40 OR
00288                    PAT-AGCY(PRIOR-INDEX) = ZERO.
00289  SKIP1
00290      PERFORM 5200-NEW-AGENCY-LOOKUP
00291              VARYING CURR-INDEX FROM 1 BY 1
00292              UNTIL CURR-INDEX GREATER 40 OR
00293                    CAT-AGCY(CURR-INDEX) = ZERO.
00294  SKIP1
00295      WRITE CLDISANX-REC.
00296      ADD 1 TO DIS-ANX-AGCY-REC-CTR.
00297  SKIP2
00298  5100-OLD-AGENCY-LOOKUP.
00299      MOVE ZERO TO SEARCH-END-SW.
00300      PERFORM 5150-OLD-ROUTINE
00301              VARYING CURR-INDEX FROM 1 BY 1
00302              UNTIL CURR-INDEX GREATER 40 OR SEARCH-END.
00303      IF NOT SEARCH-END
00304         ADD 1 TO DA-SEGCTR
00305         ADD 1 TO DISCONNECT-OUT-TOTAL
00306         MOVE DA-SEGCTR TO SUB1
00307         MOVE 'D' TO DA-SEG-TYP (SUB1)
00308         MOVE PAT-AGCY (PRIOR-INDEX) TO DA-SEG-AGCY (SUB1)
00309      END-IF.
00310  SKIP2
00311  5150-OLD-ROUTINE.
00312      IF PAT-AGCY (PRIOR-INDEX) = CAT-AGCY (CURR-INDEX)
00313         MOVE 1 TO SEARCH-END-SW
00314      ELSE
00315         IF CAT-AGCY (CURR-INDEX) = ZERO
00316            MOVE 1 TO SEARCH-END-SW
00317            ADD 1 TO DA-SEGCTR
00318            ADD 1 TO DISCONNECT-OUT-TOTAL
00319            MOVE DA-SEGCTR TO SUB1
00320            MOVE 'D' TO DA-SEG-TYP (SUB1)
00321            MOVE PAT-AGCY (PRIOR-INDEX) TO DA-SEG-AGCY (SUB1)
00322         END-IF
00323      END-IF.
00324  EJECT
00325  5200-NEW-AGENCY-LOOKUP.
00326      MOVE ZERO TO SEARCH-END-SW.
00327      PERFORM 5250-NEW-ROUTINE
00328              VARYING PRIOR-INDEX FROM 1 BY 1
00329              UNTIL PRIOR-INDEX GREATER 40 OR SEARCH-END.
00330      IF NOT SEARCH-END
00331         ADD 1 TO DA-SEGCTR
00332         ADD 1 TO ANNEXATION-OUT-TOTAL
00333         MOVE DA-SEGCTR TO SUB1
00334         MOVE 'A' TO DA-SEG-TYP (SUB1)
00335         MOVE CAT-AGCY (CURR-INDEX) TO DA-SEG-AGCY (SUB1)
00336      END-IF.
00337  SKIP2
00338  5250-NEW-ROUTINE.
00339      IF CAT-AGCY (CURR-INDEX) = PAT-AGCY (PRIOR-INDEX)
00340         MOVE 1 TO SEARCH-END-SW
00341      ELSE
00342         IF PAT-AGCY (PRIOR-INDEX) = ZERO
00343            MOVE 1 TO SEARCH-END-SW
00344            ADD 1 TO DA-SEGCTR
00345            ADD 1 TO ANNEXATION-OUT-TOTAL
00346            MOVE DA-SEGCTR TO SUB1
00347            MOVE 'A' TO DA-SEG-TYP (SUB1)
00348            MOVE CAT-AGCY (CURR-INDEX) TO DA-SEG-AGCY (SUB1)
00349         END-IF
00350      END-IF.
00351  EJECT
00352  7000-FORMAT-DETAIL-LINE.
00353      INSPECT D-PROP REPLACING ALL SPACES BY '-'.
00354      IF LINE-CTR GREATER 58
00355         PERFORM 8000-HEADING-RTN.
00356      WRITE PRINT-REC FROM DETAIL-LINE AFTER ADVANCING 2 LINES.
00357      ADD 2 TO LINE-CTR.
00358  SKIP2
00359  8000-HEADING-RTN.
00360      ADD 1 TO PAGE-CTR.
00361      MOVE PAGE-CTR TO H1-PAGE.
00362      WRITE PRINT-REC FROM HEADING1 AFTER ADVANCING PAGE.
00363      WRITE PRINT-REC FROM HEADING2 AFTER ADVANCING 2 LINES.
00364      WRITE PRINT-REC FROM HEADING3 AFTER ADVANCING 2 LINES.
00365      WRITE PRINT-REC FROM HEADING4 AFTER ADVANCING 1 LINES.
00366      MOVE 6 TO LINE-CTR.
00367  EJECT
00368  9000-FINALIZATION.
00369      DISPLAY 'TOTAL PRIOR AGENCY MASTER RECORDS READ             '
00370              PRIOR-AGCY-REC-CTR.
00371      DISPLAY 'TOTAL CURRENT AGENCY MASTER RECORDS READ           '
00372              CURR-AGCY-REC-CTR.
00373      DISPLAY 'TOTAL DISCONNECTED/ANNEXED AGENCY RECORDS WRITTEN  '
00374              DIS-ANX-AGCY-REC-CTR.
00375      DISPLAY 'TOTAL DISCONNECT SEGMENTS WRITTEN                  '
00376              DISCONNECT-OUT-TOTAL.
00377      DISPLAY 'TOTAL ANNEXATION SEGMENTS WRITTEN                  '
00378              ANNEXATION-OUT-TOTAL.
00379  SKIP1
00380      IF LINE-CTR GREATER 53
00381         PERFORM 8000-HEADING-RTN.
00382  SKIP1
00383      MOVE 'TOTAL PRIOR YEAR RECORDS READ'     TO T-MESSG.
00384      MOVE PRIOR-AGCY-REC-CTR                  TO T-TOTAL.
00385      WRITE PRINT-REC FROM TOTAL-LINE AFTER ADVANCING 3 LINES.
00386      MOVE 'TOTAL CURRENT YEAR RECORDS READ'   TO T-MESSG.
00387      MOVE CURR-AGCY-REC-CTR                   TO T-TOTAL.
00388      WRITE PRINT-REC FROM TOTAL-LINE AFTER ADVANCING 1 LINES.
00389      MOVE 'TOTAL OUTPUT RECORDS WRITTEN'      TO T-MESSG.
00390      MOVE DIS-ANX-AGCY-REC-CTR                TO T-TOTAL.
00391      WRITE PRINT-REC FROM TOTAL-LINE AFTER ADVANCING 1 LINES.
00392      MOVE 'TOTAL DISCONNECT SEGMENTS WRITTEN' TO T-MESSG.
00393      MOVE DISCONNECT-OUT-TOTAL                TO T-TOTAL.
00394      WRITE PRINT-REC FROM TOTAL-LINE AFTER ADVANCING 1 LINES.
00395      MOVE 'TOTAL ANNEXATION SEGMENTS WRITTEN' TO T-MESSG.
00396      MOVE ANNEXATION-OUT-TOTAL                TO T-TOTAL.
00397      WRITE PRINT-REC FROM TOTAL-LINE AFTER ADVANCING 1 LINES.
00398      CLOSE PRIOR-AGCY-MAST  CURR-AGCY-MAST  CLDISANXFILE
00399            PRINT-FILE.
00400  EJECT
