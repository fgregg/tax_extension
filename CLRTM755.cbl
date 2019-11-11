00001  IDENTIFICATION DIVISION.
00002  PROGRAM-ID. CLRTM755.
00003 *****************************************************************
00004 * AUTHOR.                                                       *
00005 *       ANALYST    - STAN MOLIS/RICHARD KALEMBA                 *
00006 *       PROGRAMMER - THOMAS D. BROWN                            *
00007 *                                                               *
00008 * DATE-WRITTEN.                                                 *
00009 *       AUGUST 1995.                                            *
00010 *                                                               *
00011 * REMARKS.                                                      *
00012 *       THIS PROGRAM IS TO UPDATE THE FROZEN AGENCY FILE WITH   *
00013 *       DISCONNECTED AND ANNEXED VALUATIONS.                    *
00014 *****************************************************************
00015 *****************************************************************
00016 *           PROGRAM WORK REQUEST (MODIFICATION)                 *
00017 *                                                               *
00018 * PROGRAMMER: DPSILVA                                           *
00019 * DATE: 09/02/99                                                *
00020 * REQUEST LETTER NAME: CL071799.001                             *
00021 *****************************************************************
00022 *                MODIFICATION DESCRIPTION                       *
00023 *                                                               *
00024 * MODIFIED PROGRAM CLRTM755 FOR YEAR 2000.                      *
00025 *                                                               *
00026 * 1. IN WORKING STORAGE:                                        *
00027 *   A). EXPANDED HD1-DATE     FROM 8 TO 10 AND CHANGED THE      *
00028 *       PROGRAM TO DISPLAY THE CURRENT DATE ALONG WITH CENTURY  *
00029 *       IN THIS FIELD.                                          *
00030 *****************************************************************
00031  EJECT
00032  ENVIRONMENT DIVISION.
00033  INPUT-OUTPUT SECTION.
00034  FILE-CONTROL.
00035      SELECT CLDISANXFILE       ASSIGN TO UT-S-DISCANNX.
00036      SELECT AGENCY-ASMT-MASTER ASSIGN TO UT-S-OAGENCY.
00037      SELECT CURR-AGCY-MAST     ASSIGN TO UT-S-NAGENCY.
00038      SELECT PRINT-FILE         ASSIGN TO UT-S-PRNTFILE.
00039      SELECT FRZAGCY-FILE       ASSIGN TO DA-FRZAGIN
00040                                   ORGANIZATION IS INDEXED
00041                                   ACCESS IS RANDOM
00042                                   RECORD KEY IS FA-KEY
00043                                   FILE STATUS IS FRZN-STATUS.
00044
00045  DATA DIVISION.
00046  FILE SECTION.
00047  COPY PRINTFILE.
00048  EJECT
00049  COPY CLFRZAGYF1.
00050  EJECT
00051  COPY CLDISANXF1.
00052  EJECT
00053  COPY CLASRAGYF1.
00054  EJECT
00055  COPY CLASRAGYF2.
00056  EJECT
00057  WORKING-STORAGE SECTION.
00058  SKIP2
00059  77  OLDMASTER-EOF-SW   PIC X         VALUE 'N'.
00060      88  OLDMASTER-EOF  VALUE 'Y'.
00061  77  NEWMASTER-EOF-SW   PIC X         VALUE 'N'.
00062      88  NEWMASTER-EOF  VALUE 'Y'.
00063  77  WS-AM-DIVNO        PIC 9(15)     VALUE 0   PACKED-DECIMAL.
00064  77  WS-CA-DIVNO        PIC 9(15)     VALUE 0   PACKED-DECIMAL.
00065  77  ANNEX-READ         PIC 9(11)     VALUE 0   PACKED-DECIMAL.
00066  77  ANNEX-UPDATED      PIC 9(11)     VALUE 0   PACKED-DECIMAL.
00067  77  ANNEX-WRITTEN      PIC 9(11)     VALUE 0   PACKED-DECIMAL.
00068  77  CURR-YEAR-CNTR     PIC 9(11)     VALUE 0   PACKED-DECIMAL.
00069  77  PRIOR-YEAR-CNTR    PIC 9(11)     VALUE 0   PACKED-DECIMAL.
00070  77  SAVE-OLD-KEY       PIC 9(15)     VALUE 0   PACKED-DECIMAL.
00071  77  SAVE-NEW-KEY       PIC 9(15)     VALUE 0   PACKED-DECIMAL.
00072  77  PRIOR-OLD-KEY      PIC 9(15)     VALUE 0   PACKED-DECIMAL.
00073  77  PRIOR-NEW-KEY      PIC 9(15)     VALUE 0   PACKED-DECIMAL.
00074  77  WS-DISC-VALUE      PIC S9(11)              PACKED-DECIMAL.
00075  77  WS-SEGCTR          PIC 9(3)      VALUE 0.
00076  77  LINE-CNTR          PIC 9(2)      VALUE 99.
00077  77  PAGE-CNTR          PIC 9(5)      VALUE 0.
00078  77  ACPT-DATE          PIC 9(8).
00079  77  SUB1               PIC S9(4)               COMP.
00080  SKIP2
00081  77  OLD-AGCY-EOF-SW    PIC 9         VALUE ZERO.
00082      88  OLD-AGCY-EOF                 VALUE 1.
00083  77  NEW-AGCY-EOF-SW    PIC 9         VALUE ZERO.
00084      88  NEW-AGCY-EOF                 VALUE 1.
00085  77  END-OLD-MASTER-SW  PIC 9         VALUE ZERO.
00086      88  END-OLD-MASTER               VALUE 1.
00087  77  END-NEW-MASTER-SW  PIC 9         VALUE ZERO.
00088      88  END-NEW-MASTER               VALUE 1.
00089  77  SEARCH-SWITCH      PIC 9         VALUE ZERO.
00090      88  SEARCH-END                   VALUE 1.
00091  77  PRINT-SWITCH       PIC X         VALUE SPACE.
00092      88  PRINT-ANNEX                  VALUE 'A'.
00093  77  DISC-ANX-SWITCH    PIC X         VALUE 'N'.
00094      88  DISC-ANX-EOF                 VALUE 'Y'.
00095  77  FRZN-STATUS        PIC 99.
00096      88  GOOD-STATUS                  VALUE 00 97.
00097      88  FRZN-EOF                     VALUE 10.
00098      88  REC-FND                      VALUE 00.
00099      88  NOT-FND                      VALUE 23.
00100  01  FRZN-STATUS-2               BINARY.
00101      10  VS-RETURN      PIC 9(2)      VALUE 0.
00102      10  VS-FUNCTION    PIC 9         VALUE 0.
00103      10  VS-FEEDBACK    PIC 9(3)      VALUE 0.
00104  EJECT
00105  01  WORK-AREA.
00106      05  TABLES.
00107          10  DISC-TABLE.
00108              15  DISC-TABLE-ENTRY OCCURS 50 TIMES
00109                                   INDEXED BY DISC-INDEX.
00110                  20  DISC-AGENCY  PIC 9(9).
00111  SKIP2
00112          10  ANNEX-TABLE.
00113              15  ANNEX-TABLE-ENTRY OCCURS 50 TIMES
00114                                    INDEXED BY ANNEX-INDEX.
00115                  20  ANNEX-AGENCY  PIC 9(9).
00116  SKIP2
00117      05  HEADING1.
00118          10  FILLER       PIC XX      VALUE SPACE.
00119          10  HD1-DATE     PIC X(10).
00120          10  FILLER       PIC X(30)   VALUE SPACES.
00121          10  FILLER       PIC X(70)   VALUE
00122              'OFFICE OF THE COOK COUNTY CLERK    '.
00123          10  FILLER       PIC X(7)    VALUE 'PAGE   '.
00124          10  HD1-PAGE     PIC ZZ,ZZ9.
00125  SKIP2
00126      05  HEADING2.
00127          10  FILLER       PIC X(37)   VALUE '  CLRTM754'.
00128          10  FILLER       PIC X(45)   VALUE
00129              'REPORT OF UPDATED FROZEN AGENCY PARCELS '.
00130  EJECT
00131      05  HEADING3.
00132          10  FILLER       PIC X(19)   VALUE '  DIVISION NUMBER '.
00133          10  FILLER       PIC X(18)   VALUE 'PROPERTY INDEX NO.'.
00134          10  FILLER       PIC X(17)   VALUE '  TX CODE  AGENCY'.
00135          10  FILLER       PIC X(17)   VALUE '   EQ. VALUATION '.
00136          10  FILLER       PIC X(17)   VALUE '    PCT CHANGE   '.
00137          10  FILLER       PIC X(22)   VALUE '   ANNEXED VALUE  '.
00138          10  FILLER       PIC X(22)   VALUE 'DISCONNECTED VALUE'.
00139  SKIP2
00140      05  HEADING4.
00141          10  FILLER       PIC XX      VALUE SPACES.
00142          10  HDR-DIV-NO   PIC 9(14).
00143          10  FILLER       PIC X(3)    VALUE SPACES.
00144          10  HDR-PROP-NO  PIC 99,99,999,999,9999.
00145          10  FILLER       PIC X(2)    VALUE SPACES.
00146          10  HDR-TX-CODE  PIC 9(5).
00147          10  FILLER       PIC X(1)    VALUE SPACES.
00148          10  HDR-AGENCY   PIC 9(2),9(4),9(3).
00149          10  FILLER       PIC X(1)    VALUE SPACES.
00150          10  HDR-TOT-VAL  PIC ZZZ,ZZZ,ZZ9.
00151          10  FILLER       PIC X(5)    VALUE SPACES.
00152          10  HDR-PCT-CHG  PIC 999.9(7).
00153          10  FILLER       PIC X(3)    VALUE SPACES.
00154          10  HDR-ANX-VAL  PIC Z,ZZZ,ZZZ,ZZZ,ZZZ.
00155          10  FILLER       PIC X(5)    VALUE SPACES.
00156          10  HDR-DISC-VAL PIC Z,ZZZ,ZZZ,ZZZ,ZZZ.
00157  SKIP2
00158      05  HEADING5.
00159          10  FILLER       PIC XX      VALUE SPACES.
00160          10  HD5-CNTR     PIC Z,ZZZ,ZZZ,ZZ9.
00161          10  FILLER       PIC X(3)    VALUE SPACES.
00162          10  HD5-MESSAGE  PIC X(50).
00163  EJECT
00164  PROCEDURE DIVISION.
00165  0000-MAINLINE.
00166      PERFORM 1000-INITIALIZATION.
00167      IF RETURN-CODE NOT EQUAL TO 16
00168         PERFORM 2000-PROCESS
00169                 UNTIL DISC-ANX-EOF OR RETURN-CODE = 16
00170      END-IF.
00171      PERFORM 9000-FINALIZATION.
00172      STOP RUN.
00173  SKIP2
00174  1000-INITIALIZATION.
00175      OPEN INPUT CLDISANXFILE  CURR-AGCY-MAST
00176                 AGENCY-ASMT-MASTER
00177          OUTPUT PRINT-FILE.
00178 *    ACCEPT ACPT-DATE FROM DATE.
00179      MOVE FUNCTION CURRENT-DATE(1:8) TO ACPT-DATE.
00180      STRING ACPT-DATE(5:2) '/' ACPT-DATE(7:2) '/'
00181             ACPT-DATE(1:4) DELIMITED BY SIZE INTO HD1-DATE.
00182  SKIP2
00183      OPEN I-O FRZAGCY-FILE.
00184      IF NOT GOOD-STATUS
00185         MOVE 16 TO RETURN-CODE
00186         DISPLAY 'INVALID OPEN ON FROZEN AGENCY FILE   '
00187         DISPLAY 'FILE STATUS = ' FRZN-STATUS
00188      END-IF.
00189  EJECT
00190  2000-PROCESS.
00191      PERFORM 2100-READ-DISC-ANX
00192          IF NOT DISC-ANX-EOF
00193              IF DA-SEGCTR > 0
00194                  MOVE ZERO TO END-OLD-MASTER-SW
00195                  IF NOT OLDMASTER-EOF
00196                      PERFORM 3100-PROCESS-OLD-MASTER
00197                          UNTIL END-OLD-MASTER OR OLDMASTER-EOF
00198                  END-IF
00199                  MOVE 0 TO END-NEW-MASTER-SW
00200                  IF NOT NEWMASTER-EOF
00201                      PERFORM 3300-PROCESS-NEW-MASTER
00202                          UNTIL END-NEW-MASTER OR NEWMASTER-EOF
00203                  END-IF.
00204  SKIP2
00205  2100-READ-DISC-ANX.
00206      INITIALIZE  DISC-TABLE  ANNEX-TABLE.
00207      READ CLDISANXFILE AT END MOVE 'Y' TO DISC-ANX-SWITCH.
00208      IF NOT DISC-ANX-EOF
00209         ADD 1 TO ANNEX-READ
00210         IF DA-SEGCTR > 0
00211            SET DISC-INDEX TO 1
00212            SET ANNEX-INDEX TO 1
00213            MOVE DA-SEGCTR TO WS-SEGCTR
00214            MOVE 1 TO DA-SEGCTR
00215            PERFORM 2150-MOVE-ROUTINE UNTIL WS-SEGCTR = 0
00216         END-IF
00217      END-IF.
00218  SKIP2
00219  2150-MOVE-ROUTINE.
00220      EVALUATE DA-SEG-TYP(DA-SEGCTR)
00221      WHEN 'A'
00222         MOVE DA-SEG-AGCY(DA-SEGCTR) TO ANNEX-AGENCY(ANNEX-INDEX)
00223         SET ANNEX-INDEX UP BY 1
00224      WHEN 'D'
00225         MOVE DA-SEG-AGCY(DA-SEGCTR) TO DISC-AGENCY(DISC-INDEX)
00226         SET DISC-INDEX UP BY 1
00227      END-EVALUATE.
00228      SUBTRACT 1 FROM WS-SEGCTR.
00229      ADD 1 TO DA-SEGCTR.
00230  EJECT
00231  3100-PROCESS-OLD-MASTER.
00232      IF WS-AM-DIVNO = DA-DIVNO
00233 *       MOVE 1 TO END-OLD-MASTER-SW
00234         PERFORM 3200-PROCESS-OLD-AGENCIES
00235            VARYING SUB1 FROM 1 BY 1
00236            UNTIL SUB1 GREATER 40 OR AM-AGENCY (SUB1) = ZERO
00237            PERFORM 4100-READ-OLD-AGCY-MAST
00238 *       MOVE 0 TO END-NEW-MASTER-SW
00239 *       PERFORM 3300-PROCESS-NEW-MASTER UNTIL END-NEW-MASTER
00240      ELSE
00241         IF WS-AM-DIVNO > DA-DIVNO
00242            MOVE 1 TO END-OLD-MASTER-SW
00243 *          PERFORM 3300-PROCESS-NEW-MASTER
00244         ELSE
00245            PERFORM 4100-READ-OLD-AGCY-MAST
00246         END-IF
00247      END-IF.
00248  EJECT
00249  3200-PROCESS-OLD-AGENCIES.
00250      MOVE ZERO TO SEARCH-SWITCH.
00251      PERFORM 3250-OLD-ROUTINE
00252         VARYING DISC-INDEX FROM 1 BY 1
00253         UNTIL DISC-INDEX GREATER 40 OR SEARCH-END
00254            OR DISC-AGENCY (DISC-INDEX) = 0.
00255  SKIP2
00256  3250-OLD-ROUTINE.
00257      IF AM-AGENCY (SUB1) = DISC-AGENCY (DISC-INDEX)
00258         MOVE 1 TO SEARCH-SWITCH
00259         COMPUTE WS-DISC-VALUE ROUNDED =
00260             (AM-EQUL-VAL * DA-PCTCHG) / 100
00261         MOVE AM-TXCD TO FA-TXCD
00262         MOVE AM-AGENCY(SUB1) TO FA-AGCY
00263         MOVE 'A' TO PRINT-SWITCH
00264         PERFORM 4300-READ-FROZEN-FILE
00265      END-IF.
00266  EJECT
00267  3300-PROCESS-NEW-MASTER.
00268      IF WS-CA-DIVNO = DA-DIVNO
00269 *       MOVE 1 TO END-NEW-MASTER-SW
00270         PERFORM 3400-PROCESS-NEW-AGENCIES
00271            VARYING SUB1 FROM 1 BY 1
00272            UNTIL SUB1 GREATER 40 OR CA-AGENCY (SUB1) = ZERO
00273            PERFORM 4200-READ-NEW-AGCY-MAST
00274      ELSE
00275         IF WS-CA-DIVNO > DA-DIVNO
00276            MOVE 1 TO END-NEW-MASTER-SW
00277         ELSE
00278            PERFORM 4200-READ-NEW-AGCY-MAST
00279         END-IF
00280      END-IF.
00281  EJECT
00282  3400-PROCESS-NEW-AGENCIES.
00283      MOVE ZERO TO SEARCH-SWITCH.
00284      PERFORM 3450-NEW-ROUTINE
00285         VARYING ANNEX-INDEX FROM 1 BY 1
00286         UNTIL ANNEX-INDEX GREATER 40 OR SEARCH-END
00287            OR ANNEX-AGENCY (ANNEX-INDEX) = 0.
00288  SKIP2
00289  3450-NEW-ROUTINE.
00290      IF CA-AGENCY (SUB1) = ANNEX-AGENCY (ANNEX-INDEX)
00291         MOVE 1 TO SEARCH-SWITCH
00292         COMPUTE WS-DISC-VALUE ROUNDED =
00293             (CA-EQUL-VAL * DA-PCTCHG) / 100
00294         MOVE CA-TXCD TO FA-TXCD
00295         MOVE CA-AGENCY(SUB1) TO FA-AGCY
00296         MOVE 'D' TO PRINT-SWITCH
00297         PERFORM 4300-READ-FROZEN-FILE
00298      END-IF.
00299  EJECT
00300  4100-READ-OLD-AGCY-MAST.
00301      READ AGENCY-ASMT-MASTER AT END
00302         MOVE 1 TO OLD-AGCY-EOF-SW
00303         MOVE 'Y' TO OLDMASTER-EOF-SW
00304         MOVE 99999999999999 TO SAVE-OLD-KEY.
00305      IF NOT OLD-AGCY-EOF
00306         MOVE AM-DIVNO TO WS-AM-DIVNO
00307         ADD 1 TO PRIOR-YEAR-CNTR
00308         IF AM-DIVNO LESS PRIOR-OLD-KEY
00309            DISPLAY 'PRIOR-AGCY-MAST IS OUT OF SEQUENCE'
00310            DISPLAY 'CURRENT KEY IS  : ' AM-DIVNO
00311            DISPLAY 'PREVIOUS KEY IS : ' PRIOR-OLD-KEY
00312            MOVE 16 TO RETURN-CODE
00313         ELSE
00314            MOVE AM-DIVNO TO SAVE-OLD-KEY  PRIOR-OLD-KEY.
00315  SKIP2
00316  4200-READ-NEW-AGCY-MAST.
00317      READ CURR-AGCY-MAST AT END
00318         MOVE 1 TO NEW-AGCY-EOF-SW
00319         MOVE 'Y' TO NEWMASTER-EOF-SW
00320         MOVE 99999999999999 TO SAVE-NEW-KEY.
00321      IF NOT NEW-AGCY-EOF
00322         MOVE CA-DIVNO TO WS-CA-DIVNO
00323         ADD 1 TO CURR-YEAR-CNTR
00324         IF CA-DIVNO LESS PRIOR-NEW-KEY
00325            DISPLAY 'CURR-AGCY-MAST IS OUT OF SEQUENCE'
00326            DISPLAY 'CURRENT KEY IS  : ' CA-DIVNO
00327            DISPLAY 'PREVIOUS KEY IS : ' PRIOR-NEW-KEY
00328            MOVE 16 TO RETURN-CODE
00329         ELSE
00330            MOVE CA-DIVNO TO SAVE-NEW-KEY  PRIOR-NEW-KEY.
00331  EJECT
00332  4300-READ-FROZEN-FILE.
00333      READ FRZAGCY-FILE.
00334      IF REC-FND
00335         IF PRINT-SWITCH = 'A'
00336            COMPUTE FA-DISCONEV = FA-DISCONEV + WS-DISC-VALUE
00337         ELSE
00338            COMPUTE FA-ANNEXEDEV = FA-ANNEXEDEV + WS-DISC-VALUE
00339         END-IF
00340         PERFORM 5000-PRINT-ROUTINE
00341         PERFORM 6100-REWRITE-ROUTINE
00342      ELSE
00343         IF NOT-FND
00344            IF PRINT-SWITCH = 'A'
00345               MOVE WS-DISC-VALUE TO FA-DISCONEV
00346               MOVE ZERO          TO FA-ANNEXEDEV
00347            ELSE
00348               MOVE ZERO          TO FA-DISCONEV
00349               MOVE WS-DISC-VALUE TO FA-ANNEXEDEV
00350            END-IF
00351            PERFORM 5000-PRINT-ROUTINE
00352            PERFORM 6200-WRITE-ROUTINE
00353         ELSE
00354            DISPLAY 'FROZEN FILE MASTER READ ERROR : ' FRZN-STATUS
00355            DISPLAY '    FILE RETURN               : ' VS-RETURN
00356            DISPLAY '    FILE FUNCTION             : ' VS-FUNCTION
00357            DISPLAY '    FILE FEEDBACK             : ' VS-FEEDBACK
00358            MOVE 16 TO RETURN-CODE
00359         END-IF
00360      END-IF.
00361  EJECT
00362  5000-PRINT-ROUTINE.
00363      IF PRINT-ANNEX
00364         MOVE AM-DIVNO        TO HDR-DIV-NO
00365         MOVE AM-PROP         TO HDR-PROP-NO
00366         MOVE AM-TXCD         TO HDR-TX-CODE
00367         MOVE AM-AGENCY(SUB1) TO HDR-AGENCY
00368         MOVE WS-DISC-VALUE   TO HDR-DISC-VAL
00369         MOVE ZERO            TO HDR-ANX-VAL
00370         MOVE AM-EQUL-VAL     TO HDR-TOT-VAL
00371      ELSE
00372         MOVE CA-DIVNO        TO HDR-DIV-NO
00373         MOVE CA-PROP         TO HDR-PROP-NO
00374         MOVE CA-TXCD         TO HDR-TX-CODE
00375         MOVE CA-AGENCY(SUB1) TO HDR-AGENCY
00376         MOVE WS-DISC-VALUE   TO HDR-ANX-VAL
00377         MOVE ZERO            TO HDR-DISC-VAL
00378         MOVE CA-EQUL-VAL     TO HDR-TOT-VAL
00379      END-IF.
00380      MOVE DA-PCTCHG TO HDR-PCT-CHG.
00381      INSPECT HDR-PROP-NO REPLACING ALL ',' BY '-'.
00382      INSPECT HDR-AGENCY REPLACING ALL ',' BY '-'.
00383      IF LINE-CNTR >= 55
00384         PERFORM 5100-HEADER-ROUTINE
00385      END-IF.
00386      WRITE PRINT-REC FROM HEADING4 AFTER ADVANCING 1 LINE.
00387      ADD 1 TO LINE-CNTR.
00388  SKIP2
00389  5100-HEADER-ROUTINE.
00390      ADD +1 TO PAGE-CNTR.
00391      MOVE PAGE-CNTR TO HD1-PAGE.
00392      WRITE PRINT-REC FROM HEADING1 AFTER ADVANCING PAGE.
00393      WRITE PRINT-REC FROM HEADING2 AFTER ADVANCING 2.
00394      WRITE PRINT-REC FROM HEADING3 AFTER ADVANCING 3.
00395      MOVE 3 TO LINE-CNTR.
00396  EJECT
00397  6100-REWRITE-ROUTINE.
00398      REWRITE FRZAGCY-REC.
00399      IF GOOD-STATUS
00400         ADD 1 TO ANNEX-UPDATED
00401      ELSE
00402         DISPLAY 'FROZEN FILE MASTER REWRITE ERROR : ' FRZN-STATUS
00403         DISPLAY '       FILE RETURN               : ' VS-RETURN
00404         DISPLAY '       FILE FUNCTION             : ' VS-FUNCTION
00405         DISPLAY '       FILE FEEDBACK             : ' VS-FEEDBACK
00406         MOVE 16 TO RETURN-CODE
00407      END-IF.
00408  EJECT
00409  6200-WRITE-ROUTINE.
00410      MOVE ZERO TO FA-TXRTE
00411                   FA-CURR-288
00412                   FA-EXPR-288
00413                   FA-FRST-TIME
00414                   FA-TOT-FROZ
00415                   FA-FROZ-EQLZD
00416                   FA-FROZ-TXAMT
00417                   FA-ANNEXEDAV
00418                   FA-DISCONAV
00419                   FA-TIFAV
00420                   FA-TIFEV
00421                   FA-CURREV
00422                   FA-EXPINC
00423                   FA-EXPINCEV.
00424      WRITE FRZAGCY-REC.
00425      IF GOOD-STATUS
00426         ADD 1 TO ANNEX-WRITTEN
00427      ELSE
00428         DISPLAY 'FROZEN FILE MASTER REWRITE ERROR : ' FRZN-STATUS
00429         DISPLAY '       FILE RETURN               : ' VS-RETURN
00430         DISPLAY '       FILE FUNCTION             : ' VS-FUNCTION
00431         DISPLAY '       FILE FEEDBACK             : ' VS-FEEDBACK
00432         MOVE 16 TO RETURN-CODE
00433      END-IF.
00434  EJECT
00435  9000-FINALIZATION.
00436      MOVE 'TOTAL PRIOR YEAR RECORDS READ       ' TO HD5-MESSAGE.
00437      MOVE  PRIOR-YEAR-CNTR TO HD5-CNTR.
00438      WRITE PRINT-REC FROM HEADING5 AFTER ADVANCING 1 LINE.
00439  SKIP2
00440      MOVE 'TOTAL CURRENT YR RECORDS READ       ' TO HD5-MESSAGE.
00441      MOVE  CURR-YEAR-CNTR TO HD5-CNTR.
00442      WRITE PRINT-REC FROM HEADING5 AFTER ADVANCING 1 LINE.
00443  SKIP2
00444      MOVE 'TOTAL ANNEX/DISCONNECT RECORDS READ ' TO HD5-MESSAGE.
00445      MOVE  ANNEX-READ TO HD5-CNTR.
00446      WRITE PRINT-REC FROM HEADING5 AFTER ADVANCING 1 LINE.
00447  SKIP2
00448      MOVE 'TOTAL ANNEX/DISCONNECT AGENCY RECORDS UPDATED '
00449            TO HD5-MESSAGE.
00450      MOVE  ANNEX-UPDATED TO HD5-CNTR.
00451      WRITE PRINT-REC FROM HEADING5 AFTER ADVANCING 1 LINE.
00452  SKIP2
00453      MOVE 'TOTAL ANNEX/DISCONNECT AGENCY RECORDS WRITTEN '
00454            TO HD5-MESSAGE.
00455      MOVE  ANNEX-WRITTEN TO HD5-CNTR.
00456      WRITE PRINT-REC FROM HEADING5 AFTER ADVANCING 1 LINE.
00457  SKIP2
00458      CLOSE CLDISANXFILE  CURR-AGCY-MAST.
00459      CLOSE AGENCY-ASMT-MASTER  PRINT-FILE.
00460      CLOSE FRZAGCY-FILE.
00461      IF NOT GOOD-STATUS
00462         MOVE 16 TO RETURN-CODE
00463         DISPLAY 'INVALID CLOSE ON FROZEN AGENCY FILE   '
00464         DISPLAY 'FILE STATUS = ' FRZN-STATUS
00465      END-IF.
