00001  IDENTIFICATION DIVISION.
00002  PROGRAM-ID.    ASHMA921.
00003 *****************************************************************
00004 * AUTHOR.                                                       *
00005 *       ANALYST    - RICHARD KALEMBA                            *
00006 *       PROGRAMMER - THOMAS D. BROWN                            *
00007 *                                                               *
00008 * DATE-WRITTEN.                                                 *
00009 *       MARCH 1996.                                             *
00010 *                                                               *
00011 * REMARKS.                                                      *
00012 *       THIS PROGRAM IS TO CREATE AN OUTPUT FILE OF DATA FROM   *
00013 *       THE MASTER AND D1 SEGMENTS ON THE ASSESSORS DATABASE.   *
00014 *****************************************************************
00015  SKIP2
00016  ENVIRONMENT DIVISION.
00017  INPUT-OUTPUT SECTION.
00018  FILE-CONTROL.
00019      SELECT EXEMPT-DATA-FILE ASSIGN TO UT-S-OUTFILE.
00020  EJECT
00021  DATA DIVISION.
00022  FILE SECTION.
00023  COPY ASSFEXMTF1.
00024  EJECT
00025  WORKING-STORAGE SECTION.
00026  77  WS-SEGMENTS-CNTR       PIC 9(7)     VALUE 0 PACKED-DECIMAL.
00027  77  WS-MASTER-CNTR         PIC 9(7)     VALUE 0 PACKED-DECIMAL.
00028  77  WS-DETAIL-CNTR         PIC 9(7)     VALUE 0 PACKED-DECIMAL.
00029  77  WS-OUTPUT-CNTR         PIC 9(7)     VALUE 0 PACKED-DECIMAL.
00030  77  EOF-SW                 PIC X        VALUE 'N'.
00031      88 END-OF-DATABASE                  VALUE 'Y'.
00032      88 TERMI                            VALUE 'T'.
00033
00034  01  CONTROL-CARD.
00035      05  CONTROL-YEAR-C     PIC X(4).
00036      05  CONTROL-YEAR-N     PIC 9(4).
00037      05  FILLER             PIC X(72).
00038
00039  01  IO-AREA.
00040  COPY PIROOTSEGM.
00041  COPY PIASSESSMT.
00042  COPY PILOCATSEG.
00043  COPY ASAIS145SG.
00044  COPY ASAIS150SG.
00045  COPY ASAIS155SG.
00046  COPY IMSCALLS.
00047
00048  COPY ASAISSSA29.
00049
00050 *****************************************************************
00051  LINKAGE SECTION.
00052  COPY IOPCBDESC.
00053
00054  01  PI-PCB1.
00055  COPY PIPCB1DESC.
00056          07  PI-KEY-FB-ITEM          PIC X(22).
00057      EJECT
00058  PROCEDURE DIVISION.
00059  0000-MAINLINE.
00060  SKIP2
00061      ENTRY 'DLITCBL' USING IO-PCB  PI-PCB1.
00062  SKIP2
00063      PERFORM 1000-INITIALIZATION.
00064      PERFORM 2000-PROCESS-DATABASE UNTIL RETURN-CODE = 16
00065              OR END-OF-DATABASE OR TERMI.
00066      PERFORM 9000-FINALIZATION.
00067      STOP RUN.
00068  EJECT
00069  1000-INITIALIZATION.
00070      OPEN OUTPUT EXEMPT-DATA-FILE.
00071      ACCEPT CONTROL-CARD.
00072      IF CONTROL-YEAR-N NOT NUMERIC OR CONTROL-YEAR-N <= 1993
00073         OR CONTROL-YEAR-C NOT EQUAL TO 'YEAR'
00074         DISPLAY 'SELECTION YEAR INVALID ON PARM CARD'
00075         DISPLAY 'PARM CARD = ' CONTROL-CARD
00076         MOVE 16 TO RETURN-CODE
00077      END-IF.
00078
00079  2000-PROCESS-DATABASE.
00080      PERFORM 2100-GN-ROOT.
00081      IF PI-DB-GOOD-STATUS
00082         PERFORM 3100-GNP-ASSESSMENT
00083         IF PI-DB-GOOD-STATUS
00084            PERFORM 4100-GNP-SENIOR-FREEZE
00085            IF PI-DB-GOOD-STATUS
00086               PERFORM 5100-GNP-SENIOR-DETAIL
00087                       UNTIL NOT PI-DB-GOOD-STATUS
00088            END-IF
00089         END-IF
00090      END-IF.
00091
00092  2100-GN-ROOT.
00093      CALL 'CBLTDLI' USING GN, PI-PCB1, P-ROOT, LVL1-UNQUAL-SSA.
00094      EVALUATE TRUE
00095      WHEN PI-DB-GOOD-STATUS
00096           ADD +1 TO WS-SEGMENTS-CNTR
00097           MOVE P-PROP TO LVL1-ARG
00098      WHEN PI-DB-END
00099           MOVE 'Y' TO EOF-SW
00100      WHEN PI-SEG-NOTFND
00101           DISPLAY 'DATA BASE DOES NOT CONTAIN A ROOT SEGMENT'
00102           PERFORM 9900-DATABASE-ERROR
00103      WHEN OTHER
00104           DISPLAY 'ERROR IN MODULE 2100-GN-ROOT'
00105           PERFORM 9900-DATABASE-ERROR
00106      END-EVALUATE.
00107
00108  3100-GNP-ASSESSMENT.
00109      MOVE CONTROL-YEAR-N(3:2) TO LVL2-PROCYR.
00110      MOVE CONTROL-YEAR-N(3:2) TO LVL2-TXYR.
00111      MOVE ZERO                TO LVL2-TXTYP.
00112      CALL 'CBLTDLI' USING GNP, PI-PCB1, PY-ASMTDATA,
00113                           LVL1-QUAL-SSA, LVL2-QUAL-SSA.
00114      EVALUATE TRUE
00115         WHEN PI-DB-GOOD-STATUS
00116              CONTINUE
00117         WHEN PI-SEG-NOTFND
00118              CONTINUE
00119         WHEN PI-DB-END
00120              MOVE 'Y' TO EOF-SW
00121         WHEN OTHER
00122              DISPLAY 'ERROR IN MODULE 3100-GNP-ASSESSMENT'
00123              PERFORM 9900-DATABASE-ERROR
00124      END-EVALUATE.
00125
00126  4100-GNP-SENIOR-FREEZE.
00127      CALL 'CBLTDLI' USING GNP, PI-PCB1, C145-SENFRZMASTER,
00128                           LVL1-QUAL-SSA, LVL2-QUAL-SSA,
00129                           LVL3-UNQUAL-SSA.
00130      EVALUATE TRUE
00131         WHEN PI-DB-GOOD-STATUS
00132              ADD +1 TO WS-MASTER-CNTR
00133         WHEN PI-SEG-NOTFND
00134              CONTINUE
00135         WHEN PI-DB-END
00136              MOVE 'Y' TO EOF-SW
00137         WHEN OTHER
00138              DISPLAY 'ERROR IN MODULE 4100-GNP-SENIOR-FREEZE'
00139              PERFORM 9900-DATABASE-ERROR
00140      END-EVALUATE.
00141
00142  5100-GNP-SENIOR-DETAIL.
00143      MOVE C145-RECCODE TO LVL3-ARG.
00144      CALL 'CBLTDLI' USING GNP, PI-PCB1, C150-SENFRZDETAIL,
00145                           LVL1-QUAL-SSA, LVL2-QUAL-SSA,
00146                           LVL3-QUAL-SSA, LVL4-UNQUAL-SSA.
00147      EVALUATE TRUE
00148         WHEN PI-DB-GOOD-STATUS
00149              ADD +1 TO WS-DETAIL-CNTR
00150              PERFORM 6100-SNR-FRZE-TO-OUTPUT
00151              PERFORM 6200-SNR-DETAIL-TO-OUTPUT
00152         WHEN PI-SEG-NOTFND
00153              CONTINUE
00154         WHEN PI-DB-END
00155              MOVE 'Y' TO EOF-SW
00156         WHEN OTHER
00157              DISPLAY 'ERROR IN MODULE 5100-GNP-SENIOR-DETAIL'
00158              PERFORM 9900-DATABASE-ERROR
00159      END-EVALUATE.
00160
00161  6100-SNR-FRZE-TO-OUTPUT.
00162      MOVE P-TOWN            TO EX-TOWN-NO.
00163      MOVE P-VOL             TO EX-VOLUME.
00164      MOVE P-PROP            TO EX-PROP-NO.
00165
00166      MOVE PY-YEAR           TO EX-YEAR.
00167      MOVE PY-TXYR           TO EX-TAX-YEAR.
00168      MOVE PY-TXTYPE         TO EX-TAX-TYPE.
00169
00170      MOVE C145-SENFRZMASTER TO EX-SENIOR-FREEZE.
00171      MOVE C145-CALC-TYP     TO EX-CALC-TYP.
00172
00173  6200-SNR-DETAIL-TO-OUTPUT.
00174      MOVE C150-BIRTHDTE    TO EX-BIRTHDATE.
00175      MOVE C150-APPOLDNAM   TO EX-APPOLDNAM.
00176      MOVE C150-SFSTAT      TO EX-SFSTAT.
00177      MOVE C150-FSTAPPDTE   TO EX-FSTAPPDTE.
00178      MOVE C150-LSTAPPDTE   TO EX-LSTAPPDTE.
00179      MOVE C150-QUALDTE     TO EX-QUALDTE.
00180      MOVE C150-RTNDDTE     TO EX-RTNDDTE.
00181      MOVE C150-MANRTNCODES TO EX-MANRTNCODES.
00182      MOVE C150-MISRTNCODES TO EX-MISRTNCODES.
00183      MOVE C150-COOPSENSHRS TO EX-COOPSENSHRS.
00184      MOVE C150-PCTSENSHRS  TO EX-PCTSENSHRS.
00185      MOVE C150-BASE-YR     TO EX-BASE-YR.
00186      MOVE C150-BSYR-IND    TO EX-BSYR-IND.
00187      MOVE C150-BSYR-EQVAL  TO EX-BSYR-EQVAL.
00188      MOVE C150-HSSTAT      TO EX-HSSTAT.
00189      MOVE C150-SHARES      TO EX-SHARES.
00190      MOVE C150-PCTSHARES   TO EX-PCTSHARES.
00191      MOVE C150-HOSTAT      TO EX-HOSTAT.
00192      MOVE C150-HOBASYR     TO EX-HOBASYR.
00193      MOVE C150-HOBYEQFAC   TO EX-HOBYEQFAC.
00194      MOVE C150-HOBYAV      TO EX-HOBYAV.
00195      MOVE C150-HOBYEV      TO EX-HOBYEV.
00196      MOVE C150-ELGIND      TO EX-ELGIND.
00197      MOVE C150-MAINTIND    TO EX-MAINTIND.
00198      MOVE C150-MTTERMID    TO EX-MTTERMID.
00199      MOVE C150-MTUPID      TO EX-MUTPID.
00200      MOVE C150-SFPCT       TO EX-SFPCT.
00201      WRITE EXEMPT-DATA-REC.
00202      ADD +1 TO WS-OUTPUT-CNTR.
00203
00204  9000-FINALIZATION.
00205      DISPLAY 'TOTAL SEGMENTS READ.............' WS-SEGMENTS-CNTR.
00206      DISPLAY 'TOTAL MASTER SEGMENTS WRITTEN...' WS-MASTER-CNTR.
00207      DISPLAY 'TOTAL DETAIL SEGMENTS WRITTEN...' WS-DETAIL-CNTR.
00208      DISPLAY 'TOTAL OUTPUT RECORDS WRITTEN....' WS-OUTPUT-CNTR.
00209      CLOSE EXEMPT-DATA-FILE.
00210
00211  9900-DATABASE-ERROR.
00212      MOVE 'T' TO EOF-SW.
00213      MOVE 16  TO RETURN-CODE.
00214      DISPLAY 'DBD NAME................' PI-DBD-NAME.
00215      DISPLAY 'SEGMENT LEVEL...........' PI-SEGMENT-LVL.
00216      DISPLAY 'STATUS CODE.............' PI-STATUS-CODE.
00217      DISPLAY 'PROCESSING OPTIONS......' PI-PROC-OPT.
00218      DISPLAY 'LEVEL1 ARGUEMENT........' P-PROP.
00219      DISPLAY 'KEY LENGTH..............' PI-KEY-FDBK-LNG.
00220      DISPLAY 'SENSITIVE SEGMENTS......' PI-SEN-SEG.
00221      DISPLAY 'KEY FEEDBACK AREA IS....' PI-KEY-FDBK.
