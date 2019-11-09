00001  IDENTIFICATION DIVISION.
00002  PROGRAM-ID.    ASHMA921.
00003 *****************************************************************
00004 * AUTHOR.                                                       *
00005 *       ANALYST    - KATHY LEE.                                 *
00006 *       PROGRAMMER - TOM MCMAHON.                               *
00007 *                                                               *
00008 * DATE-WRITTEN.                                                 *
00009 *       JUNE, 2000.                                             *
00010 *                                                               *
00011 * REMARKS.                                                      *
00012 *       THIS PROGRAM CREATES AN OUTPUT FILE OF DATA FROM THE    *
00013 *       MASTER AND D1 SEGMENTS ON THE ASSESSORS DATABASE WHEN   *
00014 *       'C150-SF-STAT' IS EQUAL TO 'Q'.  IT IS A COPY OF PROGRAM*
00015 *       ASHMA921, WITH MINOR CHANGES.                           *
00016 *****************************************************************
00017  SKIP2
00018  ENVIRONMENT DIVISION.
00019  INPUT-OUTPUT SECTION.
00020  FILE-CONTROL.
00021      SELECT EXEMPT-DATA-FILE ASSIGN TO UT-S-OUTFILE.
00022  EJECT
00023  DATA DIVISION.
00024  FILE SECTION.
00025  COPY ASSFEXMTF1.
00026  EJECT
00027  WORKING-STORAGE SECTION.
00028  77  WS-SEGMENTS-CNTR       PIC 9(7)     VALUE 0 PACKED-DECIMAL.
00029  77  WS-MASTER-CNTR         PIC 9(7)     VALUE 0 PACKED-DECIMAL.
00030  77  WS-DETAIL-CNTR         PIC 9(7)     VALUE 0 PACKED-DECIMAL.
00031  77  WS-OUTPUT-CNTR         PIC 9(7)     VALUE 0 PACKED-DECIMAL.
00032  77  EOF-SW                 PIC X        VALUE 'N'.
00033      88 END-OF-DATABASE                  VALUE 'Y'.
00034      88 TERMI                            VALUE 'T'.
00035
00036  01  CONTROL-CARD.
00037      05  CONTROL-YEAR-C     PIC X(4).
00038      05  CONTROL-YEAR-N     PIC 9(4).
00039      05  FILLER             PIC X(72).
00040
00041  01  IO-AREA.
00042  COPY PIROOTSEGM.
00043  COPY PIASSESSMT.
00044  COPY PILOCATSEG.
00045  COPY ASAIS145SG.
00046  COPY ASAIS150SG.
00047  COPY ASAIS155SG.
00048  COPY IMSCALLS.
00049
00050  COPY ASAISSSA29.
00051
00052 *****************************************************************
00053  LINKAGE SECTION.
00054  COPY IOPCBDESC.
00055
00056  01  PI-PCB1.
00057  COPY PIPCB1DESC.
00058          07  PI-KEY-FB-ITEM          PIC X(22).
00059      EJECT
00060  PROCEDURE DIVISION.
00061  0000-MAINLINE.
00062  SKIP2
00063      ENTRY 'DLITCBL' USING IO-PCB  PI-PCB1.
00064  SKIP2
00065      PERFORM 1000-INITIALIZATION.
00066      PERFORM 2000-PROCESS-DATABASE UNTIL RETURN-CODE = 16
00067              OR END-OF-DATABASE OR TERMI.
00068      PERFORM 9000-FINALIZATION.
00069      STOP RUN.
00070  EJECT
00071  1000-INITIALIZATION.
00072      OPEN OUTPUT EXEMPT-DATA-FILE.
00073      ACCEPT CONTROL-CARD.
00074      IF CONTROL-YEAR-N NOT NUMERIC OR CONTROL-YEAR-N <= 1993
00075         OR CONTROL-YEAR-C NOT EQUAL TO 'YEAR'
00076         DISPLAY 'SELECTION YEAR INVALID ON PARM CARD'
00077         DISPLAY 'PARM CARD = ' CONTROL-CARD
00078         MOVE 16 TO RETURN-CODE
00079      END-IF.
00080
00081  2000-PROCESS-DATABASE.
00082      PERFORM 2100-GN-ROOT.
00083      IF PI-DB-GOOD-STATUS
00084         PERFORM 3100-GNP-ASSESSMENT
00085         IF PI-DB-GOOD-STATUS
00086            PERFORM 4100-GNP-SENIOR-FREEZE
00087            IF PI-DB-GOOD-STATUS
00088               PERFORM 5100-GNP-SENIOR-DETAIL
00089                       UNTIL NOT PI-DB-GOOD-STATUS
00090            END-IF
00091         END-IF
00092      END-IF.
00093
00094  2100-GN-ROOT.
00095      CALL 'CBLTDLI' USING GN, PI-PCB1, P-ROOT, LVL1-UNQUAL-SSA.
00096      EVALUATE TRUE
00097      WHEN PI-DB-GOOD-STATUS
00098           ADD +1 TO WS-SEGMENTS-CNTR
00099           MOVE P-PROP TO LVL1-ARG
00100      WHEN PI-DB-END
00101           MOVE 'Y' TO EOF-SW
00102      WHEN PI-SEG-NOTFND
00103           DISPLAY 'DATA BASE DOES NOT CONTAIN A ROOT SEGMENT'
00104           PERFORM 9900-DATABASE-ERROR
00105      WHEN OTHER
00106           DISPLAY 'ERROR IN MODULE 2100-GN-ROOT'
00107           PERFORM 9900-DATABASE-ERROR
00108      END-EVALUATE.
00109
00110  3100-GNP-ASSESSMENT.
00111      MOVE CONTROL-YEAR-N(3:2) TO LVL2-PROCYR.
00112      MOVE CONTROL-YEAR-N(3:2) TO LVL2-TXYR.
00113      MOVE ZERO                TO LVL2-TXTYP.
00114      CALL 'CBLTDLI' USING GNP, PI-PCB1, PY-ASMTDATA,
00115                           LVL1-QUAL-SSA, LVL2-QUAL-SSA.
00116      EVALUATE TRUE
00117         WHEN PI-DB-GOOD-STATUS
00118              CONTINUE
00119         WHEN PI-SEG-NOTFND
00120              CONTINUE
00121         WHEN PI-DB-END
00122              MOVE 'Y' TO EOF-SW
00123         WHEN OTHER
00124              DISPLAY 'ERROR IN MODULE 3100-GNP-ASSESSMENT'
00125              PERFORM 9900-DATABASE-ERROR
00126      END-EVALUATE.
00127
00128  4100-GNP-SENIOR-FREEZE.
00129      CALL 'CBLTDLI' USING GNP, PI-PCB1, C145-SENFRZMASTER,
00130                           LVL1-QUAL-SSA, LVL2-QUAL-SSA,
00131                           LVL3-UNQUAL-SSA.
00132      EVALUATE TRUE
00133         WHEN PI-DB-GOOD-STATUS
00134              ADD +1 TO WS-MASTER-CNTR
00135         WHEN PI-SEG-NOTFND
00136              CONTINUE
00137         WHEN PI-DB-END
00138              MOVE 'Y' TO EOF-SW
00139         WHEN OTHER
00140              DISPLAY 'ERROR IN MODULE 4100-GNP-SENIOR-FREEZE'
00141              PERFORM 9900-DATABASE-ERROR
00142      END-EVALUATE.
00143
00144  5100-GNP-SENIOR-DETAIL.
00145      MOVE C145-RECCODE TO LVL3-ARG.
00146      CALL 'CBLTDLI' USING GNP, PI-PCB1, C150-SENFRZDETAIL,
00147                           LVL1-QUAL-SSA, LVL2-QUAL-SSA,
00148                           LVL3-QUAL-SSA, LVL4-UNQUAL-SSA.
00149      EVALUATE TRUE
00150         WHEN PI-DB-GOOD-STATUS
00151              ADD +1 TO WS-DETAIL-CNTR
00152              PERFORM 6100-SNR-FRZE-TO-OUTPUT
00153 *            PERFORM 6200-SNR-DETAIL-TO-OUTPUT
00154         WHEN PI-SEG-NOTFND
00155              CONTINUE
00156         WHEN PI-DB-END
00157              MOVE 'Y' TO EOF-SW
00158         WHEN OTHER
00159              DISPLAY 'ERROR IN MODULE 5100-GNP-SENIOR-DETAIL'
00160              PERFORM 9900-DATABASE-ERROR
00161      END-EVALUATE.
00162
00163  6100-SNR-FRZE-TO-OUTPUT.
00164      IF C150-SFSTAT = 'Q'
00165         MOVE P-TOWN            TO EX-TOWN-NO
00166         MOVE P-VOL             TO EX-VOLUME
00167         MOVE P-PROP            TO EX-PROP-NO
00168         MOVE PY-YEAR           TO EX-YEAR
00169         MOVE PY-TXYR           TO EX-TAX-YEAR
00170         MOVE PY-TXTYPE         TO EX-TAX-TYPE
00171         MOVE C145-SENFRZMASTER TO EX-SENIOR-FREEZE
00172         MOVE C145-CALC-TYP     TO EX-CALC-TYP
00173         PERFORM 6200-SNR-DETAIL-TO-OUTPUT.
00174
00175  6200-SNR-DETAIL-TO-OUTPUT.
00176      MOVE C150-BIRTHDTE    TO EX-BIRTHDATE.
00177      MOVE C150-APPOLDNAM   TO EX-APPOLDNAM.
00178      MOVE C150-SFSTAT      TO EX-SFSTAT.
00179      MOVE C150-FSTAPPDTE   TO EX-FSTAPPDTE.
00180      MOVE C150-LSTAPPDTE   TO EX-LSTAPPDTE.
00181      MOVE C150-QUALDTE     TO EX-QUALDTE.
00182      MOVE C150-RTNDDTE     TO EX-RTNDDTE.
00183      MOVE C150-MANRTNCODES TO EX-MANRTNCODES.
00184      MOVE C150-MISRTNCODES TO EX-MISRTNCODES.
00185      MOVE C150-COOPSENSHRS TO EX-COOPSENSHRS.
00186      MOVE C150-PCTSENSHRS  TO EX-PCTSENSHRS.
00187      MOVE C150-BASE-YR     TO EX-BASE-YR.
00188      MOVE C150-BSYR-IND    TO EX-BSYR-IND.
00189      MOVE C150-BSYR-EQVAL  TO EX-BSYR-EQVAL.
00190      MOVE C150-HSSTAT      TO EX-HSSTAT.
00191      MOVE C150-SHARES      TO EX-SHARES.
00192      MOVE C150-PCTSHARES   TO EX-PCTSHARES.
00193      MOVE C150-HOSTAT      TO EX-HOSTAT.
00194      MOVE C150-HOBASYR     TO EX-HOBASYR.
00195      MOVE C150-HOBYEQFAC   TO EX-HOBYEQFAC.
00196      MOVE C150-HOBYAV      TO EX-HOBYAV.
00197      MOVE C150-HOBYEV      TO EX-HOBYEV.
00198      MOVE C150-ELGIND      TO EX-ELGIND.
00199      MOVE C150-MAINTIND    TO EX-MAINTIND.
00200      MOVE C150-MTTERMID    TO EX-MTTERMID.
00201      MOVE C150-MTUPID      TO EX-MUTPID.
00202      WRITE EXEMPT-DATA-REC.
00203      ADD +1 TO WS-OUTPUT-CNTR.
00204
00205  9000-FINALIZATION.
00206      DISPLAY 'TOTAL SEGMENTS READ.............' WS-SEGMENTS-CNTR.
00207      DISPLAY 'TOTAL MASTER SEGMENTS WRITTEN...' WS-MASTER-CNTR.
00208      DISPLAY 'TOTAL DETAIL SEGMENTS WRITTEN...' WS-DETAIL-CNTR.
00209      DISPLAY 'TOTAL OUTPUT RECORDS WRITTEN....' WS-OUTPUT-CNTR.
00210      CLOSE EXEMPT-DATA-FILE.
00211
00212  9900-DATABASE-ERROR.
00213      MOVE 'T' TO EOF-SW.
00214      MOVE 16  TO RETURN-CODE.
00215      DISPLAY 'DBD NAME................' PI-DBD-NAME.
00216      DISPLAY 'SEGMENT LEVEL...........' PI-SEGMENT-LVL.
00217      DISPLAY 'STATUS CODE.............' PI-STATUS-CODE.
00218      DISPLAY 'PROCESSING OPTIONS......' PI-PROC-OPT.
00219      DISPLAY 'LEVEL1 ARGUEMENT........' P-PROP.
00220      DISPLAY 'KEY LENGTH..............' PI-KEY-FDBK-LNG.
00221      DISPLAY 'SENSITIVE SEGMENTS......' PI-SEN-SEG.
00222      DISPLAY 'KEY FEEDBACK AREA IS....' PI-KEY-FDBK.