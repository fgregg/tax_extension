00001  IDENTIFICATION DIVISION.
00002  PROGRAM-ID. ASREA745.
00003 *AUTHOR. RUGIS - MOLIS.
00004 *DATE-WRITTEN. AUGUST 14, 1992.
00005 *REMARKS. THE PURPOSE OF THIS PROGRAM IS TO UPDATE THE
00006 *         FROZEN VALUATIONS TO THE FROZEN VALUE FILE.
00007  SKIP2
00008  ENVIRONMENT DIVISION.
00009  CONFIGURATION SECTION.
00010  SOURCE-COMPUTER. IBM-370.
00011  OBJECT-COMPUTER. IBM-370.
00012  SKIP1
00013  INPUT-OUTPUT SECTION.
00014  FILE-CONTROL.
00015      SELECT FRZDIV-PRCNT-FILE ASSIGN TO UT-S-FRZINFL.
00016      SELECT FRZVALFL ASSIGN TO DA-FRZVALFL
00017         ORGANIZATION IS INDEXED
00018         ACCESS  MODE IS RANDOM
00019         RECORD   KEY IS FV-KEY
00020         FILE  STATUS IS FILE-STATUS  FILE-STATUS2.
00021      SELECT FRZDIV-PRCNT-OUT-FILE ASSIGN TO UT-S-FRZOUTFL.
00022  SKIP1
00023  DATA DIVISION.
00024  FILE SECTION.
00025  SKIP1
00026  COPY CLFZDVPCF1.
00027  SKIP1
00028  COPY ASFRZVALF1.
00029  SKIP1
00030  COPY CLFZDVPCF2.
00031  SKIP3
00032  WORKING-STORAGE SECTION.
00033      SKIP1
00034  77  FRZ-READ-CNTR      PIC S9(7)     VALUE +0  PACKED-DECIMAL.
00035      88  FIRST-REC      VALUE +1.
00036  77  FRZ-WRITTEN-CNTR   PIC S9(7)     VALUE +0  PACKED-DECIMAL.
00037  77  FRZ-UNMTCHD-CNTR   PIC S9(7)     VALUE +0  PACKED-DECIMAL.
00038  77  DIV-CUR-288        PIC S9(13)    VALUE +0  PACKED-DECIMAL.
00039  77  DIV-1ST-TIME       PIC S9(13)    VALUE +0  PACKED-DECIMAL.
00040  77  TXCD-CUR-288       PIC S9(13)    VALUE +0  PACKED-DECIMAL.
00041  77  TXCD-1ST-TIME      PIC S9(13)    VALUE +0  PACKED-DECIMAL.
00042  77  TXCD-CNT           PIC S9(13)    VALUE +0  PACKED-DECIMAL.
00043  77  SUB                PIC S9(4)     VALUE +0  BINARY.
00044      SKIP1
00045  77  FRZ-EOF-SW         PIC X         VALUE 'N'.
00046      88  FRZ-EOF                      VALUE 'Y'.
00047  77  ERR-SW             PIC X         VALUE 'N'.
00048      88  ERR                          VALUE 'Y'.
00049      SKIP1
00050  77  FILE-STATUS        PIC 99.
00051      88  GOOD-STATUS                  VALUE 00.
00052      88  REC-NOTFND                   VALUE 23.
00053      SKIP3
00054  01  WORK-AREA.
00055      SKIP1
00056      05  FILE-STATUS2                 BINARY.
00057          10  FS-RETURN    PIC 99      VALUE 0.
00058          10  FS-FUNCTION  PIC 9       VALUE 0.
00059          10  FS-FEEDBACK  PIC 999     VALUE 0.
00060      SKIP1
00061      05  CURR-FRZ-KEY.
00062          10  CF-DIVNO    PIC 9(15).
00063          10  CF-TXCD     PIC 9(5).
00064      SKIP1
00065      05  PREV-FRZ-KEY    PIC X(20)    VALUE LOW-VALUES.
00066      EJECT
00067  PROCEDURE DIVISION.
00068      SKIP1
00069  010-HOUSEKEEPING.
00070      SKIP1
00071      OPEN INPUT FRZVALFL.
00072      IF NOT GOOD-STATUS
00073          DISPLAY 'INVALID OPEN ON FROZEN VALUE DIVISION FILE'
00074          DISPLAY 'FILE STATUS IS ' FILE-STATUS
00075          DISPLAY 'FEEDBACK STATUS IS ' FS-RETURN ' '
00076                    FS-FUNCTION ' ' FS-FEEDBACK
00077          MOVE 16 TO RETURN-CODE
00078          STOP RUN.
00079      SKIP1
00080      OPEN INPUT FRZDIV-PRCNT-FILE
00081          OUTPUT FRZDIV-PRCNT-OUT-FILE.
00082      SKIP3
00083      PERFORM 020-MAINLINE
00084         UNTIL FRZ-EOF OR ERR.
00085      SKIP3
00086      DISPLAY 'TOTAL FROZEN RECORDS READ      ' FRZ-READ-CNTR.
00087      DISPLAY 'TOTAL FROZEN RECORDS WRITTEN   ' FRZ-WRITTEN-CNTR.
00088      DISPLAY 'TOTAL FROZEN RECORDS UNMATCHED ' FRZ-UNMTCHD-CNTR.
00089      SKIP1
00090      CLOSE FRZDIV-PRCNT-FILE FRZDIV-PRCNT-OUT-FILE.
00091      SKIP1
00092      CLOSE FRZVALFL.
00093      IF NOT GOOD-STATUS
00094          DISPLAY 'INVALID CLOSE ON FROZEN VALUE DIVISION FILE'
00095          DISPLAY 'FILE STATUS IS ' FILE-STATUS
00096          DISPLAY 'FEEDBACK STATUS IS ' FS-RETURN ' '
00097                    FS-FUNCTION ' ' FS-FEEDBACK
00098          MOVE 16 TO RETURN-CODE.
00099      STOP RUN.
00100      SKIP3
00101  020-MAINLINE.
00102      PERFORM 030-READ-FRZVAL.
00103      IF NOT (FRZ-EOF OR ERR)
00104          PERFORM 040-ACCESS-DIVISION-FILE
00105          IF GOOD-STATUS
00106              IF FDP-TXCD-COUNT EQUAL 1
00107                  PERFORM 050-TXCD-CNT-EQ1-UPDATE-RTN
00108              ELSE
00109                  PERFORM 060-TXCD-CNT-GR1-UPDATE-RTN.
00110      SKIP3
00111  030-READ-FRZVAL.
00112      READ FRZDIV-PRCNT-FILE
00113        AT END
00114          MOVE 'Y' TO FRZ-EOF-SW.
00115      IF NOT FRZ-EOF
00116          ADD +1 TO FRZ-READ-CNTR
00117          MOVE FDP-DIVNO TO CF-DIVNO
00118          MOVE FDP-TXCD  TO CF-TXCD
00119          IF CURR-FRZ-KEY LESS PREV-FRZ-KEY
00120              DISPLAY 'FROZEN DIVISION FILE OUT OF SEQUENCE'
00121              DISPLAY 'CURRENT  KEY IS ' CURR-FRZ-KEY
00122              DISPLAY 'PREVIOUS KEY IS ' PREV-FRZ-KEY
00123              MOVE 16  TO RETURN-CODE
00124              MOVE 'Y' TO ERR-SW
00125          ELSE
00126              MOVE CURR-FRZ-KEY TO PREV-FRZ-KEY.
00127      SKIP3
00128  040-ACCESS-DIVISION-FILE.
00129      MOVE CF-DIVNO TO FV-DIVNO.
00130      READ FRZVALFL.
00131      IF NOT GOOD-STATUS AND NOT REC-NOTFND
00132          DISPLAY 'INVALID READ ON FROZEN VALUE DIVISION FILE'
00133          DISPLAY 'FILE STATUS IS ' FILE-STATUS
00134          DISPLAY 'FEEDBACK STATUS IS ' FS-RETURN ' '
00135                    FS-FUNCTION ' ' FS-FEEDBACK
00136          MOVE 16 TO RETURN-CODE
00137          MOVE 'Y' TO ERR-SW
00138      ELSE
00139          IF REC-NOTFND
00140              ADD +1 TO FRZ-UNMTCHD-CNTR.
00141 *            DISPLAY 'NO MATCHING DIVISION RECORD'
00142 *            DISPLAY 'FROZEN VALUE DIVNO IS ' CF-DIVNO
00143 *            DISPLAY 'FROZEN VALUE TXCD  IS ' CF-TXCD.
00144      SKIP3
00145  050-TXCD-CNT-EQ1-UPDATE-RTN.
00146      MOVE SPACES TO FRZDIV-PRCNT-OUT-REC.
00147      MOVE FDP-DIVNO            TO FDPO-DIVNO.
00148      MOVE FDP-TXCD             TO FDPO-TXCD.
00149      MOVE FDP-CURR-288-PCNT    TO FDPO-CURR-288-PCNT.
00150      MOVE FDP-FRST-PCNT        TO FDPO-FRST-PCNT.
00151      MOVE FDP-TXCD-COUNT       TO FDPO-TXCD-COUNT.
00152      MOVE FV-PROP-IMP-VAL      TO FDPO-FRST-VAL.
00153      MOVE FV-PROP-CURR-288-VAL TO FDPO-CURR-288-VAL.
00154      MOVE FV-PROP-EXPR-288-VAL TO FDPO-EXPR-288-VAL.
00155      MOVE ZEROES               TO FDPO-EXP-INC.
00156      WRITE FRZDIV-PRCNT-OUT-REC.
00157      ADD +1 TO FRZ-WRITTEN-CNTR.
00158      SKIP3
00159  060-TXCD-CNT-GR1-UPDATE-RTN.
00160      MOVE FRZDIV-PRCNT-REC TO FRZDIV-PRCNT-OUT-REC.
00161      IF FDP-FRST-PCNT GREATER ZERO
00162          COMPUTE FDPO-FRST-VAL = FDP-FRST-PCNT * FV-PROP-IMP-VAL
00163      ELSE
00164 *        IF FV-PROP-IMP-VAL GREATER ZERO
00165 *            COMPUTE FDPO-FRST-VAL =
00166 *                   FV-PROP-IMP-VAL / FDP-TXCD-COUNT
00167 *        ELSE
00168              MOVE ZEROES TO FDPO-FRST-VAL.
00169      SKIP1
00170      IF FDP-CURR-288-PCNT GREATER ZERO
00171          COMPUTE FDPO-CURR-288-VAL =
00172                 FDP-CURR-288-PCNT * FV-PROP-CURR-288-VAL
00173      ELSE
00174 *        IF FV-PROP-IMP-VAL GREATER ZERO
00175 *            COMPUTE FDPO-CURR-288-VAL =
00176 *                   FV-PROP-IMP-VAL / FDP-TXCD-COUNT
00177 *        ELSE
00178              MOVE ZEROES TO FDPO-CURR-288-VAL.
00179      SKIP1
00180      IF FV-PROP-EXPR-288-VAL EQUAL ZERO
00181          MOVE ZEROES TO FDPO-EXPR-288-VAL
00182      ELSE
00183          COMPUTE FDPO-EXPR-288-VAL =
00184                 FV-PROP-EXPR-288-VAL / FDP-TXCD-COUNT.
00185      SKIP1
00186      MOVE ZEROES TO FDPO-EXP-INC.
00187      SKIP1
00188      WRITE FRZDIV-PRCNT-OUT-REC.
00189      ADD +1 TO FRZ-WRITTEN-CNTR.
