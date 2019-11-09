00001  IDENTIFICATION DIVISION.
00002  PROGRAM-ID. ASHMA857.
00003 *AUTHOR. RUBINGER/KALEMBA.
00004 *DATE-WRITTEN. 06/2000.
00005 *REMARKS. ASHMA857 WILL COMPUTE THE SENIOR FREEZE BASE VALUE TAX
00006 *         DIFFERENCE. THIS IS A COPY OF ASHMA850, WILL NOT UPDATE
00007 *         THE SENIOR FREEZE SEGMENT OF THE PIF DATABASE BUT WILL
00008 *         INSTEAD CREATE A TEMPORARY SEQUENTIAL FILE FOR FURTHER
00009 *         PROCESSING.
00010 *         CHANGED 07/2000 R.CRUZ/K.LEE.
00011 * CHANGED 05/11/2006 BY TJM TO ADD A COUNTER, TESTS AND DISPLAYS
00012 *                        FOR THE YEAR 2005.
00013 * CHANGED 07/24/2007 BY KLF TO ADD A COUNTER, TESTS AND DISPLAYS
00014 *                        FOR THE YEAR 2006.
00015 * CHANGED 07/18/2007 BY KLF TO ADD A COUNTER, TESTS AND DISPLAYS
00016 *                        FOR THE YEAR 2007.
00017 * CHANGED 07/17/2009 BY TJM TO ADD A COUNTER, TESTS AND DISPLAYS
00018 *                        FOR THE YEAR 2008.
00019 * CHANGED 09/17/2010 BY TJM TO ADD A COUNTER, TESTS AND DISPLAYS
00020 *                        FOR THE YEAR 2009.
00021 * CHANGED 07/18/2011 BY TJM TO ADD A COUNTER, TESTS AND DISPLAYS
00022 *                        FOR THE YEAR 2010.
00023 * CHANGED 04/17/2012 BY KLF TO ADD A COUNTER, TESTS AND DISPLAYS
00024 *                        FOR THE YEAR 2011.
00025 * CHANGED 04/17/2013 BY KLF TO ADD A COUNTER, TESTS AND DISPLAYS
00026 *                        FOR THE YEAR 2012.
00027 * CHANGED 04/17/2014 BY JTS TO ADD A COUNTER, TESTS AND DISPLAYS
00028 *                        FOR THE YEAR 2013.
00029 * CHANGED 04/06/2015 BY TMB TO ADD A COUNTER, TESTS AND DISPLAYS
00030 *                        FOR THE YEAR 2014.
00031 * CHANGED 04/07/2016 BY KLF TO ADD A COUNTER, TESTS AND DISPLAYS
00032 *                        FOR THE YEAR 2015.
00033 * CHANGED 04/07/2017 BY TMB TO ADD A COUNTER, TESTS AND DISPLAYS
00034 *                        FOR THE YEAR 2016.
00035 * CHANGED 04/08/2018 BY TMB TO ADD A COUNTER, TESTS AND DISPLAYS
00036 *                        FOR THE YEAR 2017.
00037 * CHANGED 04/24/2019 BY GKW TO ADD A COUNTER, TESTS AND DISPLAYS
00038 *                        FOR THE YEAR 2018.
00039
00040  ENVIRONMENT DIVISION.
00041  CONFIGURATION SECTION.
00042  SOURCE-COMPUTER. IBM-370.
00043  OBJECT-COMPUTER. IBM-370.
00044  INPUT-OUTPUT SECTION.
00045  FILE-CONTROL.
00046      SELECT PARAM-LINE      ASSIGN TO UT-S-PARAM.
00047      SELECT CURR-ASSMT      ASSIGN TO UT-S-CURRASS.
00048      SELECT PRIOR-ASSMT     ASSIGN TO UT-S-PRIORASS.
00049      SELECT EXPIRED-ASSMT   ASSIGN TO UT-S-EXPIRASS.
00050      SELECT EQUALFCT        ASSIGN TO DA-EQUALFCT
00051                             RECORD KEY IS EQ-KEY
00052                             ORGANIZATION IS INDEXED
00053                             ACCESS IS RANDOM
00054                             FILE STATUS IS FILE-STATUS,
00055                                            FILE-STATUSA.
00056      SELECT SNRFREZ-FILE    ASSIGN TO UT-S-SNRFREZ.
00057      SELECT PRINT-FILE-1    ASSIGN TO UT-S-PRINT1.
00058      SELECT PRINT-FILE-2    ASSIGN TO UT-S-PRINT2.
00059      SELECT PRINT-FILE-3    ASSIGN TO UT-S-PRINT3.
00060      SELECT PRINT-FILE-4    ASSIGN TO UT-S-PRINT4.
00061
00062  DATA DIVISION.
00063  FILE SECTION.
00064
00065  FD  PARAM-LINE
00066      RECORDING MODE IS F
00067      BLOCK CONTAINS 0 RECORDS
00068      RECORD CONTAINS 80 CHARACTERS
00069      LABEL RECORDS ARE STANDARD
00070      DATA RECORD IS PARAM-REC.
00071  01  PARAM-REC.
00072      03 PR-CICV             PIC X(4).
00073      03 PR-CICV-VAL         PIC X(4).
00074      03 PR-CICV-VAL-N       REDEFINES PR-CICV-VAL  PIC 9(4).
00075      03 PR-PROC-YR.
00076         05  PR-PROC-N    PIC 9(4).
00077      03 PR-EQ-FCTR          PIC X(5).
00078      03 PR-EQ-FCTR-N        REDEFINES PR-EQ-FCTR   PIC 9V9(4).
00079      03 PR-FR-YR            PIC XX.
00080      03 PR-FR-YR-N          REDEFINES PR-FR-YR     PIC 99.
00081      03 PR-TO-YR            PIC XX.
00082      03 PR-TO-YR-N          REDEFINES PR-TO-YR     PIC 99.
00083      03 PR-FR-BSYR          PIC X(4).
00084      03 PR-FR-BSYR-N        REDEFINES PR-FR-BSYR   PIC 9(4).
00085      03 PR-TO-BSYR          PIC X(4).
00086      03 PR-TO-BSYR-N        REDEFINES PR-TO-BSYR   PIC 9(4).
00087      03 PR-CURR-YR.
00088         05  PR-CURR-N    PIC 9(4).
00089      03 FILLER              PIC X(47).
00090
00091  FD  CURR-ASSMT
00092      BLOCK CONTAINS 0 CHARACTERS
00093      RECORD CONTAINS 122 TO 18706 CHARACTERS
00094      LABEL RECORDS ARE STANDARD
00095      RECORDING MODE IS S
00096      DATA RECORD IS CURR-ASSMT-REC.
00097  01  CURR-ASSMT-REC.
00098  COPY ASREASRD02.
00099
00100  FD  PRIOR-ASSMT
00101      BLOCK CONTAINS 0 CHARACTERS
00102      RECORD CONTAINS 122 TO 18706 CHARACTERS
00103      LABEL RECORDS ARE STANDARD
00104      RECORDING MODE IS S
00105      DATA RECORD IS PRIOR-ASSMT-REC.
00106  01  PRIOR-ASSMT-REC.
00107  COPY ASREASRD03.
00108
00109  FD  EXPIRED-ASSMT
00110      BLOCK CONTAINS 0 CHARACTERS
00111      RECORD CONTAINS 122 TO 18706 CHARACTERS
00112      LABEL RECORDS ARE STANDARD
00113      RECORDING MODE IS S
00114      DATA RECORD IS EXPIRED-ASSMT-REC.
00115  01  EXPIRED-ASSMT-REC.
00116  COPY ASREASRD09.
00117
00118  FD  EQUALFCT
00119      RECORD CONTAINS 21 CHARACTERS
00120 *    BLOCK CONTAINS 0 RECORDS
00121 *    LABEL RECORDS ARE STANDARD
00122      DATA RECORD IS EQUAL-REC.
00123  01  EQUAL-REC.
00124  COPY REBEQFRD01.
00125
00126  FD  SNRFREZ-FILE
00127      RECORDING MODE IS F
00128      BLOCK CONTAINS 0 RECORDS
00129      RECORD CONTAINS 400 CHARACTERS
00130      LABEL RECORDS ARE STANDARD
00131      DATA RECORD IS SNRFREZ-REC.
00132  01  SNRFREZ-REC.
00133  COPY ASHMASFR01.
00134
00135  FD  PRINT-FILE-1
00136      RECORDING MODE IS F
00137      RECORD CONTAINS 133 CHARACTERS
00138      BLOCK CONTAINS 0 RECORDS
00139      LABEL RECORDS ARE STANDARD
00140      DATA RECORD IS PRINT-REC-1.
00141  01  PRINT-REC-1            PIC X(133).
00142
00143  FD  PRINT-FILE-2
00144      RECORDING MODE IS F
00145      RECORD CONTAINS 133 CHARACTERS
00146      BLOCK CONTAINS 0 RECORDS
00147      LABEL RECORDS ARE STANDARD
00148      DATA RECORD IS PRINT-REC-2.
00149  01  PRINT-REC-2            PIC X(133).
00150
00151  FD  PRINT-FILE-3
00152      RECORDING MODE IS F
00153      RECORD CONTAINS 133 CHARACTERS
00154      BLOCK CONTAINS 0 RECORDS
00155      LABEL RECORDS ARE STANDARD
00156      DATA RECORD IS PRINT-REC-3.
00157  01  PRINT-REC-3            PIC X(133).
00158
00159  FD  PRINT-FILE-4
00160      RECORDING MODE IS F
00161      RECORD CONTAINS 133 CHARACTERS
00162      BLOCK CONTAINS 0 RECORDS
00163      LABEL RECORDS ARE STANDARD
00164      DATA RECORD IS PRINT-REC-4.
00165  01  PRINT-REC-4            PIC X(133).
00166
00167  WORKING-STORAGE SECTION.
00168  77  N-YR-CLS               PIC 999.
00169  77  CURR-ASSMT-REC-CTR     PIC S9(8) VALUE +0     BINARY.
00170  77  PRIOR-ASSMT-REC-CTR    PIC S9(8) VALUE +0     BINARY.
00171  77  WS-236-VALUE           PIC S9(8) VALUE +0.
00172  77  WS-IMPRV-VALUE         PIC S9(8) VALUE +0.
00173  77  WS-TYPE1-VALUE         PIC S9(8) VALUE +0.
00174  77  EXPIRED-ASSMT-REC-CTR  PIC S9(8) VALUE +0     BINARY.
00175  77  EXPIRED-ASSMT-SEL-CTR  PIC S9(8) VALUE +0     BINARY.
00176  77  CURR-PRIOR-MATCH-CTR   PIC S9(8) VALUE +0     BINARY.
00177  77  CURR-UNMATCH-CTR       PIC S9(8) VALUE +0     BINARY.
00178  77  CURR-NO-CALC-CTR       PIC S9(8) VALUE +0     BINARY.
00179  77  EXEMP-MAST-REPL-CTR    PIC S9(8) VALUE +0     BINARY.
00180  77  EXEMP-DET-SEG-CTR      PIC S9(8) VALUE +0     BINARY.
00181  77  SNRFREZ-REC-CTR        PIC S9(8) VALUE +0     BINARY.
00182  77  INVALID-CLS-COND-CTR   PIC S9(8) VALUE +0     BINARY.
00183  77  CICV-CTR               PIC S9(8) VALUE +99    BINARY.
00184  77  BASE-YR-1993-CTR       PIC S9(8) VALUE +0     BINARY.
00185  77  BASE-YR-1994-CTR       PIC S9(8) VALUE +0     BINARY.
00186  77  BASE-YR-1995-CTR       PIC S9(8) VALUE +0     BINARY.
00187  77  BASE-YR-1996-CTR       PIC S9(8) VALUE +0     BINARY.
00188  77  BASE-YR-1997-CTR       PIC S9(8) VALUE +0     BINARY.
00189  77  BASE-YR-1998-CTR       PIC S9(8) VALUE +0     BINARY.
00190  77  BASE-YR-1999-CTR       PIC S9(8) VALUE +0     BINARY.
00191  77  BASE-YR-2000-CTR       PIC S9(8) VALUE +0     BINARY.
00192  77  BASE-YR-2001-CTR       PIC S9(8) VALUE +0     BINARY.
00193  77  BASE-YR-2002-CTR       PIC S9(8) VALUE +0     BINARY.
00194  77  BASE-YR-2003-CTR       PIC S9(8) VALUE +0     BINARY.
00195  77  BASE-YR-2004-CTR       PIC S9(8) VALUE +0     BINARY.
00196  77  BASE-YR-2005-CTR       PIC S9(8) VALUE +0     BINARY.
00197  77  BASE-YR-2006-CTR       PIC S9(8) VALUE +0     BINARY.
00198  77  BASE-YR-2007-CTR       PIC S9(8) VALUE +0     BINARY.
00199  77  BASE-YR-2008-CTR       PIC S9(8) VALUE +0     BINARY.
00200  77  BASE-YR-2009-CTR       PIC S9(8) VALUE +0     BINARY.
00201  77  BASE-YR-2010-CTR       PIC S9(8) VALUE +0     BINARY.
00202  77  BASE-YR-2011-CTR       PIC S9(8) VALUE +0     BINARY.
00203  77  BASE-YR-2012-CTR       PIC S9(8) VALUE +0     BINARY.
00204  77  BASE-YR-2013-CTR       PIC S9(8) VALUE +0     BINARY.
00205  77  BASE-YR-2014-CTR       PIC S9(8) VALUE +0     BINARY.
00206  77  BASE-YR-2015-CTR       PIC S9(8) VALUE +0     BINARY.
00207  77  BASE-YR-2016-CTR       PIC S9(8) VALUE +0     BINARY.
00208  77  BASE-YR-2017-CTR       PIC S9(8) VALUE +0     BINARY.
00209  77  BASE-YR-2018-CTR       PIC S9(8) VALUE +0     BINARY.
00210  77  BASE-YR-OTHR-CTR       PIC S9(8) VALUE +0     BINARY.
00211  77  SUB1                   PIC S9(4) VALUE +0     BINARY.
00212  77  SUB2                   PIC S9(4) VALUE +0     BINARY.
00213  77  LN-CTR-1               PIC S9(4) VALUE +99    BINARY.
00214  77  LN-CTR-2               PIC S9(4) VALUE +99    BINARY.
00215  77  LN-CTR-3               PIC S9(4) VALUE +99    BINARY.
00216  77  LN-CTR-4               PIC S9(4) VALUE +99    BINARY.
00217  77  PG-CTR-1               PIC S9(4) VALUE +0     BINARY.
00218  77  PG-CTR-2               PIC S9(4) VALUE +0     BINARY.
00219  77  PG-CTR-3               PIC S9(4) VALUE +0     BINARY.
00220  77  PG-CTR-4               PIC S9(4) VALUE +0     BINARY.
00221  77  PRIOR-RESD-IMPV-CTR    PIC S9(4) VALUE +0     BINARY.
00222  77  CURR-RESD-IMPV-CTR     PIC S9(4) VALUE +0     BINARY.
00223  77  T4-ASSD-VAL            PIC S9(9) VALUE +0     PACKED-DECIMAL
00224  77  T5-ASSD-VAL            PIC S9(9) VALUE +0     PACKED-DECIMAL
00225  77  BASE-VAL-AV            PIC S9(9) VALUE +0     PACKED-DECIMAL
00226  77  BASE-VAL-EAV           PIC S9(9) VALUE +0     PACKED-DECIMAL
00227  77  BASE-ELIG-COMP-AV      PIC S9(9) VALUE +0     PACKED-DECIMAL
00228  77  BASE-ELIG-COMP-EAV     PIC S9(9) VALUE +0     PACKED-DECIMAL
00229  77  288-OVER-30000-AV      PIC S9(9) VALUE +0     PACKED-DECIMAL
00230  77  CURR-VAL-AV            PIC S9(9) VALUE +0     PACKED-DECIMAL
00231  77  CURR-ELIG-COMP-EAV     PIC S9(9) VALUE +0     PACKED-DECIMAL
00232  77  CURR-ELIG-COMP-AV      PIC S9(9) VALUE +0     PACKED-DECIMAL
00233  77  ASSD-VAL               PIC S9(9) VALUE +0     PACKED-DECIMAL
00234  77  288-OVER-30000-EAV     PIC S9(9) VALUE +0     PACKED-DECIMAL
00235  77  288-EXPIRE-AV          PIC S9(9) VALUE +0     PACKED-DECIMAL
00236  77  288-EXPIRE-EAV         PIC S9(9) VALUE +0     PACKED-DECIMAL
00237  77  FINAL-DIFF-TX-COMP     PIC S9(9) VALUE +0     PACKED-DECIMAL
00238  77  TOT-BASE-COMP-EAV      PIC S9(9) VALUE +0     PACKED-DECIMAL
00239  77  HIMP-TOT               PIC S9(9) VALUE +0     PACKED-DECIMAL
00240  77  CURR-BASE-VAL-EAV      PIC S9(9) VALUE +0     PACKED-DECIMAL
00241  77  CURR-NOT-ELIG-AV       PIC S9(9) VALUE +0     PACKED-DECIMAL
00242  77  CURR-NOT-ELIG-EAV      PIC S9(9) VALUE +0     PACKED-DECIMAL
00243  77  PRIOR-IMPV-TOT         PIC S9(9) VALUE +0     PACKED-DECIMAL
00244  77  CURR-IMPV-TOT          PIC S9(9) VALUE +0     PACKED-DECIMAL
00245  77  PRIOR-LAND-TOT         PIC S9(9) VALUE +0     PACKED-DECIMAL
00246  77  CURR-LAND-TOT          PIC S9(9) VALUE +0     PACKED-DECIMAL
00247  77  BASE-DIFF              PIC S9(9) VALUE +0     PACKED-DECIMAL
00248  77  CURR-DIFF              PIC S9(9) VALUE +0     PACKED-DECIMAL
00249  77  CURR-TAX-DIFF          PIC S9(9) VALUE +0     PACKED-DECIMAL
00250  77  PCT-SHARES             PIC V9(6) VALUE 0      PACKED-DECIMAL
00251  77  WS-BASVALYR            PIC 9(4)  VALUE 0      PACKED-DECIMAL
00252  77  WS-BVYTOTELCOMPEV      PIC 9(9)  VALUE 0      PACKED-DECIMAL
00253  77  BLNK                   PIC X     VALUE SPACE.
00254  77  WS-PROC-YR             PIC 9(4).
00255  77  CURR-ASSMT-EOF-SW      PIC 9     VALUE ZEROES.
00256      88 CURR-ASSMT-EOF                VALUE 1.
00257  77  PRIOR-ASSMT-EOF-SW     PIC 9     VALUE ZEROES.
00258      88 PRIOR-ASSMT-EOF               VALUE 1.
00259  77  EXPIRED-ASSMT-EOF-SW   PIC 9     VALUE ZEROES.
00260      88 EXPIRED-ASSMT-EOF             VALUE 1.
00261  77  PRIOR-ASSMT-PRESENT-SW PIC 9     VALUE ZEROES.
00262      88 PRIOR-ASSMT-PRESENT           VALUE 1.
00263  77  EXEMP-DET-EOF-SW       PIC 9     VALUE ZEROES.
00264      88 EXEMP-DET-EOF                 VALUE 1.
00265  77  QUALIFY-SW             PIC 9     VALUE ZEROES.
00266  77  YR-HLD                 PIC 99.
00267  77  FARM-SW                PIC X     VALUE SPACES.
00268  77  NO-CALC-SW             PIC X     VALUE SPACES.
00269  77  MESSG-SW               PIC X     VALUE SPACES.
00270  77  FILE-SW                PIC X     VALUE SPACES.
00271  77  BASE-YR                PIC X(4).
00272  77  BASE-YR-CLS            PIC XXX.
00273  77  CURR-YR                PIC X(4).
00274  77  CURR-YR-CLS            PIC XXX.
00275  77  SAVE-CA-KEY            PIC X(21) VALUE LOW-VALUES.
00276  77  SAVE-PA-KEY            PIC X(21) VALUE LOW-VALUES.
00277  77  SAVE-EX-KEY            PIC X(21) VALUE LOW-VALUES.
00278  77  TXCD-HLD               PIC X(5).
00279  77  HOLD-CALC-TYP          PIC X     VALUE SPACE.
00280  77  CURR-MIXED-CLS-SW      PIC X     VALUE 'N'.
00281      88  CURR-MIXED-CLS               VALUE 'Y'.
00282  77  PRIOR-MIXED-CLS-SW     PIC X     VALUE 'N'.
00283      88  PRIOR-MIXED-CLS              VALUE 'Y'.
00284  77  COOP-SW                PIC X     VALUE 'N'.
00285      88  COOP-YES                     VALUE 'Y'.
00286  77  BYPASS-EQFILE-SW       PIC X     VALUE 'N'.
00287      88  BYPASS-EQFILE                VALUE 'Y'.
00288  77  FILE-STATUS            PIC 99.
00289      88 NORMAL-STATUS                 VALUE ZEROES.
00290      88 RECORD-NOT-FOUND              VALUE 23.
00291  77  CLASS-HLD              PIC 999.
00292      88 RESD-IMPV                     VALUE 202 THRU 213, 218,
00293                                             219, 220, 221, 234,
00294                                             278, 294, 295, 297,
00295                                             299.
00296      88 RESD-LAND                     VALUE 200, 241.
00297      88 FARM-LAND                     VALUE 239.
00298      88 MIXED-USE                     VALUE 236.
00299      88 NON-RESD                      VALUE ZEROES THRU 199,
00300                                             201, 214 THRU 217,
00301                                             222 THRU 233,
00302                                             235, 237, 238, 240,
00303                                             242 THRU 277,
00304                                             279 THRU 287,
00305                                             289 THRU 293, 296,
00306                                             298, 300 THRU 999.
00307      88 HIMP                          VALUE 288.
00308      88 SEG-WHERE-QUEST-FOLLOWS       VALUE 202 THRU 212, 234,
00309                                             278, 295,
00310                                             405 THRU 412, 434,
00311                                             478, 495.
00312      88 MAJOR-CLASS-2                 VALUE 200 THRU 299.
00313  77  WS-CURR-YR-CLS         PIC X(3).
00314      88 VALID-CURR-YR-CLS   VALUE '202' THRU '299' '517' '597'.
00315  77  NULL-CLASS-TAB         PIC X(24) VALUE
00316      '            '.
00317
00318  01  FILLER.
00319      03 CLASS-TABLE.
00320 *       CLASS-TAB POSITIONS CORRESPOND TO THE FOLLOWING:
00321 *       ELEMENT  1:  PRIOR-ASSMT RESD-IMPV
00322 *       ELEMENT  2:  PRIOR-ASSMT RESD-IMPV WITH AN AGE OF 001
00323 *       ELEMENT  3:  PRIOR-ASSMT RESD-LAND
00324 *       ELEMENT  4:  PRIOR-ASSMT FARM-LAND
00325 *       ELEMENT  5:  PRIOR-ASSMT MIXED-USE
00326 *       ELEMENT  6:  PRIOR-ASSMT NON-RESD
00327 *       ELEMENT  7:  CURRENT-ASSMT RESD-IMPV
00328 *       ELEMENT  8:  CURRENT-ASSMT RESD-IMPV WITH AN AGE OF 001
00329 *       ELEMENT  9:  CURRENT-ASSMT RESD-LAND
00330 *       ELEMENT 10:  CURRENT-ASSMT FARM-LAND
00331 *       ELEMENT 11:  CURRENT-ASSMT MIXED-USE
00332 *       ELEMENT 12:  CURRENT-ASSMT NON-RESD
00333         05 CLASS-TAB        OCCURS 12 TIMES        PIC S999
00334                                                    PACKED-DECIMAL
00335      03 FILE-STATUSA.
00336         05 FS-RETURN        PIC S9(4)              BINARY.
00337         05 FS-FUNC          PIC S9(4)              BINARY.
00338         05 FS-FEEDBACK      PIC S9(4)              BINARY.
00339      03 WS-AS3-YEAR         PIC 9(3)  VALUE ZEROS.
00340      03 WS-AS3-YEAR-X REDEFINES WS-AS3-YEAR.
00341         05 FILLER           PIC 9(1).
00342         05 WS-AS3-YR        PIC 9(2).
00343      03 WK-AS3-YEAR         PIC 9(4).
00344      03 WK-AS3-YEAR-X REDEFINES WK-AS3-YEAR.
00345         05 WK-AS3-CC        PIC 9(2).
00346         05 WK-AS3-YR        PIC 9(2).
00347      03 WS-FR-YEAR          PIC 9(4).
00348      03 WS-FR-YEAR-X REDEFINES WS-FR-YEAR.
00349         05 WS-FR-CC         PIC 9(2).
00350         05 WS-FR-YR         PIC 9(2).
00351      03 WS-TO-YEAR          PIC 9(4).
00352      03 WS-TO-YEAR-X REDEFINES WS-TO-YEAR.
00353         05 WS-TO-CC         PIC 9(2).
00354         05 WS-TO-YR         PIC 9(2).
00355
00356      03 CURR-CA-KEY.
00357         05 CCK-TOWN         PIC XX.
00358         05 CCK-VOL          PIC XXX.
00359         05 CCK-PROP         PIC X(15).
00360         05 CCK-TXTYP        PIC X.
00361      03 PREV-CA-KEY.
00362         05 PCK-TOWN         PIC XX    VALUE LOW-VALUES.
00363         05 PCK-VOL          PIC XXX   VALUE LOW-VALUES.
00364         05 PCK-PROP         PIC X(15) VALUE LOW-VALUES.
00365         05 PCK-TXTYP        PIC X     VALUE LOW-VALUES.
00366      03 CURR-PA-KEY.
00367         05 CPK-TOWN         PIC XX.
00368         05 CPK-VOL          PIC XXX.
00369         05 CPK-PROP         PIC X(15).
00370         05 CPK-TXTYP        PIC X.
00371      03 PREV-PA-KEY.
00372         05 PPK-TOWN         PIC XX    VALUE LOW-VALUES.
00373         05 PPK-VOL          PIC XXX   VALUE LOW-VALUES.
00374         05 PPK-PROP         PIC X(15) VALUE LOW-VALUES.
00375         05 PPK-TXTYP        PIC X     VALUE LOW-VALUES.
00376      03 CURR-EX-KEY.
00377         05 CEK-TOWN         PIC XX.
00378         05 CEK-VOL          PIC XXX.
00379         05 CEK-PROP         PIC X(15).
00380         05 CEK-TXTYP        PIC X.
00381      03 PREV-EX-KEY.
00382         05 PEK-TOWN         PIC XX    VALUE LOW-VALUES.
00383         05 PEK-VOL          PIC XXX   VALUE LOW-VALUES.
00384         05 PEK-PROP         PIC X(15) VALUE LOW-VALUES.
00385         05 PEK-TXTYP        PIC X     VALUE LOW-VALUES.
00386      03 MDY-DATE.
00387         05 MO               PIC XX.
00388         05 FILLER           PIC X     VALUE '/'.
00389         05 DA               PIC XX.
00390         05 FILLER           PIC X     VALUE '/'.
00391         05 YR               PIC XX.
00392      03 YMD-DATE.
00393         05 YR               PIC XX.
00394         05 MO               PIC XX.
00395         05 DA               PIC XX.
00396
00397      03 HEAD1.
00398         05 FILLER           PIC XX    VALUE SPACES.
00399         05 H1-CURR-DTE      PIC X(8).
00400         05 FILLER           PIC X(39) VALUE SPACES.
00401         05 FILLER           PIC X(19) VALUE 'OFFICE OF THE COOK '
00402         05 FILLER           PIC X(15) VALUE 'COUNTY ASSESSOR'.
00403         05 FILLER           PIC X(31) VALUE SPACES.
00404         05 FILLER           PIC X(5)  VALUE 'PAGE '.
00405         05 H1-PAGE          PIC ZZZ,ZZ9.
00406
00407      03 HEAD2.
00408         05 FILLER           PIC X(11) VALUE '  ASHMA857-'.
00409         05 H2-REPORT-TYP    PIC X.
00410         05 FILLER           PIC X(20) VALUE SPACES.
00411         05 H2-REPORT-TITLE  PIC X(70).
00412
00413      03 HEAD3.
00414         05 FILLER           PIC X(19) VALUE '  VOL.   PROPERTY I'
00415         05 FILLER           PIC X(19) VALUE 'NDEX NO.   TAX TYPE'
00416         05 FILLER           PIC X(8)  VALUE '   CLASS'.
00417
00418      03 HEAD3-4.
00419         05 FILLER           PIC X(22) VALUE SPACES.
00420         05 FILLER           PIC X(11) VALUE 'CURRENT'.
00421         05 FILLER           PIC X(8)  VALUE 'BASE'.
00422         05 FILLER           PIC X(10) VALUE 'NO CALC'.
00423         05 FILLER           PIC X(13) VALUE 'TOT BASE'.
00424         05 FILLER           PIC X(13) VALUE 'CURR-ELIG'.
00425         05 FILLER           PIC X(12) VALUE 'FINAL DIFF'.
00426         05 FILLER           PIC X(12) VALUE 'BASE VAL'.
00427         05 FILLER           PIC X(12) VALUE 'CURR VAL'.
00428         05 FILLER           PIC X(8)  VALUE 'CURR NOT'.
00429
00430      03 HEAD4.
00431         05 FILLER           PIC X(22) VALUE '  PROPERTY NUMBER'.
00432         05 FILLER           PIC X(11) VALUE 'YR CLASS'.
00433         05 FILLER           PIC X(10) VALUE 'YEAR'.
00434         05 FILLER           PIC X(8)  VALUE 'SW'.
00435         05 FILLER           PIC X(14) VALUE 'COMP EAV'.
00436         05 FILLER           PIC X(14) VALUE 'COMP EAV'.
00437         05 FILLER           PIC X(13) VALUE 'TAX COMP'.
00438         05 FILLER           PIC X(12) VALUE 'AV'.
00439         05 FILLER           PIC X(12) VALUE 'AV'.
00440         05 FILLER           PIC X(8)  VALUE 'ELIG EAV'.
00441
00442      03 DETAIL1.
00443         05 FILLER           PIC XX    VALUE SPACES.
00444         05 DL1-VOL          PIC XXX.
00445         05 FILLER           PIC X(4)  VALUE SPACES.
00446         05 DL1-PROP         PIC 99B99B999B999B9(4).
00447         05 FILLER           PIC X(7)  VALUE SPACES.
00448         05 DL1-TXTYP        PIC X.
00449         05 FILLER           PIC X(6)  VALUE SPACES.
00450         05 DL1-CLS          PIC 9B99.
00451         05 FILLER           PIC X(5)  VALUE SPACES.
00452         05 DL1-MESSG        PIC X(63).
00453
00454      03 DETAIL4               VALUE SPACES.
00455         05 FILLER             PIC XX.
00456         05 DL4-PROP           PIC Z99B99B999B999B9(4).
00457         05 FILLER             PIC X(3).
00458         05 DL4-CURR-YR-CLS    PIC 9B99.
00459         05 FILLER             PIC X(5).
00460         05 DL4-BASE-YR        PIC X(4).
00461         05 FILLER             PIC X(7).
00462         05 DL4-NO-CALC-SW     PIC X.
00463         05 FILLER             PIC X(3).
00464         05 DL4-TOT-BASE-EAV   PIC ZZZ,ZZZ,ZZ9.
00465         05 FILLER             PIC X(3).
00466         05 DL4-CURR-ELIG-EAV  PIC ZZZ,ZZZ,ZZ9.
00467         05 FILLER             PIC X.
00468         05 DL4-FINAL-DIFF-TX  PIC ZZZ,ZZZ,ZZ9.
00469         05 FILLER             PIC X.
00470         05 DL4-BASE-VAL-AV    PIC ZZZ,ZZZ,ZZ9.
00471         05 FILLER             PIC X.
00472         05 DL4-CURR-VAL-AV    PIC ZZZ,ZZZ,ZZ9.
00473         05 FILLER             PIC X.
00474         05 DL4-CURR-NOT-ELIG-EAV  PIC ZZZ,ZZZ,ZZ9.
00475
00476      03 TOTAL1.
00477         05 FILLER           PIC XXX   VALUE SPACES.
00478         05 T1-TOTAL         PIC Z,ZZZ,ZZ9.
00479         05 FILLER           PIC XX    VALUE SPACES.
00480         05 T1-MESSG         PIC X(54).
00481
00482  01  IO-AREA.
00483  COPY PIASSESSMT.
00484
00485  COPY ASAIS145SG.
00486
00487  COPY ASAIS150SG.
00488
00489  COPY IMSCALLS.
00490
00491  COPY ASAISSSA29.
00492
00493  01  HLD-ASSMT-SEGS.
00494      05  H-BASE.
00495 *                                              1-122 FIXED SEGMEN
00496          10  H-STAT1        PIC X.
00497              88  HR-NON-ASSESSED      VALUE '0'.
00498              88  HR-ASSESSED          VALUE '1'.
00499 *                                              1     STATUS-1
00500          10  H-VOLPROP.
00501 *                                              2-11  RECORD KEY
00502              15  H-VOL      PIC S9(3)    COMP-3.
00503                  88  HR-RE-VOL        VALUE +001 THRU +601.
00504                  88  HR-RR-VOL        VALUE +605.
00505 *                                              2-3     VOLUME
00506              15  H-PROP     PIC S9(15)   COMP-3.
00507 *                                              4-11    PROPERTY-N
00508          10  H-TXTYP        PIC X.
00509 *                                             12-12  TAX TYPE
00510          10  FILLER         PIC X.
00511 *                                             13-13  FILLER
00512          10  H-TXCD         PIC S9(5)    COMP-3.
00513 *                                             14-16  TAX CODE
00514          10  H-STAT2        PIC X.
00515              88  HR-TAXABLE-PARCEL    VALUE '0'.
00516              88  HR-EXEMPT            VALUE '1'.
00517              88  HR-RAILROAD          VALUE '2'.
00518              88  HR-HOMESTEAD-NON-COOP VALUE '3'.
00519              88  HR-VETERAN           VALUE '4'.
00520              88  HR-HOMESTEAD-COOP    VALUE '5'.
00521 *                                             17     STATUS-2
00522          10  H-CLS          PIC S9(3)    COMP-3.
00523 *                                             18-19  CLASS (9-99)
00524 *                                                    (MAJOR-MINOR
00525          10  H-NBHD         PIC S9(3)    COMP-3.
00526 *                                             20-21  NEIGHBORHD-C
00527          10  H-STRT         PIC S9(5)    COMP-3.
00528 *                                             22-24  STREET CODE
00529          10  H-HSENO        PIC S9(5)    COMP-3.
00530 *                                             25-27  HOUSE-NO
00531          10  H-LNDDIM       PIC S9(7)    COMP-3.
00532 *                                             28-31  LND-DIMENSIO
00533          10  H-LNDCD        PIC X.
00534 *                                             32     LAND CODE
00535          10  H-LNDSQFT      PIC S9(7)    COMP-3.
00536 *                                             33-36  LAND-SQ-FT
00537          10  H-IRREG        PIC X.
00538 *                                             37     IRREGULAR
00539          10  H-STAT3        PIC X.
00540 *                                             38     COMPLAINT ST
00541          10  H-CMPLNTNO     PIC S9(7)    COMP-3.
00542 *                                             39-42  B.A.COMPLAIN
00543          10  H-BA-ACTION.
00544 *                                             43-44  B.A. ACTION
00545              15  H-BA-YR    PIC X.
00546 *                                             43       BA-YEAR
00547              15  H-BA-REV   PIC X.
00548 *                                             44       BA-REVISIO
00549          10  H-ASMT-ACTION OCCURS 4 TIMES.
00550 *                                             45-56  ASMT-ACTIONS
00551 *                                                    (YR-CHNG-REV
00552              15  H-ASMT-YR  PIC X.
00553 *                                             45       AA-YEAR1
00554 *                                             48       AA-YEAR2
00555 *                                             51       AA-YEAR3
00556 *                                             54       AA-YEAR4
00557              15  H-ASMT-CHG PIC X.
00558 *                                             46       AA-CHANGE1
00559 *                                             49       AA-CHANGE2
00560 *                                             52       AA-CHANGE3
00561 *                                             55       AA-CHANGE4
00562              15  H-ASMT-REV PIC X.
00563 *                                             47       AA-REVSION
00564 *                                             50       AA-REVSION
00565 *                                             53       AA-REVSION
00566 *                                             56       AA-REVSION
00567          10  H-VALUE-1  OCCURS 12 TIMES
00568                             PIC S9(9)    COMP-3.
00569 *                                             57-116 VALUE FIELDS
00570 *                                             57-61    PRIOR LAND
00571 *                                             62-66    PRIOR IMPR
00572 *                                             67-71    PRIOR TOTA
00573 *                                             72-76    CURRNT-LAN
00574 *                                             77-81    CURR-IMPRV
00575 *                                             82-86    CURR-TOTAL
00576 *                                             87-91    PROPOSE-LN
00577 *                                             92-96    PROP-IMPRV
00578 *                                             97-101   PROP-TOTAL
00579 *                                            102-106   PRIOR-REG/
00580 *                                                      COMPL DATE
00581 *                                            107-111   CURRENT-RE
00582 *                                            112-116   PRIOR-PROP
00583          10  FILLER         PIC XX.
00584 *                                            117-118 FILLER
00585          10  H-REC-CTRS-1.
00586 *                                            119-122 RECORD CNTRS
00587              15  H-SLS-CTR-1
00588                             PIC S9.
00589                  88  HR-NO-SALES      VALUE +0.
00590                  88  HR-SALES-PRESENT VALUE +1.
00591 *                                            119       SALES-CNTR
00592              15  H-DTL-QST-CTR-1
00593                             PIC S9(3).
00594                  88  HR-NO-DETAIL     VALUE +0.
00595                  88  HR-DETAIL-PRESENT VALUE +1 THRU +350.
00596                  88  HR-MAXIMUM-DETAIL VALUE +350.
00597 *                                            120-122   DETAIL-CNT
00598 *----------------------------------------------------------------
00599 * D E T A I L  S E G M E N T S, IF PRESENT,EACH CONTAINS 53 BYTES
00600 *    DESCRIPTIONS RELATIVE TO 1ST BYTE IN EACH SEGMENT
00601 *
00602      05  H-DTL-QST-1
00603              OCCURS 0 TO 350 TIMES DEPENDING ON H-DTL-QST-CTR-1.
00604          10  H-TYP1.
00605 *                                              1-53  TYPE1-LAND
00606              15  H1-MC      PIC S999     COMP-3.
00607 *                                              1-2   S1-MULTI-COD
00608              15  H1-TYP     PIC X.
00609                  88  H1R-TYPE1        VALUE '1'.
00610 *                                              3     S1-TYPE1
00611              15  H1-CD      PIC X.
00612 *                                              4     S1-CODE 0/1
00613 *                                                    (0=VACANT)
00614 *                                                    (1=IMPROVED)
00615              15  H1-DEC     PIC X.
00616 *                                              5     S1-DECIMAL
00617              15  H1-UM      PIC XX.
00618 *                                              6-7   S1-UNIT-MEAS
00619              15  H1-CLS     PIC S999     COMP-3.
00620 *                                              8-9   S1-CLASS
00621 *                                                    (MAJOR-MINOR
00622              15  H1-EXRR    PIC X.
00623 *                                             10     S1-EX-RR
00624              15  H1-FF      PIC S9(7)    COMP-3.
00625 *                                             11-14  S1-FRONT-FT
00626              15  H1-DPTH    PIC S9(5)    COMP-3.
00627 *                                             15-17  S1-DEPTH
00628              15  H1-UPR     PIC S9(5)V99 COMP-3.
00629 *                                             18-21  S1-UNIT-PRIC
00630              15  H1-DFCTR   PIC S99V999  COMP-3.
00631 *                                             22-24  S1-DPTH-FACT
00632              15  H1-CFCTR   PIC S9V9(4)  COMP-3.
00633 *                                             25-27  S1-CORNR-FCT
00634              15  H1-ECFCTR  PIC SV9(5)   COMP-3.
00635 *                                             28-30  S1-EXTRA
00636 *                                                    CORNER FACTO
00637              15  H1-PCASSD  PIC S99V9(5) COMP-3.
00638 *                                             31-34  S1-% ASSESSE
00639              15  H1-EI      PIC S99V9    COMP-3.
00640                  88  H1R-ECON1        VALUE +22.0.
00641                  88  H1R-ECON2        VALUE +16.0.
00642                  88  H1R-ECON3        VALUE +33.0.
00643                  88  H1R-ECON4        VALUE +30.0.
00644                  88  H1R-ECON5        VALUE +36.0.
00645                  88  H1R-ECON6        VALUE +38.0.
00646                  88  H1R-ECON-IND     VALUE +16.0 +22.0 +30.0
00647                                             +33.0 +36.0 +38.0.
00648 *                                             35-36  S1-ECON-IND
00649              15  H1-VAL     PIC S9(9)    COMP-3.
00650 *                                             37-41  S1-VALUATION
00651              15  FILLER     PIC X(11).
00652 *                                             42-52  FILLER
00653              15  H1-UI      PIC X.
00654 *                                             53     S1-UNIT-IND
00655 *----------------------------------------------------------------
00656          10  H-TYP2 REDEFINES H-TYP1.
00657 *                                              1-53  TYPE2-IMPRVM
00658              15  H2-MC      PIC S999     COMP-3.
00659 *                                              1-2   S2-MULTI-COD
00660              15  H2-TYP     PIC X.
00661                  88  H2R-TYPE2        VALUE '2'.
00662                  88  H2R-TYPE2-5      VALUE '2' THRU '5'.
00663 *                                              3     S2-TYPE2
00664              15  H2-CD      PIC X.
00665 *                                              4     S2-CODE 2/3
00666 *                                                    2=MAJOR-IMPR
00667 *                                                    3=MINOR-IMPR
00668              15  H2-DEC     PIC X.
00669 *                                              5     S2-DECIMAL
00670              15  H2-UM      PIC XX.
00671 *                                              6-7   S2-UNIT-MEAS
00672              15  H2-CLS     PIC S999     COMP-3.
00673                  88  H2R-RES-CLASS    VALUE +202 THRU +212
00674                                             +234 +278 +295.
00675                  88  H2R-QUES-CLASS   VALUE +202 THRU +212
00676                                             +234 +278 +295
00677                                             +402 THRU +412
00678                                             +434 +478 +495.
00679                  88  H2R-CLS-288      VALUE +288.
00680                  88  H2R-QCLS1-3      VALUE +210 +211 +212 +295
00681                                             +410 +411 +412 +495.
00682                  88  H2R-QCLS1OR5     VALUE +202 +203 +204
00683                                             +402 +403 +404.
00684                  88  H2R-QCLS2        VALUE +205 THRU +208 +278
00685                                             +405 THRU +408 +478.
00686                  88  H2R-QCLS2-3      VALUE +209 +409.
00687                  88  H2R-QCLS4        VALUE +234 +434.
00688 *                                              8-9   S2-CLASS
00689 *                                                    (MAJOR-MINOR
00690              15  H2-CDU     PIC XX.
00691 *                                             10-11  S2-CDU
00692              15  H2-AREA    PIC S9(7)    COMP-3.
00693 *                                             12-15  S2-AREA
00694              15  H2-UPR     PIC S9(5)V99 COMP-3.
00695 *                                             16-19  S2-UNIT-PRIC
00696              15  H2-PRDCT   PIC S9(9)    COMP-3.
00697 *                                             20-24  S2-PRODUCT
00698              15  H2-AGE     PIC S999     COMP-3.
00699                  88  H2-0-AGE         VALUE +0.
00700                  88  H2-OVER-80       VALUE +081 THRU +999.
00701 *                                             25-26  S2-AGE
00702              15  H2-COND    PIC S99V9    COMP-3.
00703 *                                             27-28  S2-CONDITION
00704              15  H2-PCASSD  PIC S99V9(5) COMP-3.
00705 *                                             29-32  S2-% ASSESSE
00706              15  H2-BUFF    PIC S999     COMP-3.
00707 *                                             33-34  S2-BUFF-NO
00708              15  FILLER     PIC XX.
00709 *                                             35-36  FILLER
00710              15  H2-VAL     PIC S9(9)    COMP-3.
00711 *                                             37-41  S2-VALUATION
00712              15  H2-KEYPCL  PIC S9(15)   COMP-3.
00713 *                                             42-49  S2-KEY PARCE
00714              15  H2-SC      PIC X.
00715 *                                             50     S2-SPLIT COD
00716              15  FILLER     PIC X(3).
00717 *                                             51-53  FILLER
00718 *----------------------------------------------------------------
00719          10  H-TYP3 REDEFINES H-TYP1.
00720 *                                              1-53  TYPE3-IMPRVM
00721              15  H3-MC      PIC S999     COMP-3.
00722 *                                              1-2   S3-MULTI-COD
00723              15  H3-TYP     PIC X.
00724                  88  H3R-TYPE3        VALUE '3'.
00725 *                                              3     S3-TYPE3
00726              15  H3-CD      PIC X.
00727 *                                              4     S3-CODE 2/3
00728 *                                                    2=MAJOR-IMPR
00729 *                                                    3=MINOR-IMPR
00730              15  FILLER     PIC X(3).
00731 *                                              5-7   FILLER
00732              15  H3-CLS     PIC S999     COMP-3.
00733 *                                              8-9   S3-CLASS
00734 *                                                    (MAJOR-MINOR
00735              15  H3-CDU     PIC XX.
00736 *                                             10-11  S3-CDU
00737              15  H3-REPCST  PIC S9(9)    COMP-3.
00738 *                                             12-16  S3-REPRODUCT
00739 *                                                    COST
00740              15  FILLER     PIC X(6).
00741 *                                             17-22  FILLER
00742              15  H3-YR      PIC S999     COMP-3.
00743 *                                             23-24  S3-YEAR
00744              15  H3-AGE     PIC S999     COMP-3.
00745                  88  H3-0-AGE         VALUE +0.
00746                  88  H3-OVER-80       VALUE +081 THRU +999.
00747 *                                             25-26  S3-AGE
00748              15  H3-COND    PIC S99V9    COMP-3.
00749 *                                             27-28  S3-CONDITION
00750              15  H3-PCASSD  PIC S99V9(5) COMP-3.
00751 *                                             29-32  S3-% ASSESSE
00752              15  H3-BUFF    PIC S999     COMP-3.
00753 *                                             33-34  S3-BUFF-NO
00754              15  FILLER     PIC XX.
00755 *                                             35-36  FILLER
00756              15  H3-VAL     PIC S9(9)    COMP-3.
00757 *                                             37-41  S3-VALUATION
00758              15  H3-KEYPCL  PIC S9(15)   COMP-3.
00759 *                                             42-49  S3-KEY PARCE
00760              15  H3-SC      PIC X.
00761 *                                             50     S3-SPLIT COD
00762              15  FILLER     PIC X(3).
00763 *                                             51-53  FILLER
00764 *----------------------------------------------------------------
00765          10  H-TYP4 REDEFINES H-TYP1.
00766 *                                              1-53  TYPE4-IMPRVM
00767              15  H4-MC      PIC S999     COMP-3.
00768 *                                              1-2   S4-MULTI-COD
00769              15  H4-TYP     PIC X.
00770                  88  H4R-TYPE4        VALUE '4'.
00771 *                                              3     S4-TYPE4
00772              15  H4-CD      PIC X.
00773 *                                              4     S4-CODE4
00774              15  FILLER     PIC X(3).
00775 *                                              5-7   FILLER
00776              15  H4-CLS     PIC S999     COMP-3.
00777 *                                              8-9   S4-CLASS
00778 *                                                    (MAJOR-MINOR
00779              15  H4-CDU     PIC XX.
00780 *                                             10-11  S4-CDU
00781              15  H4-REPCST  PIC S9(9)    COMP-3.
00782 *                                             12-16  S4-REPRODUCT
00783 *                                                    COST
00784              15  H4-TOTVAL  PIC S9(9)    COMP-3.
00785 *                                             17-21  S4-TOTAL-VAL
00786              15  FILLER     PIC X.
00787 *                                             22     FILLER
00788              15  H4-OCCFAC  PIC S99V9    COMP-3.
00789 *                                             23-24  S4-OCCUPANCY
00790 *                                                    FACTOR
00791              15  H4-AGE     PIC S999     COMP-3.
00792                  88  H4-0-AGE         VALUE +0.
00793                  88  H4-OVER-AGE      VALUE +081 THRU +999.
00794 *                                             25-26  S4-AGE
00795              15  H4-COND    PIC S99V9    COMP-3.
00796 *                                             27-28  S4-CONDITION
00797              15  H4-PCASSD  PIC S99V9(5) COMP-3.
00798 *                                             29-32  S4-% ASSESSE
00799              15  H4-BUFF    PIC S999     COMP-3.
00800 *                                             33-34  S4-BUFF-NO
00801              15  FILLER     PIC XX.
00802 *                                             35-36  FILLER
00803              15  H4-VAL     PIC S9(9)    COMP-3.
00804 *                                             37-41  S4-VALUATION
00805              15  H4-KEYPCL  PIC S9(15)   COMP-3.
00806 *                                             42-49  S4-KEY PARCE
00807              15  H4-SC      PIC X.
00808 *                                             50     S4-SPLIT COD
00809              15  FILLER     PIC X(3).
00810 *                                             51-53  FILLER
00811 *----------------------------------------------------------------
00812 *                                              1-53  TYPE5-IMPRVM
00813          10  H-TYP5 REDEFINES H-TYP1.
00814              15  H5-MC      PIC S999     COMP-3.
00815 *                                              1-2   S5-MULTI-COD
00816              15  H5-TYP     PIC X.
00817                  88  H5R-TYPE5        VALUE '5'.
00818 *                                              3     S5-TYPE5
00819              15  H5-CD      PIC X.
00820 *                                              4     S5-CODE5
00821              15  FILLER     PIC X(3).
00822 *                                              5-7   FILLER
00823              15  H5-CLS     PIC S999     COMP-3.
00824 *                                              8-9   S5-CLASS
00825 *                                                    (MAJOR-MINOR
00826              15  H5-CDU     PIC XX.
00827 *                                             10-11  S5-CDU
00828              15  H5-REPCST  PIC S9(9)    COMP-3.
00829 *                                             12-16  S5-REPRODUCT
00830 *                                                    COST
00831              15  FILLER     PIC X(6).
00832 *                                             17-22  FILLER
00833              15  H5-OCCFAC  PIC S99V9    COMP-3.
00834 *                                             23-24  S5-OCCUPANCY
00835 *                                                    FACTOR
00836              15  H5-AGE     PIC S999     COMP-3.
00837                  88  H5-0-AGE         VALUE +0.
00838                  88  H5-OVER-80       VALUE +081 THRU +999.
00839 *                                             25-26  S5-AGE
00840              15  H5-COND    PIC S99V9    COMP-3.
00841 *                                             27-28  S5-CONDITION
00842              15  H5-PCASSD  PIC S99V9(5) COMP-3.
00843 *                                             29-32  S5-% ASSESSE
00844              15  H5-BUFF    PIC S999     COMP-3.
00845 *                                             33-34  S5-BUFF-NO
00846              15  FILLER     PIC XX.
00847 *                                             35-36  FILLER
00848              15  H5-VAL     PIC S9(9)    COMP-3.
00849 *                                             37-41  S5-VALUATION
00850              15  H5-KEYPCL  PIC S9(15)   COMP-3.
00851 *                                             42-49  S5-KEY PARCE
00852              15  H5-SC      PIC X.
00853 *                                             50     S5-SPLIT COD
00854              15  FILLER     PIC X(3).
00855 *                                             51-53  FILLER
00856 *----------------------------------------------------------------
00857 * --  Q U E S T I O N N A I R E  S E G M E N T  -- IS PRESENT,
00858 *      IF THE PRECEDING DETAIL SEGMENT IS - TYPE = '2' THRU '5'
00859 *      AND CLASS = 202-212,234,278,295,405-412,434,478,495
00860          10  H-QST  REDEFINES H-TYP1 PIC X(53).
00861 *                                              1-53  QUESTIONNAIR
00862 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
00863      EJECT
00864  LINKAGE SECTION.
00865  COPY IOPCBDESC.
00866
00867  01  PI-PCB1.
00868  COPY PIPCB1DESC.
00869          07  PI-KEY-FB-ITEM PIC X(22).
00870      EJECT
00871  PROCEDURE DIVISION.
00872  010-BEGIN.
00873      ENTRY 'DLITCBL' USING IO-PCB, PI-PCB1
00874      DISPLAY 'PROGRAM ASHMA857'
00875      DISPLAY SPACES
00876      OPEN INPUT PARAM-LINE
00877      READ PARAM-LINE AT END
00878         DISPLAY 'PARAM LINE IS MISSING'
00879         MOVE 16 TO RETURN-CODE
00880         GOBACK.
00881      DISPLAY 'CONTROL CARD  '   PARAM-REC.
00882
00883      IF PR-CICV NOT EQUAL 'CICV'
00884         DISPLAY 'PARAM CHECK POINT CONTROL VALUE NOT EQUAL "CICV"
00885 -               ' ' PR-CICV
00886         MOVE 16 TO RETURN-CODE.
00887      IF PR-CICV-VAL NOT NUMERIC OR PR-CICV-VAL EQUAL ZEROES
00888         DISPLAY 'PARAM CICV VALUE NOT VALID  ' PR-CICV-VAL
00889         MOVE 16 TO RETURN-CODE.
00890      IF PR-PROC-YR NOT NUMERIC OR PR-PROC-YR EQUAL ZEROES
00891         DISPLAY 'PARAM PRIOR PROCESS YEAR NOT VALID ' PR-PROC-YR
00892         MOVE 16 TO RETURN-CODE
00893      END-IF.
00894      IF PR-CURR-YR NOT NUMERIC OR PR-CURR-YR EQUAL ZEROES
00895         DISPLAY 'PARAM CURRENT PROCESS YEAR NOT VALID ' PR-CURR-Y
00896         MOVE 16 TO RETURN-CODE
00897      ELSE
00898         COMPUTE WS-PROC-YR = PR-CURR-N.
00899      IF PR-EQ-FCTR NOT NUMERIC OR PR-EQ-FCTR EQUAL ZEROES
00900         DISPLAY 'PARAM EQUALIZATION FACTOR NOT VALID  ' PR-EQ-FCT
00901         MOVE 16 TO RETURN-CODE.
00902      IF PR-FR-YR NOT NUMERIC
00903         DISPLAY 'PARAM FROM YEAR NOT VALID  ' PR-FR-YR
00904         MOVE 16 TO RETURN-CODE.
00905      IF PR-TO-YR NOT NUMERIC
00906         DISPLAY 'PARAM TO YEAR NOT VALID  ' PR-TO-YR
00907         MOVE 16 TO RETURN-CODE.
00908      IF PR-FR-YR NUMERIC AND PR-TO-YR NUMERIC
00909         IF PR-FR-YR GREATER PR-TO-YR
00910            DISPLAY 'PARAM FROM YEAR CANNOT BE GREATER THAN PARAM
00911 -                  'O YEAR  ' PR-FR-YR ' ' PR-TO-YR
00912            MOVE 16 TO RETURN-CODE.
00913      IF PR-FR-BSYR NOT NUMERIC
00914         DISPLAY 'PARAM FROM BASE YEAR NOT VALID  ' PR-FR-BSYR
00915         MOVE 16 TO RETURN-CODE.
00916      IF PR-TO-BSYR NOT NUMERIC
00917         DISPLAY 'PARAM TO BASE YEAR NOT VALID  ' PR-TO-BSYR
00918         MOVE 16 TO RETURN-CODE.
00919      IF PR-FR-BSYR NUMERIC AND PR-TO-BSYR NUMERIC
00920         IF PR-FR-BSYR GREATER PR-TO-BSYR
00921            DISPLAY 'PARAM FROM BASE YEAR CANNOT BE GREATER THAN P
00922 -                  'RAM TO BASE YEAR  ' PR-FR-BSYR ' ' PR-TO-BSYR
00923            MOVE 16 TO RETURN-CODE.
00924      DISPLAY '   PARAMETERS   '
00925      DISPLAY '       CICV     ' PR-CICV
00926      DISPLAY '    PROC YEAR   ' PR-PROC-YR
00927      DISPLAY '    PR-EQ-FCTR  ' PR-EQ-FCTR
00928      DISPLAY '    PR-FR-YR    ' PR-FR-YR
00929      DISPLAY '    PR-TO-YR    ' PR-TO-YR
00930      DISPLAY '   PR-FR-BSYR   ' PR-FR-BSYR
00931      DISPLAY '   PR-TO-BSYR   ' PR-TO-BSYR
00932      DISPLAY '   PR-CURR-YR   ' PR-CURR-YR
00933
00934      IF RETURN-CODE EQUAL 16
00935         CLOSE PARAM-LINE
00936         GOBACK.
00937      OPEN INPUT EQUALFCT
00938      IF NOT NORMAL-STATUS
00939         DISPLAY 'EQUALFCT OPEN FAILED  ' FILE-STATUS
00940                 '   RETURN  '   FS-RETURN
00941                 '   FUNCTION  ' FS-FUNC
00942                 '   FEEDBACK  ' FS-FEEDBACK
00943         MOVE 16 TO RETURN-CODE
00944         CLOSE PARAM-LINE
00945         GOBACK.
00946      ACCEPT YMD-DATE             FROM DATE
00947      MOVE CORRESPONDING YMD-DATE TO MDY-DATE
00948      MOVE MDY-DATE               TO H1-CURR-DTE
00949      OPEN INPUT  CURR-ASSMT, PRIOR-ASSMT, EXPIRED-ASSMT
00950           OUTPUT SNRFREZ-FILE, PRINT-FILE-1, PRINT-FILE-2,
00951                  PRINT-FILE-3, PRINT-FILE-4
00952      WRITE PRINT-REC-1 FROM BLNK AFTER ADVANCING PAGE
00953      WRITE PRINT-REC-2 FROM BLNK AFTER ADVANCING PAGE
00954      WRITE PRINT-REC-3 FROM BLNK AFTER ADVANCING PAGE
00955      WRITE PRINT-REC-4 FROM BLNK AFTER ADVANCING PAGE
00956      PERFORM 200-READ-CURR-ASSMT
00957      PERFORM 250-READ-PRIOR-ASSMT
00958
00959      PERFORM 100-MAINLINE
00960      UNTIL CURR-ASSMT-EOF OR RETURN-CODE EQUAL 16.
00961
00962  070-END.
00963      IF RETURN-CODE NOT EQUAL 16
00964         PERFORM 1500-FORMAT-TOTALS.
00965      DISPLAY 'TOTAL PRIOR ASSESSMENT RECORDS READ               '
00966              PRIOR-ASSMT-REC-CTR
00967      DISPLAY 'TOTAL CURRENT ASSESSMENT RECORDS READ             '
00968              CURR-ASSMT-REC-CTR
00969      DISPLAY 'TOTAL EXPIRED ASSESSMENT RECORDS READ             '
00970              EXPIRED-ASSMT-REC-CTR
00971      DISPLAY 'TOTAL CURRENT & PRIOR ASSESSMENT RECORDS MATCHED  '
00972              CURR-PRIOR-MATCH-CTR
00973      DISPLAY 'TOTAL CURRENT ASSESSMENT RECORDS UNMATCHED        '
00974              CURR-UNMATCH-CTR
00975      DISPLAY 'TOTAL CURRENT RECORDS WITH NO-CALC INDICATOR      '
00976              CURR-NO-CALC-CTR
00977      DISPLAY 'TOTAL EXPIRED ASSESSMENT RECORDS SELECTED         '
00978              EXPIRED-ASSMT-SEL-CTR
00979      DISPLAY 'TOTAL EXEMPTION MASTER SEGMENTS READ              '
00980              EXEMP-MAST-REPL-CTR
00981      DISPLAY 'TOTAL SNR FREEZE RECORDS WRITTEN                  '
00982              SNRFREZ-REC-CTR
00983      DISPLAY 'TOTAL RECORDS WITH UNACCOUNTED CLASS CONDITIONS   '
00984              INVALID-CLS-COND-CTR
00985      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 1993                 '
00986              BASE-YR-1993-CTR
00987      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 1994                 '
00988              BASE-YR-1994-CTR
00989      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 1995                 '
00990              BASE-YR-1995-CTR
00991      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 1996                 '
00992              BASE-YR-1996-CTR
00993      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 1997                 '
00994              BASE-YR-1997-CTR
00995      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 1998                 '
00996              BASE-YR-1998-CTR
00997      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 1999                 '
00998              BASE-YR-1999-CTR
00999      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 2000                 '
01000              BASE-YR-2000-CTR
01001      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 2001                 '
01002              BASE-YR-2001-CTR
01003      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 2002                 '
01004              BASE-YR-2002-CTR
01005      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 2003                 '
01006              BASE-YR-2003-CTR
01007      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 2004                 '
01008              BASE-YR-2004-CTR
01009      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 2005                 '
01010              BASE-YR-2005-CTR
01011      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 2006                 '
01012              BASE-YR-2006-CTR
01013      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 2007                 '
01014              BASE-YR-2007-CTR
01015      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 2008                 '
01016              BASE-YR-2008-CTR
01017      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 2009                 '
01018              BASE-YR-2009-CTR
01019      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 2010                 '
01020              BASE-YR-2010-CTR
01021      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 2011                 '
01022              BASE-YR-2011-CTR
01023      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 2012                 '
01024              BASE-YR-2012-CTR
01025      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 2013                 '
01026              BASE-YR-2013-CTR
01027      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 2014                 '
01028              BASE-YR-2014-CTR
01029      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 2015                 '
01030              BASE-YR-2015-CTR
01031      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 2016                 '
01032              BASE-YR-2016-CTR
01033      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 2017                 '
01034              BASE-YR-2017-CTR
01035      DISPLAY 'TOTAL RECDS WITH BASE YEAR OTHER THAN 1993 THRU 201
01036 -            ' ' BASE-YR-OTHR-CTR
01037      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 2018                 '
01038              BASE-YR-2018-CTR
01039      DISPLAY 'TOTAL RECDS WITH BASE YEAR OTHER THAN 1993 THRU 201
01040 -            ' ' BASE-YR-OTHR-CTR
01041      CLOSE EQUALFCT
01042      IF NOT NORMAL-STATUS
01043         DISPLAY 'EQUALFCT CLOSE FAILED  ' FILE-STATUS
01044                 '   RETURN  '   FS-RETURN
01045                 '   FUNCTION  ' FS-FUNC
01046                 '   FEEDBACK  ' FS-FEEDBACK
01047         MOVE 16 TO RETURN-CODE.
01048      CLOSE PARAM-LINE, CURR-ASSMT, PRIOR-ASSMT, EXPIRED-ASSMT,
01049            SNRFREZ-FILE, PRINT-FILE-1, PRINT-FILE-2, PRINT-FILE-3
01050            PRINT-FILE-4.
01051      GOBACK.
01052
01053  100-MAINLINE.
01054      PERFORM 175-INITIALIZE-FIELDS
01055      IF SAVE-CA-KEY EQUAL SAVE-PA-KEY
01056         PERFORM 1600-GU-ROOT-ASSESS-SEG
01057         IF PI-SEG-NOTFND
01058            MOVE 'NO MATCHING ROOT, NOMATCHING ASSSESSMENT DATA SE
01059 -               'MENT' TO DL1-MESSG
01060            PERFORM 1250-FORMAT-REPORT-1
01061            PERFORM 200-READ-CURR-ASSMT
01062            PERFORM 250-READ-PRIOR-ASSMT
01063         ELSE
01064            IF PI-DB-GOOD-STATUS
01065               PERFORM 1625-GNP-EXEMP-MAST-SEG
01066               IF PI-SEG-NOTFND
01067                  MOVE 'NO MATCHING MASTER EXEMPTION SEGMENT'
01068                                          TO DL1-MESSG
01069                  PERFORM 1250-FORMAT-REPORT-1
01070                  PERFORM 200-READ-CURR-ASSMT
01071                  PERFORM 250-READ-PRIOR-ASSMT
01072               ELSE
01073                  IF PI-DB-GOOD-STATUS
01074                     MOVE 1 TO PRIOR-ASSMT-PRESENT-SW
01075                     ADD 1  TO CURR-PRIOR-MATCH-CTR
01076                     PERFORM 450-MOVE-CURR-ASSMT-TO-WS
01077                     MOVE 'C' TO FILE-SW
01078                     IF COOP-YES
01079                        PERFORM 425-COOP-CLASS-CHECK
01080                         VARYING SUB1 FROM 1 BY 1
01081                         UNTIL SUB1 GREATER H-DTL-QST-CTR-1
01082                         OR NO-CALC-SW = 'Y'
01083                        IF NO-CALC-SW = 'N'
01084                           PERFORM 400-CODE-CLASS-CONDITIONS
01085                           VARYING SUB1 FROM 1 BY 1
01086                           UNTIL SUB1 GREATER H-DTL-QST-CTR-1
01087                        ELSE
01088                           PERFORM 1050-STEP13
01089                     ELSE
01090                        PERFORM 400-CODE-CLASS-CONDITIONS
01091                        VARYING SUB1 FROM 1 BY 1
01092                        UNTIL SUB1 GREATER H-DTL-QST-CTR-1
01093                     END-IF
01094                     PERFORM 500-MOVE-PRIOR-ASSMT-TO-WS
01095                     MOVE 'P' TO FILE-SW
01096                     IF COOP-YES
01097                        PERFORM 425-COOP-CLASS-CHECK
01098                         VARYING SUB1 FROM 1 BY 1
01099                         UNTIL SUB1 GREATER H-DTL-QST-CTR-1
01100                         OR NO-CALC-SW = 'Y'
01101                        IF NO-CALC-SW = 'N'
01102                           PERFORM 400-CODE-CLASS-CONDITIONS
01103                           VARYING SUB1 FROM 1 BY 1
01104                           UNTIL SUB1 GREATER H-DTL-QST-CTR-1
01105                        ELSE
01106                           PERFORM 1050-STEP13
01107                     ELSE
01108                        PERFORM 400-CODE-CLASS-CONDITIONS
01109                        VARYING SUB1 FROM 1 BY 1
01110                        UNTIL SUB1 GREATER H-DTL-QST-CTR-1
01111                     END-IF
01112                     PERFORM 150-SUB-MAINLINE
01113                     PERFORM 200-READ-CURR-ASSMT
01114                     PERFORM 250-READ-PRIOR-ASSMT
01115                  END-IF
01116               END-IF
01117            END-IF
01118         END-IF
01119      ELSE
01120         IF SAVE-CA-KEY LESS SAVE-PA-KEY
01121            PERFORM 1600-GU-ROOT-ASSESS-SEG
01122            IF PI-SEG-NOTFND
01123               MOVE 'NO MATCHING ROOT, NOMATCHING ASSESSMENT DATA
01124 -                  'EGMENT' TO DL1-MESSG
01125               PERFORM 1250-FORMAT-REPORT-1
01126               PERFORM 200-READ-CURR-ASSMT
01127            ELSE
01128               IF PI-DB-GOOD-STATUS
01129                  PERFORM 1625-GNP-EXEMP-MAST-SEG
01130                  IF PI-SEG-NOTFND
01131                     MOVE 'NO MATCHING MASTER EXEMPTION SEGMENT'
01132                                             TO DL1-MESSG
01133                     PERFORM 1250-FORMAT-REPORT-1
01134                     PERFORM 200-READ-CURR-ASSMT
01135                  ELSE
01136                     IF PI-DB-GOOD-STATUS
01137                        ADD 1 TO CURR-UNMATCH-CTR
01138                        PERFORM 450-MOVE-CURR-ASSMT-TO-WS
01139                        MOVE 'C' TO FILE-SW
01140                        IF COOP-YES
01141                           PERFORM 425-COOP-CLASS-CHECK
01142                            VARYING SUB1 FROM 1 BY 1
01143                            UNTIL SUB1 GREATER H-DTL-QST-CTR-1
01144                            OR NO-CALC-SW = 'Y'
01145                           IF NO-CALC-SW = 'N'
01146                              PERFORM 400-CODE-CLASS-CONDITIONS
01147                              VARYING SUB1 FROM 1 BY 1
01148                              UNTIL SUB1 GREATER H-DTL-QST-CTR-1
01149                           ELSE
01150                              PERFORM 1050-STEP13
01151                        ELSE
01152                           PERFORM 400-CODE-CLASS-CONDITIONS
01153                           VARYING SUB1 FROM 1 BY 1
01154                           UNTIL SUB1 GREATER H-DTL-QST-CTR-1
01155                        END-IF
01156                        PERFORM 150-SUB-MAINLINE
01157                        PERFORM 200-READ-CURR-ASSMT
01158                     END-IF
01159                 END-IF
01160               END-IF
01161            END-IF
01162         ELSE
01163            PERFORM 250-READ-PRIOR-ASSMT.
01164
01165  150-SUB-MAINLINE.
01166      IF COOP-YES
01167         IF CURR-MIXED-CLS OR PRIOR-MIXED-CLS
01168            MOVE 'Y' TO NO-CALC-SW
01169            DISPLAY 'COOP CONTAINS MIXED MAJOR CLASSES '
01170               SAVE-CA-KEY (1:2) ' ' SAVE-CA-KEY (3:3) ' '
01171               SAVE-CA-KEY (6:15) ' ' SAVE-CA-KEY (21:1)
01172            MOVE 15 TO RETURN-CODE
01173            ADD 1   TO INVALID-CLS-COND-CTR
01174            PERFORM 1050-STEP13
01175         END-IF
01176      ELSE
01177      IF PRIOR-ASSMT-PRESENT
01178         IF CLASS-TAB (1)  EQUAL ZEROES OR
01179            CLASS-TAB (7)  EQUAL ZEROES
01180            MOVE 'Y' TO NO-CALC-SW
01181            MOVE 'A' TO HOLD-CALC-TYP
01182            ADD 1    TO CURR-NO-CALC-CTR
01183            PERFORM 1150-STEP17
01184         ELSE
01185            IF CLASS-TAB (5)  GREATER ZEROES OR
01186               CLASS-TAB (11) GREATER ZEROES
01187               MOVE 'B' TO HOLD-CALC-TYP
01188               PERFORM 605-STEP07
01189               IF NO-CALC-SW = SPACES
01190                  PERFORM 950-STEP11
01191               ELSE
01192                  PERFORM 1050-STEP13
01193               END-IF
01194            ELSE
01195               IF CLASS-TAB (2)  GREATER ZEROES OR
01196                  CLASS-TAB (8)  GREATER ZEROES
01197                  MOVE 'Y' TO NO-CALC-SW
01198                  MOVE 'C' TO HOLD-CALC-TYP
01199                  ADD 1    TO CURR-NO-CALC-CTR
01200                  PERFORM 1050-STEP13
01201               ELSE
01202                  IF (CLASS-TAB (3)  GREATER ZEROES  AND
01203                      CLASS-TAB (1)  EQUAL   1)      AND
01204                     (CLASS-TAB (9)  GREATER ZEROES  AND
01205                      CLASS-TAB (7)  EQUAL   1)
01206                     MOVE 'D' TO HOLD-CALC-TYP
01207                     PERFORM 600-STEP07 THRU 600-EXIT
01208                     IF NO-CALC-SW EQUAL SPACES
01209                        PERFORM 950-STEP11
01210                     ELSE
01211                        PERFORM 1050-STEP13
01212                  ELSE
01213                     IF (CLASS-TAB (3)  GREATER ZEROES  AND
01214                         CLASS-TAB (1)  GREATER 1)      AND
01215                        (CLASS-TAB (9)  GREATER ZEROES  AND
01216                         CLASS-TAB (7)  GREATER 1)
01217                        MOVE 'E' TO HOLD-CALC-TYP
01218                        PERFORM 800-STEP08 THRU 800-EXIT
01219                        IF NO-CALC-SW EQUAL SPACES
01220                           PERFORM 950-STEP11
01221                        ELSE
01222                           PERFORM 1050-STEP13
01223                     ELSE
01224                        IF (CLASS-TAB (3)  GREATER ZEROES  AND
01225                            CLASS-TAB (1)  GREATER ZEROES  AND
01226                            CLASS-TAB (4)  GREATER ZEROES) AND
01227                           (CLASS-TAB (9)  GREATER ZEROES  AND
01228                            CLASS-TAB (7)  GREATER ZEROES  AND
01229                            CLASS-TAB (10) GREATER ZEROES)
01230                           MOVE 'F' TO HOLD-CALC-TYP
01231                           PERFORM 850-STEP09 THRU 850-EXIT
01232                           IF NO-CALC-SW EQUAL SPACES
01233                              PERFORM 950-STEP11
01234                           ELSE
01235                              PERFORM 1050-STEP13
01236                        ELSE
01237                           IF (CLASS-TAB (3)  GREATER ZEROES  AND
01238                               CLASS-TAB (1)  GREATER ZEROES  AND
01239                               CLASS-TAB (4)  GREATER ZEROES) OR
01240                              (CLASS-TAB (9)  GREATER ZEROES  AND
01241                               CLASS-TAB (7)  GREATER ZEROES  AND
01242                               CLASS-TAB (10) GREATER ZEROES)
01243                              MOVE 'Y' TO NO-CALC-SW
01244                              MOVE 'G' TO HOLD-CALC-TYP
01245                              ADD 1    TO CURR-NO-CALC-CTR
01246                              PERFORM 1050-STEP13
01247                           ELSE
01248                        IF (CLASS-TAB (3)  GREATER ZEROES  AND
01249                            CLASS-TAB (1)  EQUAL   1       AND
01250                            CLASS-TAB (6)  GREATER ZEROES) AND
01251                           (CLASS-TAB (9)  GREATER ZEROES  AND
01252                            CLASS-TAB (7)  EQUAL   1       AND
01253                            CLASS-TAB (12) GREATER ZEROES)
01254                           MOVE 'H' TO HOLD-CALC-TYP
01255                           PERFORM 900-STEP10 THRU 900-EXIT
01256                           IF NO-CALC-SW EQUAL SPACES
01257                              PERFORM 950-STEP11
01258                           ELSE
01259                              PERFORM 1050-STEP13
01260                        ELSE
01261                     IF (CLASS-TAB (3)  GREATER ZEROES  AND
01262                         CLASS-TAB (1)  GREATER 1       AND
01263                         CLASS-TAB (6)  GREATER ZEROES) AND
01264                        (CLASS-TAB (9)  GREATER ZEROES  AND
01265                         CLASS-TAB (7)  GREATER 1       AND
01266                         CLASS-TAB (12) GREATER ZEROES)
01267                        MOVE 'I' TO HOLD-CALC-TYP
01268                        PERFORM 800-STEP08 THRU 800-EXIT
01269                        IF NO-CALC-SW EQUAL SPACES
01270                           PERFORM 950-STEP11
01271                        ELSE
01272                           PERFORM 1050-STEP13
01273                     ELSE
01274                        DISPLAY 'CURRENT AND/OR PRIOR ASSESSMENT M
01275 -                              'STER CONTAINS AN UNACCOUNTED CLAS
01276 -                              ' CONDITION  '
01277                                SAVE-CA-KEY (1:2)  ' '
01278                                SAVE-CA-KEY (3:3)  ' '
01279                                SAVE-CA-KEY (6:15) ' '
01280                                SAVE-CA-KEY (21:1)
01281                        MOVE 15 TO RETURN-CODE
01282                        ADD 1   TO INVALID-CLS-COND-CTR
01283      ELSE
01284         IF CLASS-TAB (7)  EQUAL ZEROES
01285            MOVE 'Y' TO NO-CALC-SW
01286            MOVE 'J' TO HOLD-CALC-TYP
01287            ADD 1    TO CURR-NO-CALC-CTR
01288            PERFORM 1050-STEP13
01289         ELSE
01290            IF CLASS-TAB (11) GREATER ZEROES
01291               MOVE 'K' TO HOLD-CALC-TYP
01292 *             PERFORM 1150-STEP17
01293               PERFORM 605-STEP07
01294               IF NO-CALC-SW = SPACES
01295                  PERFORM 950-STEP11
01296               ELSE
01297                  PERFORM 1050-STEP13
01298               END-IF
01299            ELSE
01300               IF CLASS-TAB (8)  GREATER ZEROES
01301                  MOVE 'Y' TO NO-CALC-SW
01302                  MOVE 'L' TO HOLD-CALC-TYP
01303                  ADD 1    TO CURR-NO-CALC-CTR
01304                  PERFORM 1050-STEP13
01305               ELSE
01306                  IF CLASS-TAB (9)  GREATER ZEROES AND
01307                     CLASS-TAB (7)  EQUAL   1
01308                     MOVE 'M' TO HOLD-CALC-TYP
01309                     PERFORM 600-STEP07 THRU 600-EXIT
01310                     PERFORM 750-STEP07-B
01311                     IF NO-CALC-SW EQUAL SPACES
01312                        PERFORM 950-STEP11
01313                     ELSE
01314                        PERFORM 1050-STEP13
01315                  ELSE
01316                     IF CLASS-TAB (9)  GREATER ZEROES AND
01317                        CLASS-TAB (7)  GREATER 1
01318                        MOVE 'N' TO HOLD-CALC-TYP
01319                        PERFORM 800-STEP08 THRU 800-EXIT
01320                        IF NO-CALC-SW EQUAL SPACES
01321                           PERFORM 950-STEP11
01322                        ELSE
01323                           PERFORM 1050-STEP13
01324                     ELSE
01325                        IF CLASS-TAB (9)  GREATER ZEROES AND
01326                           CLASS-TAB (7)  GREATER ZEROES AND
01327                           CLASS-TAB (10) GREATER ZEROES
01328                           MOVE 'O' TO HOLD-CALC-TYP
01329                           PERFORM 850-STEP09 THRU 850-EXIT
01330                           IF NO-CALC-SW EQUAL SPACES
01331                              PERFORM 950-STEP11
01332                           ELSE
01333                              PERFORM 1050-STEP13
01334                        ELSE
01335                           IF CLASS-TAB (9)  GREATER ZEROES AND
01336                              CLASS-TAB (7)  GREATER ZEROES AND
01337                              CLASS-TAB (10) GREATER ZEROES
01338                              MOVE 'Y' TO NO-CALC-SW
01339                              MOVE 'P' TO HOLD-CALC-TYP
01340                              ADD 1    TO CURR-NO-CALC-CTR
01341                              PERFORM 1050-STEP13
01342                           ELSE
01343                        IF CLASS-TAB (9)  GREATER ZEROES AND
01344                           CLASS-TAB (7)  EQUAL   1      AND
01345                           CLASS-TAB (12) GREATER ZEROES
01346                           MOVE 'Q' TO HOLD-CALC-TYP
01347                           PERFORM 900-STEP10 THRU 900-EXIT
01348                           IF NO-CALC-SW EQUAL 'Y'
01349                              PERFORM 1050-STEP13
01350                           ELSE
01351                              PERFORM 950-STEP11
01352                        ELSE
01353                     IF CLASS-TAB (9)  GREATER ZEROES AND
01354                        CLASS-TAB (7)  GREATER 1      AND
01355                        CLASS-TAB (12) GREATER ZEROES
01356                        MOVE 'R' TO HOLD-CALC-TYP
01357                        PERFORM 800-STEP08 THRU 800-EXIT
01358                        IF NO-CALC-SW EQUAL SPACES
01359                           PERFORM 950-STEP11
01360                        ELSE
01361                           PERFORM 1050-STEP13
01362                     ELSE
01363                        DISPLAY 'CURRENT ASSESSMENT MASTER CONTAIN
01364 -                              ' AN UNACCOUNTED CLASS CONDITION
01365                                SAVE-CA-KEY (1:2)  ' '
01366                                SAVE-CA-KEY (3:3)  ' '
01367                                SAVE-CA-KEY (6:15) ' '
01368                                SAVE-CA-KEY (21:1)
01369                        MOVE 15 TO RETURN-CODE
01370                        ADD 1   TO INVALID-CLS-COND-CTR.
01371
01372  175-INITIALIZE-FIELDS.
01373      MOVE NULL-CLASS-TAB TO CLASS-TABLE
01374      MOVE ZEROES         TO PRIOR-ASSMT-PRESENT-SW,
01375                             BASE-YR,                BASE-YR-CLS,
01376                             BASE-VAL-AV,            HIMP-TOT,
01377                             BASE-ELIG-COMP-AV,
01378                             BASE-ELIG-COMP-EAV,
01379                             288-OVER-30000-AV,      CURR-YR,
01380                             CURR-YR-CLS,            CURR-VAL-AV,
01381                             CURR-ELIG-COMP-EAV,
01382                             CURR-ELIG-COMP-AV,      ASSD-VAL,
01383                             288-OVER-30000-EAV,     288-EXPIRE-AV
01384                             288-EXPIRE-EAV,
01385                             FINAL-DIFF-TX-COMP,
01386                             TOT-BASE-COMP-EAV,
01387                             CURR-BASE-VAL-EAV,      BASE-VAL-EAV,
01388                             CURR-NOT-ELIG-AV,
01389                             CURR-NOT-ELIG-EAV,      QUALIFY-SW,
01390                             T4-ASSD-VAL,            T5-ASSD-VAL,
01391                             PRIOR-RESD-IMPV-CTR,
01392                             CURR-RESD-IMPV-CTR,
01393                             PRIOR-IMPV-TOT,
01394                             PRIOR-LAND-TOT,
01395                             CURR-IMPV-TOT,          CURR-LAND-TOT
01396      MOVE SPACES         TO NO-CALC-SW  HOLD-CALC-TYP FARM-SW
01397                             MESSG-SW.
01398      MOVE 'N' TO COOP-SW  CURR-MIXED-CLS-SW  PRIOR-MIXED-CLS-SW
01399                  BYPASS-EQFILE-SW.
01400
01401  200-READ-CURR-ASSMT.
01402      READ CURR-ASSMT AT END
01403         MOVE 1              TO CURR-ASSMT-EOF-SW
01404         MOVE HIGH-VALUES    TO SAVE-CA-KEY.
01405      IF NOT CURR-ASSMT-EOF
01406         ADD 1               TO CURR-ASSMT-REC-CTR
01407         MOVE M-TXCD         TO TXCD-HLD
01408         MOVE TXCD-HLD (1:2) TO CCK-TOWN
01409         MOVE M-VOL          TO CCK-VOL
01410         MOVE M-PROP         TO CCK-PROP
01411         MOVE M-TXTYP        TO CCK-TXTYP
01412         IF CURR-CA-KEY NOT GREATER PREV-CA-KEY
01413            DISPLAY 'CURR-ASSMT IS OUT OF SEQUENCE'
01414            DISPLAY 'CURRENT RECORD IS   ' CCK-TOWN ' ' CCK-VOL '
01415                                           CCK-PROP ' ' CCK-TXTYP
01416            DISPLAY 'PREVIOUS RECORD IS  ' PCK-TOWN ' ' PCK-VOL '
01417                                           PCK-PROP ' ' PCK-TXTYP
01418            MOVE 16          TO RETURN-CODE
01419         ELSE
01420            MOVE CURR-CA-KEY TO PREV-CA-KEY, SAVE-CA-KEY.
01421
01422  250-READ-PRIOR-ASSMT.
01423      READ PRIOR-ASSMT AT END
01424         MOVE 1              TO PRIOR-ASSMT-EOF-SW
01425         MOVE HIGH-VALUES    TO SAVE-PA-KEY.
01426      IF NOT PRIOR-ASSMT-EOF
01427         ADD 1               TO PRIOR-ASSMT-REC-CTR
01428         MOVE MA-TXCD        TO TXCD-HLD
01429         MOVE TXCD-HLD (1:2) TO CPK-TOWN
01430         MOVE MA-VOL         TO CPK-VOL
01431         MOVE MA-PROP        TO CPK-PROP
01432         MOVE MA-TXTYP       TO CPK-TXTYP
01433         IF CURR-PA-KEY NOT GREATER PREV-PA-KEY
01434            DISPLAY 'PRIOR-ASSMT IS OUT OF SEQUENCE'
01435            DISPLAY 'CURRENT RECORD IS   ' CPK-TOWN ' ' CPK-VOL '
01436                                           CPK-PROP ' ' CPK-TXTYP
01437            DISPLAY 'PREVIOUS RECORD IS  ' PPK-TOWN ' ' PPK-VOL '
01438                                           PPK-PROP ' ' PPK-TXTYP
01439            MOVE 16          TO RETURN-CODE
01440         ELSE
01441            MOVE CURR-PA-KEY TO PREV-PA-KEY, SAVE-PA-KEY.
01442
01443  300-READ-EXPIRED-ASSMT.
01444      READ EXPIRED-ASSMT AT END
01445         MOVE 1              TO EXPIRED-ASSMT-EOF-SW
01446         MOVE HIGH-VALUES    TO SAVE-EX-KEY.
01447      IF NOT EXPIRED-ASSMT-EOF
01448         ADD 1               TO EXPIRED-ASSMT-REC-CTR
01449         MOVE AS-TXCD        TO TXCD-HLD
01450         MOVE TXCD-HLD (1:2) TO CEK-TOWN
01451         MOVE AS-VOL         TO CEK-VOL
01452         MOVE AS-PROP        TO CEK-PROP
01453         MOVE AS-TXTYP       TO CEK-TXTYP
01454         IF CURR-EX-KEY NOT GREATER PREV-EX-KEY
01455            DISPLAY 'EXPIRED-ASSMT IS OUT OF SEQUENCE'
01456            DISPLAY 'CURRENT RECORD IS   ' CEK-TOWN ' ' CEK-VOL '
01457                                           CEK-PROP ' ' CEK-TXTYP
01458            DISPLAY 'PREVIOUS RECORD IS  ' PEK-TOWN ' ' PEK-VOL '
01459                                           PEK-PROP ' ' PEK-TXTYP
01460            MOVE 16          TO RETURN-CODE
01461         ELSE
01462            MOVE CURR-EX-KEY TO PREV-EX-KEY, SAVE-EX-KEY.
01463
01464  350-READ-EQUALFCT.
01465      MOVE BASE-YR (3:2) TO EQ-YEAR
01466      MOVE 1             TO EQ-QUAD
01467      READ EQUALFCT
01468      IF NORMAL-STATUS OR RECORD-NOT-FOUND
01469         NEXT SENTENCE
01470      ELSE
01471         DISPLAY 'EQUALFCT READ FAILED  ' FILE-STATUS
01472                 '   RETURN  '   FS-RETURN
01473                 '   FUNCTION  ' FS-FUNC
01474                 '   FEEDBACK  ' FS-FEEDBACK
01475         MOVE 16 TO RETURN-CODE.
01476
01477  400-CODE-CLASS-CONDITIONS.
01478      IF FILE-SW EQUAL 'P'
01479         MOVE ZEROES TO SUB2
01480      ELSE
01481         MOVE 6      TO SUB2.
01482      MOVE H1-CLS (SUB1) TO CLASS-HLD
01483      IF RESD-IMPV
01484         ADD 1 TO SUB2
01485         ADD 1 TO CLASS-TAB (SUB2)
01486      ELSE
01487         IF RESD-LAND
01488            ADD 3 TO SUB2
01489            ADD 1 TO CLASS-TAB (SUB2)
01490         ELSE
01491            IF FARM-LAND
01492               ADD 4 TO SUB2
01493               ADD 1 TO CLASS-TAB (SUB2)
01494               MOVE 'Y' TO FARM-SW
01495            ELSE
01496               IF MIXED-USE
01497                  ADD 5 TO SUB2
01498                  ADD 1 TO CLASS-TAB (SUB2)
01499               ELSE
01500                  IF NON-RESD
01501                     ADD 6 TO SUB2
01502                     ADD 1 TO CLASS-TAB (SUB2).
01503      IF SEG-WHERE-QUEST-FOLLOWS
01504         ADD 1 TO SUB1.
01505
01506  425-COOP-CLASS-CHECK.
01507      MOVE H1-CLS (SUB1) TO CLASS-HLD
01508      IF NOT MAJOR-CLASS-2
01509         IF FILE-SW = 'C'
01510            MOVE 'Y' TO CURR-MIXED-CLS-SW
01511         ELSE
01512            IF FILE-SW = 'P'
01513               MOVE 'Y' TO PRIOR-MIXED-CLS-SW
01514            END-IF
01515         END-IF
01516         MOVE 'Y' TO NO-CALC-SW
01517         DISPLAY 'COOP CONTAINS MIXED MAJOR CLASSES '
01518            SAVE-CA-KEY (1:2) ' ' SAVE-CA-KEY (3:3) ' '
01519            SAVE-CA-KEY (6:15) ' ' SAVE-CA-KEY (21:1)
01520         MOVE 15 TO RETURN-CODE
01521         ADD 1   TO INVALID-CLS-COND-CTR.
01522      IF SEG-WHERE-QUEST-FOLLOWS
01523         ADD 1 TO SUB1.
01524
01525  450-MOVE-CURR-ASSMT-TO-WS.
01526      MOVE M-BASE          TO H-BASE
01527      MOVE M-SLS-CTR-1     TO H-SLS-CTR-1
01528      MOVE M-DTL-QST-CTR-1 TO H-DTL-QST-CTR-1
01529      PERFORM
01530      VARYING SUB1 FROM 1 BY 1
01531      UNTIL SUB1 GREATER M-DTL-QST-CTR-1
01532         MOVE M-DTL-QST-1 (SUB1) TO H-DTL-QST-1 (SUB1)
01533      END-PERFORM.
01534
01535  500-MOVE-PRIOR-ASSMT-TO-WS.
01536      MOVE MA-BASE          TO H-BASE
01537      MOVE MA-SLS-CTR-1     TO H-SLS-CTR-1
01538      MOVE MA-DTL-QST-CTR-1 TO H-DTL-QST-CTR-1
01539      PERFORM
01540      VARYING SUB1 FROM 1 BY 1
01541      UNTIL SUB1 GREATER MA-DTL-QST-CTR-1
01542         MOVE MA-DTL-QST-1 (SUB1) TO H-DTL-QST-1 (SUB1)
01543      END-PERFORM.
01544
01545  600-STEP07.
01546      IF PRIOR-ASSMT-PRESENT
01547         PERFORM 700-STEP07-A
01548         MOVE WS-PROC-YR TO BASE-YR
01549         MOVE H-CLS  TO BASE-YR-CLS
01550         PERFORM
01551         VARYING SUB1 FROM 1 BY 1
01552         UNTIL SUB1 GREATER H-DTL-QST-CTR-1
01553            ADD H1-VAL (SUB1)  TO BASE-VAL-AV
01554            MOVE H1-CLS (SUB1) TO CLASS-HLD
01555            IF RESD-LAND OR RESD-IMPV
01556               ADD H1-VAL (SUB1) TO BASE-ELIG-COMP-AV
01557            ELSE
01558               IF HIMP
01559                  ADD H1-VAL (SUB1) TO 288-OVER-30000-AV
01560               END-IF
01561            END-IF
01562            IF SEG-WHERE-QUEST-FOLLOWS
01563               ADD 1 TO SUB1
01564            END-IF
01565         END-PERFORM
01566         PERFORM 350-READ-EQUALFCT
01567         IF NORMAL-STATUS
01568            COMPUTE 288-OVER-30000-EAV ROUNDED = EQ-FACTOR *
01569                                                 288-OVER-30000-AV
01570         ELSE
01571            IF RECORD-NOT-FOUND
01572               MOVE 'Y' TO NO-CALC-SW
01573               ADD 1    TO CURR-NO-CALC-CTR
01574               MOVE 'Q' TO MESSG-SW
01575               GO TO 600-EXIT
01576            END-IF
01577         END-IF
01578      END-IF.
01579      MOVE PR-PROC-YR TO CURR-YR
01580      MOVE M-CLS      TO CURR-YR-CLS
01581      PERFORM 450-MOVE-CURR-ASSMT-TO-WS
01582      PERFORM
01583      VARYING SUB1 FROM 1 BY 1
01584      UNTIL SUB1 GREATER H-DTL-QST-CTR-1
01585         ADD H1-VAL  (SUB1) TO CURR-VAL-AV
01586         MOVE H1-CLS (SUB1) TO CLASS-HLD
01587         MOVE H3-YR  (SUB1) TO YR-HLD
01588         IF RESD-LAND OR RESD-IMPV OR HIMP
01589            ADD H1-VAL (SUB1) TO CURR-ELIG-COMP-AV
01590         END-IF
01591         IF HIMP AND YR-HLD EQUAL PR-PROC-YR (3:2)
01592            ADD H1-VAL (SUB1) TO 288-OVER-30000-AV HIMP-TOT
01593         END-IF
01594         IF SEG-WHERE-QUEST-FOLLOWS
01595            ADD 1             TO SUB1
01596         END-IF
01597      END-PERFORM
01598      COMPUTE 288-OVER-30000-EAV ROUNDED = 288-OVER-30000-EAV +
01599              (HIMP-TOT * PR-EQ-FCTR-N).
01600  600-EXIT. EXIT.
01601
01602  605-STEP07.
01603      IF PRIOR-ASSMT-PRESENT
01604         PERFORM 700-STEP07-A
01605         MOVE WS-PROC-YR TO BASE-YR
01606         MOVE H-CLS  TO BASE-YR-CLS
01607         PERFORM
01608         VARYING SUB1 FROM 1 BY 1
01609         UNTIL SUB1 GREATER H-DTL-QST-CTR-1
01610            MOVE H1-CLS (SUB1) TO CLASS-HLD
01611            IF CLASS-HLD = 236
01612               ADD H1-VAL (SUB1)  TO WS-236-VALUE
01613            ELSE
01614               IF H1-TYP (SUB1) = 2 OR 3 OR 4 OR 5
01615                  ADD H1-VAL (SUB1) TO WS-IMPRV-VALUE
01616               ELSE
01617                  IF H1R-TYPE1 (SUB1)
01618                     ADD H1-VAL (SUB1) TO WS-TYPE1-VALUE
01619                  ELSE
01620                     IF HIMP
01621                        ADD H1-VAL (SUB1) TO 288-OVER-30000-AV
01622                     END-IF
01623                  END-IF
01624               END-IF
01625            END-IF
01626            IF SEG-WHERE-QUEST-FOLLOWS
01627               ADD 1 TO SUB1
01628            END-IF
01629         END-PERFORM
01630         PERFORM 350-READ-EQUALFCT
01631         IF NORMAL-STATUS
01632            COMPUTE 288-OVER-30000-EAV ROUNDED = EQ-FACTOR *
01633                                                 288-OVER-30000-AV
01634         ELSE
01635            IF RECORD-NOT-FOUND
01636               MOVE 'Y' TO NO-CALC-SW
01637               ADD 1    TO CURR-NO-CALC-CTR
01638               MOVE 'Q' TO MESSG-SW
01639               GO TO 600-EXIT
01640            END-IF
01641         END-IF
01642      END-IF.
01643      MOVE PR-PROC-YR TO CURR-YR
01644      MOVE M-CLS      TO CURR-YR-CLS
01645      PERFORM 450-MOVE-CURR-ASSMT-TO-WS
01646      PERFORM
01647      VARYING SUB1 FROM 1 BY 1
01648      UNTIL SUB1 GREATER H-DTL-QST-CTR-1
01649         MOVE H1-CLS (SUB1) TO CLASS-HLD
01650         IF CLASS-HLD = 236
01651            ADD H1-VAL (SUB1) TO CURR-VAL-AV
01652            MOVE H3-YR (SUB1) TO YR-HLD
01653            IF CLASS-HLD = 236
01654               ADD H1-VAL (SUB1) TO WS-236-VALUE
01655            ELSE
01656               IF H1-TYP (SUB1) = 2 OR 3 OR 4 OR 5
01657                  ADD H1-VAL (SUB1) TO WS-IMPRV-VALUE
01658               ELSE
01659                  IF H1R-TYPE1 (SUB1)
01660                     ADD H1-VAL (SUB1) TO WS-TYPE1-VALUE
01661                  END-IF
01662               END-IF
01663            END-IF
01664         END-IF
01665         IF HIMP AND YR-HLD EQUAL PR-PROC-YR (3:2)
01666            ADD H1-VAL (SUB1) TO 288-OVER-30000-AV HIMP-TOT
01667         END-IF
01668         IF SEG-WHERE-QUEST-FOLLOWS
01669            ADD 1             TO SUB1
01670         END-IF
01671      END-PERFORM
01672      COMPUTE 288-OVER-30000-EAV ROUNDED = 288-OVER-30000-EAV +
01673              (HIMP-TOT * PR-EQ-FCTR-N).
01674  605-EXIT. EXIT.
01675
01676  700-STEP07-A.
01677      PERFORM
01678      VARYING SUB1 FROM 1 BY 1
01679      UNTIL SUB1 GREATER H-DTL-QST-CTR-1
01680            IF H1-TYP (SUB1) EQUAL '5'
01681               IF H5-OCCFAC (SUB1) GREATER ZEROES
01682                  COMPUTE T5-ASSD-VAL ROUNDED =
01683                     (H5-REPCST (SUB1) * 100) /
01684                      H5-OCCFAC (SUB1) * ( H5-COND (SUB1)) / 100
01685                  IF H5-PCASSD (SUB1) GREATER ZEROES
01686                     COMPUTE T5-ASSD-VAL ROUNDED =
01687                        (T5-ASSD-VAL *  H5-PCASSD (SUB1)) / 100
01688                  END-IF
01689                  MOVE T5-ASSD-VAL TO H5-VAL (SUB1)
01690               END-IF
01691            END-IF
01692         MOVE H1-CLS (SUB1) TO CLASS-HLD
01693         IF SEG-WHERE-QUEST-FOLLOWS
01694            ADD 1 TO SUB1
01695         END-IF
01696      END-PERFORM.
01697
01698  750-STEP07-B.
01699      MOVE WS-PROC-YR        TO BASE-YR
01700      MOVE CURR-VAL-AV       TO BASE-VAL-AV
01701      MOVE CURR-ELIG-COMP-AV TO BASE-ELIG-COMP-AV
01702      SUBTRACT HIMP-TOT      FROM BASE-ELIG-COMP-AV.
01703
01704  800-STEP08.
01705      PERFORM 600-STEP07 THRU 600-EXIT
01706      IF NO-CALC-SW EQUAL 'Y'
01707         GO TO 800-EXIT.
01708      IF PRIOR-ASSMT-PRESENT
01709         PERFORM 500-MOVE-PRIOR-ASSMT-TO-WS
01710         PERFORM 700-STEP07-A
01711         PERFORM
01712         VARYING SUB1 FROM 1 BY 1
01713         UNTIL SUB1 GREATER H-DTL-QST-CTR-1
01714            MOVE H1-CLS (SUB1) TO CLASS-HLD
01715            IF RESD-IMPV OR COOP-YES
01716               ADD 1             TO PRIOR-RESD-IMPV-CTR
01717               ADD H1-VAL (SUB1) TO PRIOR-IMPV-TOT
01718            ELSE
01719               IF RESD-LAND OR COOP-YES
01720                  ADD H1-VAL (SUB1) TO PRIOR-LAND-TOT
01721               END-IF
01722            END-IF
01723            IF SEG-WHERE-QUEST-FOLLOWS
01724               ADD 1 TO SUB1
01725            END-IF
01726         END-PERFORM
01727         IF NOT COOP-YES
01728            IF PRIOR-RESD-IMPV-CTR GREATER ZERO
01729               COMPUTE PRIOR-IMPV-TOT ROUNDED = PRIOR-IMPV-TOT /
01730                                                PRIOR-RESD-IMPV-CT
01731               COMPUTE PRIOR-LAND-TOT ROUNDED = PRIOR-LAND-TOT /
01732                                                PRIOR-RESD-IMPV-CT
01733            ELSE
01734               MOVE 'Y' TO NO-CALC-SW
01735            END-IF
01736         END-IF
01737         MOVE ZEROES  TO BASE-ELIG-COMP-AV
01738         ADD PRIOR-IMPV-TOT PRIOR-LAND-TOT GIVING BASE-ELIG-COMP-A
01739      END-IF.
01740      PERFORM 450-MOVE-CURR-ASSMT-TO-WS
01741      PERFORM
01742      VARYING SUB1 FROM 1 BY 1
01743      UNTIL SUB1 GREATER H-DTL-QST-CTR-1
01744         MOVE H1-CLS (SUB1) TO CLASS-HLD
01745         IF RESD-IMPV OR COOP-YES
01746            ADD 1             TO CURR-RESD-IMPV-CTR
01747            ADD H1-VAL (SUB1) TO CURR-IMPV-TOT
01748         ELSE
01749            IF RESD-LAND OR COOP-YES
01750               ADD H1-VAL (SUB1) TO CURR-LAND-TOT
01751            ELSE
01752               IF HIMP
01753                  ADD H1-VAL (SUB1) TO CURR-ELIG-COMP-AV
01754               END-IF
01755            END-IF
01756         END-IF
01757         IF SEG-WHERE-QUEST-FOLLOWS
01758            ADD 1 TO SUB1
01759         END-IF
01760      END-PERFORM
01761      IF NOT COOP-YES
01762         IF CURR-RESD-IMPV-CTR GREATER ZERO
01763            COMPUTE CURR-IMPV-TOT ROUNDED = CURR-IMPV-TOT /
01764                                            CURR-RESD-IMPV-CTR
01765            COMPUTE CURR-LAND-TOT ROUNDED = CURR-LAND-TOT /
01766                                            CURR-RESD-IMPV-CTR
01767         ELSE
01768            MOVE 'Y' TO NO-CALC-SW
01769         END-IF
01770      END-IF
01771      MOVE ZEROES TO CURR-ELIG-COMP-AV
01772      ADD CURR-IMPV-TOT CURR-LAND-TOT GIVING CURR-ELIG-COMP-AV
01773      SUBTRACT CURR-ELIG-COMP-AV      FROM CURR-VAL-AV GIVING
01774               CURR-NOT-ELIG-AV
01775      IF PRIOR-RESD-IMPV-CTR EQUAL CURR-RESD-IMPV-CTR
01776         PERFORM 950-STEP11
01777      ELSE
01778         MOVE 'Y'    TO NO-CALC-SW
01779         ADD 1       TO CURR-NO-CALC-CTR
01780         MOVE ZEROES TO PRIOR-IMPV-TOT,    PRIOR-LAND-TOT,
01781                        CURR-IMPV-TOT,     CURR-LAND-TOT,
01782                        BASE-ELIG-COMP-AV, CURR-ELIG-COMP-AV,
01783                        CURR-VAL-AV,       CURR-NOT-ELIG-AV
01784         GO TO 800-EXIT.
01785      IF NOT PRIOR-ASSMT-PRESENT
01786         MOVE WS-PROC-YR                 TO BASE-YR
01787         MOVE CURR-YR-CLS                TO BASE-YR-CLS
01788         ADD CURR-IMPV-TOT CURR-LAND-TOT GIVING BASE-ELIG-COMP-AV.
01789      PERFORM 950-STEP11.
01790  800-EXIT. EXIT.
01791
01792  850-STEP09.
01793      PERFORM 600-STEP07 THRU 600-EXIT
01794      IF NO-CALC-SW EQUAL 'Y'
01795         GO TO 850-EXIT.
01796      PERFORM 500-MOVE-PRIOR-ASSMT-TO-WS
01797      PERFORM 700-STEP07-A
01798      PERFORM
01799      VARYING SUB1 FROM 1 BY 1
01800      UNTIL SUB1 GREATER H-DTL-QST-CTR-1
01801         MOVE H1-CLS (SUB1) TO CLASS-HLD
01802         IF RESD-LAND OR RESD-IMPV OR FARM-LAND
01803            ADD H1-VAL (SUB1) TO BASE-ELIG-COMP-AV
01804         END-IF
01805         IF SEG-WHERE-QUEST-FOLLOWS
01806            ADD 1             TO SUB1
01807         END-IF
01808      END-PERFORM
01809      PERFORM 450-MOVE-CURR-ASSMT-TO-WS
01810      PERFORM
01811      VARYING SUB1 FROM 1 BY 1
01812      UNTIL SUB1 GREATER H-DTL-QST-CTR-1
01813         MOVE H1-CLS (SUB1) TO CLASS-HLD
01814         IF RESD-LAND OR RESD-IMPV OR FARM-LAND OR HIMP
01815            ADD H1-VAL (SUB1) TO CURR-ELIG-COMP-AV
01816         END-IF
01817         IF SEG-WHERE-QUEST-FOLLOWS
01818            ADD 1             TO SUB1
01819         END-IF
01820      END-PERFORM.
01821      IF NOT PRIOR-ASSMT-PRESENT
01822         MOVE WS-PROC-YR        TO BASE-YR
01823         MOVE CURR-YR-CLS       TO BASE-YR-CLS
01824         MOVE CURR-ELIG-COMP-AV TO BASE-ELIG-COMP-AV
01825         SUBTRACT HIMP-TOT      FROM BASE-ELIG-COMP-AV.
01826      PERFORM 950-STEP11.
01827  850-EXIT. EXIT.
01828
01829  900-STEP10.
01830      PERFORM 600-STEP07 THRU 600-EXIT
01831      IF NO-CALC-SW EQUAL 'Y'
01832         GO TO 900-EXIT.
01833      IF PRIOR-ASSMT-PRESENT
01834         PERFORM 500-MOVE-PRIOR-ASSMT-TO-WS
01835         PERFORM 700-STEP07-A
01836         PERFORM
01837         VARYING SUB1 FROM 1 BY 1
01838         UNTIL SUB1 GREATER H-DTL-QST-CTR-1
01839            MOVE H1-CLS (SUB1) TO CLASS-HLD
01840            IF RESD-IMPV
01841               ADD H1-VAL (SUB1) TO PRIOR-IMPV-TOT
01842            ELSE
01843               IF RESD-LAND
01844                  ADD H1-VAL (SUB1) TO PRIOR-LAND-TOT
01845               END-IF
01846            END-IF
01847            IF SEG-WHERE-QUEST-FOLLOWS
01848               ADD 1 TO SUB1
01849            END-IF
01850         END-PERFORM
01851         ADD PRIOR-IMPV-TOT PRIOR-LAND-TOT GIVING BASE-ELIG-COMP-A
01852      END-IF.
01853      PERFORM 450-MOVE-CURR-ASSMT-TO-WS
01854      PERFORM
01855      VARYING SUB1 FROM 1 BY 1
01856      UNTIL SUB1 GREATER H-DTL-QST-CTR-1
01857         MOVE H1-CLS (SUB1) TO CLASS-HLD
01858         IF RESD-IMPV
01859            ADD H1-VAL (SUB1) TO CURR-IMPV-TOT
01860         ELSE
01861            IF RESD-LAND
01862               ADD H1-VAL (SUB1) TO CURR-LAND-TOT
01863            ELSE
01864               IF HIMP
01865                  ADD H1-VAL (SUB1) TO CURR-ELIG-COMP-AV
01866               END-IF
01867            END-IF
01868         END-IF
01869         IF SEG-WHERE-QUEST-FOLLOWS
01870            ADD 1 TO SUB1
01871         END-IF
01872      END-PERFORM
01873      ADD CURR-IMPV-TOT CURR-LAND-TOT CURR-ELIG-COMP-AV GIVING
01874          CURR-ELIG-COMP-AV
01875      SUBTRACT CURR-ELIG-COMP-AV FROM CURR-VAL-AV GIVING
01876               CURR-NOT-ELIG-AV
01877      IF NOT PRIOR-ASSMT-PRESENT
01878         MOVE WS-PROC-YR        TO BASE-YR
01879         MOVE CURR-YR-CLS       TO BASE-YR-CLS
01880         MOVE CURR-ELIG-COMP-AV TO BASE-ELIG-COMP-AV
01881         SUBTRACT HIMP-TOT      FROM BASE-ELIG-COMP-AV.
01882      PERFORM 950-STEP11.
01883  900-EXIT. EXIT.
01884
01885  950-STEP11.
01886      PERFORM 300-READ-EXPIRED-ASSMT
01887      UNTIL EXPIRED-ASSMT-EOF OR RETURN-CODE EQUAL 16
01888      OR SAVE-EX-KEY NOT LESS SAVE-CA-KEY
01889      IF SAVE-EX-KEY EQUAL SAVE-CA-KEY
01890         ADD 1 TO EXPIRED-ASSMT-SEL-CTR
01891         PERFORM
01892         VARYING SUB1 FROM 1 BY 1
01893         UNTIL SUB1 GREATER AS-DTL-QST-CTR-1
01894            MOVE AS1-CLS (SUB1) TO CLASS-HLD
01895            MOVE AS3-YR (SUB1) TO WS-AS3-YEAR
01896            MOVE WS-AS3-YR     TO WK-AS3-YR
01897            IF WS-AS3-YR > 60
01898               MOVE 19 TO WK-AS3-CC
01899            ELSE
01900               MOVE 20 TO WK-AS3-CC
01901            END-IF
01902            MOVE PR-FR-YR-N TO WS-FR-YR
01903            IF PR-FR-YR-N > 60
01904               MOVE 19 TO WS-FR-CC
01905            ELSE
01906               MOVE 20 TO WS-FR-CC
01907            END-IF
01908            MOVE PR-TO-YR-N TO WS-TO-YR
01909            IF PR-TO-YR-N > 60
01910               MOVE 19 TO WS-TO-CC
01911            ELSE
01912               MOVE 20 TO WS-TO-CC
01913            END-IF
01914
01915            IF HIMP AND AS-PERMITIND EQUAL '2'      AND
01916 *             AS3-YR (SUB1) NOT LESS    PR-FR-YR-N AND
01917 *             AS3-YR (SUB1) NOT GREATER PR-TO-YR-N
01918               WK-AS3-YEAR NOT LESS WS-FR-YEAR AND
01919               WK-AS3-YEAR  NOT GREATER WS-TO-YEAR
01920               IF AS3-VAL (SUB1) EQUAL ZEROES
01921                  COMPUTE 288-EXPIRE-AV ROUNDED =
01922                                        288-EXPIRE-AV     +
01923                                        AS3-REPCST (SUB1) * .16
01924               ELSE
01925                  ADD 4800              TO 288-EXPIRE-AV
01926               END-IF
01927            END-IF
01928            IF SEG-WHERE-QUEST-FOLLOWS
01929               ADD 1 TO SUB1
01930            END-IF
01931         END-PERFORM
01932      END-IF.
01933      PERFORM 1050-STEP13.
01934
01935  1010-CHECK-BASE-YR-GN-DET-SEG.
01936      MOVE ZEROES TO EXEMP-DET-EOF-SW  EXEMP-DET-SEG-CTR
01937      PERFORM
01938      UNTIL EXEMP-DET-EOF-SW EQUAL 1
01939      OR RETURN-CODE EQUAL 16
01940         PERFORM 1650-GNP-EXEMP-DET-SEG
01941         IF PI-DB-GOOD-STATUS
01942            IF C150-SFSTAT EQUAL 'Q'
01943               IF C145-BLDGSHARES GREATER ZERO
01944                  COMPUTE PCT-SHARES =
01945                    C150-COOPSENSHRS / C145-BLDGSHARES
01946               ELSE
01947                  MOVE 'Y' TO NO-CALC-SW
01948               END-IF
01949               COMPUTE BASE-DIFF =
01950                 C150-BSYR-EQVAL * PCT-SHARES
01951               COMPUTE CURR-DIFF =
01952                 CURR-ELIG-COMP-EAV * PCT-SHARES
01953               COMPUTE CURR-TAX-DIFF =
01954                 CURR-DIFF - BASE-DIFF
01955               IF CURR-TAX-DIFF POSITIVE
01956                  ADD CURR-TAX-DIFF TO FINAL-DIFF-TX-COMP
01957               END-IF
01958            END-IF
01959         END-IF
01960      END-PERFORM.
01961      IF EXEMP-DET-SEG-CTR EQUAL ZEROES
01962         MOVE 'NO-MATCHING DETAIL SEGMENT' TO DL1-MESSG
01963         PERFORM 1250-FORMAT-REPORT-1.
01964
01965  1050-STEP13.
01966      IF NO-CALC-SW EQUAL 'Y'
01967         MOVE '1' TO QUALIFY-SW
01968         MOVE 'CANNOT BE CALCULATED PROGRAMMATICALLY' TO
01969              DL1-MESSG
01970         PERFORM 1250-FORMAT-REPORT-1.
01971      PERFORM 1060-CHECK-MAST-BASE-YEAR
01972      PERFORM 1070-STEP12.
01973
01974  1060-CHECK-MAST-BASE-YEAR.
01975      IF C145-BASVALYR NOT LESS PR-FR-BSYR-N AND
01976         C145-BASVALYR NOT GREATER PR-TO-BSYR-N
01977         IF TOT-BASE-COMP-EAV EQUAL ZERO
01978            MOVE PR-CURR-N TO BASE-YR
01979         ELSE
01980            MOVE PR-CURR-N TO BASE-YR
01981            MOVE TOT-BASE-COMP-EAV TO BASE-ELIG-COMP-EAV
01982            MOVE 'Y' TO BYPASS-EQFILE-SW.
01983
01984  1070-STEP12.
01985      IF NOT BYPASS-EQFILE
01986         PERFORM 350-READ-EQUALFCT
01987         IF NORMAL-STATUS
01988            COMPUTE BASE-VAL-EAV ROUNDED =
01989                    BASE-VAL-AV * EQ-FACTOR
01990            COMPUTE BASE-ELIG-COMP-EAV ROUNDED =
01991                    BASE-ELIG-COMP-AV * EQ-FACTOR
01992         ELSE
01993            IF RECORD-NOT-FOUND
01994               MOVE 'Y' TO NO-CALC-SW
01995               ADD 1    TO CURR-NO-CALC-CTR
01996               MOVE 'Q' TO MESSG-SW.
01997      COMPUTE 288-EXPIRE-EAV ROUNDED = 288-EXPIRE-AV *
01998                                       PR-EQ-FCTR-N
01999      ADD BASE-ELIG-COMP-EAV 288-EXPIRE-EAV
02000          288-OVER-30000-EAV GIVING TOT-BASE-COMP-EAV
02001      COMPUTE CURR-BASE-VAL-EAV  ROUNDED = CURR-VAL-AV *
02002                                           PR-EQ-FCTR-N
02003      COMPUTE CURR-ELIG-COMP-EAV ROUNDED = CURR-ELIG-COMP-AV *
02004                                           PR-EQ-FCTR-N
02005      SUBTRACT TOT-BASE-COMP-EAV FROM CURR-ELIG-COMP-EAV GIVING
02006               CURR-NOT-ELIG-EAV
02007      IF C145-SFSHARES EQUAL ZERO
02008         IF CURR-NOT-ELIG-EAV NEGATIVE
02009            MOVE ZEROES TO FINAL-DIFF-TX-COMP
02010         ELSE
02011            IF CURR-NOT-ELIG-EAV POSITIVE OR
02012               CURR-NOT-ELIG-EAV EQUAL ZERO
02013                  MOVE CURR-NOT-ELIG-EAV TO FINAL-DIFF-TX-COMP
02014            END-IF
02015         END-IF
02016         IF QUALIFY-SW EQUAL 1
02017            PERFORM 1100-STEP16
02018         ELSE
02019            PERFORM 1150-STEP17
02020         END-IF
02021      ELSE
02022         PERFORM 1010-CHECK-BASE-YR-GN-DET-SEG
02023         PERFORM 1150-STEP17.
02024
02025  1100-STEP16.
02026      IF TOT-BASE-COMP-EAV EQUAL ZEROES OR
02027         CURR-BASE-VAL-EAV EQUAL ZEROES
02028         IF TOT-BASE-COMP-EAV EQUAL ZEROES
02029            MOVE 'TOTAL ELIG COMPARISON EAV IS EQUAL TO ZERO' TO
02030                 DL1-MESSG
02031            PERFORM 1250-FORMAT-REPORT-1
02032         END-IF
02033         IF CURR-BASE-VAL-EAV EQUAL ZEROES
02034            MOVE 'TOTAL CURRENT BASE VALUE EAV IS EQUAL TO ZERO' T
02035                 DL1-MESSG
02036            PERFORM 1250-FORMAT-REPORT-1
02037         END-IF
02038      ELSE
02039         IF 288-EXPIRE-EAV     GREATER ZEROES OR
02040            288-OVER-30000-EAV GREATER ZEROES
02041            IF 288-EXPIRE-EAV GREATER ZEROES
02042               MOVE '288 EXPIRE VALUE GREATER THAN ZERO' TO
02043                    DL1-MESSG
02044               PERFORM 1300-FORMAT-REPORT-2
02045            END-IF
02046            IF 288-OVER-30000-EAV GREATER ZEROES
02047               MOVE '288 OVER 30,000 GREATER THAN ZERO' TO
02048                    DL1-MESSG
02049               PERFORM 1300-FORMAT-REPORT-2
02050            END-IF
02051         ELSE
02052            IF FINAL-DIFF-TX-COMP EQUAL ZEROES
02053               MOVE 'FINAL EAV TAX COMPUTATION EQUAL TO ZERO' TO
02054                    DL1-MESSG
02055               PERFORM 1350-FORMAT-REPORT-3
02056            END-IF
02057         END-IF
02058      END-IF.
02059      PERFORM 1150-STEP17.
02060
02061  1150-STEP17.
02062      IF RETURN-CODE NOT EQUAL 16
02063         PERFORM 1675-GU-ROOT-ASS-EX-MAST-SEG
02064         IF PI-DB-GOOD-STATUS
02065            PERFORM 1200-UPDATE-EXEMP-MAST-SEG
02066            PERFORM 1725-CREATE-SNRFREZ-RECORD
02067               THRU 1725-ROUT-END.
02068
02069  1200-UPDATE-EXEMP-MAST-SEG.
02070       IF NO-CALC-SW EQUAL 'Y'
02071          MOVE NO-CALC-SW         TO C145-BVYNOCALCIND
02072       ELSE
02073          MOVE NO-CALC-SW         TO C145-BVYNOCALCIND
02074          MOVE BASE-YR            TO C145-BASVALYR
02075          MOVE BASE-YR-CLS        TO C145-BVYCLS
02076          MOVE BASE-VAL-AV        TO C145-BVYFULLAV
02077          MOVE BASE-ELIG-COMP-AV  TO C145-BVYELCOMPFULLAV
02078          MOVE 288-OVER-30000-AV  TO C145-288OVRLIMAV
02079          MOVE CURR-YR-CLS        TO C145-CYCLS
02080          MOVE CURR-VAL-AV        TO C145-CYFULLAV
02081          MOVE CURR-ELIG-COMP-AV  TO C145-CYELCOMPAV
02082          MOVE 288-OVER-30000-EAV TO C145-288OVRLIMEV
02083          MOVE 288-EXPIRE-AV      TO C145-288EXPAV
02084          MOVE 288-EXPIRE-EAV     TO C145-288EXPEV
02085          MOVE FINAL-DIFF-TX-COMP TO C145-CYFNLEVDIFF
02086          MOVE BASE-VAL-EAV       TO C145-BVYEV
02087          MOVE FARM-SW            TO C145-CYFARMIND
02088          MOVE CURR-ELIG-COMP-EAV TO C145-CYELCOMPEV
02089          MOVE CURR-NOT-ELIG-AV   TO C145-CYNOTELAV
02090          MOVE CURR-BASE-VAL-EAV  TO C145-CYFULLEV
02091          MOVE TOT-BASE-COMP-EAV  TO C145-BVYTOTELCOMPEV
02092          MOVE CURR-NOT-ELIG-EAV  TO C145-CYNOTELEV.
02093          MOVE HOLD-CALC-TYP      TO C145-CALC-TYP.
02094          MOVE CURR-YR-CLS TO WS-CURR-YR-CLS
02095          IF BASE-YR EQUAL '1993'
02096             ADD 1 TO BASE-YR-1993-CTR
02097          ELSE
02098          IF BASE-YR EQUAL '1994'
02099             ADD 1 TO BASE-YR-1994-CTR
02100          ELSE
02101          IF BASE-YR EQUAL '1995'
02102             ADD 1 TO BASE-YR-1995-CTR
02103          ELSE
02104          IF BASE-YR EQUAL '1996'
02105             ADD 1 TO BASE-YR-1996-CTR
02106          ELSE
02107          IF BASE-YR EQUAL '1997'
02108             ADD 1 TO BASE-YR-1997-CTR
02109          ELSE
02110          IF BASE-YR EQUAL '1998'
02111             ADD 1 TO BASE-YR-1998-CTR
02112          ELSE
02113          IF BASE-YR EQUAL '1999'
02114             ADD 1 TO BASE-YR-1999-CTR
02115          ELSE
02116          IF BASE-YR EQUAL '2000'
02117             ADD 1 TO BASE-YR-2000-CTR
02118          ELSE
02119          IF BASE-YR EQUAL '2001'
02120             ADD 1 TO BASE-YR-2001-CTR
02121          ELSE
02122          IF BASE-YR EQUAL '2002'
02123             ADD 1 TO BASE-YR-2002-CTR
02124          ELSE
02125          IF BASE-YR EQUAL '2003'
02126             ADD 1 TO BASE-YR-2003-CTR
02127          ELSE
02128          IF BASE-YR EQUAL '2004'
02129             ADD 1 TO BASE-YR-2004-CTR
02130          ELSE
02131          IF BASE-YR EQUAL '2005'
02132             ADD 1 TO BASE-YR-2005-CTR
02133          ELSE
02134          IF BASE-YR EQUAL '2006'
02135             ADD 1 TO BASE-YR-2006-CTR
02136          ELSE
02137          IF BASE-YR EQUAL '2007'
02138             ADD 1 TO BASE-YR-2007-CTR
02139          ELSE
02140          IF BASE-YR EQUAL '2008'
02141             ADD 1 TO BASE-YR-2008-CTR
02142          ELSE
02143          IF BASE-YR EQUAL '2009'
02144             ADD 1 TO BASE-YR-2009-CTR
02145          ELSE
02146          IF BASE-YR EQUAL '2010'
02147             ADD 1 TO BASE-YR-2010-CTR
02148          ELSE
02149          IF BASE-YR EQUAL '2011'
02150             ADD 1 TO BASE-YR-2011-CTR
02151          ELSE
02152          IF BASE-YR EQUAL '2012'
02153             ADD 1 TO BASE-YR-2012-CTR
02154          ELSE
02155          IF BASE-YR EQUAL '2013'
02156             ADD 1 TO BASE-YR-2013-CTR
02157          ELSE
02158          IF BASE-YR EQUAL '2014'
02159             ADD 1 TO BASE-YR-2014-CTR
02160          ELSE
02161          IF BASE-YR EQUAL '2015'
02162             ADD 1 TO BASE-YR-2015-CTR
02163          ELSE
02164          IF BASE-YR EQUAL '2016'
02165             ADD 1 TO BASE-YR-2016-CTR
02166          ELSE
02167          IF BASE-YR EQUAL '2017'
02168             ADD 1 TO BASE-YR-2017-CTR
02169          ELSE
02170          IF BASE-YR EQUAL '2018'
02171             ADD 1 TO BASE-YR-2018-CTR
02172          ELSE
02173             ADD 1 TO BASE-YR-OTHR-CTR.
02174       IF VALID-CURR-YR-CLS
02175          OR M-CLS GREATER 201 AND M-CLS LESS 300
02176          OR M-CLS EQUAL 517
02177          OR M-CLS EQUAL 597
02178          PERFORM 1360-FORMAT-REPORT-4.
02179
02180  1250-FORMAT-REPORT-1.
02181      MOVE M-VOL       TO DL1-VOL
02182      MOVE M-PROP      TO DL1-PROP
02183      INSPECT DL1-PROP REPLACING ALL SPACES BY '-'
02184      MOVE M-TXTYP     TO DL1-TXTYP
02185      MOVE M-CLS       TO DL1-CLS
02186      INSPECT DL1-CLS  REPLACING ALL SPACES BY '-'
02187      IF LN-CTR-1 GREATER 57
02188         PERFORM 1400-HDR-RTN-1.
02189      WRITE PRINT-REC-1 FROM DETAIL1 AFTER ADVANCING 2 LINES
02190      ADD 2            TO LN-CTR-1.
02191
02192  1300-FORMAT-REPORT-2.
02193      MOVE M-VOL       TO DL1-VOL
02194      MOVE M-PROP      TO DL1-PROP
02195      INSPECT DL1-PROP REPLACING ALL SPACES BY '-'
02196      MOVE M-TXTYP     TO DL1-TXTYP
02197      MOVE M-CLS       TO DL1-CLS
02198      INSPECT DL1-CLS  REPLACING ALL SPACES BY '-'
02199      IF LN-CTR-2 GREATER 57
02200         PERFORM 1425-HDR-RTN-2.
02201      WRITE PRINT-REC-2 FROM DETAIL1 AFTER ADVANCING 2 LINES
02202      ADD 2            TO LN-CTR-2.
02203
02204  1350-FORMAT-REPORT-3.
02205      MOVE M-VOL       TO DL1-VOL
02206      MOVE M-PROP      TO DL1-PROP
02207      INSPECT DL1-PROP REPLACING ALL SPACES BY '-'
02208      MOVE M-TXTYP     TO DL1-TXTYP
02209      MOVE M-CLS       TO DL1-CLS
02210      INSPECT DL1-CLS  REPLACING ALL SPACES BY '-'
02211      IF LN-CTR-3 GREATER 57
02212         PERFORM 1450-HDR-RTN-3.
02213      WRITE PRINT-REC-3 FROM DETAIL1 AFTER ADVANCING 2 LINES
02214      ADD 2            TO LN-CTR-3.
02215
02216  1360-FORMAT-REPORT-4.
02217      MOVE M-PROP            TO DL4-PROP
02218      INSPECT DL4-PROP REPLACING ALL SPACES BY '-'
02219      MOVE CURR-YR-CLS       TO DL4-CURR-YR-CLS
02220      INSPECT DL4-CURR-YR-CLS REPLACING ALL SPACES BY '-'
02221      MOVE BASE-YR           TO DL4-BASE-YR
02222      MOVE NO-CALC-SW        TO DL4-NO-CALC-SW
02223      MOVE TOT-BASE-COMP-EAV TO DL4-TOT-BASE-EAV
02224      MOVE CURR-ELIG-COMP-EAV TO DL4-CURR-ELIG-EAV
02225      MOVE FINAL-DIFF-TX-COMP TO DL4-FINAL-DIFF-TX
02226      MOVE BASE-VAL-AV       TO DL4-BASE-VAL-AV
02227      MOVE CURR-VAL-AV       TO DL4-CURR-VAL-AV
02228      MOVE CURR-NOT-ELIG-EAV TO DL4-CURR-NOT-ELIG-EAV
02229      IF LN-CTR-4 GREATER 57
02230         PERFORM 1460-HDR-RTN-4.
02231      WRITE PRINT-REC-4 FROM DETAIL4 AFTER ADVANCING 2 LINES
02232      ADD 2            TO LN-CTR-4.
02233
02234  1400-HDR-RTN-1.
02235      ADD 1         TO PG-CTR-1
02236      MOVE PG-CTR-1 TO H1-PAGE
02237      MOVE '1'      TO H2-REPORT-TYP
02238      MOVE '                          ERROR REPORT #1'
02239                    TO H2-REPORT-TITLE
02240      WRITE PRINT-REC-1 FROM HEAD1 AFTER ADVANCING PAGE
02241      WRITE PRINT-REC-1 FROM HEAD2 AFTER ADVANCING 2
02242      WRITE PRINT-REC-1 FROM HEAD3 AFTER ADVANCING 2
02243      MOVE 5        TO LN-CTR-1.
02244
02245  1425-HDR-RTN-2.
02246      ADD 1                TO PG-CTR-2
02247      MOVE PG-CTR-2        TO H1-PAGE
02248      MOVE '2'             TO H2-REPORT-TYP
02249      MOVE '288 EXPIRE EAV AND 288 OVER 30,000 EAV GREATER THAN ZE
02250 -         'O - REPORT #2' TO H2-REPORT-TITLE
02251      WRITE PRINT-REC-2 FROM HEAD1 AFTER ADVANCING PAGE
02252      WRITE PRINT-REC-2 FROM HEAD2 AFTER ADVANCING 2
02253      WRITE PRINT-REC-2 FROM HEAD3 AFTER ADVANCING 2
02254      MOVE 5               TO LN-CTR-2.
02255
02256  1450-HDR-RTN-3.
02257      ADD 1          TO PG-CTR-3
02258      MOVE PG-CTR-3  TO H1-PAGE
02259      MOVE '3'       TO H2-REPORT-TYP
02260      MOVE '   FINAL EAV DIFFERENCE TAX COMPUTATION EQUAL TO ZERO
02261 -         ' REPORT #3' TO H2-REPORT-TITLE
02262      WRITE PRINT-REC-3 FROM HEAD1 AFTER ADVANCING PAGE
02263      WRITE PRINT-REC-3 FROM HEAD2 AFTER ADVANCING 2
02264      WRITE PRINT-REC-3 FROM HEAD3 AFTER ADVANCING 2
02265      MOVE 5         TO LN-CTR-3.
02266
02267  1460-HDR-RTN-4.
02268      ADD 1          TO PG-CTR-4
02269      MOVE PG-CTR-4  TO H1-PAGE
02270      MOVE '4'       TO H2-REPORT-TYP
02271      MOVE '                SENIOR FREEZE BASE VALUE UPDATE REPORT
02272                        TO H2-REPORT-TITLE
02273      WRITE PRINT-REC-4 FROM HEAD1 AFTER ADVANCING PAGE
02274      WRITE PRINT-REC-4 FROM HEAD2 AFTER ADVANCING 2
02275      WRITE PRINT-REC-4 FROM HEAD3-4 AFTER ADVANCING 2
02276      WRITE PRINT-REC-4 FROM HEAD4 AFTER ADVANCING 1
02277      MOVE 6         TO LN-CTR-4.
02278
02279  1500-FORMAT-TOTALS.
02280      IF LN-CTR-1 GREATER 45
02281         PERFORM 1400-HDR-RTN-1.
02282      MOVE 'TOTAL PRIOR YEAR RECORDS READ'           TO T1-MESSG
02283      MOVE PRIOR-ASSMT-REC-CTR                       TO T1-TOTAL
02284      WRITE PRINT-REC-1 FROM TOTAL1 AFTER ADVANCING 3
02285      MOVE 'TOTAL CURRENT YEAR RECORDS READ'         TO T1-MESSG
02286      MOVE CURR-ASSMT-REC-CTR                        TO T1-TOTAL
02287      WRITE PRINT-REC-1 FROM TOTAL1 AFTER ADVANCING 1
02288      MOVE 'TOTAL EXPIRED RECORDS READ'              TO T1-MESSG
02289      MOVE EXPIRED-ASSMT-REC-CTR                     TO T1-TOTAL
02290      WRITE PRINT-REC-1 FROM TOTAL1 AFTER ADVANCING 1
02291      MOVE 'TOTAL PRIOR AND CURRENT RECORDS MATCHED' TO T1-MESSG
02292      MOVE CURR-PRIOR-MATCH-CTR                      TO T1-TOTAL
02293      WRITE PRINT-REC-1 FROM TOTAL1 AFTER ADVANCING 1
02294      MOVE 'TOTAL CURRENT RECORDS UNMATCHED'         TO T1-MESSG
02295      MOVE CURR-UNMATCH-CTR                          TO T1-TOTAL
02296      WRITE PRINT-REC-1 FROM TOTAL1 AFTER ADVANCING 1
02297      MOVE 'TOTAL CURRENT RECORDS WITH NO-CALC INDICATOR'
02298                                                     TO T1-MESSG
02299      MOVE CURR-NO-CALC-CTR                          TO T1-TOTAL
02300      WRITE PRINT-REC-1 FROM TOTAL1 AFTER ADVANCING 1
02301      MOVE 'TOTAL EXPIRED RECORDS SELECTED'          TO T1-MESSG
02302      MOVE EXPIRED-ASSMT-SEL-CTR                     TO T1-TOTAL
02303      WRITE PRINT-REC-1 FROM TOTAL1 AFTER ADVANCING 1
02304      MOVE 'TOTAL MASTER SEGMENTS READ   '           TO T1-MESSG
02305      MOVE EXEMP-MAST-REPL-CTR                       TO T1-TOTAL
02306      WRITE PRINT-REC-1 FROM TOTAL1 AFTER ADVANCING 1
02307      MOVE 'TOTAL SNRFREZ RECORDS WRITTEN'           TO T1-MESSG
02308      MOVE SNRFREZ-REC-CTR                           TO T1-TOTAL
02309      WRITE PRINT-REC-1 FROM TOTAL1 AFTER ADVANCING 1
02310      MOVE 'TOTAL RECORDS WITH UNACCOUNTED CLASS CONDITIONS'
02311                                                     TO T1-MESSG
02312      MOVE INVALID-CLS-COND-CTR                      TO T1-TOTAL
02313      WRITE PRINT-REC-1 FROM TOTAL1 AFTER ADVANCING 1
02314      IF LN-CTR-2 GREATER 45
02315         PERFORM 1425-HDR-RTN-2.
02316      MOVE 'TOTAL PRIOR YEAR RECORDS READ'           TO T1-MESSG
02317      MOVE PRIOR-ASSMT-REC-CTR                       TO T1-TOTAL
02318      WRITE PRINT-REC-2 FROM TOTAL1 AFTER ADVANCING 3
02319      MOVE 'TOTAL CURRENT YEAR RECORDS READ'         TO T1-MESSG
02320      MOVE CURR-ASSMT-REC-CTR                        TO T1-TOTAL
02321      WRITE PRINT-REC-2 FROM TOTAL1 AFTER ADVANCING 1
02322      MOVE 'TOTAL EXPIRED RECORDS READ'              TO T1-MESSG
02323      MOVE EXPIRED-ASSMT-REC-CTR                     TO T1-TOTAL
02324      WRITE PRINT-REC-2 FROM TOTAL1 AFTER ADVANCING 1
02325      MOVE 'TOTAL PRIOR AND CURRENT RECORDS MATCHED' TO T1-MESSG
02326      MOVE CURR-PRIOR-MATCH-CTR                      TO T1-TOTAL
02327      WRITE PRINT-REC-2 FROM TOTAL1 AFTER ADVANCING 1
02328      MOVE 'TOTAL CURRENT RECORDS UNMATCHED'         TO T1-MESSG
02329      MOVE CURR-UNMATCH-CTR                          TO T1-TOTAL
02330      WRITE PRINT-REC-2 FROM TOTAL1 AFTER ADVANCING 1
02331      MOVE 'TOTAL CURRENT RECORDS WITH NO-CALC INDICATOR'
02332                                                     TO T1-MESSG
02333      MOVE CURR-NO-CALC-CTR                          TO T1-TOTAL
02334      WRITE PRINT-REC-2 FROM TOTAL1 AFTER ADVANCING 1
02335      MOVE 'TOTAL EXPIRED RECORDS SELECTED'          TO T1-MESSG
02336      MOVE EXPIRED-ASSMT-SEL-CTR                     TO T1-TOTAL
02337      WRITE PRINT-REC-2 FROM TOTAL1 AFTER ADVANCING 1
02338      MOVE 'TOTAL MASTER SEGMENTS READ   '           TO T1-MESSG
02339      MOVE EXEMP-MAST-REPL-CTR                       TO T1-TOTAL
02340      WRITE PRINT-REC-2 FROM TOTAL1 AFTER ADVANCING 1
02341      MOVE 'TOTAL SNRFREZ RECORDS WRITTEN'           TO T1-MESSG
02342      MOVE SNRFREZ-REC-CTR                           TO T1-TOTAL
02343      WRITE PRINT-REC-2 FROM TOTAL1 AFTER ADVANCING 1
02344      MOVE 'TOTAL RECORDS WITH UNACCOUNTED CLASS CONDITIONS'
02345                                                     TO T1-MESSG
02346      MOVE INVALID-CLS-COND-CTR                      TO T1-TOTAL
02347      WRITE PRINT-REC-2 FROM TOTAL1 AFTER ADVANCING 1
02348      IF LN-CTR-3 GREATER 45
02349         PERFORM 1450-HDR-RTN-3.
02350      MOVE 'TOTAL PRIOR YEAR RECORDS READ'           TO T1-MESSG
02351      MOVE PRIOR-ASSMT-REC-CTR                       TO T1-TOTAL
02352      WRITE PRINT-REC-3 FROM TOTAL1 AFTER ADVANCING 3
02353      MOVE 'TOTAL CURRENT YEAR RECORDS READ'         TO T1-MESSG
02354      MOVE CURR-ASSMT-REC-CTR                        TO T1-TOTAL
02355      WRITE PRINT-REC-3 FROM TOTAL1 AFTER ADVANCING 1
02356      MOVE 'TOTAL EXPIRED RECORDS READ'              TO T1-MESSG
02357      MOVE EXPIRED-ASSMT-REC-CTR                     TO T1-TOTAL
02358      WRITE PRINT-REC-3 FROM TOTAL1 AFTER ADVANCING 1
02359      MOVE 'TOTAL PRIOR AND CURRENT RECORDS MATCHED' TO T1-MESSG
02360      MOVE CURR-PRIOR-MATCH-CTR                      TO T1-TOTAL
02361      WRITE PRINT-REC-3 FROM TOTAL1 AFTER ADVANCING 1
02362      MOVE 'TOTAL CURRENT RECORDS UNMATCHED'         TO T1-MESSG
02363      MOVE CURR-UNMATCH-CTR                          TO T1-TOTAL
02364      WRITE PRINT-REC-3 FROM TOTAL1 AFTER ADVANCING 1
02365      MOVE 'TOTAL CURRENT RECORDS WITH NO-CALC INDICATOR'
02366                                                     TO T1-MESSG
02367      MOVE CURR-NO-CALC-CTR                          TO T1-TOTAL
02368      WRITE PRINT-REC-3 FROM TOTAL1 AFTER ADVANCING 1
02369      MOVE 'TOTAL EXPIRED RECORDS SELECTED'          TO T1-MESSG
02370      MOVE EXPIRED-ASSMT-SEL-CTR                     TO T1-TOTAL
02371      WRITE PRINT-REC-3 FROM TOTAL1 AFTER ADVANCING 1
02372      MOVE 'TOTAL MASTER SEGMENTS READ   '           TO T1-MESSG
02373      MOVE EXEMP-MAST-REPL-CTR                       TO T1-TOTAL
02374      WRITE PRINT-REC-3 FROM TOTAL1 AFTER ADVANCING 1
02375      MOVE 'TOTAL SNRFREZ RECORDS WRITTEN'           TO T1-MESSG
02376      MOVE SNRFREZ-REC-CTR                           TO T1-TOTAL
02377      WRITE PRINT-REC-3 FROM TOTAL1 AFTER ADVANCING 1
02378      MOVE 'TOTAL RECORDS WITH UNACCOUNTED CLASS CONDITIONS'
02379                                                     TO T1-MESSG
02380      MOVE INVALID-CLS-COND-CTR                      TO T1-TOTAL
02381      WRITE PRINT-REC-3 FROM TOTAL1 AFTER ADVANCING 1.
02382
02383  1600-GU-ROOT-ASSESS-SEG.
02384      MOVE M-PROP           TO LVL1-ARG
02385      MOVE PR-PROC-YR (3:2) TO LVL2-PROCYR, LVL2-TXYR
02386      MOVE M-TXTYP          TO LVL2-TXTYP
02387      CALL 'CBLTDLI' USING GU
02388                           PI-PCB1
02389                           PY-ASMTDATA
02390                           LVL1-QUAL-SSA
02391                           LVL2-QUAL-SSA.
02392      IF PI-DB-GOOD-STATUS
02393         NEXT SENTENCE
02394      ELSE
02395         IF PI-SEG-NOTFND AND PI-SEGMENT-LVL EQUAL ZEROES
02396            DISPLAY 'ROOT SEGMENT NOT FOUND USING A GU  ' LVL1-ARG
02397            PERFORM 1900-DISPLAY-PI-PCB.
02398 *       ELSE
02399 *          IF PI-SEG-NOTFND AND PI-SEGMENT-LVL EQUAL '01'
02400 *             DISPLAY 'ASSESSMENT SEGMENT NOT FOUND USING A GU  '
02401 *                     LVL1-ARG ' ' LVL2-ARG
02402 *          ELSE
02403 *             DISPLAY 'ROOT/ASSESSMENT SEGMENT GU ERROR  '
02404 *                     LVL1-ARG ' ' LVL2-ARG.
02405
02406  1625-GNP-EXEMP-MAST-SEG.
02407      CALL 'CBLTDLI' USING GNP
02408                           PI-PCB1
02409                           C145-SENFRZMASTER
02410                           LVL3-UNQUAL-SSA.
02411      IF PI-DB-GOOD-STATUS
02412         IF C150-SHARES GREATER ZEROES
02413            MOVE 'Y' TO COOP-SW
02414         ELSE
02415            NEXT SENTENCE
02416      ELSE
02417          IF PI-SEG-NOTFND
02418             NEXT SENTENCE
02419          ELSE
02420             DISPLAY 'EXEMPTION MASTER SEGMENT GNP ERROR  '
02421                     LVL1-ARG ' ' LVL2-ARG
02422             PERFORM 1900-DISPLAY-PI-PCB.
02423
02424  1650-GNP-EXEMP-DET-SEG.
02425      CALL 'CBLTDLI' USING GNP
02426                           PI-PCB1
02427                           C150-SENFRZDETAIL
02428                           LVL4-UNQUAL-SSA.
02429      IF PI-DB-GOOD-STATUS
02430         ADD 1 TO EXEMP-DET-SEG-CTR, EXEMP-MAST-REPL-CTR
02431      ELSE
02432         IF PI-SEG-NOTFND
02433            MOVE 1 TO EXEMP-DET-EOF-SW
02434         ELSE
02435            DISPLAY 'EXEMPTION DETAIL SEGMENT GNP ERROR  '
02436                    LVL1-ARG ' ' LVL2-ARG ' ' C145-RECCODE
02437            PERFORM 1900-DISPLAY-PI-PCB.
02438
02439  1675-GU-ROOT-ASS-EX-MAST-SEG.
02440      MOVE M-PROP           TO LVL1-ARG
02441      MOVE PR-PROC-YR (3:2) TO LVL2-PROCYR, LVL2-TXYR
02442      MOVE M-TXTYP          TO LVL2-TXTYP
02443      MOVE C145-RECCODE     TO LVL3-ARG
02444      CALL 'CBLTDLI' USING GU
02445                           PI-PCB1
02446                           C145-SENFRZMASTER
02447                           LVL1-QUAL-SSA
02448                           LVL2-QUAL-SSA
02449                           LVL3-QUAL-SSA.
02450      IF PI-DB-GOOD-STATUS
02451         NEXT SENTENCE
02452      ELSE
02453         IF PI-SEG-NOTFND AND PI-SEGMENT-LVL EQUAL ZEROES
02454            DISPLAY 'ROOT SEGMENT NOT FOUND USING GU  ' LVL1-ARG
02455         ELSE
02456            IF PI-SEG-NOTFND AND PI-SEGMENT-LVL EQUAL '01'
02457               DISPLAY 'ASSESSMENT SEGMENT NOT FOUND USING GU  '
02458                       LVL1-ARG ' ' LVL2-ARG
02459            ELSE
02460               IF PI-SEG-NOTFND AND PI-SEGMENT-LVL EQUAL '02'
02461                  DISPLAY 'EXEMPTION MASTER SEGMENT NOT FOUND USIN
02462 -                        ' GU  ' LVL1-ARG ' ' LVL2-ARG ' '
02463                          LVL3-ARG
02464               ELSE
02465                  DISPLAY 'ROOT/ASSESSMENT/EXEMPTION MASTER SEGMEN
02466 -                        ' GU ERROR  ' LVL1-ARG ' ' LVL2-ARG ' '
02467                          LVL3-ARG
02468                  PERFORM 1900-DISPLAY-PI-PCB.
02469
02470  1725-CREATE-SNRFREZ-RECORD.
02471      MOVE SPACES TO SF-REC.
02472      MOVE ZEROES TO SF-TOWN, SF-VOL, SF-PROP,
02473                     SF-BLDGUNITS, SF-BLDGSHARES,
02474                     SF-PROPCT, SF-KEYPCL, SF-SPLTCD,
02475                     SF-OCCFAC, SF-288EXPAV, SF-288EXPEV,
02476                     SF-288OVRLIMAV, SF-288OVRLIMEV,
02477                     SF-BASVALYR, SF-BVYCLS, SF-BVYFULLAV,
02478                     SF-BVYEV, SF-BVYELCOMPFULLAV,
02479                     SF-BVYTOTELCOMPEV, SF-CYCLS, SF-CYFULLAV,
02480                     SF-CYELCOMPAV, SF-CYELCOMPEV,
02481                     SF-CYNOTELAV,  SF-CYNOTELEV,
02482                     SF-CYFNLEVDIFF, SF-HOUNITS,
02483                     SF-HSUNITS, SF-SFSHARES, SF-CYFULLEV,
02484                     SF-DENIALDTE,  SF-FSTAPPDTE,
02485                     SF-LSTAPPDTE,  SF-QUALDTE,
02486                     SF-COOPSENSHRS, SF-PCTSENSHRS,
02487                     SF-BASE-YR,    SF-BSYR-EQVAL,
02488                     SF-OTHBEN,     SF-VETBEN.
02489
02490      MOVE TXCD-HLD (1:2)  TO SF-TOWN.
02491      MOVE M-VOL TO SF-VOL.
02492      MOVE M-PROP TO SF-PROP.
02493      MOVE PR-PROC-YR  TO SF-PROC-YR, SF-TAX-YR.
02494      MOVE '0' TO SF-TAX-TYPE.
02495      MOVE C145-RECCODE TO SF-RECCODE.
02496      MOVE C145-BLDGUNITS TO SF-BLDGUNITS.
02497      MOVE C145-BLDGSHARES TO SF-BLDGSHARES.
02498      MOVE C145-PROPCT TO SF-PROPCT.
02499      MOVE C145-KEYPCL TO SF-KEYPCL.
02500      MOVE C145-SPLTCD TO SF-SPLTCD.
02501      MOVE C145-OCCFAC TO SF-OCCFAC.
02502      MOVE NO-CALC-SW TO SF-BVYNOCALCIND.
02503      MOVE BASE-YR TO SF-BASVALYR.
02504      MOVE BASE-YR-CLS TO N-YR-CLS.
02505      MOVE N-YR-CLS TO SF-BVYCLS.
02506      MOVE BASE-VAL-AV TO SF-BVYFULLAV.
02507      MOVE BASE-ELIG-COMP-AV TO SF-BVYELCOMPFULLAV.
02508      MOVE 288-OVER-30000-AV TO SF-288OVRLIMAV.
02509      MOVE CURR-YR-CLS TO N-YR-CLS.
02510      MOVE N-YR-CLS TO SF-CYCLS.
02511      MOVE CURR-VAL-AV  TO SF-CYFULLAV.
02512      MOVE CURR-ELIG-COMP-AV TO SF-CYELCOMPAV.
02513      MOVE 288-OVER-30000-EAV TO SF-288OVRLIMEV.
02514      MOVE 288-EXPIRE-AV TO SF-288EXPAV.
02515      MOVE 288-EXPIRE-EAV TO SF-288EXPEV.
02516      MOVE FINAL-DIFF-TX-COMP TO SF-CYFNLEVDIFF.
02517      MOVE BASE-VAL-EAV TO SF-BVYEV.
02518      MOVE FARM-SW  TO SF-CYFARMIND.
02519      MOVE CURR-ELIG-COMP-EAV TO SF-CYELCOMPEV.
02520      MOVE CURR-NOT-ELIG-AV   TO SF-CYNOTELAV.
02521      MOVE CURR-BASE-VAL-EAV TO SF-CYFULLEV.
02522      MOVE TOT-BASE-COMP-EAV TO SF-BVYTOTELCOMPEV.
02523      MOVE CURR-NOT-ELIG-EAV TO SF-CYNOTELEV.
02524      MOVE C145-BVYMANCALCIND TO SF-BVYMANCALCIND.
02525      MOVE C145-HOUNITS TO SF-HOUNITS.
02526      MOVE C145-HSUNITS TO SF-HSUNITS.
02527      MOVE C145-SFSHARES TO SF-SFSHARES.
02528      MOVE HOLD-CALC-TYP TO SF-CALC-TYP.
02529      IF EXEMP-DET-SEG-CTR = +0
02530         GO TO 1725-WRITE.
02531      IF C150-BIRTHDTE NUMERIC
02532         MOVE C150-BIRTHDTE TO SF-BIRTHDTE.
02533      MOVE C150-SFSTAT TO SF-SFSTAT.
02534      IF C150-DENIALDTE NUMERIC
02535         MOVE C150-DENIALDTE TO SF-DENIALDTE.
02536      IF C150-FSTAPPDTE NUMERIC
02537         MOVE C150-FSTAPPDTE TO SF-FSTAPPDTE.
02538      IF C150-LSTAPPDTE NUMERIC
02539         MOVE C150-LSTAPPDTE TO SF-LSTAPPDTE.
02540      IF C150-QUALDTE   NUMERIC
02541         MOVE C150-QUALDTE TO SF-QUALDTE.
02542      IF C150-COOPSENSHRS  NUMERIC
02543         MOVE C150-COOPSENSHRS TO SF-COOPSENSHRS.
02544      IF   C150-PCTSENSHRS NUMERIC
02545         MOVE C150-PCTSENSHRS TO SF-PCTSENSHRS.
02546      MOVE C150-LIFECARE TO SF-LIFECARE.
02547      MOVE WS-PROC-YR TO SF-BASE-YR.
02548      MOVE C150-BSYR-IND TO SF-BSYR-IND.
02549      MOVE BASE-VAL-EAV TO SF-BSYR-EQVAL.
02550      IF   C150-OTHBEN NUMERIC
02551         MOVE C150-OTHBEN TO SF-OTHBEN.
02552      IF   C150-VETBEN  NUMERIC
02553         MOVE C150-VETBEN TO SF-VETBEN.
02554  1725-WRITE.
02555      WRITE SNRFREZ-REC.
02556      ADD +1 TO SNRFREZ-REC-CTR.
02557  1725-ROUT-END. EXIT.
02558      SKIP2
02559  1900-DISPLAY-PI-PCB.
02560      DISPLAY 'THE DBD NAME IS                         '
02561              PI-DBD-NAME
02562      DISPLAY 'THE SEGMENT LEVEL IS                    '
02563              PI-SEGMENT-LVL
02564      DISPLAY 'THE STATUS CODE IS                      '
02565              PI-STATUS-CODE
02566      DISPLAY 'THE PROCESSING OPTIONS ARE              '
02567              PI-PROC-OPT
02568      DISPLAY 'THE SEGMENT NAME IS                     '
02569              PI-NAME-FDBK
02570      DISPLAY 'THE LENGTH OF THE KEY FEEDBACK AREA IS  '
02571              PI-KEY-FDBK-LNG
02572      DISPLAY 'THE NUMBER OF SENSITIVE SEGMENTS ARE    ' PI-SEN-SE
02573      DISPLAY 'THE KEY FEEDBACK AREA IS                '
02574              PI-KEY-FB-ITEM
02575      MOVE 16 TO RETURN-CODE.
