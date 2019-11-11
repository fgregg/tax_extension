00001  IDENTIFICATION DIVISION.
00002  PROGRAM-ID. ASHMA850.
00003 *AUTHOR. RUBINGER/KALEMBA.
00004 *DATE-WRITTEN. 08/95.
00005 *REMARKS. ASHMA850 WILL COMPUTE THE SENIOR FREEZE BASE VALUE TAX
00006 *         DIFFERENCE.
00007
00008 * CHANGED 05/11/2006 BY TJM TO ADD A COUNTER, TESTS AND DISPLAYS
00009 *                        FOR THE YEAR 2005.
00010 * CHANGED 07/24/2007 BY KLF TO ADD A COUNTER, TESTS AND DISPLAYS
00011 *                        FOR THE YEAR 2006.
00012 * CHANGED 07/18/2007 BY KLF TO ADD A COUNTER, TESTS AND DISPLAYS
00013 *                        FOR THE YEAR 2007.
00014 * CHANGED 07/17/2009 BY TJM TO ADD A COUNTER, TESTS AND DISPLAYS
00015 *                        FOR THE YEAR 2008.
00016 * CHANGED 09/17/2010 BY TJM TO ADD A COUNTER, TESTS AND DISPLAYS
00017 *                        FOR THE YEAR 2009.
00018 * CHANGED 07/18/2011 BY TJM TO ADD A COUNTER, TESTS AND DISPLAYS
00019 *                        FOR THE YEAR 2010.
00020 * CHANGED 04/17/2012 BY KLF TO ADD A COUNTER, TESTS AND DISPLAYS
00021 *                        FOR THE YEAR 2011.
00022 * CHANGED 04/17/2013 BY KLF TO ADD A COUNTER, TESTS AND DISPLAYS
00023 *                        FOR THE YEAR 2012.
00024 * CHANGED 04/17/2014 BY JTS TO ADD A COUNTER, TESTS AND DISPLAYS
00025 *                        FOR THE YEAR 2013.
00026 * CHANGED 04/06/2015 BY TMB TO ADD A COUNTER, TESTS AND DISPLAYS
00027 *                        FOR THE YEAR 2014.
00028 * NOTE ALSO CHECK LINE
00029 * 'TOTAL RECDS WITH BASE YEAR OTHER THAN 1993 THRU 2016'
00030 *      TO PUT IN CURRENT ENDING YEAR
00031 *
00032 * ALSO CHANGE ASHMA857 SAME WAY
00033 *
00034 * CHANGED 04/07/2016 BY KLF TO ADD A COUNTER, TESTS AND DISPLAYS
00035 *                        FOR THE YEAR 2015.
00036 *
00037 * CHANGED 04/07/2017 BY TMB TO ADD A COUNTER, TESTS AND DISPLAYS
00038 *                        FOR THE YEAR 2016.
00039 *
00040 * CHANGED 04/08/2018 BY TMB TO ADD A COUNTER, TESTS AND DISPLAYS
00041 *                        FOR THE YEAR 2017.
00042 *
00043 * CHANGED 04/24/2019 BY GKW TO ADD A COUNTER, TESTS AND DISPLAYS
00044 *                        FOR THE YEAR 2018.
00045 *
00046  ENVIRONMENT DIVISION.
00047  CONFIGURATION SECTION.
00048  SOURCE-COMPUTER. IBM-370.
00049  OBJECT-COMPUTER. IBM-370.
00050  INPUT-OUTPUT SECTION.
00051  FILE-CONTROL.
00052      SELECT PARAM-LINE      ASSIGN TO UT-S-PARAM.
00053      SELECT CURR-ASSMT      ASSIGN TO UT-S-CURRASS.
00054      SELECT PRIOR-ASSMT     ASSIGN TO UT-S-PRIORASS.
00055      SELECT EXPIRED-ASSMT   ASSIGN TO UT-S-EXPIRASS.
00056      SELECT EQUALFCT        ASSIGN TO DA-EQUALFCT
00057                             RECORD KEY IS EQ-KEY
00058                             ORGANIZATION IS INDEXED
00059                             ACCESS IS RANDOM
00060                             FILE STATUS IS FILE-STATUS.
00061
00062      SELECT FINDER-FILE     ASSIGN TO UT-S-FINDER.
00063      SELECT PRINT-FILE-1    ASSIGN TO UT-S-PRINT1.
00064      SELECT PRINT-FILE-2    ASSIGN TO UT-S-PRINT2.
00065      SELECT PRINT-FILE-3    ASSIGN TO UT-S-PRINT3.
00066      SELECT PRINT-FILE-4    ASSIGN TO UT-S-PRINT4.
00067
00068  DATA DIVISION.
00069  FILE SECTION.
00070
00071  FD  PARAM-LINE
00072      RECORDING MODE IS F
00073      BLOCK CONTAINS 0 RECORDS
00074      RECORD CONTAINS 80 CHARACTERS
00075      LABEL RECORDS ARE STANDARD
00076      DATA RECORD IS PARAM-REC.
00077  01  PARAM-REC.
00078      03 PR-CICV             PIC X(4).
00079      03 PR-CICV-VAL         PIC X(4).
00080      03 PR-CICV-VAL-N       REDEFINES PR-CICV-VAL  PIC 9(4).
00081      03 PR-PROC-YR.
00082         05  PR-PROC-YR-N    PIC 9(4).
00083      03 PR-EQ-FCTR          PIC X(5).
00084      03 PR-EQ-FCTR-N        REDEFINES PR-EQ-FCTR   PIC 9V9(4).
00085      03 PR-FR-YR            PIC XX.
00086      03 PR-FR-YR-N          REDEFINES PR-FR-YR     PIC 99.
00087      03 PR-TO-YR            PIC XX.
00088      03 PR-TO-YR-N          REDEFINES PR-TO-YR     PIC 99.
00089      03 PR-FR-BSYR          PIC X(4).
00090      03 PR-FR-BSYR-N        REDEFINES PR-FR-BSYR   PIC 9(4).
00091      03 PR-TO-BSYR          PIC X(4).
00092      03 PR-TO-BSYR-N        REDEFINES PR-TO-BSYR   PIC 9(4).
00093      03 FILLER              PIC X(51).
00094
00095  FD  CURR-ASSMT
00096      BLOCK CONTAINS 0 CHARACTERS
00097      RECORD CONTAINS 122 TO 18706 CHARACTERS
00098      LABEL RECORDS ARE STANDARD
00099      RECORDING MODE IS S
00100      DATA RECORD IS CURR-ASSMT-REC.
00101  01  CURR-ASSMT-REC.
00102  COPY ASREASRD02.
00103
00104  FD  PRIOR-ASSMT
00105      BLOCK CONTAINS 0 CHARACTERS
00106      RECORD CONTAINS 122 TO 18706 CHARACTERS
00107      LABEL RECORDS ARE STANDARD
00108      RECORDING MODE IS S
00109      DATA RECORD IS PRIOR-ASSMT-REC.
00110  01  PRIOR-ASSMT-REC.
00111  COPY ASREASRD03.
00112
00113  FD  EXPIRED-ASSMT
00114      BLOCK CONTAINS 0 CHARACTERS
00115      RECORD CONTAINS 122 TO 18706 CHARACTERS
00116      LABEL RECORDS ARE STANDARD
00117      RECORDING MODE IS S
00118      DATA RECORD IS EXPIRED-ASSMT-REC.
00119  01  EXPIRED-ASSMT-REC.
00120  COPY ASREASRD09.
00121
00122  FD  EQUALFCT
00123      RECORD CONTAINS 21 CHARACTERS
00124 *    BLOCK CONTAINS 0 RECORDS
00125 *    LABEL RECORDS ARE STANDARD
00126      DATA RECORD IS EQUAL-REC.
00127  01  EQUAL-REC.
00128  COPY REBEQFRD01.
00129
00130  FD  FINDER-FILE
00131      RECORDING MODE IS F
00132      BLOCK CONTAINS 0 RECORDS
00133      RECORD CONTAINS 80 CHARACTERS
00134      LABEL RECORDS ARE STANDARD
00135      DATA RECORD IS FINDER-REC.
00136  01  FINDER-REC.
00137      03  FR-VOL             PIC XXX.
00138      03  FR-PROP            PIC X(14).
00139      03  FILLER             PIC X(63).
00140
00141  FD  PRINT-FILE-1
00142      RECORDING MODE IS F
00143      RECORD CONTAINS 133 CHARACTERS
00144      BLOCK CONTAINS 0 RECORDS
00145      LABEL RECORDS ARE STANDARD
00146      DATA RECORD IS PRINT-REC-1.
00147  01  PRINT-REC-1            PIC X(133).
00148
00149  FD  PRINT-FILE-2
00150      RECORDING MODE IS F
00151      RECORD CONTAINS 133 CHARACTERS
00152      BLOCK CONTAINS 0 RECORDS
00153      LABEL RECORDS ARE STANDARD
00154      DATA RECORD IS PRINT-REC-2.
00155  01  PRINT-REC-2            PIC X(133).
00156
00157  FD  PRINT-FILE-3
00158      RECORDING MODE IS F
00159      RECORD CONTAINS 133 CHARACTERS
00160      BLOCK CONTAINS 0 RECORDS
00161      LABEL RECORDS ARE STANDARD
00162      DATA RECORD IS PRINT-REC-3.
00163  01  PRINT-REC-3            PIC X(133).
00164
00165  FD  PRINT-FILE-4
00166      RECORDING MODE IS F
00167      RECORD CONTAINS 133 CHARACTERS
00168      BLOCK CONTAINS 0 RECORDS
00169      LABEL RECORDS ARE STANDARD
00170      DATA RECORD IS PRINT-REC-4.
00171  01  PRINT-REC-4            PIC X(133).
00172
00173  WORKING-STORAGE SECTION.
00174  77  CURR-ASSMT-REC-CTR     PIC S9(8) VALUE +0     BINARY.
00175  77  PRIOR-ASSMT-REC-CTR    PIC S9(8) VALUE +0     BINARY.
00176  77  WS-236-VALUE           PIC S9(8) VALUE +0     BINARY.
00177  77  WS-IMPRV-VALUE         PIC S9(8) VALUE +0     BINARY.
00178  77  WS-TYPE1-VALUE         PIC S9(8) VALUE +0     BINARY.
00179  77  EXPIRED-ASSMT-REC-CTR  PIC S9(8) VALUE +0     BINARY.
00180  77  EXPIRED-ASSMT-SEL-CTR  PIC S9(8) VALUE +0     BINARY.
00181  77  CURR-PRIOR-MATCH-CTR   PIC S9(8) VALUE +0     BINARY.
00182  77  CURR-UNMATCH-CTR       PIC S9(8) VALUE +0     BINARY.
00183  77  CURR-NO-CALC-CTR       PIC S9(8) VALUE +0     BINARY.
00184  77  EXEMP-MAST-REPL-CTR    PIC S9(8) VALUE +0     BINARY.
00185  77  EXEMP-DET-SEG-CTR      PIC S9(8) VALUE +0     BINARY.
00186  77  FINDER-REC-CTR         PIC S9(8) VALUE +0     BINARY.
00187  77  INVALID-CLS-COND-CTR   PIC S9(8) VALUE +0     BINARY.
00188  77  CICV-CTR               PIC S9(8) VALUE +99    BINARY.
00189  77  BASE-YR-1993-CTR       PIC S9(8) VALUE +0     BINARY.
00190  77  BASE-YR-1994-CTR       PIC S9(8) VALUE +0     BINARY.
00191  77  BASE-YR-1995-CTR       PIC S9(8) VALUE +0     BINARY.
00192  77  BASE-YR-1996-CTR       PIC S9(8) VALUE +0     BINARY.
00193  77  BASE-YR-1997-CTR       PIC S9(8) VALUE +0     BINARY.
00194  77  BASE-YR-1998-CTR       PIC S9(8) VALUE +0     BINARY.
00195  77  BASE-YR-1999-CTR       PIC S9(8) VALUE +0     BINARY.
00196  77  BASE-YR-2000-CTR       PIC S9(8) VALUE +0     BINARY.
00197  77  BASE-YR-2001-CTR       PIC S9(8) VALUE +0     BINARY.
00198  77  BASE-YR-2002-CTR       PIC S9(8) VALUE +0     BINARY.
00199  77  BASE-YR-2003-CTR       PIC S9(8) VALUE +0     BINARY.
00200  77  BASE-YR-2004-CTR       PIC S9(8) VALUE +0     BINARY.
00201  77  BASE-YR-2005-CTR       PIC S9(8) VALUE +0     BINARY.
00202  77  BASE-YR-2006-CTR       PIC S9(8) VALUE +0     BINARY.
00203  77  BASE-YR-2007-CTR       PIC S9(8) VALUE +0     BINARY.
00204  77  BASE-YR-2008-CTR       PIC S9(8) VALUE +0     BINARY.
00205  77  BASE-YR-2009-CTR       PIC S9(8) VALUE +0     BINARY.
00206  77  BASE-YR-2010-CTR       PIC S9(8) VALUE +0     BINARY.
00207  77  BASE-YR-2011-CTR       PIC S9(8) VALUE +0     BINARY.
00208  77  BASE-YR-2012-CTR       PIC S9(8) VALUE +0     BINARY.
00209  77  BASE-YR-2013-CTR       PIC S9(8) VALUE +0     BINARY.
00210  77  BASE-YR-2014-CTR       PIC S9(8) VALUE +0     BINARY.
00211  77  BASE-YR-2015-CTR       PIC S9(8) VALUE +0     BINARY.
00212  77  BASE-YR-2016-CTR       PIC S9(8) VALUE +0     BINARY.
00213  77  BASE-YR-2017-CTR       PIC S9(8) VALUE +0     BINARY.
00214  77  BASE-YR-2018-CTR       PIC S9(8) VALUE +0     BINARY.
00215  77  BASE-YR-OTHR-CTR       PIC S9(8) VALUE +0     BINARY.
00216  77  SUB1                   PIC S9(4) VALUE +0     BINARY.
00217  77  SUB2                   PIC S9(4) VALUE +0     BINARY.
00218  77  LN-CTR-1               PIC S9(4) VALUE +99    BINARY.
00219  77  LN-CTR-2               PIC S9(4) VALUE +99    BINARY.
00220  77  LN-CTR-3               PIC S9(4) VALUE +99    BINARY.
00221  77  LN-CTR-4               PIC S9(4) VALUE +99    BINARY.
00222  77  PG-CTR-1               PIC S9(4) VALUE +0     BINARY.
00223  77  PG-CTR-2               PIC S9(4) VALUE +0     BINARY.
00224  77  PG-CTR-3               PIC S9(4) VALUE +0     BINARY.
00225  77  PG-CTR-4               PIC S9(4) VALUE +0     BINARY.
00226  77  PRIOR-RESD-IMPV-CTR    PIC S9(4) VALUE +0     BINARY.
00227  77  CURR-RESD-IMPV-CTR     PIC S9(4) VALUE +0     BINARY.
00228  77  T4-ASSD-VAL            PIC S9(9) VALUE +0     PACKED-DECIMAL.
00229  77  T5-ASSD-VAL            PIC S9(9) VALUE +0     PACKED-DECIMAL.
00230  77  BASE-VAL-AV            PIC S9(9) VALUE +0     PACKED-DECIMAL.
00231  77  BASE-VAL-EAV           PIC S9(9) VALUE +0     PACKED-DECIMAL.
00232  77  BASE-ELIG-COMP-AV      PIC S9(9) VALUE +0     PACKED-DECIMAL.
00233  77  BASE-ELIG-COMP-EAV     PIC S9(9) VALUE +0     PACKED-DECIMAL.
00234  77  288-OVER-30000-AV      PIC S9(9) VALUE +0     PACKED-DECIMAL.
00235  77  CURR-VAL-AV            PIC S9(9) VALUE +0     PACKED-DECIMAL.
00236  77  CURR-ELIG-COMP-EAV     PIC S9(9) VALUE +0     PACKED-DECIMAL.
00237  77  CURR-ELIG-COMP-AV      PIC S9(9) VALUE +0     PACKED-DECIMAL.
00238  77  ASSD-VAL               PIC S9(9) VALUE +0     PACKED-DECIMAL.
00239  77  288-OVER-30000-EAV     PIC S9(9) VALUE +0     PACKED-DECIMAL.
00240  77  288-EXPIRE-AV          PIC S9(9) VALUE +0     PACKED-DECIMAL.
00241  77  288-EXPIRE-EAV         PIC S9(9) VALUE +0     PACKED-DECIMAL.
00242  77  FINAL-DIFF-TX-COMP     PIC S9(9) VALUE +0     PACKED-DECIMAL.
00243  77  TOT-BASE-COMP-EAV      PIC S9(9) VALUE +0     PACKED-DECIMAL.
00244  77  HIMP-TOT               PIC S9(9) VALUE +0     PACKED-DECIMAL.
00245  77  CURR-BASE-VAL-EAV      PIC S9(9) VALUE +0     PACKED-DECIMAL.
00246  77  CURR-NOT-ELIG-AV       PIC S9(9) VALUE +0     PACKED-DECIMAL.
00247  77  CURR-NOT-ELIG-EAV      PIC S9(9) VALUE +0     PACKED-DECIMAL.
00248  77  PRIOR-IMPV-TOT         PIC S9(9) VALUE +0     PACKED-DECIMAL.
00249  77  CURR-IMPV-TOT          PIC S9(9) VALUE +0     PACKED-DECIMAL.
00250  77  PRIOR-LAND-TOT         PIC S9(9) VALUE +0     PACKED-DECIMAL.
00251  77  CURR-LAND-TOT          PIC S9(9) VALUE +0     PACKED-DECIMAL.
00252  77  BASE-DIFF              PIC S9(9) VALUE +0     PACKED-DECIMAL.
00253  77  CURR-DIFF              PIC S9(9) VALUE +0     PACKED-DECIMAL.
00254  77  CURR-TAX-DIFF          PIC S9(9) VALUE +0     PACKED-DECIMAL.
00255  77  PCT-SHARES             PIC V9(6) VALUE 0      PACKED-DECIMAL.
00256  77  WS-BASVALYR            PIC 9(4)  VALUE 0      PACKED-DECIMAL.
00257  77  WS-BVYTOTELCOMPEV      PIC 9(9)  VALUE 0      PACKED-DECIMAL.
00258  77  BLNK                   PIC X     VALUE SPACE.
00259  77  WS-PROC-YR             PIC 9(4).
00260  77  CURR-ASSMT-EOF-SW      PIC 9     VALUE ZEROES.
00261      88 CURR-ASSMT-EOF                VALUE 1.
00262  77  PRIOR-ASSMT-EOF-SW     PIC 9     VALUE ZEROES.
00263      88 PRIOR-ASSMT-EOF               VALUE 1.
00264  77  EXPIRED-ASSMT-EOF-SW   PIC 9     VALUE ZEROES.
00265      88 EXPIRED-ASSMT-EOF             VALUE 1.
00266  77  PRIOR-ASSMT-PRESENT-SW PIC 9     VALUE ZEROES.
00267      88 PRIOR-ASSMT-PRESENT           VALUE 1.
00268  77  EXEMP-DET-EOF-SW       PIC 9     VALUE ZEROES.
00269      88 EXEMP-DET-EOF                 VALUE 1.
00270  77  QUALIFY-SW             PIC 9     VALUE ZEROES.
00271  77  YR-HLD                 PIC 99.
00272  77  FARM-SW                PIC X     VALUE SPACES.
00273  77  NO-CALC-SW             PIC X     VALUE SPACES.
00274  77  MESSG-SW               PIC X     VALUE SPACES.
00275  77  FILE-SW                PIC X     VALUE SPACES.
00276  77  BASE-YR                PIC X(4).
00277  77  BASE-YR-CLS            PIC XXX.
00278  77  CURR-YR                PIC X(4).
00279  77  CURR-YR-CLS            PIC XXX.
00280  77  SAVE-CA-KEY            PIC X(21) VALUE LOW-VALUES.
00281  77  SAVE-PA-KEY            PIC X(21) VALUE LOW-VALUES.
00282  77  SAVE-EX-KEY            PIC X(21) VALUE LOW-VALUES.
00283  77  TXCD-HLD               PIC X(5).
00284  77  HOLD-CALC-TYP          PIC X     VALUE SPACE.
00285  77  CURR-MIXED-CLS-SW      PIC X     VALUE 'N'.
00286      88  CURR-MIXED-CLS               VALUE 'Y'.
00287  77  PRIOR-MIXED-CLS-SW     PIC X     VALUE 'N'.
00288      88  PRIOR-MIXED-CLS              VALUE 'Y'.
00289  77  COOP-SW                PIC X     VALUE 'N'.
00290      88  COOP-YES                     VALUE 'Y'.
00291  77  BYPASS-EQFILE-SW       PIC X     VALUE 'N'.
00292      88  BYPASS-EQFILE                VALUE 'Y'.
00293  77  FILE-STATUS            PIC 99.
00294      88 NORMAL-STATUS                 VALUE ZEROES.
00295      88 RECORD-NOT-FOUND              VALUE 23.
00296  77  CLASS-HLD              PIC 999.
00297      88 RESD-IMPV                     VALUE 202 THRU 213, 218,
00298                                             219, 220, 221, 234,
00299                                             278, 294, 295, 297,
00300                                             299.
00301      88 RESD-LAND                     VALUE 200, 241.
00302      88 FARM-LAND                     VALUE 239.
00303      88 MIXED-USE                     VALUE 236.
00304      88 NON-RESD                      VALUE ZEROES THRU 199,
00305 *                                           201, 214 THRU 233,
00306                                             201, 214 THRU 217,
00307                                             222 THRU 233,
00308                                             235, 237, 238, 240,
00309                                             242 THRU 277,
00310                                             279 THRU 287,
00311                                             289 THRU 293, 296,
00312                                             298, 300 THRU 999.
00313      88 HIMP                          VALUE 288.
00314      88 SEG-WHERE-QUEST-FOLLOWS       VALUE 202 THRU 212, 234,
00315                                             278, 295,
00316                                             405 THRU 412, 434,
00317                                             478, 495.
00318      88 MAJOR-CLASS-2                 VALUE 200 THRU 299.
00319  77  WS-CURR-YR-CLS         PIC X(3).
00320      88 VALID-CURR-YR-CLS   VALUE '202' THRU '299' '517' '597'.
00321  77  NULL-CLASS-TAB         PIC X(24) VALUE NULLS.
00322
00323
00324  01  FILLER.
00325      03 CLASS-TABLE.
00326 *       CLASS-TAB POSITIONS CORRESPOND TO THE FOLLOWING:
00327 *       ELEMENT  1:  PRIOR-ASSMT RESD-IMPV
00328 *       ELEMENT  2:  PRIOR-ASSMT RESD-IMPV WITH AN AGE OF 001
00329 *       ELEMENT  3:  PRIOR-ASSMT RESD-LAND
00330 *       ELEMENT  4:  PRIOR-ASSMT FARM-LAND
00331 *       ELEMENT  5:  PRIOR-ASSMT MIXED-USE
00332 *       ELEMENT  6:  PRIOR-ASSMT NON-RESD
00333 *       ELEMENT  7:  CURRENT-ASSMT RESD-IMPV
00334 *       ELEMENT  8:  CURRENT-ASSMT RESD-IMPV WITH AN AGE OF 001
00335 *       ELEMENT  9:  CURRENT-ASSMT RESD-LAND
00336 *       ELEMENT 10:  CURRENT-ASSMT FARM-LAND
00337 *       ELEMENT 11:  CURRENT-ASSMT MIXED-USE
00338 *       ELEMENT 12:  CURRENT-ASSMT NON-RESD
00339         05 CLASS-TAB        OCCURS 12 TIMES        PIC S999
00340                                                    PACKED-DECIMAL.
00341      03 FILE-STATUSA.
00342         05 FS-RETURN        PIC S9(4)              BINARY.
00343         05 FS-FUNC          PIC S9(4)              BINARY.
00344         05 FS-FEEDBACK      PIC S9(4)              BINARY.
00345      03 WS-AS3-YEAR         PIC 9(3)   VALUE ZEROS.
00346      03 WS-AS3-YEAR-X REDEFINES WS-AS3-YEAR.
00347         05 FILLER           PIC 9(1).
00348         05 WS-AS3-YR        PIC 9(2).
00349      03 WK-AS3-YEAR         PIC 9(4).
00350      03 WK-AS3-YEAR-X REDEFINES WK-AS3-YEAR.
00351         05 WK-AS3-CC        PIC 9(2).
00352         05 WK-AS3-YR        PIC 9(2).
00353      03 WS-FR-YEAR          PIC 9(4)   VALUE ZEROS.
00354      03 WS-FR-YEAR-X REDEFINES WS-FR-YEAR.
00355         05 WS-FR-CC         PIC 9(2).
00356         05 WS-FR-YR         PIC 9(2).
00357      03 WS-TO-YEAR          PIC 9(4)   VALUE ZEROS.
00358      03 WS-TO-YEAR-X REDEFINES WS-TO-YEAR.
00359         05 WS-TO-CC         PIC 9(2).
00360         05 WS-TO-YR         PIC 9(2).
00361
00362      03 CURR-CA-KEY.
00363         05 CCK-TOWN         PIC XX.
00364         05 CCK-VOL          PIC XXX.
00365         05 CCK-PROP         PIC X(15).
00366         05 CCK-TXTYP        PIC X.
00367      03 PREV-CA-KEY.
00368         05 PCK-TOWN         PIC XX    VALUE LOW-VALUES.
00369         05 PCK-VOL          PIC XXX   VALUE LOW-VALUES.
00370         05 PCK-PROP         PIC X(15) VALUE LOW-VALUES.
00371         05 PCK-TXTYP        PIC X     VALUE LOW-VALUES.
00372      03 CURR-PA-KEY.
00373         05 CPK-TOWN         PIC XX.
00374         05 CPK-VOL          PIC XXX.
00375         05 CPK-PROP         PIC X(15).
00376         05 CPK-TXTYP        PIC X.
00377      03 PREV-PA-KEY.
00378         05 PPK-TOWN         PIC XX    VALUE LOW-VALUES.
00379         05 PPK-VOL          PIC XXX   VALUE LOW-VALUES.
00380         05 PPK-PROP         PIC X(15) VALUE LOW-VALUES.
00381         05 PPK-TXTYP        PIC X     VALUE LOW-VALUES.
00382      03 CURR-EX-KEY.
00383         05 CEK-TOWN         PIC XX.
00384         05 CEK-VOL          PIC XXX.
00385         05 CEK-PROP         PIC X(15).
00386         05 CEK-TXTYP        PIC X.
00387      03 PREV-EX-KEY.
00388         05 PEK-TOWN         PIC XX    VALUE LOW-VALUES.
00389         05 PEK-VOL          PIC XXX   VALUE LOW-VALUES.
00390         05 PEK-PROP         PIC X(15) VALUE LOW-VALUES.
00391         05 PEK-TXTYP        PIC X     VALUE LOW-VALUES.
00392      03 CHECK-POINT-ID.
00393         05 FILLER           PIC XX    VALUE 'AS'.
00394         05 CPI-TRANS-NO     PIC 9(6)  VALUE ZEROES.
00395      03 MDY-DATE.
00396         05 MO               PIC XX.
00397         05 FILLER           PIC X     VALUE '/'.
00398         05 DA               PIC XX.
00399         05 FILLER           PIC X     VALUE '/'.
00400         05 YR               PIC XX.
00401      03 YMD-DATE.
00402         05 YR               PIC XX.
00403         05 MO               PIC XX.
00404         05 DA               PIC XX.
00405
00406      03 HEAD1.
00407         05 FILLER           PIC XX    VALUE SPACES.
00408         05 H1-CURR-DTE      PIC X(8).
00409         05 FILLER           PIC X(39) VALUE SPACES.
00410         05 FILLER           PIC X(19) VALUE 'OFFICE OF THE COOK '.
00411         05 FILLER           PIC X(15) VALUE 'COUNTY ASSESSOR'.
00412         05 FILLER           PIC X(31) VALUE SPACES.
00413         05 FILLER           PIC X(5)  VALUE 'PAGE '.
00414         05 H1-PAGE          PIC ZZZ,ZZ9.
00415
00416      03 HEAD2.
00417         05 FILLER           PIC X(11) VALUE '  ASHMA850-'.
00418         05 H2-REPORT-TYP    PIC X.
00419         05 FILLER           PIC X(20) VALUE SPACES.
00420         05 H2-REPORT-TITLE  PIC X(70).
00421
00422      03 HEAD3.
00423         05 FILLER           PIC X(19) VALUE '  VOL.   PROPERTY I'.
00424         05 FILLER           PIC X(19) VALUE 'NDEX NO.   TAX TYPE'.
00425         05 FILLER           PIC X(8)  VALUE '   CLASS'.
00426
00427      03 HEAD3-4.
00428         05 FILLER           PIC X(22) VALUE SPACES.
00429         05 FILLER           PIC X(11) VALUE 'CURRENT'.
00430         05 FILLER           PIC X(8)  VALUE 'BASE'.
00431         05 FILLER           PIC X(10) VALUE 'NO CALC'.
00432         05 FILLER           PIC X(13) VALUE 'TOT BASE'.
00433         05 FILLER           PIC X(13) VALUE 'CURR-ELIG'.
00434         05 FILLER           PIC X(12) VALUE 'FINAL DIFF'.
00435         05 FILLER           PIC X(12) VALUE 'BASE VAL'.
00436         05 FILLER           PIC X(12) VALUE 'CURR VAL'.
00437         05 FILLER           PIC X(8)  VALUE 'CURR NOT'.
00438
00439      03 HEAD4.
00440         05 FILLER           PIC X(22) VALUE '  PROPERTY NUMBER'.
00441         05 FILLER           PIC X(11) VALUE 'YR CLASS'.
00442         05 FILLER           PIC X(10) VALUE 'YEAR'.
00443         05 FILLER           PIC X(8)  VALUE 'SW'.
00444         05 FILLER           PIC X(14) VALUE 'COMP EAV'.
00445         05 FILLER           PIC X(14) VALUE 'COMP EAV'.
00446         05 FILLER           PIC X(13) VALUE 'TAX COMP'.
00447         05 FILLER           PIC X(12) VALUE 'AV'.
00448         05 FILLER           PIC X(12) VALUE 'AV'.
00449         05 FILLER           PIC X(8)  VALUE 'ELIG EAV'.
00450
00451      03 DETAIL1.
00452         05 FILLER           PIC XX    VALUE SPACES.
00453         05 DL1-VOL          PIC XXX.
00454         05 FILLER           PIC X(4)  VALUE SPACES.
00455         05 DL1-PROP         PIC 99B99B999B999B9(4).
00456         05 FILLER           PIC X(7)  VALUE SPACES.
00457         05 DL1-TXTYP        PIC X.
00458         05 FILLER           PIC X(6)  VALUE SPACES.
00459         05 DL1-CLS          PIC 9B99.
00460         05 FILLER           PIC X(5)  VALUE SPACES.
00461         05 DL1-MESSG        PIC X(63).
00462
00463      03 DETAIL4               VALUE SPACES.
00464         05 FILLER             PIC XX.
00465         05 DL4-PROP           PIC Z99B99B999B999B9(4).
00466         05 FILLER             PIC X(3).
00467         05 DL4-CURR-YR-CLS    PIC 9B99.
00468         05 FILLER             PIC X(5).
00469         05 DL4-BASE-YR        PIC X(4).
00470         05 FILLER             PIC X(7).
00471         05 DL4-NO-CALC-SW     PIC X.
00472         05 FILLER             PIC X(3).
00473         05 DL4-TOT-BASE-EAV   PIC ZZZ,ZZZ,ZZ9.
00474         05 FILLER             PIC X(3).
00475         05 DL4-CURR-ELIG-EAV  PIC ZZZ,ZZZ,ZZ9.
00476         05 FILLER             PIC X.
00477         05 DL4-FINAL-DIFF-TX  PIC ZZZ,ZZZ,ZZ9.
00478         05 FILLER             PIC X.
00479         05 DL4-BASE-VAL-AV    PIC ZZZ,ZZZ,ZZ9.
00480         05 FILLER             PIC X.
00481         05 DL4-CURR-VAL-AV    PIC ZZZ,ZZZ,ZZ9.
00482         05 FILLER             PIC X.
00483         05 DL4-CURR-NOT-ELIG-EAV  PIC ZZZ,ZZZ,ZZ9.
00484
00485      03 TOTAL1.
00486         05 FILLER           PIC XXX   VALUE SPACES.
00487         05 T1-TOTAL         PIC Z,ZZZ,ZZ9.
00488         05 FILLER           PIC XX    VALUE SPACES.
00489         05 T1-MESSG         PIC X(54).
00490
00491  01  IO-AREA.
00492  COPY PIASSESSMT.
00493
00494  COPY ASAIS145SG.
00495
00496  COPY ASAIS150SG.
00497
00498  COPY IMSCALLS.
00499
00500  COPY ASAISSSA29.
00501
00502  01  HLD-ASSMT-SEGS.
00503      05  H-BASE.
00504 *                                              1-122 FIXED SEGMEN
00505          10  H-STAT1        PIC X.
00506              88  HR-NON-ASSESSED      VALUE '0'.
00507              88  HR-ASSESSED          VALUE '1'.
00508 *                                              1     STATUS-1
00509          10  H-VOLPROP.
00510 *                                              2-11  RECORD KEY
00511              15  H-VOL      PIC S9(3)    COMP-3.
00512                  88  HR-RE-VOL        VALUE +001 THRU +601.
00513                  88  HR-RR-VOL        VALUE +605.
00514 *                                              2-3     VOLUME
00515              15  H-PROP     PIC S9(15)   COMP-3.
00516 *                                              4-11    PROPERTY-N
00517          10  H-TXTYP        PIC X.
00518 *                                             12-12  TAX TYPE
00519          10  FILLER         PIC X.
00520 *                                             13-13  FILLER
00521          10  H-TXCD         PIC S9(5)    COMP-3.
00522 *                                             14-16  TAX CODE
00523          10  H-STAT2        PIC X.
00524              88  HR-TAXABLE-PARCEL    VALUE '0'.
00525              88  HR-EXEMPT            VALUE '1'.
00526              88  HR-RAILROAD          VALUE '2'.
00527              88  HR-HOMESTEAD-NON-COOP VALUE '3'.
00528              88  HR-VETERAN           VALUE '4'.
00529              88  HR-HOMESTEAD-COOP    VALUE '5'.
00530 *                                             17     STATUS-2
00531          10  H-CLS          PIC S9(3)    COMP-3.
00532 *                                             18-19  CLASS (9-99)
00533 *                                                    (MAJOR-MINOR
00534          10  H-NBHD         PIC S9(3)    COMP-3.
00535 *                                             20-21  NEIGHBORHD-C
00536          10  H-STRT         PIC S9(5)    COMP-3.
00537 *                                             22-24  STREET CODE
00538          10  H-HSENO        PIC S9(5)    COMP-3.
00539 *                                             25-27  HOUSE-NO
00540          10  H-LNDDIM       PIC S9(7)    COMP-3.
00541 *                                             28-31  LND-DIMENSIO
00542          10  H-LNDCD        PIC X.
00543 *                                             32     LAND CODE
00544          10  H-LNDSQFT      PIC S9(7)    COMP-3.
00545 *                                             33-36  LAND-SQ-FT
00546          10  H-IRREG        PIC X.
00547 *                                             37     IRREGULAR
00548          10  H-STAT3        PIC X.
00549 *                                             38     COMPLAINT ST
00550          10  H-CMPLNTNO     PIC S9(7)    COMP-3.
00551 *                                             39-42  B.A.COMPLAIN
00552          10  H-BA-ACTION.
00553 *                                             43-44  B.A. ACTION
00554              15  H-BA-YR    PIC X.
00555 *                                             43       BA-YEAR
00556              15  H-BA-REV   PIC X.
00557 *                                             44       BA-REVISIO
00558          10  H-ASMT-ACTION OCCURS 4 TIMES.
00559 *                                             45-56  ASMT-ACTIONS
00560 *                                                    (YR-CHNG-REV
00561              15  H-ASMT-YR  PIC X.
00562 *                                             45       AA-YEAR1
00563 *                                             48       AA-YEAR2
00564 *                                             51       AA-YEAR3
00565 *                                             54       AA-YEAR4
00566              15  H-ASMT-CHG PIC X.
00567 *                                             46       AA-CHANGE1
00568 *                                             49       AA-CHANGE2
00569 *                                             52       AA-CHANGE3
00570 *                                             55       AA-CHANGE4
00571              15  H-ASMT-REV PIC X.
00572 *                                             47       AA-REVSION
00573 *                                             50       AA-REVSION
00574 *                                             53       AA-REVSION
00575 *                                             56       AA-REVSION
00576          10  H-VALUE-1  OCCURS 12 TIMES
00577                             PIC S9(9)    COMP-3.
00578 *                                             57-116 VALUE FIELDS
00579 *                                             57-61    PRIOR LAND
00580 *                                             62-66    PRIOR IMPR
00581 *                                             67-71    PRIOR TOTA
00582 *                                             72-76    CURRNT-LAN
00583 *                                             77-81    CURR-IMPRV
00584 *                                             82-86    CURR-TOTAL
00585 *                                             87-91    PROPOSE-LN
00586 *                                             92-96    PROP-IMPRV
00587 *                                             97-101   PROP-TOTAL
00588 *                                            102-106   PRIOR-REG/
00589 *                                                      COMPL DATE
00590 *                                            107-111   CURRENT-RE
00591 *                                            112-116   PRIOR-PROP
00592          10  FILLER         PIC XX.
00593 *                                            117-118 FILLER
00594          10  H-REC-CTRS-1.
00595 *                                            119-122 RECORD CNTRS
00596              15  H-SLS-CTR-1
00597                             PIC S9.
00598                  88  HR-NO-SALES      VALUE +0.
00599                  88  HR-SALES-PRESENT VALUE +1.
00600 *                                            119       SALES-CNTR
00601              15  H-DTL-QST-CTR-1
00602                             PIC S9(3).
00603                  88  HR-NO-DETAIL     VALUE +0.
00604                  88  HR-DETAIL-PRESENT VALUE +1 THRU +350.
00605                  88  HR-MAXIMUM-DETAIL VALUE +350.
00606 *                                            120-122   DETAIL-CNT
00607 *----------------------------------------------------------------
00608 * D E T A I L  S E G M E N T S, IF PRESENT,EACH CONTAINS 53 BYTES
00609 *    DESCRIPTIONS RELATIVE TO 1ST BYTE IN EACH SEGMENT
00610 *
00611      05  H-DTL-QST-1
00612              OCCURS 0 TO 350 TIMES DEPENDING ON H-DTL-QST-CTR-1.
00613          10  H-TYP1.
00614 *                                              1-53  TYPE1-LAND
00615              15  H1-MC      PIC S999     COMP-3.
00616 *                                              1-2   S1-MULTI-COD
00617              15  H1-TYP     PIC X.
00618                  88  H1R-TYPE1        VALUE '1'.
00619 *                                              3     S1-TYPE1
00620              15  H1-CD      PIC X.
00621 *                                              4     S1-CODE 0/1
00622 *                                                    (0=VACANT)
00623 *                                                    (1=IMPROVED)
00624              15  H1-DEC     PIC X.
00625 *                                              5     S1-DECIMAL
00626              15  H1-UM      PIC XX.
00627 *                                              6-7   S1-UNIT-MEAS
00628              15  H1-CLS     PIC S999     COMP-3.
00629 *                                              8-9   S1-CLASS
00630 *                                                    (MAJOR-MINOR
00631              15  H1-EXRR    PIC X.
00632 *                                             10     S1-EX-RR
00633              15  H1-FF      PIC S9(7)    COMP-3.
00634 *                                             11-14  S1-FRONT-FT
00635              15  H1-DPTH    PIC S9(5)    COMP-3.
00636 *                                             15-17  S1-DEPTH
00637              15  H1-UPR     PIC S9(5)V99 COMP-3.
00638 *                                             18-21  S1-UNIT-PRIC
00639              15  H1-DFCTR   PIC S99V999  COMP-3.
00640 *                                             22-24  S1-DPTH-FACT
00641              15  H1-CFCTR   PIC S9V9(4)  COMP-3.
00642 *                                             25-27  S1-CORNR-FCT
00643              15  H1-ECFCTR  PIC SV9(5)   COMP-3.
00644 *                                             28-30  S1-EXTRA
00645 *                                                    CORNER FACTO
00646              15  H1-PCASSD  PIC S99V9(5) COMP-3.
00647 *                                             31-34  S1-% ASSESSE
00648              15  H1-EI      PIC S99V9    COMP-3.
00649                  88  H1R-ECON1        VALUE +22.0.
00650                  88  H1R-ECON2        VALUE +16.0.
00651                  88  H1R-ECON3        VALUE +33.0.
00652                  88  H1R-ECON4        VALUE +30.0.
00653                  88  H1R-ECON5        VALUE +36.0.
00654                  88  H1R-ECON6        VALUE +38.0.
00655                  88  H1R-ECON-IND     VALUE +16.0 +22.0 +30.0
00656                                             +33.0 +36.0 +38.0.
00657 *                                             35-36  S1-ECON-IND
00658              15  H1-VAL     PIC S9(9)    COMP-3.
00659 *                                             37-41  S1-VALUATION
00660              15  FILLER     PIC X(11).
00661 *                                             42-52  FILLER
00662              15  H1-UI      PIC X.
00663 *                                             53     S1-UNIT-IND
00664 *----------------------------------------------------------------
00665          10  H-TYP2 REDEFINES H-TYP1.
00666 *                                              1-53  TYPE2-IMPRVM
00667              15  H2-MC      PIC S999     COMP-3.
00668 *                                              1-2   S2-MULTI-COD
00669              15  H2-TYP     PIC X.
00670                  88  H2R-TYPE2        VALUE '2'.
00671                  88  H2R-TYPE2-5      VALUE '2' THRU '5'.
00672 *                                              3     S2-TYPE2
00673              15  H2-CD      PIC X.
00674 *                                              4     S2-CODE 2/3
00675 *                                                    2=MAJOR-IMPR
00676 *                                                    3=MINOR-IMPR
00677              15  H2-DEC     PIC X.
00678 *                                              5     S2-DECIMAL
00679              15  H2-UM      PIC XX.
00680 *                                              6-7   S2-UNIT-MEAS
00681              15  H2-CLS     PIC S999     COMP-3.
00682                  88  H2R-RES-CLASS    VALUE +202 THRU +212
00683                                             +234 +278 +295.
00684                  88  H2R-QUES-CLASS   VALUE +202 THRU +212
00685                                             +234 +278 +295
00686                                             +402 THRU +412
00687                                             +434 +478 +495.
00688                  88  H2R-CLS-288      VALUE +288.
00689                  88  H2R-QCLS1-3      VALUE +210 +211 +212 +295
00690                                             +410 +411 +412 +495.
00691                  88  H2R-QCLS1OR5     VALUE +202 +203 +204
00692                                             +402 +403 +404.
00693                  88  H2R-QCLS2        VALUE +205 THRU +208 +278
00694                                             +405 THRU +408 +478.
00695                  88  H2R-QCLS2-3      VALUE +209 +409.
00696                  88  H2R-QCLS4        VALUE +234 +434.
00697 *                                              8-9   S2-CLASS
00698 *                                                    (MAJOR-MINOR
00699              15  H2-CDU     PIC XX.
00700 *                                             10-11  S2-CDU
00701              15  H2-AREA    PIC S9(7)    COMP-3.
00702 *                                             12-15  S2-AREA
00703              15  H2-UPR     PIC S9(5)V99 COMP-3.
00704 *                                             16-19  S2-UNIT-PRIC
00705              15  H2-PRDCT   PIC S9(9)    COMP-3.
00706 *                                             20-24  S2-PRODUCT
00707              15  H2-AGE     PIC S999     COMP-3.
00708                  88  H2-0-AGE         VALUE +0.
00709                  88  H2-OVER-80       VALUE +081 THRU +999.
00710 *                                             25-26  S2-AGE
00711              15  H2-COND    PIC S99V9    COMP-3.
00712 *                                             27-28  S2-CONDITION
00713              15  H2-PCASSD  PIC S99V9(5) COMP-3.
00714 *                                             29-32  S2-% ASSESSE
00715              15  H2-BUFF    PIC S999     COMP-3.
00716 *                                             33-34  S2-BUFF-NO
00717              15  FILLER     PIC XX.
00718 *                                             35-36  FILLER
00719              15  H2-VAL     PIC S9(9)    COMP-3.
00720 *                                             37-41  S2-VALUATION
00721              15  H2-KEYPCL  PIC S9(15)   COMP-3.
00722 *                                             42-49  S2-KEY PARCE
00723              15  H2-SC      PIC X.
00724 *                                             50     S2-SPLIT COD
00725              15  FILLER     PIC X(3).
00726 *                                             51-53  FILLER
00727 *----------------------------------------------------------------
00728          10  H-TYP3 REDEFINES H-TYP1.
00729 *                                              1-53  TYPE3-IMPRVM
00730              15  H3-MC      PIC S999     COMP-3.
00731 *                                              1-2   S3-MULTI-COD
00732              15  H3-TYP     PIC X.
00733                  88  H3R-TYPE3        VALUE '3'.
00734 *                                              3     S3-TYPE3
00735              15  H3-CD      PIC X.
00736 *                                              4     S3-CODE 2/3
00737 *                                                    2=MAJOR-IMPR
00738 *                                                    3=MINOR-IMPR
00739              15  FILLER     PIC X(3).
00740 *                                              5-7   FILLER
00741              15  H3-CLS     PIC S999     COMP-3.
00742 *                                              8-9   S3-CLASS
00743 *                                                    (MAJOR-MINOR
00744              15  H3-CDU     PIC XX.
00745 *                                             10-11  S3-CDU
00746              15  H3-REPCST  PIC S9(9)    COMP-3.
00747 *                                             12-16  S3-REPRODUCT
00748 *                                                    COST
00749              15  FILLER     PIC X(6).
00750 *                                             17-22  FILLER
00751              15  H3-YR      PIC S999     COMP-3.
00752 *                                             23-24  S3-YEAR
00753              15  H3-AGE     PIC S999     COMP-3.
00754                  88  H3-0-AGE         VALUE +0.
00755                  88  H3-OVER-80       VALUE +081 THRU +999.
00756 *                                             25-26  S3-AGE
00757              15  H3-COND    PIC S99V9    COMP-3.
00758 *                                             27-28  S3-CONDITION
00759              15  H3-PCASSD  PIC S99V9(5) COMP-3.
00760 *                                             29-32  S3-% ASSESSE
00761              15  H3-BUFF    PIC S999     COMP-3.
00762 *                                             33-34  S3-BUFF-NO
00763              15  FILLER     PIC XX.
00764 *                                             35-36  FILLER
00765              15  H3-VAL     PIC S9(9)    COMP-3.
00766 *                                             37-41  S3-VALUATION
00767              15  H3-KEYPCL  PIC S9(15)   COMP-3.
00768 *                                             42-49  S3-KEY PARCE
00769              15  H3-SC      PIC X.
00770 *                                             50     S3-SPLIT COD
00771              15  FILLER     PIC X(3).
00772 *                                             51-53  FILLER
00773 *----------------------------------------------------------------
00774          10  H-TYP4 REDEFINES H-TYP1.
00775 *                                              1-53  TYPE4-IMPRVM
00776              15  H4-MC      PIC S999     COMP-3.
00777 *                                              1-2   S4-MULTI-COD
00778              15  H4-TYP     PIC X.
00779                  88  H4R-TYPE4        VALUE '4'.
00780 *                                              3     S4-TYPE4
00781              15  H4-CD      PIC X.
00782 *                                              4     S4-CODE4
00783              15  FILLER     PIC X(3).
00784 *                                              5-7   FILLER
00785              15  H4-CLS     PIC S999     COMP-3.
00786 *                                              8-9   S4-CLASS
00787 *                                                    (MAJOR-MINOR
00788              15  H4-CDU     PIC XX.
00789 *                                             10-11  S4-CDU
00790              15  H4-REPCST  PIC S9(9)    COMP-3.
00791 *                                             12-16  S4-REPRODUCT
00792 *                                                    COST
00793              15  H4-TOTVAL  PIC S9(9)    COMP-3.
00794 *                                             17-21  S4-TOTAL-VAL
00795              15  FILLER     PIC X.
00796 *                                             22     FILLER
00797              15  H4-OCCFAC  PIC S99V9    COMP-3.
00798 *                                             23-24  S4-OCCUPANCY
00799 *                                                    FACTOR
00800              15  H4-AGE     PIC S999     COMP-3.
00801                  88  H4-0-AGE         VALUE +0.
00802                  88  H4-OVER-AGE      VALUE +081 THRU +999.
00803 *                                             25-26  S4-AGE
00804              15  H4-COND    PIC S99V9    COMP-3.
00805 *                                             27-28  S4-CONDITION
00806              15  H4-PCASSD  PIC S99V9(5) COMP-3.
00807 *                                             29-32  S4-% ASSESSE
00808              15  H4-BUFF    PIC S999     COMP-3.
00809 *                                             33-34  S4-BUFF-NO
00810              15  FILLER     PIC XX.
00811 *                                             35-36  FILLER
00812              15  H4-VAL     PIC S9(9)    COMP-3.
00813 *                                             37-41  S4-VALUATION
00814              15  H4-KEYPCL  PIC S9(15)   COMP-3.
00815 *                                             42-49  S4-KEY PARCE
00816              15  H4-SC      PIC X.
00817 *                                             50     S4-SPLIT COD
00818              15  FILLER     PIC X(3).
00819 *                                             51-53  FILLER
00820 *----------------------------------------------------------------
00821 *                                              1-53  TYPE5-IMPRVM
00822          10  H-TYP5 REDEFINES H-TYP1.
00823              15  H5-MC      PIC S999     COMP-3.
00824 *                                              1-2   S5-MULTI-COD
00825              15  H5-TYP     PIC X.
00826                  88  H5R-TYPE5        VALUE '5'.
00827 *                                              3     S5-TYPE5
00828              15  H5-CD      PIC X.
00829 *                                              4     S5-CODE5
00830              15  FILLER     PIC X(3).
00831 *                                              5-7   FILLER
00832              15  H5-CLS     PIC S999     COMP-3.
00833 *                                              8-9   S5-CLASS
00834 *                                                    (MAJOR-MINOR
00835              15  H5-CDU     PIC XX.
00836 *                                             10-11  S5-CDU
00837              15  H5-REPCST  PIC S9(9)    COMP-3.
00838 *                                             12-16  S5-REPRODUCT
00839 *                                                    COST
00840              15  FILLER     PIC X(6).
00841 *                                             17-22  FILLER
00842              15  H5-OCCFAC  PIC S99V9    COMP-3.
00843 *                                             23-24  S5-OCCUPANCY
00844 *                                                    FACTOR
00845              15  H5-AGE     PIC S999     COMP-3.
00846                  88  H5-0-AGE         VALUE +0.
00847                  88  H5-OVER-80       VALUE +081 THRU +999.
00848 *                                             25-26  S5-AGE
00849              15  H5-COND    PIC S99V9    COMP-3.
00850 *                                             27-28  S5-CONDITION
00851              15  H5-PCASSD  PIC S99V9(5) COMP-3.
00852 *                                             29-32  S5-% ASSESSE
00853              15  H5-BUFF    PIC S999     COMP-3.
00854 *                                             33-34  S5-BUFF-NO
00855              15  FILLER     PIC XX.
00856 *                                             35-36  FILLER
00857              15  H5-VAL     PIC S9(9)    COMP-3.
00858 *                                             37-41  S5-VALUATION
00859              15  H5-KEYPCL  PIC S9(15)   COMP-3.
00860 *                                             42-49  S5-KEY PARCE
00861              15  H5-SC      PIC X.
00862 *                                             50     S5-SPLIT COD
00863              15  FILLER     PIC X(3).
00864 *                                             51-53  FILLER
00865 *----------------------------------------------------------------
00866 * --  Q U E S T I O N N A I R E  S E G M E N T  -- IS PRESENT,
00867 *      IF THE PRECEDING DETAIL SEGMENT IS - TYPE = '2' THRU '5'
00868 *      AND CLASS = 202-212,234,278,295,405-412,434,478,495
00869          10  H-QST  REDEFINES H-TYP1 PIC X(53).
00870 *                                              1-53  QUESTIONNAIR
00871 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
00872      EJECT
00873  LINKAGE SECTION.
00874  COPY IOPCBDESC.
00875
00876  01  PI-PCB1.
00877  COPY PIPCB1DESC.
00878          07  PI-KEY-FB-ITEM PIC X(22).
00879
00880  PROCEDURE DIVISION.
00881  010-BEGIN.
00882      ENTRY 'DLITCBL' USING IO-PCB, PI-PCB1
00883      DISPLAY 'PROGRAM ASHMA850'
00884      DISPLAY SPACES
00885      OPEN INPUT PARAM-LINE
00886      READ PARAM-LINE AT END
00887         DISPLAY 'PARAM LINE IS MISSING'
00888         MOVE 16 TO RETURN-CODE
00889         GOBACK.
00890      DISPLAY 'CONTROL CARD  '  PARAM-REC.
00891
00892      IF PR-CICV NOT EQUAL 'CICV'
00893         DISPLAY 'PARAM CHECK POINT CONTROL VALUE NOT EQUAL "CICV"
00894 -               ' ' PR-CICV
00895         MOVE 16 TO RETURN-CODE.
00896      IF PR-CICV-VAL NOT NUMERIC OR PR-CICV-VAL EQUAL ZEROES
00897         DISPLAY 'PARAM CICV VALUE NOT VALID  ' PR-CICV-VAL
00898         MOVE 16 TO RETURN-CODE.
00899      IF PR-PROC-YR NOT NUMERIC OR PR-PROC-YR EQUAL ZEROES
00900         DISPLAY 'PARAM PROCESS YEAR NOT VALID  ' PR-PROC-YR
00901         MOVE 16 TO RETURN-CODE
00902      ELSE
00903         COMPUTE WS-PROC-YR = PR-PROC-YR-N - 1.
00904      IF PR-EQ-FCTR NOT NUMERIC OR PR-EQ-FCTR EQUAL ZEROES
00905         DISPLAY 'PARAM EQUALIZATION FACTOR NOT VALID  ' PR-EQ-FCTR
00906         MOVE 16 TO RETURN-CODE.
00907      IF PR-FR-YR NOT NUMERIC
00908         DISPLAY 'PARAM FROM YEAR NOT VALID  ' PR-FR-YR
00909         MOVE 16 TO RETURN-CODE.
00910      IF PR-TO-YR NOT NUMERIC
00911         DISPLAY 'PARAM TO YEAR NOT VALID  ' PR-TO-YR
00912         MOVE 16 TO RETURN-CODE.
00913      IF PR-FR-YR NUMERIC AND PR-TO-YR NUMERIC
00914         IF PR-FR-YR GREATER PR-TO-YR
00915            DISPLAY 'PARAM FROM YEAR CANNOT BE GREATER THAN PARAM
00916 -                  'O YEAR  ' PR-FR-YR ' ' PR-TO-YR
00917            MOVE 16 TO RETURN-CODE.
00918      IF PR-FR-BSYR NOT NUMERIC
00919         DISPLAY 'PARAM FROM BASE YEAR NOT VALID  ' PR-FR-BSYR
00920         MOVE 16 TO RETURN-CODE.
00921      IF PR-TO-BSYR NOT NUMERIC
00922         DISPLAY 'PARAM TO BASE YEAR NOT VALID  ' PR-TO-BSYR
00923         MOVE 16 TO RETURN-CODE.
00924      IF PR-FR-BSYR NUMERIC AND PR-TO-BSYR NUMERIC
00925         IF PR-FR-BSYR GREATER PR-TO-BSYR
00926            DISPLAY 'PARAM FROM BASE YEAR CANNOT BE GREATER THAN P
00927 -                  'RAM TO BASE YEAR  ' PR-FR-BSYR ' ' PR-TO-BSYR
00928            MOVE 16 TO RETURN-CODE.
00929      IF RETURN-CODE EQUAL 16
00930         CLOSE PARAM-LINE
00931         GOBACK.
00932      OPEN INPUT EQUALFCT
00933      IF NOT NORMAL-STATUS
00934         DISPLAY 'EQUALFCT OPEN FAILED  ' FILE-STATUS
00935                 '   RETURN  '   FS-RETURN
00936                 '   FUNCTION  ' FS-FUNC
00937                 '   FEEDBACK  ' FS-FEEDBACK
00938         MOVE 16 TO RETURN-CODE
00939         CLOSE PARAM-LINE
00940         GOBACK.
00941      ACCEPT YMD-DATE             FROM DATE
00942      MOVE CORRESPONDING YMD-DATE TO MDY-DATE
00943      MOVE MDY-DATE               TO H1-CURR-DTE
00944      OPEN INPUT  CURR-ASSMT, PRIOR-ASSMT, EXPIRED-ASSMT
00945           OUTPUT FINDER-FILE, PRINT-FILE-1, PRINT-FILE-2,
00946                  PRINT-FILE-3, PRINT-FILE-4
00947      WRITE PRINT-REC-1 FROM BLNK AFTER ADVANCING PAGE
00948      WRITE PRINT-REC-2 FROM BLNK AFTER ADVANCING PAGE
00949      WRITE PRINT-REC-3 FROM BLNK AFTER ADVANCING PAGE
00950      WRITE PRINT-REC-4 FROM BLNK AFTER ADVANCING PAGE
00951      PERFORM 200-READ-CURR-ASSMT
00952      PERFORM 250-READ-PRIOR-ASSMT
00953
00954      PERFORM 100-MAINLINE
00955      UNTIL CURR-ASSMT-EOF OR RETURN-CODE EQUAL 16.
00956
00957  070-END.
00958      IF RETURN-CODE NOT EQUAL 16
00959         PERFORM 1500-FORMAT-TOTALS.
00960      DISPLAY 'TOTAL NO. OF CHECKPOINTS TAKEN                    '
00961              CPI-TRANS-NO
00962      DISPLAY 'TOTAL PRIOR ASSESSMENT RECORDS READ               '
00963              PRIOR-ASSMT-REC-CTR
00964      DISPLAY 'TOTAL CURRENT ASSESSMENT RECORDS READ             '
00965              CURR-ASSMT-REC-CTR
00966      DISPLAY 'TOTAL EXPIRED ASSESSMENT RECORDS READ             '
00967              EXPIRED-ASSMT-REC-CTR
00968      DISPLAY 'TOTAL CURRENT & PRIOR ASSESSMENT RECORDS MATCHED  '
00969              CURR-PRIOR-MATCH-CTR
00970      DISPLAY 'TOTAL CURRENT ASSESSMENT RECORDS UNMATCHED        '
00971              CURR-UNMATCH-CTR
00972      DISPLAY 'TOTAL CURRENT RECORDS WITH NO-CALC INDICATOR      '
00973              CURR-NO-CALC-CTR
00974      DISPLAY 'TOTAL EXPIRED ASSESSMENT RECORDS SELECTED         '
00975              EXPIRED-ASSMT-SEL-CTR
00976      DISPLAY 'TOTAL EXEMPTION MASTER SEGMENTS UPDATED           '
00977              EXEMP-MAST-REPL-CTR
00978      DISPLAY 'TOTAL FINDER RECORDS WRITTEN                      '
00979              FINDER-REC-CTR
00980      DISPLAY 'TOTAL RECORDS WITH UNACCOUNTED CLASS CONDITIONS   '
00981              INVALID-CLS-COND-CTR
00982      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 1993                 '
00983              BASE-YR-1993-CTR
00984      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 1994                 '
00985              BASE-YR-1994-CTR
00986      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 1995                 '
00987              BASE-YR-1995-CTR
00988      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 1996                 '
00989              BASE-YR-1996-CTR
00990      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 1997                 '
00991              BASE-YR-1997-CTR
00992      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 1998                 '
00993              BASE-YR-1998-CTR
00994      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 1999                 '
00995              BASE-YR-1999-CTR
00996      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 2000                 '
00997              BASE-YR-2000-CTR
00998      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 2001                 '
00999              BASE-YR-2001-CTR
01000      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 2002                 '
01001              BASE-YR-2002-CTR
01002      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 2003                 '
01003              BASE-YR-2003-CTR
01004      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 2004                 '
01005              BASE-YR-2004-CTR
01006      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 2005                 '
01007              BASE-YR-2005-CTR
01008      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 2006                 '
01009              BASE-YR-2006-CTR
01010      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 2007                 '
01011              BASE-YR-2007-CTR
01012      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 2008                 '
01013              BASE-YR-2008-CTR
01014      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 2009                 '
01015              BASE-YR-2009-CTR
01016      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 2010                 '
01017              BASE-YR-2010-CTR
01018      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 2011                 '
01019              BASE-YR-2011-CTR
01020      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 2012                 '
01021              BASE-YR-2012-CTR
01022      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 2013                 '
01023              BASE-YR-2013-CTR
01024      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 2014                 '
01025              BASE-YR-2014-CTR
01026      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 2015                 '
01027              BASE-YR-2015-CTR
01028      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 2016                 '
01029              BASE-YR-2016-CTR
01030      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 2017                 '
01031              BASE-YR-2017-CTR
01032      DISPLAY 'TOTAL RECDS WITH BASE YEAR OTHER THAN 1993 THRU 201
01033 -            ' ' BASE-YR-OTHR-CTR
01034 *-> GKW
01035      DISPLAY 'TOTAL RECORDS WITH BASE YEAR 2018                 '
01036              BASE-YR-2018-CTR
01037      DISPLAY 'TOTAL RECDS WITH BASE YEAR OTHER THAN 1993 THRU 201
01038 -            ' ' BASE-YR-OTHR-CTR
01039 *-> GKW
01040      CLOSE EQUALFCT
01041      IF NOT NORMAL-STATUS
01042         DISPLAY 'EQUALFCT CLOSE FAILED  ' FILE-STATUS
01043                 '   RETURN  '   FS-RETURN
01044                 '   FUNCTION  ' FS-FUNC
01045                 '   FEEDBACK  ' FS-FEEDBACK
01046         MOVE 16 TO RETURN-CODE.
01047      CLOSE PARAM-LINE, CURR-ASSMT, PRIOR-ASSMT, EXPIRED-ASSMT,
01048            FINDER-FILE, PRINT-FILE-1, PRINT-FILE-2, PRINT-FILE-3,
01049            PRINT-FILE-4
01050      GOBACK.
01051
01052  100-MAINLINE.
01053      PERFORM 175-INITIALIZE-FIELDS
01054      IF SAVE-CA-KEY EQUAL SAVE-PA-KEY
01055         PERFORM 1600-GU-ROOT-ASSESS-SEG
01056         IF PI-SEG-NOTFND
01057            MOVE 'NO MATCHING ROOT, NOMATCHING ASSSESSMENT DATA SE
01058 -               'MENT' TO DL1-MESSG
01059            PERFORM 1250-FORMAT-REPORT-1
01060            PERFORM 200-READ-CURR-ASSMT
01061            PERFORM 250-READ-PRIOR-ASSMT
01062         ELSE
01063            IF PI-DB-GOOD-STATUS
01064               PERFORM 1625-GNP-EXEMP-MAST-SEG
01065               IF PI-SEG-NOTFND
01066                  MOVE 'NO MATCHING MASTER EXEMPTION SEGMENT'
01067                                          TO DL1-MESSG
01068                  PERFORM 1250-FORMAT-REPORT-1
01069                  PERFORM 200-READ-CURR-ASSMT
01070                  PERFORM 250-READ-PRIOR-ASSMT
01071               ELSE
01072                  IF PI-DB-GOOD-STATUS
01073                     MOVE 1 TO PRIOR-ASSMT-PRESENT-SW
01074                     ADD 1  TO CURR-PRIOR-MATCH-CTR
01075                     PERFORM 450-MOVE-CURR-ASSMT-TO-WS
01076                     MOVE 'C' TO FILE-SW
01077                     IF COOP-YES
01078                        PERFORM 425-COOP-CLASS-CHECK
01079                         VARYING SUB1 FROM 1 BY 1
01080                         UNTIL SUB1 GREATER H-DTL-QST-CTR-1
01081                         OR NO-CALC-SW = 'Y'
01082                        IF NO-CALC-SW = 'N'
01083                           PERFORM 400-CODE-CLASS-CONDITIONS
01084                           VARYING SUB1 FROM 1 BY 1
01085                           UNTIL SUB1 GREATER H-DTL-QST-CTR-1
01086                        ELSE
01087                           PERFORM 1050-STEP13
01088                     ELSE
01089                        PERFORM 400-CODE-CLASS-CONDITIONS
01090                        VARYING SUB1 FROM 1 BY 1
01091                        UNTIL SUB1 GREATER H-DTL-QST-CTR-1
01092                     END-IF
01093                     PERFORM 500-MOVE-PRIOR-ASSMT-TO-WS
01094                     MOVE 'P' TO FILE-SW
01095                     IF COOP-YES
01096                        PERFORM 425-COOP-CLASS-CHECK
01097                         VARYING SUB1 FROM 1 BY 1
01098                         UNTIL SUB1 GREATER H-DTL-QST-CTR-1
01099                         OR NO-CALC-SW = 'Y'
01100                        IF NO-CALC-SW = 'N'
01101                           PERFORM 400-CODE-CLASS-CONDITIONS
01102                           VARYING SUB1 FROM 1 BY 1
01103                           UNTIL SUB1 GREATER H-DTL-QST-CTR-1
01104                        ELSE
01105                           PERFORM 1050-STEP13
01106                     ELSE
01107                        PERFORM 400-CODE-CLASS-CONDITIONS
01108                        VARYING SUB1 FROM 1 BY 1
01109                        UNTIL SUB1 GREATER H-DTL-QST-CTR-1
01110                     END-IF
01111                     PERFORM 150-SUB-MAINLINE
01112                     PERFORM 200-READ-CURR-ASSMT
01113                     PERFORM 250-READ-PRIOR-ASSMT
01114                  END-IF
01115               END-IF
01116            END-IF
01117         END-IF
01118      ELSE
01119         IF SAVE-CA-KEY LESS SAVE-PA-KEY
01120            PERFORM 1600-GU-ROOT-ASSESS-SEG
01121            IF PI-SEG-NOTFND
01122               MOVE 'NO MATCHING ROOT, NOMATCHING ASSESSMENT DATA
01123 -                  'EGMENT' TO DL1-MESSG
01124               PERFORM 1250-FORMAT-REPORT-1
01125               PERFORM 200-READ-CURR-ASSMT
01126            ELSE
01127               IF PI-DB-GOOD-STATUS
01128                  PERFORM 1625-GNP-EXEMP-MAST-SEG
01129                  IF PI-SEG-NOTFND
01130                     MOVE 'NO MATCHING MASTER EXEMPTION SEGMENT'
01131                                             TO DL1-MESSG
01132                     PERFORM 1250-FORMAT-REPORT-1
01133                     PERFORM 200-READ-CURR-ASSMT
01134                  ELSE
01135                     IF PI-DB-GOOD-STATUS
01136                        ADD 1 TO CURR-UNMATCH-CTR
01137                        PERFORM 450-MOVE-CURR-ASSMT-TO-WS
01138                        MOVE 'C' TO FILE-SW
01139                        IF COOP-YES
01140                           PERFORM 425-COOP-CLASS-CHECK
01141                            VARYING SUB1 FROM 1 BY 1
01142                            UNTIL SUB1 GREATER H-DTL-QST-CTR-1
01143                            OR NO-CALC-SW = 'Y'
01144                           IF NO-CALC-SW = 'N'
01145                              PERFORM 400-CODE-CLASS-CONDITIONS
01146                              VARYING SUB1 FROM 1 BY 1
01147                              UNTIL SUB1 GREATER H-DTL-QST-CTR-1
01148                           ELSE
01149                              PERFORM 1050-STEP13
01150                        ELSE
01151                           PERFORM 400-CODE-CLASS-CONDITIONS
01152                           VARYING SUB1 FROM 1 BY 1
01153                           UNTIL SUB1 GREATER H-DTL-QST-CTR-1
01154                        END-IF
01155                        PERFORM 150-SUB-MAINLINE
01156                        PERFORM 200-READ-CURR-ASSMT
01157                     END-IF
01158                 END-IF
01159               END-IF
01160            END-IF
01161         ELSE
01162            PERFORM 250-READ-PRIOR-ASSMT.
01163
01164  150-SUB-MAINLINE.
01165      IF COOP-YES
01166         IF CURR-MIXED-CLS OR PRIOR-MIXED-CLS
01167            MOVE 'Y' TO NO-CALC-SW
01168            DISPLAY 'COOP CONTAINS MIXED MAJOR CLASSES '
01169               SAVE-CA-KEY (1:2) ' ' SAVE-CA-KEY (3:3) ' '
01170               SAVE-CA-KEY (6:15) ' ' SAVE-CA-KEY (21:1)
01171            MOVE 15 TO RETURN-CODE
01172            ADD 1   TO INVALID-CLS-COND-CTR
01173            PERFORM 1050-STEP13
01174         END-IF
01175      ELSE
01176      IF PRIOR-ASSMT-PRESENT
01177         IF CLASS-TAB (1)  EQUAL ZEROES OR
01178            CLASS-TAB (7)  EQUAL ZEROES
01179            MOVE 'Y' TO NO-CALC-SW
01180            MOVE 'A' TO HOLD-CALC-TYP
01181            ADD 1    TO CURR-NO-CALC-CTR
01182            PERFORM 1150-STEP17
01183         ELSE
01184            IF CLASS-TAB (5)  GREATER ZEROES OR
01185               CLASS-TAB (11) GREATER ZEROES
01186               MOVE 'B' TO HOLD-CALC-TYP
01187               PERFORM 605-STEP07
01188               IF NO-CALC-SW = SPACES
01189                  PERFORM 950-STEP11
01190               ELSE
01191                  PERFORM 1050-STEP13
01192               END-IF
01193            ELSE
01194               IF CLASS-TAB (2)  GREATER ZEROES OR
01195                  CLASS-TAB (8)  GREATER ZEROES
01196                  MOVE 'Y' TO NO-CALC-SW
01197                  MOVE 'C' TO HOLD-CALC-TYP
01198                  ADD 1    TO CURR-NO-CALC-CTR
01199                  PERFORM 1050-STEP13
01200               ELSE
01201                  IF (CLASS-TAB (3)  GREATER ZEROES  AND
01202                      CLASS-TAB (1)  EQUAL   1)      AND
01203                     (CLASS-TAB (9)  GREATER ZEROES  AND
01204                      CLASS-TAB (7)  EQUAL   1)
01205                      MOVE 'D' TO HOLD-CALC-TYP
01206                      PERFORM 600-STEP07 THRU 600-EXIT
01207                      IF NO-CALC-SW EQUAL SPACES
01208                         PERFORM 950-STEP11
01209                      ELSE
01210                         PERFORM 1050-STEP13
01211                  ELSE
01212                     IF (CLASS-TAB (3)  GREATER ZEROES  AND
01213                         CLASS-TAB (1)  GREATER 1)      AND
01214                        (CLASS-TAB (9)  GREATER ZEROES  AND
01215                         CLASS-TAB (7)  GREATER 1)
01216                         MOVE 'E' TO HOLD-CALC-TYP
01217                         PERFORM 800-STEP08 THRU 800-EXIT
01218                         IF NO-CALC-SW EQUAL SPACES
01219                            PERFORM 950-STEP11
01220                         ELSE
01221                            PERFORM 1050-STEP13
01222                     ELSE
01223                        IF (CLASS-TAB (3)  GREATER ZEROES  AND
01224                            CLASS-TAB (1)  GREATER ZEROES  AND
01225                            CLASS-TAB (4)  GREATER ZEROES) AND
01226                           (CLASS-TAB (9)  GREATER ZEROES  AND
01227                            CLASS-TAB (7)  GREATER ZEROES  AND
01228                            CLASS-TAB (10) GREATER ZEROES)
01229                            MOVE 'F' TO HOLD-CALC-TYP
01230                            PERFORM 850-STEP09 THRU 850-EXIT
01231                            IF NO-CALC-SW EQUAL SPACES
01232                               PERFORM 950-STEP11
01233                            ELSE
01234                               PERFORM 1050-STEP13
01235                        ELSE
01236                            IF (CLASS-TAB (3)  GREATER ZEROES  AND
01237                                CLASS-TAB (1)  GREATER ZEROES  AND
01238                                CLASS-TAB (4)  GREATER ZEROES) OR
01239                               (CLASS-TAB (9)  GREATER ZEROES  AND
01240                                CLASS-TAB (7)  GREATER ZEROES  AND
01241                                CLASS-TAB (10) GREATER ZEROES)
01242                                MOVE 'Y' TO NO-CALC-SW
01243                                MOVE 'G' TO HOLD-CALC-TYP
01244                                ADD 1    TO CURR-NO-CALC-CTR
01245                                PERFORM 1050-STEP13
01246                            ELSE
01247                        IF (CLASS-TAB (3)  GREATER ZEROES  AND
01248                            CLASS-TAB (1)  EQUAL   1       AND
01249                            CLASS-TAB (6)  GREATER ZEROES) AND
01250                           (CLASS-TAB (9)  GREATER ZEROES  AND
01251                            CLASS-TAB (7)  EQUAL   1       AND
01252                            CLASS-TAB (12) GREATER ZEROES)
01253                           MOVE 'H' TO HOLD-CALC-TYP
01254                           PERFORM 900-STEP10 THRU 900-EXIT
01255                           IF NO-CALC-SW EQUAL SPACES
01256                              PERFORM 950-STEP11
01257                           ELSE
01258                              PERFORM 1050-STEP13
01259                        ELSE
01260                     IF (CLASS-TAB (3)  GREATER ZEROES  AND
01261                         CLASS-TAB (1)  GREATER 1       AND
01262                         CLASS-TAB (6)  GREATER ZEROES) AND
01263                        (CLASS-TAB (9)  GREATER ZEROES  AND
01264                         CLASS-TAB (7)  GREATER 1       AND
01265                         CLASS-TAB (12) GREATER ZEROES)
01266                        MOVE 'I' TO HOLD-CALC-TYP
01267                        PERFORM 800-STEP08 THRU 800-EXIT
01268                        IF NO-CALC-SW EQUAL SPACES
01269                           PERFORM 950-STEP11
01270                        ELSE
01271                           PERFORM 1050-STEP13
01272                     ELSE
01273                        DISPLAY 'CURRENT AND/OR PRIOR ASSESSMENT M
01274 -                              'STER CONTAINS AN UNACCOUNTED CLAS
01275 -                              ' CONDITION  '
01276                                SAVE-CA-KEY (1:2)  ' '
01277                                SAVE-CA-KEY (3:3)  ' '
01278                                SAVE-CA-KEY (6:15) ' '
01279                                SAVE-CA-KEY (21:1)
01280                        MOVE 15 TO RETURN-CODE
01281                        ADD 1   TO INVALID-CLS-COND-CTR
01282      ELSE
01283         IF CLASS-TAB (7)  EQUAL ZEROES
01284            MOVE 'Y' TO NO-CALC-SW
01285            MOVE 'J' TO HOLD-CALC-TYP
01286            ADD 1    TO CURR-NO-CALC-CTR
01287            PERFORM 1050-STEP13
01288         ELSE
01289            IF CLASS-TAB (11) GREATER ZEROES
01290               MOVE 'K' TO HOLD-CALC-TYP
01291 *             PERFORM 1150-STEP17
01292               PERFORM 605-STEP07
01293               IF NO-CALC-SW = SPACES
01294                  PERFORM 950-STEP11
01295               ELSE
01296                  PERFORM 1050-STEP13
01297               END-IF
01298            ELSE
01299               IF CLASS-TAB (8)  GREATER ZEROES
01300                  MOVE 'Y' TO NO-CALC-SW
01301                  MOVE 'L' TO HOLD-CALC-TYP
01302                  ADD 1    TO CURR-NO-CALC-CTR
01303                  PERFORM 1050-STEP13
01304               ELSE
01305                  IF CLASS-TAB (9)  GREATER ZEROES AND
01306                     CLASS-TAB (7)  EQUAL   1
01307                     MOVE 'M' TO HOLD-CALC-TYP
01308                     PERFORM 600-STEP07 THRU 600-EXIT
01309                     PERFORM 750-STEP07-B
01310                     IF NO-CALC-SW EQUAL SPACES
01311                        PERFORM 950-STEP11
01312                     ELSE
01313                        PERFORM 1050-STEP13
01314                  ELSE
01315                     IF CLASS-TAB (9)  GREATER ZEROES AND
01316                        CLASS-TAB (7)  GREATER 1
01317                        MOVE 'N' TO HOLD-CALC-TYP
01318                        PERFORM 800-STEP08 THRU 800-EXIT
01319                        IF NO-CALC-SW EQUAL SPACES
01320                           PERFORM 950-STEP11
01321                        ELSE
01322                           PERFORM 1050-STEP13
01323                     ELSE
01324                        IF CLASS-TAB (9)  GREATER ZEROES AND
01325                           CLASS-TAB (7)  GREATER ZEROES AND
01326                           CLASS-TAB (10) GREATER ZEROES
01327                           MOVE 'O' TO HOLD-CALC-TYP
01328                           PERFORM 850-STEP09 THRU 850-EXIT
01329                           IF NO-CALC-SW EQUAL SPACES
01330                              PERFORM 950-STEP11
01331                           ELSE
01332                              PERFORM 1050-STEP13
01333                        ELSE
01334                           IF CLASS-TAB (9)  GREATER ZEROES AND
01335                              CLASS-TAB (7)  GREATER ZEROES AND
01336                              CLASS-TAB (10) GREATER ZEROES
01337                              MOVE 'Y' TO NO-CALC-SW
01338                              MOVE 'P' TO HOLD-CALC-TYP
01339                              ADD 1    TO CURR-NO-CALC-CTR
01340                              PERFORM 1050-STEP13
01341                           ELSE
01342                        IF CLASS-TAB (9)  GREATER ZEROES AND
01343                           CLASS-TAB (7)  EQUAL   1      AND
01344                           CLASS-TAB (12) GREATER ZEROES
01345                           MOVE 'Q' TO HOLD-CALC-TYP
01346                           PERFORM 900-STEP10 THRU 900-EXIT
01347                           IF NO-CALC-SW EQUAL 'Y'
01348                              PERFORM 1050-STEP13
01349                           ELSE
01350                              PERFORM 950-STEP11
01351                        ELSE
01352                     IF CLASS-TAB (9)  GREATER ZEROES AND
01353                        CLASS-TAB (7)  GREATER 1      AND
01354                        CLASS-TAB (12) GREATER ZEROES
01355                        MOVE 'R' TO HOLD-CALC-TYP
01356                        PERFORM 800-STEP08 THRU 800-EXIT
01357                        IF NO-CALC-SW EQUAL SPACES
01358                           PERFORM 950-STEP11
01359                        ELSE
01360                           PERFORM 1050-STEP13
01361                     ELSE
01362                        DISPLAY 'CURRENT ASSESSMENT MASTER CONTAIN
01363 -                              ' AN UNACCOUNTED CLASS CONDITION  '
01364                                SAVE-CA-KEY (1:2)  ' '
01365                                SAVE-CA-KEY (3:3)  ' '
01366                                SAVE-CA-KEY (6:15) ' '
01367                                SAVE-CA-KEY (21:1)
01368                        MOVE 15 TO RETURN-CODE
01369                        ADD 1   TO INVALID-CLS-COND-CTR.
01370
01371  175-INITIALIZE-FIELDS.
01372      MOVE NULL-CLASS-TAB TO CLASS-TABLE
01373      MOVE ZEROES         TO PRIOR-ASSMT-PRESENT-SW,
01374                             BASE-YR,                BASE-YR-CLS,
01375                             BASE-VAL-AV,            HIMP-TOT,
01376                             BASE-ELIG-COMP-AV,
01377                             BASE-ELIG-COMP-EAV,
01378                             288-OVER-30000-AV,      CURR-YR,
01379                             CURR-YR-CLS,            CURR-VAL-AV,
01380                             CURR-ELIG-COMP-EAV,
01381                             CURR-ELIG-COMP-AV,      ASSD-VAL,
01382                             288-OVER-30000-EAV,     288-EXPIRE-AV
01383                             288-EXPIRE-EAV,
01384                             FINAL-DIFF-TX-COMP,
01385                             TOT-BASE-COMP-EAV,
01386                             CURR-BASE-VAL-EAV,      BASE-VAL-EAV,
01387                             CURR-NOT-ELIG-AV,
01388                             CURR-NOT-ELIG-EAV,      QUALIFY-SW,
01389                             T4-ASSD-VAL,            T5-ASSD-VAL,
01390                             PRIOR-RESD-IMPV-CTR,
01391                             CURR-RESD-IMPV-CTR,
01392                             PRIOR-IMPV-TOT,
01393                             PRIOR-LAND-TOT,
01394                             CURR-IMPV-TOT,          CURR-LAND-TOT
01395      MOVE SPACES         TO NO-CALC-SW  HOLD-CALC-TYP FARM-SW
01396                             MESSG-SW.
01397      MOVE 'N' TO COOP-SW  CURR-MIXED-CLS-SW  PRIOR-MIXED-CLS-SW
01398                  BYPASS-EQFILE-SW.
01399
01400  200-READ-CURR-ASSMT.
01401      READ CURR-ASSMT AT END
01402         MOVE 1              TO CURR-ASSMT-EOF-SW
01403         MOVE HIGH-VALUES    TO SAVE-CA-KEY.
01404      IF NOT CURR-ASSMT-EOF
01405         ADD 1               TO CURR-ASSMT-REC-CTR
01406         MOVE M-TXCD         TO TXCD-HLD
01407         MOVE TXCD-HLD (1:2) TO CCK-TOWN
01408         MOVE M-VOL          TO CCK-VOL
01409         MOVE M-PROP         TO CCK-PROP
01410         MOVE M-TXTYP        TO CCK-TXTYP
01411         IF CURR-CA-KEY NOT GREATER PREV-CA-KEY
01412            DISPLAY 'CURR-ASSMT IS OUT OF SEQUENCE'
01413            DISPLAY 'CURRENT RECORD IS   ' CCK-TOWN ' ' CCK-VOL ' '
01414                                           CCK-PROP ' ' CCK-TXTYP
01415            DISPLAY 'PREVIOUS RECORD IS  ' PCK-TOWN ' ' PCK-VOL ' '
01416                                           PCK-PROP ' ' PCK-TXTYP
01417            MOVE 16          TO RETURN-CODE
01418         ELSE
01419            MOVE CURR-CA-KEY TO PREV-CA-KEY, SAVE-CA-KEY.
01420
01421  250-READ-PRIOR-ASSMT.
01422      READ PRIOR-ASSMT AT END
01423         MOVE 1              TO PRIOR-ASSMT-EOF-SW
01424         MOVE HIGH-VALUES    TO SAVE-PA-KEY.
01425      IF NOT PRIOR-ASSMT-EOF
01426         ADD 1               TO PRIOR-ASSMT-REC-CTR
01427         MOVE MA-TXCD        TO TXCD-HLD
01428         MOVE TXCD-HLD (1:2) TO CPK-TOWN
01429         MOVE MA-VOL         TO CPK-VOL
01430         MOVE MA-PROP        TO CPK-PROP
01431         MOVE MA-TXTYP       TO CPK-TXTYP
01432         IF CURR-PA-KEY NOT GREATER PREV-PA-KEY
01433            DISPLAY 'PRIOR-ASSMT IS OUT OF SEQUENCE'
01434            DISPLAY 'CURRENT RECORD IS   ' CPK-TOWN ' ' CPK-VOL ' '
01435                                           CPK-PROP ' ' CPK-TXTYP
01436            DISPLAY 'PREVIOUS RECORD IS  ' PPK-TOWN ' ' PPK-VOL ' '
01437                                           PPK-PROP ' ' PPK-TXTYP
01438            MOVE 16          TO RETURN-CODE
01439         ELSE
01440            MOVE CURR-PA-KEY TO PREV-PA-KEY, SAVE-PA-KEY.
01441
01442  300-READ-EXPIRED-ASSMT.
01443      READ EXPIRED-ASSMT AT END
01444         MOVE 1              TO EXPIRED-ASSMT-EOF-SW
01445         MOVE HIGH-VALUES    TO SAVE-EX-KEY.
01446      IF NOT EXPIRED-ASSMT-EOF
01447         ADD 1               TO EXPIRED-ASSMT-REC-CTR
01448         MOVE AS-TXCD        TO TXCD-HLD
01449         MOVE TXCD-HLD (1:2) TO CEK-TOWN
01450         MOVE AS-VOL         TO CEK-VOL
01451         MOVE AS-PROP        TO CEK-PROP
01452         MOVE AS-TXTYP       TO CEK-TXTYP
01453         IF CURR-EX-KEY NOT GREATER PREV-EX-KEY
01454            DISPLAY 'EXPIRED-ASSMT IS OUT OF SEQUENCE'
01455            DISPLAY 'CURRENT RECORD IS   ' CEK-TOWN ' ' CEK-VOL ' '
01456                                           CEK-PROP ' ' CEK-TXTYP
01457            DISPLAY 'PREVIOUS RECORD IS  ' PEK-TOWN ' ' PEK-VOL ' '
01458                                           PEK-PROP ' ' PEK-TXTYP
01459            MOVE 16          TO RETURN-CODE
01460         ELSE
01461            MOVE CURR-EX-KEY TO PREV-EX-KEY, SAVE-EX-KEY.
01462
01463  350-READ-EQUALFCT.
01464      MOVE BASE-YR (3:2) TO EQ-YEAR
01465      MOVE 1             TO EQ-QUAD
01466      READ EQUALFCT
01467      IF NORMAL-STATUS OR RECORD-NOT-FOUND
01468         NEXT SENTENCE
01469      ELSE
01470         DISPLAY 'EQUALFCT READ FAILED  ' FILE-STATUS
01471                 '   RETURN  '   FS-RETURN
01472                 '   FUNCTION  ' FS-FUNC
01473                 '   FEEDBACK  ' FS-FEEDBACK
01474         MOVE 16 TO RETURN-CODE.
01475
01476  400-CODE-CLASS-CONDITIONS.
01477      IF FILE-SW EQUAL 'P'
01478         MOVE ZEROES TO SUB2
01479      ELSE
01480         MOVE 6      TO SUB2.
01481      MOVE H1-CLS (SUB1) TO CLASS-HLD
01482      IF RESD-IMPV
01483         ADD 1 TO SUB2
01484         ADD 1 TO CLASS-TAB (SUB2)
01485      ELSE
01486         IF RESD-LAND
01487            ADD 3 TO SUB2
01488            ADD 1 TO CLASS-TAB (SUB2)
01489         ELSE
01490            IF FARM-LAND
01491               ADD 4 TO SUB2
01492               ADD 1 TO CLASS-TAB (SUB2)
01493               MOVE 'Y' TO FARM-SW
01494            ELSE
01495               IF MIXED-USE
01496                  ADD 5 TO SUB2
01497                  ADD 1 TO CLASS-TAB (SUB2)
01498               ELSE
01499                  IF NON-RESD
01500                     ADD 6 TO SUB2
01501                     ADD 1 TO CLASS-TAB (SUB2).
01502      IF SEG-WHERE-QUEST-FOLLOWS
01503         ADD 1 TO SUB1.
01504
01505  425-COOP-CLASS-CHECK.
01506      MOVE H1-CLS (SUB1) TO CLASS-HLD
01507      IF NOT MAJOR-CLASS-2
01508         IF FILE-SW = 'C'
01509            MOVE 'Y' TO CURR-MIXED-CLS-SW
01510         ELSE
01511            IF FILE-SW = 'P'
01512               MOVE 'Y' TO PRIOR-MIXED-CLS-SW
01513            END-IF
01514         END-IF
01515         MOVE 'Y' TO NO-CALC-SW
01516         DISPLAY 'COOP CONTAINS MIXED MAJOR CLASSES '
01517            SAVE-CA-KEY (1:2) ' ' SAVE-CA-KEY (3:3) ' '
01518            SAVE-CA-KEY (6:15) ' ' SAVE-CA-KEY (21:1)
01519         MOVE 15 TO RETURN-CODE
01520         ADD 1   TO INVALID-CLS-COND-CTR.
01521      IF SEG-WHERE-QUEST-FOLLOWS
01522         ADD 1 TO SUB1.
01523
01524  450-MOVE-CURR-ASSMT-TO-WS.
01525      MOVE M-BASE          TO H-BASE
01526      MOVE M-SLS-CTR-1     TO H-SLS-CTR-1
01527      MOVE M-DTL-QST-CTR-1 TO H-DTL-QST-CTR-1
01528      PERFORM
01529      VARYING SUB1 FROM 1 BY 1
01530      UNTIL SUB1 GREATER M-DTL-QST-CTR-1
01531         MOVE M-DTL-QST-1 (SUB1) TO H-DTL-QST-1 (SUB1)
01532      END-PERFORM.
01533
01534  500-MOVE-PRIOR-ASSMT-TO-WS.
01535      MOVE MA-BASE          TO H-BASE
01536      MOVE MA-SLS-CTR-1     TO H-SLS-CTR-1
01537      MOVE MA-DTL-QST-CTR-1 TO H-DTL-QST-CTR-1
01538      PERFORM
01539      VARYING SUB1 FROM 1 BY 1
01540      UNTIL SUB1 GREATER MA-DTL-QST-CTR-1
01541         MOVE MA-DTL-QST-1 (SUB1) TO H-DTL-QST-1 (SUB1)
01542      END-PERFORM.
01543
01544  600-STEP07.
01545      IF PRIOR-ASSMT-PRESENT
01546         PERFORM 700-STEP07-A
01547         MOVE WS-PROC-YR TO BASE-YR
01548         MOVE H-CLS  TO BASE-YR-CLS
01549         PERFORM
01550         VARYING SUB1 FROM 1 BY 1
01551         UNTIL SUB1 GREATER H-DTL-QST-CTR-1
01552            ADD H1-VAL (SUB1)  TO BASE-VAL-AV
01553            MOVE H1-CLS (SUB1) TO CLASS-HLD
01554            IF RESD-LAND OR RESD-IMPV
01555               ADD H1-VAL (SUB1) TO BASE-ELIG-COMP-AV
01556            ELSE
01557               IF HIMP
01558                  ADD H1-VAL (SUB1) TO 288-OVER-30000-AV
01559               END-IF
01560            END-IF
01561            IF SEG-WHERE-QUEST-FOLLOWS
01562               ADD 1 TO SUB1
01563            END-IF
01564         END-PERFORM
01565         PERFORM 350-READ-EQUALFCT
01566         IF NORMAL-STATUS
01567            COMPUTE 288-OVER-30000-EAV ROUNDED = EQ-FACTOR *
01568                                                 288-OVER-30000-AV
01569         ELSE
01570            IF RECORD-NOT-FOUND
01571               MOVE 'Y' TO NO-CALC-SW
01572               ADD 1    TO CURR-NO-CALC-CTR
01573               MOVE 'Q' TO MESSG-SW
01574               GO TO 600-EXIT
01575            END-IF
01576         END-IF
01577      END-IF.
01578      MOVE PR-PROC-YR TO CURR-YR
01579      MOVE M-CLS      TO CURR-YR-CLS
01580      PERFORM 450-MOVE-CURR-ASSMT-TO-WS
01581      PERFORM
01582      VARYING SUB1 FROM 1 BY 1
01583      UNTIL SUB1 GREATER H-DTL-QST-CTR-1
01584         ADD H1-VAL  (SUB1) TO CURR-VAL-AV
01585         MOVE H1-CLS (SUB1) TO CLASS-HLD
01586         MOVE H3-YR  (SUB1) TO YR-HLD
01587         IF RESD-LAND OR RESD-IMPV OR HIMP
01588            ADD H1-VAL (SUB1) TO CURR-ELIG-COMP-AV
01589         END-IF
01590         IF HIMP AND YR-HLD EQUAL PR-PROC-YR (3:2)
01591            ADD H1-VAL (SUB1) TO 288-OVER-30000-AV HIMP-TOT
01592         END-IF
01593         IF SEG-WHERE-QUEST-FOLLOWS
01594            ADD 1             TO SUB1
01595         END-IF
01596      END-PERFORM
01597      COMPUTE 288-OVER-30000-EAV ROUNDED = 288-OVER-30000-EAV +
01598              (HIMP-TOT * PR-EQ-FCTR-N).
01599  600-EXIT. EXIT.
01600
01601  605-STEP07.
01602      IF PRIOR-ASSMT-PRESENT
01603         PERFORM 700-STEP07-A
01604         MOVE WS-PROC-YR TO BASE-YR
01605         MOVE H-CLS  TO BASE-YR-CLS
01606         PERFORM
01607         VARYING SUB1 FROM 1 BY 1
01608         UNTIL SUB1 GREATER H-DTL-QST-CTR-1
01609            MOVE H1-CLS (SUB1) TO CLASS-HLD
01610               IF CLASS-HLD = 236
01611                  ADD H1-VAL (SUB1) TO WS-236-VALUE
01612               ELSE
01613                  IF H1-TYP (SUB1) = 2 OR 3 OR 4 OR 5
01614                     ADD H1-VAL (SUB1) TO WS-IMPRV-VALUE
01615                  ELSE
01616                     IF H1R-TYPE1 (SUB1)
01617                        ADD H1-VAL (SUB1) TO WS-TYPE1-VALUE
01618                     ELSE
01619                        IF HIMP
01620                          ADD H1-VAL (SUB1) TO 288-OVER-30000-AV
01621                        END-IF
01622                     END-IF
01623                  END-IF
01624               END-IF
01625            IF SEG-WHERE-QUEST-FOLLOWS
01626               ADD 1 TO SUB1
01627            END-IF
01628         END-PERFORM
01629         PERFORM 350-READ-EQUALFCT
01630         IF NORMAL-STATUS
01631            COMPUTE 288-OVER-30000-EAV ROUNDED = EQ-FACTOR *
01632                                                 288-OVER-30000-AV
01633         ELSE
01634            IF RECORD-NOT-FOUND
01635               MOVE 'Y' TO NO-CALC-SW
01636               ADD 1    TO CURR-NO-CALC-CTR
01637               MOVE 'Q' TO MESSG-SW
01638               GO TO 600-EXIT
01639            END-IF
01640         END-IF
01641      END-IF.
01642      MOVE PR-PROC-YR TO CURR-YR
01643      MOVE M-CLS      TO CURR-YR-CLS
01644      PERFORM 450-MOVE-CURR-ASSMT-TO-WS
01645      PERFORM
01646      VARYING SUB1 FROM 1 BY 1
01647      UNTIL SUB1 GREATER H-DTL-QST-CTR-1
01648         MOVE H1-CLS (SUB1) TO CLASS-HLD
01649         IF CLASS-HLD = 236
01650            ADD H1-VAL  (SUB1) TO CURR-VAL-AV
01651            MOVE H3-YR  (SUB1) TO YR-HLD
01652            IF CLASS-HLD = 236
01653               ADD H1-VAL (SUB1) TO WS-236-VALUE
01654            ELSE
01655               IF H1-TYP (SUB1) = 2 OR 3 OR 4 OR 5
01656                  ADD H1-VAL (SUB1) TO WS-IMPRV-VALUE
01657               ELSE
01658                  IF H1R-TYPE1 (SUB1)
01659                     ADD H1-VAL (SUB1) TO WS-TYPE1-VALUE
01660                  END-IF
01661               END-IF
01662            END-IF
01663         END-IF
01664         IF HIMP AND YR-HLD EQUAL PR-PROC-YR (3:2)
01665            ADD H1-VAL (SUB1) TO 288-OVER-30000-AV HIMP-TOT
01666         END-IF
01667         IF SEG-WHERE-QUEST-FOLLOWS
01668            ADD 1             TO SUB1
01669         END-IF
01670      END-PERFORM
01671      COMPUTE 288-OVER-30000-EAV ROUNDED = 288-OVER-30000-EAV +
01672              (HIMP-TOT * PR-EQ-FCTR-N).
01673  605-EXIT. EXIT.
01674
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
01727 *       IF NOT COOP-YES
01728            IF PRIOR-RESD-IMPV-CTR GREATER ZERO
01729               COMPUTE PRIOR-IMPV-TOT ROUNDED = PRIOR-IMPV-TOT /
01730                                                PRIOR-RESD-IMPV-CTR
01731               COMPUTE PRIOR-LAND-TOT ROUNDED = PRIOR-LAND-TOT /
01732                                                PRIOR-RESD-IMPV-CTR
01733            ELSE
01734               MOVE 'Y' TO NO-CALC-SW
01735            END-IF
01736 *       END-IF
01737         MOVE ZEROES  TO BASE-ELIG-COMP-AV
01738         ADD PRIOR-IMPV-TOT PRIOR-LAND-TOT GIVING BASE-ELIG-COMP-AV
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
01761 *    IF NOT COOP-YES
01762         IF CURR-RESD-IMPV-CTR GREATER ZERO
01763            COMPUTE CURR-IMPV-TOT ROUNDED = CURR-IMPV-TOT /
01764                                            CURR-RESD-IMPV-CTR
01765            COMPUTE CURR-LAND-TOT ROUNDED = CURR-LAND-TOT /
01766                                            CURR-RESD-IMPV-CTR
01767         ELSE
01768            MOVE 'Y' TO NO-CALC-SW
01769         END-IF
01770 *    END-IF
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
01851         ADD PRIOR-IMPV-TOT PRIOR-LAND-TOT GIVING BASE-ELIG-COMP-AV
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
01895            MOVE AS3-YR(SUB1) TO WS-AS3-YEAR
01896            MOVE WS-AS3-YR    TO WK-AS3-YR
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
01918               WK-AS3-YEAR NOT LESS    WS-FR-YEAR AND
01919               WK-AS3-YEAR NOT GREATER WS-TO-YEAR
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
01977         IF C145-BVYTOTELCOMPEV EQUAL ZERO
01978            MOVE C145-BASVALYR TO BASE-YR
01979         ELSE
01980            MOVE C145-BASVALYR TO BASE-YR
01981            MOVE C145-BVYTOTELCOMPEV TO BASE-ELIG-COMP-EAV
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
02034            MOVE 'TOTAL CURRENT BASE VALUE EAV IS EQUAL TO ZERO' TO
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
02062      PERFORM 1750-DETERMINE-CHECKPOINT
02063      IF RETURN-CODE NOT EQUAL 16
02064         PERFORM 1675-GHU-ROOT-ASS-EX-MAST-SEG
02065         IF PI-DB-GOOD-STATUS
02066            PERFORM 1200-UPDATE-EXEMP-MAST-SEG.
02067            PERFORM 1725-REPL-EXEMP-MAST-SEG.
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
02271      MOVE '                SENIOR FREEZE BASE VALUE UPDATE REPORT'
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
02304      MOVE 'TOTAL MASTER SEGMENTS UPDATED'           TO T1-MESSG
02305      MOVE EXEMP-MAST-REPL-CTR                       TO T1-TOTAL
02306      WRITE PRINT-REC-1 FROM TOTAL1 AFTER ADVANCING 1
02307      MOVE 'TOTAL FINDER RECORDS WRITTEN'            TO T1-MESSG
02308      MOVE FINDER-REC-CTR                            TO T1-TOTAL
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
02338      MOVE 'TOTAL MASTER SEGMENTS UPDATED'           TO T1-MESSG
02339      MOVE EXEMP-MAST-REPL-CTR                       TO T1-TOTAL
02340      WRITE PRINT-REC-2 FROM TOTAL1 AFTER ADVANCING 1
02341      MOVE 'TOTAL FINDER RECORDS WRITTEN'            TO T1-MESSG
02342      MOVE FINDER-REC-CTR                            TO T1-TOTAL
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
02372      MOVE 'TOTAL MASTER SEGMENTS UPDATED'           TO T1-MESSG
02373      MOVE EXEMP-MAST-REPL-CTR                       TO T1-TOTAL
02374      WRITE PRINT-REC-3 FROM TOTAL1 AFTER ADVANCING 1
02375      MOVE 'TOTAL FINDER RECORDS WRITTEN'            TO T1-MESSG
02376      MOVE FINDER-REC-CTR                            TO T1-TOTAL
02377      WRITE PRINT-REC-3 FROM TOTAL1 AFTER ADVANCING 1
02378      MOVE 'TOTAL RECORDS WITH UNACCOUNTED CLASS CONDITIONS'
02379                                                     TO T1-MESSG
02380      MOVE INVALID-CLS-COND-CTR                      TO T1-TOTAL
02381      WRITE PRINT-REC-3 FROM TOTAL1 AFTER ADVANCING 1.
02382
02383 *1550-FORMAT-FINDER-REC.
02384 *    MOVE SPACES TO FINDER-REC
02385 *    MOVE M-VOL  TO FR-VOL
02386 *    MOVE M-PROP TO FR-PROP
02387 *    WRITE FINDER-REC
02388 *    ADD 1       TO FINDER-REC-CTR.
02389
02390  1600-GU-ROOT-ASSESS-SEG.
02391      MOVE M-PROP           TO LVL1-ARG
02392      MOVE PR-PROC-YR (3:2) TO LVL2-PROCYR, LVL2-TXYR
02393      MOVE M-TXTYP          TO LVL2-TXTYP
02394      CALL 'CBLTDLI' USING GU
02395                           PI-PCB1
02396                           PY-ASMTDATA
02397                           LVL1-QUAL-SSA
02398                           LVL2-QUAL-SSA.
02399      IF PI-DB-GOOD-STATUS
02400         NEXT SENTENCE
02401      ELSE
02402         IF PI-SEG-NOTFND AND PI-SEGMENT-LVL EQUAL ZEROES
02403            DISPLAY 'ROOT SEGMENT NOT FOUND USING A GU  ' LVL1-ARG
02404         ELSE
02405            IF PI-SEG-NOTFND AND PI-SEGMENT-LVL EQUAL '01'
02406               DISPLAY 'ASSESSMENT SEGMENT NOT FOUND USING A GU  '
02407                       LVL1-ARG ' ' LVL2-ARG
02408            ELSE
02409               DISPLAY 'ROOT/ASSESSMENT SEGMENT GU ERROR  '
02410                       LVL1-ARG ' ' LVL2-ARG
02411               PERFORM 1900-DISPLAY-PI-PCB.
02412
02413  1625-GNP-EXEMP-MAST-SEG.
02414      CALL 'CBLTDLI' USING GNP
02415                           PI-PCB1
02416                           C145-SENFRZMASTER
02417                           LVL3-UNQUAL-SSA.
02418      IF PI-DB-GOOD-STATUS
02419         IF C150-SHARES GREATER ZEROES
02420            MOVE 'Y' TO COOP-SW
02421         ELSE
02422            NEXT SENTENCE
02423      ELSE
02424          IF PI-SEG-NOTFND
02425             NEXT SENTENCE
02426          ELSE
02427             DISPLAY 'EXEMPTION MASTER SEGMENT GNP ERROR  '
02428                     LVL1-ARG ' ' LVL2-ARG
02429             PERFORM 1900-DISPLAY-PI-PCB.
02430
02431  1650-GNP-EXEMP-DET-SEG.
02432      CALL 'CBLTDLI' USING GNP
02433                           PI-PCB1
02434                           C150-SENFRZDETAIL
02435                           LVL4-UNQUAL-SSA.
02436      IF PI-DB-GOOD-STATUS
02437         ADD 1 TO EXEMP-DET-SEG-CTR
02438      ELSE
02439         IF PI-SEG-NOTFND
02440            MOVE 1 TO EXEMP-DET-EOF-SW
02441         ELSE
02442            DISPLAY 'EXEMPTION DETAIL SEGMENT GNP ERROR  '
02443                    LVL1-ARG ' ' LVL2-ARG ' ' C145-RECCODE
02444            PERFORM 1900-DISPLAY-PI-PCB.
02445
02446  1675-GHU-ROOT-ASS-EX-MAST-SEG.
02447      MOVE M-PROP           TO LVL1-ARG
02448      MOVE PR-PROC-YR (3:2) TO LVL2-PROCYR, LVL2-TXYR
02449      MOVE M-TXTYP          TO LVL2-TXTYP
02450      MOVE C145-RECCODE     TO LVL3-ARG
02451      CALL 'CBLTDLI' USING GHU
02452                           PI-PCB1
02453                           C145-SENFRZMASTER
02454                           LVL1-QUAL-SSA
02455                           LVL2-QUAL-SSA
02456                           LVL3-QUAL-SSA.
02457      IF PI-DB-GOOD-STATUS
02458         NEXT SENTENCE
02459      ELSE
02460         IF PI-SEG-NOTFND AND PI-SEGMENT-LVL EQUAL ZEROES
02461            DISPLAY 'ROOT SEGMENT NOT FOUND USING GHU  ' LVL1-ARG
02462         ELSE
02463            IF PI-SEG-NOTFND AND PI-SEGMENT-LVL EQUAL '01'
02464               DISPLAY 'ASSESSMENT SEGMENT NOT FOUND USING GHU  '
02465                       LVL1-ARG ' ' LVL2-ARG
02466            ELSE
02467               IF PI-SEG-NOTFND AND PI-SEGMENT-LVL EQUAL '02'
02468                  DISPLAY 'EXEMPTION MASTER SEGMENT NOT FOUND USIN
02469 -                        ' GHU  ' LVL1-ARG ' ' LVL2-ARG ' '
02470                          LVL3-ARG
02471               ELSE
02472                  DISPLAY 'ROOT/ASSESSMENT/EXEMPTION MASTER SEGMEN
02473 -                        ' GHU ERROR  ' LVL1-ARG ' ' LVL2-ARG ' '
02474                          LVL3-ARG
02475                  PERFORM 1900-DISPLAY-PI-PCB.
02476
02477  1725-REPL-EXEMP-MAST-SEG.
02478      CALL 'CBLTDLI' USING REPL
02479                           PI-PCB1
02480                           C145-SENFRZMASTER.
02481      IF PI-DB-GOOD-STATUS
02482         ADD 1 TO EXEMP-MAST-REPL-CTR
02483         ADD 2 TO CICV-CTR
02484      ELSE
02485         DISPLAY 'EXEMPTION MASTER SEGMENT REPL ERROR  '
02486                 LVL1-ARG ' ' LVL2-ARG ' ' LVL3-ARG
02487         PERFORM 1900-DISPLAY-PI-PCB.
02488
02489  1750-DETERMINE-CHECKPOINT.
02490      IF CICV-CTR GREATER PR-CICV-VAL-N
02491         ADD 1       TO CPI-TRANS-NO
02492         PERFORM 1775-ISSUE-CHECKPOINT
02493         MOVE ZEROES TO CICV-CTR.
02494
02495  1775-ISSUE-CHECKPOINT.
02496      CALL 'CBLTDLI' USING CHKP, IO-PCB, CHECK-POINT-ID
02497      IF IP-GOOD-STATUS
02498         NEXT SENTENCE
02499      ELSE
02500         PERFORM 1800-ISSUE-CHKPT-ERR.
02501
02502  1800-ISSUE-CHKPT-ERR.
02503      CALL 'CBLTDLI' USING ROLL
02504      DISPLAY 'CHECK POINT ISSUE ERROR'
02505      DISPLAY 'LOGICAL TERMINAL NAME  ' IP-TERM-NAME
02506      DISPLAY 'STATUS CODE            ' IP-STATUS-CODE
02507      MOVE 16 TO RETURN-CODE.
02508
02509  1900-DISPLAY-PI-PCB.
02510      DISPLAY 'THE DBD NAME IS                         '
02511              PI-DBD-NAME
02512      DISPLAY 'THE SEGMENT LEVEL IS                    '
02513              PI-SEGMENT-LVL
02514      DISPLAY 'THE STATUS CODE IS                      '
02515              PI-STATUS-CODE
02516      DISPLAY 'THE PROCESSING OPTIONS ARE              '
02517              PI-PROC-OPT
02518      DISPLAY 'THE SEGMENT NAME IS                     '
02519              PI-NAME-FDBK
02520      DISPLAY 'THE LENGTH OF THE KEY FEEDBACK AREA IS  '
02521              PI-KEY-FDBK-LNG
02522      DISPLAY 'THE NUMBER OF SENSITIVE SEGMENTS ARE    ' PI-SEN-SEG
02523      DISPLAY 'THE KEY FEEDBACK AREA IS                '
02524              PI-KEY-FB-ITEM
02525      MOVE 16 TO RETURN-CODE.
