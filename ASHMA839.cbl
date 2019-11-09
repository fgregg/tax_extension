00001  IDENTIFICATION DIVISION.
00002                          SKIP1
00003  PROGRAM-ID.  ASHMA839.
00004 *AUTHOR.  VINCENT P. FALK -- KATHY LEE FOLAN.
00005 *DATE-WRITTEN.  JUN, 1997.
00006                          SKIP1
00007 *REMARKS.  THIS PROGRAM WILL CREATE TWO OUTPUT FILES BASED
00008 *    ON MATCH LOGIC BETWEEN THE ASSESSOR'S HOMEOWNER MASTER
00009 *    FILE AND THE ASSESSMENT MASTER FILE.
00010 *    IF M-VALUE(1) > 0 (SEN FREEZE) MOVE 4500 TO NPHE-AMOUNT
00011 *
00012 *****************************************************************
00013 * CHANGED 05/13/2005 TJM TO ADD THE LANDMARK FILE AND LOGIC FOR
00014 *                        NEW NPHE LOW VALUE AND UPDATE THE BASE
00015 *                        YEAR ON HOMEOWNER FILE.
00016 *                        ALSO, INCLUDE NEW NPHE PRORATION FILE.
00017 *****************************************************************
00018 *
00019 * CHANGED 04/25/2006 JTS INCORPORATED LOGIC CHANGES FOR SOUTH-
00020 *                        WEST TOWNS
00021 *
00022 *****************************************************************
00023 * CHANGED 06/18/2007 TJM TO MAKE CHANGES FOR THE NEW TRI AND FOR
00024 *                        CITY TOWNS, COMPARE BASE EAV WITH CURREN
00025 *                        IF DIFFERENCE IS BETWEEN 180% AND 200%,
00026 *                        ADD 5000 TO NPHE-AMT. IF GREATER THAN
00027 *                        200%, ADD 10000 TO NPHE-AMT.
00028 *****************************************************************
00029 * CHANGED 07/20/2007 TJM TO CORRECT DIVISION PROCESSING.
00030 *****************************************************************
00031 *****************************************************************
00032 * CHANGED 08/07/2007 TJM BONUS AMOUNT STARTS AT $33,000 FOR CITY.
00033 *                        IF GREATER THAN 200%, ADD $7,000 TO NPHE
00034 *                        IF GREATER THAN 180%, ADD $2,000 TO NPHE
00035 *****************************************************************
00036 * CHANGED 06/03/2008 NTF MODIFIED PROGRAM TO INCORPORATE LOGIC
00037 *                        CHANGES FOR NEW TRI.
00038 *****************************************************************
00039 * CHANGED 09/19/2008 JTS MODIFIED PROGRAM TO INCORPORATE LOGIC
00040 *                        TO UPDATE COE PROCESSING.
00041 *****************************************************************
00042 *****************************************************************
00043 * CHANGED 07/01/2009 TJM CHANGES FOR 2008 TAX YEAR.
00044 *                        CHANGE LOW VALUE NPHE TO 5500 E LOGIC AN
00045 *                        CHANGE HIGH VALUE FOR CITY TO 20000, NW
00046 *                        TO 26000 AND SW TO 33000.
00047 *****************************************************************
00048 * CHANGED 08/10/2010 KLF CHANGES FOR 2009 TAX YEAR.
00049 *                        CHANGE LOW VALUE NPHE TO 6000 LOGIC AND
00050 *                        CHANGE HIGH VALUE FOR NRWS TO 20000,
00051 *                        AND SW TO 26000.
00052 *****************************************************************
00053 *****************************************************************
00054 * CHANGED 09/01/2010 TJM USE A NEW FIELD - WS-PRIOR-MINIMUM - IN
00055 *                        2320-DIVISIONS-ROUTINE INSTEAD OF A
00056 *                        LITERAL.
00057 *****************************************************************
00058 * CHANGED 06/16/2011 TJM CHANGE WS-PRIOR-MINIMUM    TO  6000
00059 *                        CHANGE CITY HIGH VALUE FOR TO 16000
00060 *                        CHANGE NW   HIGH VALUE FOR TO 20000
00061 *****************************************************************
00062 * CHANGED 04/03/2012 TJM CHANGE WS-PRIOR-MINIMUM TO  6000
00063 *                        CHANGE NPHE-HI-VAL-CITY TO 12000
00064 *                        CHANGE NPHE-HI-VAL-NRWS TO 16000
00065 *****************************************************************
00066 * CHANGED 03/08/2013 TJM CHANGE WS-PRIOR-MINIMUM TO  6000
00067 *                        CHANGE NPHE-HI-VAL-CITY TO  6000
00068 *                        CHANGE NPHE-HI-VAL-NRWS TO 12000
00069 *                        CHANGE NPHE-HI-VAL-SWWS TO 16000
00070 *****************************************************************
00071 * CHANGED 04/15/2013 TJM CHANGE NPHE-LO-VAL      TO  7000
00072 *                        CHANGE NPHE-HI-VAL-CITY TO  7000
00073 *****************************************************************
00074 *****************************************************************
00075 * CHANGED 04/14/2014 KLF CHANGE NPHE-HI-VAL-NRWS TO  7000
00076 *                        CHANGE NPHE-HI-VAL-SWWS TO 12000
00077 *****************************************************************
00078 *****************************************************************
00079 * CHANGED 04/27/2015 KLF CHANGE NPHE-HI-VAL-SWWS TO  7000
00080 *                               NPHE-HI-VAL      TO  7000
00081 *****************************************************************
00082 *****************************************************************
00083 * CHANGED 04/20/2018 TMB CHANGE NPHE-LO-VAL      TO 10000
00084 *                        CHANGE NPHE-HI-VAL-CITY TO 10000
00085 *                        CHANGE NPHE-HI-VAL-NRWS TO 10000
00086 *                        CHANGE NPHE-HI-VAL-SWWS TO 10000
00087 *                        CHANGE NPHE-HI-VAL      TO 10000
00088 *****************************************************************
00089                          SKIP1
00090  ENVIRONMENT DIVISION.
00091                          SKIP1
00092  CONFIGURATION SECTION.
00093  SOURCE-COMPUTER.  IBM-370.
00094  OBJECT-COMPUTER.  IBM-370.
00095                          SKIP1
00096  INPUT-OUTPUT SECTION.
00097  FILE-CONTROL.
00098      SELECT HOMEOWNER-MAST   ASSIGN TO UT-S-HOMASTER.
00099      SELECT MASTER-IN        ASSIGN TO UT-S-CURASMST.
00100      SELECT MASTER-OUT-2     ASSIGN TO UT-S-PRIASMST.
00101      SELECT HOMEOWNER-MAST2  ASSIGN TO UT-S-HOMSTOUT.
00102      SELECT NPHE-FILE        ASSIGN TO UT-S-NPHEFILE.
00103 **   SELECT PRINT-FILE       ASSIGN TO UT-S-ERRRPT.
00104      SELECT LNDMRK-PROP-FILE ASSIGN TO DA-LDMKFILE
00105        ORGANIZATION IS INDEXED
00106        ACCESS IS RANDOM
00107        RECORD KEY IS LDMK-KEY
00108        FILE STATUS IS LDMK-STATUS LDMK-STATUS-2.
00109      SELECT NPHE-PRORATION-FILE ASSIGN TO DA-NPHEPRO
00110        ORGANIZATION IS INDEXED
00111        ACCESS IS RANDOM
00112        RECORD KEY IS NP-KEY
00113        FILE STATUS IS NP-STATUS NP-STATUS-2.
00114 *    SELECT EQUAL-FACTOR ASSIGN TO DA-EQUALFCT
00115 *      ORGANIZATION IS INDEXED
00116 *      ACCESS IS RANDOM
00117 *      RECORD KEY IS EQ-KEY
00118 *      FILE STATUS IS EQ-STATUS EQ-STATUS-2.
00119                          SKIP2
00120  DATA DIVISION.
00121                          SKIP1
00122  FILE SECTION.
00123  COPY HOMOWNFD01.
00124                          SKIP1
00125  COPY ASREASFD01.
00126                          SKIP1
00127  COPY ASREASFD08.
00128                          SKIP1
00129  COPY HOMOWNFD02.
00130                          SKIP1
00131  COPY ASNPHEFD01.
00132                          SKIP1
00133 ***INCLUDE PRINTFILE
00134                          SKIP2
00135  COPY ASLNDMRKF1.
00136                          SKIP1
00137  COPY ASNPHEPRF1.
00138                          SKIP1
00139 *FD  EQUAL-FACTOR
00140 *    BLOCK CONTAINS 0 RECORDS
00141 *    LABEL RECORDS STANDARD
00142 *    RECORD CONTAINS 21 CHARACTERS
00143 *    DATA RECORD IS EQ-RECORD.
00144 *01  EQ-RECORD.
00145 *COPY REBEQFRD01.
00146                          SKIP1
00147  WORKING-STORAGE SECTION.
00148                          SKIP1
00149 *****SWITCHES.
00150  77  LNDMRK-EOF-SW       PIC X   VALUE 'N'.
00151      88  LNDMRK-EOF              VALUE 'Y'.
00152      88  LNDMRK-FND              VALUE 'N'.
00153  77  LNDMRK-RECS-READ    PIC S9(9) PACKED-DECIMAL VALUE +0.
00154  77  WS-NP-EOF-SW        PIC X   VALUE 'N'.
00155      88  NP-EOF                  VALUE 'Y'.
00156      88  NP-FND                  VALUE 'N'.
00157  77  WS-NP-RECS-READ     PIC S9(9) PACKED-DECIMAL VALUE +0.
00158  77  WS-STATUS           PIC XX   VALUE SPACES.
00159  77  WS-PRORATE          PIC 9(5)  PACKED-DECIMAL VALUE 0.
00160  77  WS-PRORATE-SW       PIC X   VALUE ' '.
00161  77  WS-STAT-SW          PIC X   VALUE ' '.
00162  77  SEVERE-ERR-SW       PIC X            VALUE 'N'.
00163      88  NO-SEVERE-ERR                    VALUE 'N'.
00164      88  SEVERE-ERR                       VALUE 'Y'.
00165  77  EOF-HOMS-SW         PIC X            VALUE 'N'.
00166      88  NO-EOF-HOMS                      VALUE 'N'.
00167      88  EOF-HOMS                         VALUE 'Y'.
00168  77  EOF-CMST-SW         PIC X            VALUE 'N'.
00169      88  NO-EOF-CMST                      VALUE 'N'.
00170      88  EOF-CMST                         VALUE 'Y'.
00171  77  EOF-PMST-SW         PIC X            VALUE 'N'.
00172      88  NO-EOF-PMST                      VALUE 'N'.
00173      88  EOF-PMST                         VALUE 'Y'.
00174  77  READ-HOMS-SW        PIC X            VALUE 'N'.
00175      88  READ-HOMS-RESET                  VALUE 'N'.
00176      88  READ-HOMS                        VALUE 'Y'.
00177  77  READ-CMST-SW        PIC X            VALUE 'N'.
00178      88  READ-CMST-RESET                  VALUE 'N'.
00179      88  READ-CMST                        VALUE 'Y'.
00180  77  VALID-HOMS-SW       PIC X            VALUE 'N'.
00181      88  VALID-HOMS-RESET                 VALUE 'N'.
00182      88  VALID-HOMS                       VALUE 'Y'.
00183  77  PMST-READ-SW        PIC X            VALUE 'N'.
00184      88  PMST-READ-RESET                  VALUE 'N'.
00185      88  PMST-READ                        VALUE 'Y'.
00186  77  PRIOR-MISSINT-SW    PIC X            VALUE 'N'.
00187      88  PRIOR-MISSING-RESET              VALUE 'N'.
00188      88  PRIOR-MISSING                    VALUE 'Y'.
00189  77  AGE-SW              PIC X            VALUE 'N'.
00190      88  AGE-NO                           VALUE 'N'.
00191      88  AGE-YES                          VALUE 'Y'.
00192  77  NO-REC-SW           PIC X            VALUE 'N'.
00193      88  NO-REC-RESET                     VALUE 'N'.
00194      88  NO-REC                           VALUE 'Y'.
00195  77  HO-REC-FLAG         PIC X            VALUE 'N'.
00196      88  HO-REC-NOCHANGE                  VALUE 'N'.
00197      88  HO-REC-NPHE                      VALUE 'Y'.
00198      88  HO-REC-ZERO                      VALUE 'Z'.
00199                          SKIP1
00200 *****WORK VARIABLES.
00201  77  CMST-SUB            PIC S9(4)        VALUE +0     BINARY.
00202  77  CTR-DISPLAY         PIC Z,ZZZ,ZZ9.
00203  77  DISPLAY-EQ          PIC 9.9(4).
00204  77  WK-TOWN             PIC 9(2).
00205      88  CITY-TOWN   VALUE 70 71 72 73 74 75 76 77.
00206      88  NORTHWEST   VALUE 10 16 17 18 20 22 23 24 25 26 29 35 38
00207      88  SOUTHWEST   VALUE 11 12 13 14 15 19 21 27 28 30 31 32 33
00208                            34 36 37 39.
00209
00210  77  WS-VAL-CLS          PIC X(3)  VALUE SPACES.
00211  77  WK-VAL-CLS          PIC 9(3)  VALUE ZEROS.
00212      88  VALID-CLASS     VALUE 200 THRU 299.
00213
00214  77  WS-MVALUE           PIC S9(9)        PACKED-DECIMAL VALUE 0.
00215                          SKIP2
00216  77  WS-BASE-YR          PIC 9(4)  VALUE 0.
00217 *
00218  77  WS-MIN              PIC 9V9   VALUE 1.8.
00219  77  WS-MAX              PIC 9V9   VALUE 2.0.
00220  77  WS-MIN-R            PIC 9(9)  VALUE ZEROES.
00221  77  WS-MAX-R            PIC 9(9)  VALUE ZEROES.
00222  77  WS-BASE-EAV         PIC S9(9) VALUE ZEROES.
00223  77  WS-REC-FND-SW       PIC X     VALUE 'N'.
00224      88  WS-REC-FND                    VALUE 'Y'.
00225  77  SUB                 PIC S9(4) VALUE +0     BINARY.
00226  77  SUB1                PIC S9(4) VALUE +0     BINARY.
00227  77  WS-CTR              PIC S9(4) VALUE +0     BINARY.
00228  77  WS-TYPE-5-SW        PIC X     VALUE 'N'.
00229      88 TYPE-5-FOUND               VALUE 'Y'.
00230  77  WS-D5-OCCFAC        PIC S99V9 VALUE ZEROES COMP-3.
00231  01  PACKED-DECIMAL-VARIABLES             PACKED-DECIMAL.
00232 *    05  WS-PRIOR-MINIMUM PIC 9(5)         VALUE 5500.
00233 *    05  WS-PRIOR-MINIMUM PIC 9(5)         VALUE 6000.
00234      05  WS-PRIOR-MINIMUM PIC 9(5)         VALUE 7000.
00235      05  PREV-MULTIPLY    PIC 9V99         VALUE 1.07.
00236 *    05  NPHE-LO-VAL      PIC 9(5)         VALUE 4500.
00237 *    05  NPHE-LO-VAL      PIC 9(5)         VALUE 5000.
00238 *    05  NPHE-LO-VAL      PIC 9(5)         VALUE 5500.
00239 *    05  NPHE-LO-VAL      PIC 9(5)         VALUE 6000.
00240 *    05  NPHE-LO-VAL      PIC 9(5)         VALUE 7000.
00241      05  NPHE-LO-VAL      PIC 9(5)         VALUE 10000.
00242 *    05  NPHE-HI-VAL      PIC 9(5)         VALUE 20000.
00243 *    05  NPHE-HI-VAL      PIC 9(5)         VALUE 7000.
00244      05  NPHE-HI-VAL      PIC 9(5)         VALUE 10000.
00245 *
00246 *    05  NPHE-HI-VAL-CITY PIC 9(5)         VALUE 30000.
00247 *    05  NPHE-HI-VAL-CITY PIC 9(5)         VALUE 33000.
00248 *    05  NPHE-HI-VAL-CITY PIC 9(5)         VALUE 26000.
00249 *    05  NPHE-HI-VAL-CITY PIC 9(5)         VALUE 20000.
00250 *    05  NPHE-HI-VAL-CITY PIC 9(5)         VALUE 16000.
00251 *    05  NPHE-HI-VAL-CITY PIC 9(5)         VALUE 12000.
00252 *    05  NPHE-HI-VAL-CITY PIC 9(5)         VALUE  6000.
00253 *    05  NPHE-HI-VAL-CITY PIC 9(5)         VALUE  7000.
00254      05  NPHE-HI-VAL-CITY PIC 9(5)         VALUE 10000.
00255 *
00256 *    05  NPHE-HI-VAL-SWWS PIC 9(5)         VALUE 26000.
00257 *    05  NPHE-HI-VAL-SWWS PIC 9(5)         VALUE 20000.
00258 *    05  NPHE-HI-VAL-SWWS PIC 9(5)         VALUE 33000.
00259 *    05  NPHE-HI-VAL-SWWS PIC 9(5)         VALUE 20000.
00260 *    05  NPHE-HI-VAL-SWWS PIC 9(5)         VALUE 16000.
00261 *    05  NPHE-HI-VAL-SWWS PIC 9(5)         VALUE 12000.
00262 *    05  NPHE-HI-VAL-SWWS PIC 9(5)         VALUE 7000.
00263      05  NPHE-HI-VAL-SWWS PIC 9(5)         VALUE 10000.
00264 *
00265 *    05  NPHE-HI-VAL-NRWS PIC 9(5)         VALUE 33000.
00266 *    05  NPHE-HI-VAL-NRWS PIC 9(5)         VALUE 26000.
00267 *    05  NPHE-HI-VAL-NRWS PIC 9(5)         VALUE 20000.
00268 *    05  NPHE-HI-VAL-NRWS PIC 9(5)         VALUE 16000.
00269 *    05  NPHE-HI-VAL-NRWS PIC 9(5)         VALUE 12000.
00270 *    05  NPHE-HI-VAL-NRWS PIC 9(5)         VALUE  7000.
00271      05  NPHE-HI-VAL-NRWS PIC 9(5)         VALUE 10000.
00272 *
00273      05  HOMS-READ-CTR    PIC 9(7)         VALUE ZEROS.
00274      05  CMST-READ-CTR    PIC 9(7)         VALUE ZEROS.
00275      05  PMST-READ-CTR    PIC 9(7)         VALUE ZEROS.
00276      05  HOMS-OUT-CTR     PIC 9(7)         VALUE ZEROS.
00277      05  NPHE-OUT-CTR     PIC 9(7)         VALUE ZEROS.
00278      05  AGE-1-CTR        PIC 9(7)         VALUE ZEROS.
00279      05  PRIOR-CTR        PIC 9(7)         VALUE ZEROS.
00280      05  NO-PRIOR-CTR     PIC 9(7)         VALUE ZEROS.
00281      05  NO-CHG-CTR       PIC 9(7)         VALUE ZEROS.
00282      05  NPHE-AMOUNT      PIC S9(7)        VALUE ZEROS.
00283      05  PREV-BASE        PIC S9(9)        VALUE ZEROS.
00284      05  WS-PREV-BASE     PIC S9(9)        VALUE ZEROS.
00285      05  CURR-ADJ-BASE    PIC 9(9)         VALUE ZEROS.
00286      05  PREV-ADJ-BASE    PIC S9(9)        VALUE ZEROS.
00287      05  WS-CURR-AV       PIC  9(9)        VALUE ZEROS.
00288      05  MDCY-OR-CYMD     PIC S9(5)V9(4)   VALUE +10000.0001.
00289      05  WS-FULL-BASE     PIC S9(9)        VALUE ZEROS.
00290      05  SAVE-NPHE-AMOUNT PIC S9(7)        VALUE ZEROS.
00291                          SKIP1
00292  01  GROUP-VARIABLES.
00293      05 EDIT-TYP          PIC 9.
00294         88 TYPE1            VALUE 1.
00295         88 TYPE2            VALUE 2.
00296         88 TYPE2-5          VALUE 2 THRU 5.
00297         88 TYPE4            VALUE 4.
00298         88 TYPE5            VALUE 5.
00299         88 TYPE-5           VALUE 5.
00300      05 EDIT-CLS       PIC 9(3).
00301      05 EDIT-CLS-R REDEFINES EDIT-CLS.
00302         10 MAJ-CLS     PIC 9(1).
00303         10 MIN-CLS     PIC 9(2).
00304            88 RES      VALUES ARE 02 THRU 12 34 78 95.
00305      05  WS-HO-CLASS     PIC 9(3).
00306      05  WS-HO-CLASS-REDF REDEFINES WS-HO-CLASS.
00307          10  WS-HO-MAJOR-CLASS   PIC 9.
00308          10  WS-HO-MINOR-CLASS   PIC 9(2).
00309      05  CURRENT-DATE-RETURN              VALUE SPACES.
00310          10  SYSTEM-DATE PIC 9(8).
00311          10  SYSTEM-TIME PIC 9(6).
00312          10  FILLER      PIC X(7).
00313      05  CURRENT-DATE    PIC 99/99/9(4).
00314      05  HOMS-CURR-KEY                    VALUE SPACES.
00315          10  HOMS-VOL    PIC 999.
00316          10  HOMS-PROP   PIC 9(14).
00317          10  HOMS-TXTYP  PIC 9(1).
00318      05  HOMS-PREV-KEY   PIC X(18)        VALUE SPACES.
00319      05  CMST-CURR-KEY                    VALUE SPACES.
00320          10  CMST-VOL    PIC 999.
00321          10  CMST-PROP   PIC 9(14).
00322          10  CMST-TXTYP  PIC X.
00323      05  CMST-PREV-KEY   PIC X(18)        VALUE SPACES.
00324      05  PMST-CURR-KEY                    VALUE SPACES.
00325          10  PMST-VOL    PIC 999.
00326          10  PMST-PROP   PIC 9(14).
00327          10  PMST-TXTYP  PIC X.
00328      05  PMST-PREV-KEY   PIC X(18)        VALUE SPACES.
00329      05  HOMS-MATCH      PIC X(18)        VALUE SPACES.
00330      05  CMST-MATCH      PIC X(18)        VALUE SPACES.
00331      05  PMST-MATCH      PIC X(18)        VALUE SPACES.
00332      05  WS-MSG          PIC X(20)        VALUE SPACES.
00333      05  AGE-HOLD        PIC S999         VALUE ZEROS.
00334      05  CLS-HOLD        PIC S999         VALUE ZEROS.
00335      05  WS-AGE          PIC 999          VALUE ZEROS.
00336      05  EI-HOLD         PIC S99V9        VALUE ZEROS.
00337      05  WS-TXCD         PIC 9(5)         VALUE ZEROS.
00338      05  WS-TXCD-X REDEFINES WS-TXCD.
00339          10  WS-TOWN     PIC 9(2).
00340          10  FILLER      PIC 9(3).
00341      05  WK-M-CLS        PIC 9(3)         VALUE ZEROS.
00342      05  WK-M-CLS-X REDEFINES WK-M-CLS.
00343          10  WK-M-MAJ    PIC 9.
00344          10  WK-M-MIN    PIC 99.
00345
00346      05  WK-AS-CLS       PIC 9(3)         VALUE ZEROS.
00347      05  WK-AS-CLS-X REDEFINES WK-AS-CLS.
00348          10  WK-AS-MAJ    PIC 9.
00349          10  WK-AS-MIN    PIC 99.
00350
00351      05  WS-M-CLS                         VALUE ZEROS.
00352          10  WS-MOV-CLS  PIC 999.
00353              88  WS-MVAL-CLS         VALUE 202 THRU 212
00354                                            218 THRU 228
00355                                            234 278 295 299
00356                                            213 224 236 297 294.
00357          10  FILLER REDEFINES WS-MOV-CLS.
00358              15  WS-MMAJ2 PIC 9.
00359                  88  WS-MAJ2         VALUE 2.
00360              15  WS-MMIN2 PIC 99.
00361                  88  WS-MEXC2-CLS     VALUE 00 01 25 39 40 41
00362                                             88 90.
00363          10  FILLER REDEFINES WS-MOV-CLS.
00364              15  WS-MMAJ3 PIC 9.
00365                  88  WS-MAJ3         VALUE 3.
00366              15  WS-MMIN3 PIC 99.
00367                  88  WS-MEXC3-CLS     VALUE 00 01 90.
00368          10  FILLER REDEFINES WS-MOV-CLS.
00369              15  WS-MMAJ4 PIC 9.
00370                  88  WS-MAJ4         VALUE 4.
00371              15  WS-MMIN4 PIC 99.
00372                  88  WS-MEXC4-CLS     VALUE 00 01 80 81 90.
00373          10  FILLER REDEFINES WS-MOV-CLS.
00374              15  WS-MMAJ5 PIC 9.
00375                  88  WS-MAJ5         VALUE 5.
00376              15  WS-MMIN5 PIC 99.
00377                  88  WS-MEXC5-CLS     VALUE 00 01 35 50 80 81
00378                                             90.
00379          10  FILLER REDEFINES WS-MOV-CLS.
00380              15  WS-MMAJ6 PIC 9.
00381                  88  WS-MAJ6         VALUE 6.
00382              15  WS-MMIN6 PIC 99.
00383                  88  WS-MEXC6-CLS     VALUE 37 38 50 51 54 55
00384                                             63 70 71 80 81.
00385          10  FILLER REDEFINES WS-MOV-CLS.
00386              15  WS-MMAJ7 PIC 9.
00387                  88  WS-MAJ7         VALUE 7.
00388              15  WS-MMIN7 PIC 99.
00389                  88  WS-MEXC7-CLS     VALUE 00 01 35 42 43 45
00390                                             63 70 71 80 81.
00391          10  FILLER REDEFINES WS-MOV-CLS.
00392              15  WS-MMAJ8 PIC 9.
00393                  88  WS-MAJ8         VALUE 8.
00394              15  WS-MMIN8 PIC 99.
00395                  88  WS-MEXC8-CLS     VALUE 00 01 35 50 80 81
00396                                             90.
00397          10  FILLER REDEFINES WS-MOV-CLS.
00398              15  WS-MMAJ9 PIC 9.
00399                  88  WS-MAJ9         VALUE 9.
00400              15  WS-MMIN9 PIC 99.
00401                  88  WS-MEXC9-CLS     VALUE 00 01 90.
00402
00403      05  WS-AS-CLS                         VALUE ZEROS.
00404          10  WS-ASOV-CLS  PIC 999.
00405              88  WS-ASVAL-CLS         VALUE 202 THRU 212
00406                                            218 THRU 228
00407                                            234 278 295 299
00408                                            213 224 236 297 294.
00409          10  FILLER REDEFINES WS-ASOV-CLS.
00410              15  WS-ASMAJ2 PIC 9.
00411                  88  WS-ASMAJ2X        VALUE 2.
00412              15  WS-ASMIN2  PIC 99.
00413                  88  WS-ASEXC2-CLS     VALUE 00 01 25 39 40 41
00414                                              88 90.
00415          10  FILLER REDEFINES WS-ASOV-CLS.
00416              15  WS-ASMAJ3 PIC 9.
00417                  88  WS-ASMAJ3X        VALUE 3.
00418              15  WS-ASMIN3  PIC 99.
00419                  88  WS-ASEXC3-CLS     VALUE 00 01 90.
00420          10  FILLER REDEFINES WS-ASOV-CLS.
00421              15  WS-ASMAJ4 PIC 9.
00422                  88  WS-ASMAJ4X        VALUE 4.
00423              15  WS-ASMIN4  PIC 99.
00424                  88  WS-ASEXC4-CLS     VALUE 00 01 80 81 90.
00425          10  FILLER REDEFINES WS-ASOV-CLS.
00426              15  WS-ASMAJ5 PIC 9.
00427                  88  WS-ASMAJ5X        VALUE 5.
00428              15  WS-ASMIN5  PIC 99.
00429                  88  WS-ASEXC5-CLS     VALUE 00 01 35 50 80 81
00430                                              90.
00431          10  FILLER REDEFINES WS-ASOV-CLS.
00432              15  WS-ASMAJ6 PIC 9.
00433                  88  WS-ASMAJ6X        VALUE 6.
00434              15  WS-ASMIN6  PIC 99.
00435                  88  WS-ASEXC6-CLS     VALUE 37 38 50 51 54 55
00436                                              63 70 71 80 81.
00437          10  FILLER REDEFINES WS-ASOV-CLS.
00438              15  WS-ASMAJ7 PIC 9.
00439                  88  WS-ASMAJ7X        VALUE 7.
00440              15  WS-ASMIN7  PIC 99.
00441                  88  WS-ASEXC7-CLS     VALUE 00 01 35 42 43 45
00442                                              90.
00443          10  FILLER REDEFINES WS-ASOV-CLS.
00444              15  WS-ASMAJ8 PIC 9.
00445                  88  WS-ASMAJ8X        VALUE 8.
00446              15  WS-ASMIN8  PIC 99.
00447                  88  WS-ASEXC8-CLS     VALUE 00 01 35 50 80 81
00448                                              90.
00449          10  FILLER REDEFINES WS-ASOV-CLS.
00450              15  WS-ASMAJ9 PIC 9.
00451                  88  WS-ASMAJ9X        VALUE 9.
00452              15  WS-ASMIN9  PIC 99.
00453                  88  WS-ASEXC9-CLS     VALUE 00 01 90.
00454
00455      05  PARM-AREA.
00456          10  PA-PREV-EQ  PIC 9V9(4).
00457          10  PA-CURR-EQ  PIC 9V9(4).
00458          10  PA-COFE-EQ  PIC 9V9(4).
00459          10  PA-BASE-YR  PIC 9(4).
00460          10  PA-TAXYEAR  PIC 9(4).
00461                          SKIP1
00462 *****PRINT LINES.
00463      05  BLANK-LINE      PIC X            VALUE SPACE.
00464                          SKIP1
00465      05  HDR-1.
00466          10  FILLER      PIC XX           VALUE SPACES.
00467                          SKIP1
00468      05  DETAIL-LINE.
00469          10  FILLER      PIC X(4)         VALUE SPACES.
00470          10  DL-KEY      PIC XXBXXXBX(14).
00471          10  FILLER      PIC X(4)         VALUE SPACES.
00472          10  DL-MSG      PIC X(20).
00473                          SKIP1
00474      05  DISPLAY-LINE.
00475          10  FILLER      PIC X(4)         VALUE SPACES.
00476          10  DIS-KEY     PIC XXXBX(14).
00477          10  FILLER      PIC X(4)         VALUE SPACES.
00478          10  DIS-MSG     PIC X(20).
00479  01  LDMK-STATUS         PIC 99.
00480      88  GOOD-LAND             VALUE 00.
00481      88  LAND-NOTFND           VALUE 23.
00482  01  LDMK-STATUS-2 BINARY.
00483      05  LF-RETURN       PIC 99  VALUE 0.
00484      05  LF-FUNCTION     PIC 9   VALUE 0.
00485      05  LF-FEEDBACK     PIC 999 VALUE 0.
00486  01  NP-STATUS           PIC 99.
00487      88  NPHE-PRORATE-FND          VALUE 00.
00488      88  NP-NOTFND               VALUE 23.
00489  01  NP-STATUS-2 BINARY.
00490      05  NP-RETURN       PIC 99  VALUE 0.
00491      05  NP-FUNCTION     PIC 9   VALUE 0.
00492      05  NP-FEEDBACK     PIC 999 VALUE 0.
00493  01  EQ-STATUS           PIC 99.
00494      88  EQ-NORMAL-STATUS        VALUE 00.
00495      88  EQ-RECORD-NOT-FOUND     VALUE 23.
00496  01  EQ-STATUS-2 BINARY.
00497      05  EQ-RETURN       PIC 99  VALUE 0.
00498      05  EQ-FUNCTION     PIC 9   VALUE 0.
00499      05  EQ-FEEDBACK     PIC 999 VALUE 0.
00500                          SKIP2
00501  LINKAGE SECTION.
00502  01  PARM-INFO.
00503      05  PARM-LGTH       PIC S9(4)                     BINARY.
00504          88  VALID-PARM-LGTH              VALUE +23.
00505      05  PARM-LIST.
00506          10  PARM-PREV-EQ-X.
00507              15  PARM-PREV-EQ PIC 9V9(4).
00508          10  PARM-CURR-EQ-X.
00509              15  PARM-CURR-EQ PIC 9V9(4).
00510          10  PARM-COFE-EQ-X.
00511              15  PARM-COFE-EQ PIC 9V9(4).
00512          10  PARM-BASE-YR-X.
00513              15  PARM-BASE-YR PIC 9(4).
00514          10  PARM-TAXYEAR-X.
00515              15  PARM-TAXYEAR PIC 9(4).
00516                          EJECT
00517  PROCEDURE DIVISION  USING PARM-INFO.
00518                          SKIP1
00519  0000-BEGIN.
00520      DISPLAY SPACES.
00521      MOVE FUNCTION CURRENT-DATE  TO CURRENT-DATE-RETURN
00522      COMPUTE  CURRENT-DATE = SYSTEM-DATE * MDCY-OR-CYMD
00523      DISPLAY 'PROGRAM ASHMA839 RUN ON: '  CURRENT-DATE
00524      DISPLAY SPACES.
00525      PERFORM 9000-PARM-CHECK-RTN
00526      IF RETURN-CODE = 16
00527         STOP RUN
00528      END-IF
00529      PERFORM 9200-INITIALIZE-RTN
00530      OPEN  INPUT HOMEOWNER-MAST  MASTER-IN
00531                  MASTER-OUT-2 LNDMRK-PROP-FILE NPHE-PRORATION-FIL
00532 *                EQUAL-FACTOR
00533      OPEN  OUTPUT HOMEOWNER-MAST2  NPHE-FILE
00534 **                PRINT-FILE
00535      SET READ-HOMS  TO TRUE
00536      SET READ-CMST  TO TRUE
00537      PERFORM 1000-MAINLINE-RTN  UNTIL
00538              (EOF-HOMS  AND  EOF-CMST)  OR SEVERE-ERR
00539      CLOSE HOMEOWNER-MAST  MASTER-IN  MASTER-OUT-2
00540            HOMEOWNER-MAST2  NPHE-FILE
00541 **         PRINT-FILE
00542      DISPLAY SPACES
00543      PERFORM 9300-DISPLAY-CTRS-RTN
00544      STOP RUN.
00545                          SKIP2
00546  1000-MAINLINE-RTN.
00547      IF READ-HOMS
00548         SET READ-HOMS-RESET  TO TRUE
00549         SET VALID-HOMS-RESET  TO TRUE
00550         PERFORM 1100-GET-HOMS-RECORDS-RTN  UNTIL
00551                 VALID-HOMS  OR EOF-HOMS  OR SEVERE-ERR
00552      END-IF
00553      IF READ-CMST
00554         SET READ-CMST-RESET  TO TRUE
00555         PERFORM 1300-READ-CMST-FILE-RTN
00556      END-IF
00557      IF NO-SEVERE-ERR  AND  (NO-EOF-HOMS  OR NO-EOF-CMST)
00558         EVALUATE  TRUE
00559            WHEN CMST-MATCH = HOMS-MATCH
00560               PERFORM 2000-DETAIL-RTN
00561               SET READ-HOMS  TO TRUE
00562               SET READ-CMST  TO TRUE
00563            WHEN CMST-MATCH > HOMS-MATCH
00564               SET HO-REC-ZERO  TO TRUE
00565               PERFORM 4200-OUTPUT-HOMEOWNER-RTN
00566               SET READ-HOMS  TO TRUE
00567            WHEN CMST-MATCH < HOMS-MATCH
00568 *             MOVE 'NO HOMEOWNER RECORD'  TO WS-MSG
00569 *             PERFORM 4000-ERROR-REPORT-RTN
00570               SET READ-CMST  TO TRUE
00571         END-EVALUATE
00572      END-IF.
00573                          SKIP2
00574  1100-GET-HOMS-RECORDS-RTN.
00575      PERFORM 1110-READ-HOMS-FILE-RTN
00576      IF NO-EOF-HOMS  AND  NO-SEVERE-ERR
00577            SET VALID-HOMS  TO TRUE
00578      END-IF.
00579                          SKIP2
00580  1110-READ-HOMS-FILE-RTN.
00581      READ HOMEOWNER-MAST
00582         AT END
00583            MOVE HIGH-VALUES  TO HOMS-MATCH
00584            SET EOF-HOMS  TO TRUE
00585         NOT AT END
00586            MOVE HO-VOL  TO HOMS-VOL
00587            MOVE HO-PROP  TO HOMS-PROP
00588            MOVE HO-TXTYP TO HOMS-TXTYP
00589            ADD +1  TO HOMS-READ-CTR
00590            IF HOMS-PREV-KEY < HOMS-CURR-KEY
00591               MOVE HOMS-CURR-KEY  TO HOMS-PREV-KEY  HOMS-MATCH
00592            ELSE
00593               MOVE 16  TO RETURN-CODE
00594               SET SEVERE-ERR  TO TRUE
00595               DISPLAY 'SEQUENCE ERROR ON HOMEOWNER MASTER'
00596                       ' FILE READ'
00597               DISPLAY '   PREVIOUS KEY......: '  HOMS-PREV-KEY
00598               DISPLAY '   CURRENT KEY.......: '  HOMS-CURR-KEY
00599               MOVE HOMS-READ-CTR  TO CTR-DISPLAY
00600               DISPLAY '   ERROR AT RECORD NO: '  CTR-DISPLAY
00601            END-IF
00602      END-READ.
00603                          SKIP2
00604  1300-READ-CMST-FILE-RTN.
00605      READ MASTER-IN
00606         AT END
00607            MOVE HIGH-VALUES  TO CMST-MATCH
00608            SET EOF-CMST  TO TRUE
00609         NOT AT END
00610            MOVE M-VOL  TO CMST-VOL
00611            MOVE M-PROP  TO CMST-PROP
00612            MOVE M-TXTYP  TO CMST-TXTYP
00613            MOVE ZEROS  TO WS-VAL-CLS
00614            MOVE M-CLS  TO WS-VAL-CLS
00615            ADD +1  TO CMST-READ-CTR
00616            IF CMST-PREV-KEY < CMST-CURR-KEY
00617               MOVE CMST-CURR-KEY  TO CMST-PREV-KEY  CMST-MATCH
00618            ELSE
00619               MOVE 16  TO RETURN-CODE
00620               SET SEVERE-ERR  TO TRUE
00621               DISPLAY 'SEQUENCE ERROR ON CURR ASSESSMENT MASTER'
00622                       ' FILE READ'
00623               DISPLAY '   PREVIOUS KEY......: '  CMST-PREV-KEY
00624               DISPLAY '   CURRENT KEY.......: '  CMST-CURR-KEY
00625               MOVE CMST-READ-CTR  TO CTR-DISPLAY
00626               DISPLAY '   ERROR AT RECORD NO: '  CTR-DISPLAY
00627            END-IF
00628      END-READ.
00629                          SKIP2
00630  2000-DETAIL-RTN.
00631      MOVE ZEROS  TO AGE-HOLD CLS-HOLD
00632      IF M-DETAIL-PRESENT
00633         PERFORM  VARYING CMST-SUB FROM +1 BY +1  UNTIL
00634           (CMST-SUB > M-DTL-QST-CTR OR
00635            CMST-SUB > +350)
00636            IF D2-TYPE2 (CMST-SUB)
00637               MOVE D2-AGE (CMST-SUB)  TO AGE-HOLD
00638               MOVE D2-CLS (CMST-SUB)  TO CLS-HOLD
00639               MOVE +999  TO CMST-SUB
00640            ELSE
00641               IF D3-TYPE3 (CMST-SUB)
00642                  MOVE D3-AGE (CMST-SUB) TO AGE-HOLD
00643                  MOVE D3-CLS (CMST-SUB) TO CLS-HOLD
00644                  MOVE +999 TO CMST-SUB
00645               ELSE
00646                  IF D4-TYPE4 (CMST-SUB)
00647                     MOVE D4-AGE (CMST-SUB) TO AGE-HOLD
00648                     MOVE D4-CLS (CMST-SUB) TO CLS-HOLD
00649                     MOVE +999 TO CMST-SUB
00650                  ELSE
00651                     IF D5-TYPE5 (CMST-SUB)
00652                        MOVE D5-AGE (CMST-SUB) TO AGE-HOLD
00653                        MOVE D5-CLS (CMST-SUB) TO CLS-HOLD
00654                        MOVE +999 TO CMST-SUB
00655                     END-IF
00656                  END-IF
00657               END-IF
00658            END-IF
00659         END-PERFORM
00660      END-IF
00661      IF AGE-HOLD = +1 AND
00662         CLS-HOLD NOT = 288
00663         MOVE ZEROS TO WS-AGE
00664         ADD +1 TO AGE-1-CTR
00665         SET AGE-YES  TO TRUE
00666         PERFORM 2200-CALCULATION-RTN
00667         SET HO-REC-NPHE  TO TRUE
00668         PERFORM 4200-OUTPUT-HOMEOWNER-RTN
00669         PERFORM 4300-OUTPUT-NPHE-RTN
00670      ELSE
00671         SET PMST-READ-RESET  TO TRUE
00672         SET PRIOR-MISSING-RESET  TO TRUE
00673         PERFORM 2100-GET-PMST-RTN  UNTIL
00674                 PMST-READ  OR SEVERE-ERR
00675         IF NO-SEVERE-ERR
00676            IF PRIOR-MISSING
00677 *             MOVE 'NO PRIOR YEAR RECORD'  TO DIS-MSG
00678 *             PERFORM 4000-ERROR-REPORT-RTN
00679               SET AGE-YES TO TRUE
00680               ADD +1 TO NO-PRIOR-CTR
00681               PERFORM 2200-CALCULATION-RTN
00682               SET HO-REC-NPHE TO TRUE
00683               PERFORM 4200-OUTPUT-HOMEOWNER-RTN
00684               PERFORM 4300-OUTPUT-NPHE-RTN
00685            ELSE
00686               ADD +1 TO PRIOR-CTR
00687               SET AGE-NO  TO TRUE
00688               PERFORM 2200-CALCULATION-RTN
00689               SET HO-REC-NPHE  TO TRUE
00690               PERFORM 4200-OUTPUT-HOMEOWNER-RTN
00691               PERFORM 4300-OUTPUT-NPHE-RTN
00692            END-IF
00693         END-IF
00694      END-IF.
00695                          SKIP2
00696  2100-GET-PMST-RTN.
00697      EVALUATE  TRUE
00698         WHEN PMST-MATCH = CMST-MATCH
00699            SET PMST-READ  TO TRUE
00700         WHEN PMST-MATCH < CMST-MATCH
00701            PERFORM 2150-READ-PMST-FILE-RTN
00702         WHEN PMST-MATCH > CMST-MATCH
00703            SET PMST-READ  TO TRUE
00704            SET PRIOR-MISSING  TO TRUE
00705      END-EVALUATE.
00706                          SKIP2
00707  2150-READ-PMST-FILE-RTN.
00708      READ MASTER-OUT-2
00709         AT END
00710            MOVE HIGH-VALUES  TO PMST-MATCH
00711            SET EOF-PMST  TO TRUE
00712         NOT AT END
00713            MOVE AS-VOL  TO PMST-VOL
00714            MOVE AS-PROP  TO PMST-PROP
00715            MOVE AS-TXTYP  TO PMST-TXTYP
00716            ADD +1  TO PMST-READ-CTR
00717            IF PMST-PREV-KEY < PMST-CURR-KEY
00718               MOVE PMST-CURR-KEY  TO PMST-PREV-KEY  PMST-MATCH
00719            ELSE
00720               MOVE 16  TO RETURN-CODE
00721               SET SEVERE-ERR  TO TRUE
00722               DISPLAY 'SEQUENCE ERROR ON PREV ASSESSMENT MASTER'
00723                       ' FILE READ'
00724               DISPLAY '   PREVIOUS KEY......: '  PMST-PREV-KEY
00725               DISPLAY '   CURRENT KEY.......: '  PMST-CURR-KEY
00726               MOVE PMST-READ-CTR  TO CTR-DISPLAY
00727               DISPLAY '   ERROR AT RECORD NO: '  CTR-DISPLAY
00728            END-IF
00729      END-READ.
00730                          SKIP2
00731  2200-CALCULATION-RTN.
00732      MOVE SPACES  TO WS-STAT-SW.
00733      MOVE HO-TXCD TO WS-TXCD.
00734      MOVE WS-TXCD (1:2) TO WK-TOWN.
00735      IF CITY-TOWN
00736         PERFORM 2300-CITY-ROUTINE
00737      ELSE
00738         IF NORTHWEST
00739            PERFORM 2375-NORTHWEST-ROUTINE
00740         ELSE
00741            IF SOUTHWEST
00742               PERFORM 2400-SOUTHWEST-ROUTINE
00743            END-IF
00744         END-IF
00745      END-IF.
00746  2201-CHECK-CLS.
00747      IF PRIOR-MISSING
00748         SET AGE-YES TO TRUE
00749      ELSE
00750         MOVE ZEROS TO WK-M-CLS WK-AS-CLS WS-M-CLS WS-AS-CLS
00751         MOVE M-CLS TO WK-M-CLS WS-MOV-CLS
00752         MOVE AS-CLS TO WK-AS-CLS WS-ASOV-CLS
00753         EVALUATE  TRUE
00754            WHEN WK-M-MAJ NOT = WK-AS-MAJ
00755               SET AGE-YES TO TRUE
00756               ADD +1 TO AGE-1-CTR
00757            WHEN WS-MMAJ2 = WS-ASMAJ2
00758               IF (WS-MEXC2-CLS AND
00759                       WS-ASEXC2-CLS)
00760                  CONTINUE
00761               ELSE
00762                  IF (WS-MEXC2-CLS OR
00763                          WS-ASEXC2-CLS)
00764                     SET AGE-YES TO TRUE
00765                     ADD +1 TO AGE-1-CTR
00766                  ELSE
00767                     IF (NOT WS-MEXC2-CLS AND
00768                             NOT WS-ASEXC2-CLS)
00769                        CONTINUE
00770                     END-IF
00771                  END-IF
00772               END-IF
00773            WHEN WS-MMAJ3 = WS-ASMAJ3
00774               IF WS-MEXC3-CLS AND
00775                       WS-ASEXC3-CLS
00776                  CONTINUE
00777               ELSE
00778                  IF WS-MEXC2-CLS OR
00779                          WS-ASEXC2-CLS
00780                     SET AGE-YES TO TRUE
00781                     ADD +1 TO AGE-1-CTR
00782                  ELSE
00783                     IF NOT WS-MEXC2-CLS AND
00784                             NOT WS-ASEXC2-CLS
00785                        CONTINUE
00786                     END-IF
00787                  END-IF
00788               END-IF
00789            WHEN WS-MMAJ4 = WS-ASMAJ4
00790               IF WS-MEXC4-CLS AND
00791                       WS-ASEXC4-CLS
00792                  CONTINUE
00793               ELSE
00794                  IF WS-MEXC4-CLS OR
00795                          WS-ASEXC4-CLS
00796                     SET AGE-YES TO TRUE
00797                     ADD +1 TO AGE-1-CTR
00798                  ELSE
00799                     IF NOT WS-MEXC4-CLS AND
00800                             NOT WS-ASEXC4-CLS
00801                        CONTINUE
00802                     END-IF
00803                  END-IF
00804               END-IF
00805            WHEN WS-MMAJ5 = WS-ASMAJ5
00806               IF WS-MEXC5-CLS AND
00807                       WS-ASEXC5-CLS
00808                  CONTINUE
00809               ELSE
00810                  IF WS-MEXC5-CLS OR
00811                          WS-ASEXC5-CLS
00812                     SET AGE-YES TO TRUE
00813                     ADD +1 TO AGE-1-CTR
00814                  ELSE
00815                     IF NOT WS-MEXC5-CLS AND
00816                             NOT WS-ASEXC5-CLS
00817                        CONTINUE
00818                     END-IF
00819                  END-IF
00820               END-IF
00821            WHEN WS-MMAJ6 = WS-ASMAJ6
00822               IF WS-MEXC6-CLS AND
00823                       WS-ASEXC6-CLS
00824                   CONTINUE
00825               ELSE
00826                  IF WS-MEXC6-CLS OR
00827                          WS-ASEXC6-CLS
00828                     SET AGE-YES TO TRUE
00829                     ADD +1 TO AGE-1-CTR
00830                  ELSE
00831                     IF NOT WS-MEXC6-CLS AND
00832                             NOT WS-ASEXC6-CLS
00833                        CONTINUE
00834                     END-IF
00835                  END-IF
00836               END-IF
00837            WHEN WS-MMAJ7 = WS-ASMAJ7
00838               IF WS-MEXC7-CLS AND
00839                       WS-ASEXC7-CLS
00840                  CONTINUE
00841               ELSE
00842                  IF WS-MEXC7-CLS OR
00843                          WS-ASEXC7-CLS
00844                     SET AGE-YES TO TRUE
00845                     ADD +1 TO AGE-1-CTR
00846                  ELSE
00847                     IF NOT WS-MEXC7-CLS AND
00848                             NOT WS-ASEXC7-CLS
00849                        CONTINUE
00850                     END-IF
00851                  END-IF
00852               END-IF
00853            WHEN WS-MMAJ8 = WS-ASMAJ8
00854               IF WS-MEXC8-CLS AND
00855                       WS-ASEXC8-CLS
00856                  CONTINUE
00857               ELSE
00858                  IF WS-MEXC8-CLS OR
00859                          WS-ASEXC8-CLS
00860                     SET AGE-YES TO TRUE
00861                     ADD +1 TO AGE-1-CTR
00862                  ELSE
00863                     IF NOT WS-MEXC8-CLS AND
00864                             NOT WS-ASEXC8-CLS
00865                        CONTINUE
00866                     END-IF
00867                  END-IF
00868               END-IF
00869            WHEN WS-MMAJ9 = WS-ASMAJ9
00870               IF WS-MEXC9-CLS AND
00871                       WS-ASEXC9-CLS
00872                  CONTINUE
00873               ELSE
00874                  IF WS-MEXC9-CLS OR
00875                          WS-ASEXC9-CLS
00876                     SET AGE-YES TO TRUE
00877                     ADD +1 TO AGE-1-CTR
00878                  ELSE
00879                     IF NOT WS-MEXC9-CLS AND
00880                             NOT WS-ASEXC9-CLS
00881                        CONTINUE
00882                     END-IF
00883                  END-IF
00884               END-IF
00885         END-EVALUATE
00886      END-IF.
00887
00888  2201-CHECK-CLS2.
00889      MOVE ZEROS TO WK-M-CLS WK-AS-CLS WS-M-CLS WS-AS-CLS
00890      MOVE M-CLS TO WK-M-CLS WS-MOV-CLS
00891      MOVE AS-CLS TO WK-AS-CLS WS-ASOV-CLS
00892      EVALUATE  TRUE
00893         WHEN WK-M-MAJ NOT = WK-AS-MAJ
00894              SET AGE-YES TO TRUE
00895              ADD +1 TO AGE-1-CTR
00896         WHEN WS-MMAJ2 = WS-ASMAJ2
00897              IF (WS-MEXC2-CLS AND
00898                  WS-ASEXC2-CLS)
00899                  CONTINUE
00900              ELSE
00901                 IF (WS-MEXC2-CLS OR
00902                     WS-ASEXC2-CLS)
00903                     SET AGE-YES TO TRUE
00904                     ADD +1 TO AGE-1-CTR
00905                 ELSE
00906                    IF (NOT WS-MEXC2-CLS AND
00907                         NOT WS-ASEXC2-CLS)
00908                        CONTINUE
00909                    END-IF
00910                 END-IF
00911              END-IF
00912            WHEN WS-MMAJ3 = WS-ASMAJ3
00913               IF WS-MEXC3-CLS AND
00914                       WS-ASEXC3-CLS
00915                  CONTINUE
00916               ELSE
00917                  IF WS-MEXC2-CLS OR
00918                          WS-ASEXC2-CLS
00919                     SET AGE-YES TO TRUE
00920                     ADD +1 TO AGE-1-CTR
00921                  ELSE
00922                     IF NOT WS-MEXC2-CLS AND
00923                             NOT WS-ASEXC2-CLS
00924                        CONTINUE
00925                     END-IF
00926                  END-IF
00927               END-IF
00928            WHEN WS-MMAJ4 = WS-ASMAJ4
00929               IF WS-MEXC4-CLS AND
00930                       WS-ASEXC4-CLS
00931                  CONTINUE
00932               ELSE
00933                  IF WS-MEXC4-CLS OR
00934                          WS-ASEXC4-CLS
00935                     SET AGE-YES TO TRUE
00936                     ADD +1 TO AGE-1-CTR
00937                  ELSE
00938                     IF NOT WS-MEXC4-CLS AND
00939                             NOT WS-ASEXC4-CLS
00940                        CONTINUE
00941                     END-IF
00942                  END-IF
00943               END-IF
00944            WHEN WS-MMAJ5 = WS-ASMAJ5
00945               IF WS-MEXC5-CLS AND
00946                       WS-ASEXC5-CLS
00947                  CONTINUE
00948               ELSE
00949                  IF WS-MEXC5-CLS OR
00950                          WS-ASEXC5-CLS
00951                     SET AGE-YES TO TRUE
00952                     ADD +1 TO AGE-1-CTR
00953                  ELSE
00954                     IF NOT WS-MEXC5-CLS AND
00955                             NOT WS-ASEXC5-CLS
00956                        CONTINUE
00957                     END-IF
00958                  END-IF
00959               END-IF
00960            WHEN WS-MMAJ6 = WS-ASMAJ6
00961               IF WS-MEXC6-CLS AND
00962                       WS-ASEXC6-CLS
00963                   CONTINUE
00964               ELSE
00965                  IF WS-MEXC6-CLS OR
00966                          WS-ASEXC6-CLS
00967                     SET AGE-YES TO TRUE
00968                     ADD +1 TO AGE-1-CTR
00969                  ELSE
00970                     IF NOT WS-MEXC6-CLS AND
00971                             NOT WS-ASEXC6-CLS
00972                        CONTINUE
00973                     END-IF
00974                  END-IF
00975               END-IF
00976            WHEN WS-MMAJ7 = WS-ASMAJ7
00977               IF WS-MEXC7-CLS AND
00978                       WS-ASEXC7-CLS
00979                  CONTINUE
00980               ELSE
00981                  IF WS-MEXC7-CLS OR
00982                          WS-ASEXC7-CLS
00983                     SET AGE-YES TO TRUE
00984                     ADD +1 TO AGE-1-CTR
00985                  ELSE
00986                     IF NOT WS-MEXC7-CLS AND
00987                             NOT WS-ASEXC7-CLS
00988                        CONTINUE
00989                     END-IF
00990                  END-IF
00991               END-IF
00992            WHEN WS-MMAJ8 = WS-ASMAJ8
00993               IF WS-MEXC8-CLS AND
00994                       WS-ASEXC8-CLS
00995                  CONTINUE
00996               ELSE
00997                  IF WS-MEXC8-CLS OR
00998                          WS-ASEXC8-CLS
00999                     SET AGE-YES TO TRUE
01000                     ADD +1 TO AGE-1-CTR
01001                  ELSE
01002                     IF NOT WS-MEXC8-CLS AND
01003                             NOT WS-ASEXC8-CLS
01004                        CONTINUE
01005                     END-IF
01006                  END-IF
01007               END-IF
01008            WHEN WS-MMAJ9 = WS-ASMAJ9
01009               IF WS-MEXC9-CLS AND
01010                       WS-ASEXC9-CLS
01011                  CONTINUE
01012               ELSE
01013                  IF WS-MEXC9-CLS OR
01014                          WS-ASEXC9-CLS
01015                     SET AGE-YES TO TRUE
01016                     ADD +1 TO AGE-1-CTR
01017                  ELSE
01018                     IF NOT WS-MEXC9-CLS AND
01019                             NOT WS-ASEXC9-CLS
01020                        CONTINUE
01021                     END-IF
01022                  END-IF
01023               END-IF
01024         END-EVALUATE.
01025
01026  2205-READ-NPHE-PRORAT-FILE.
01027       READ NPHE-PRORATION-FILE.
01028       IF NPHE-PRORATE-FND
01029          MOVE 'N' TO WS-NP-EOF-SW
01030          ADD +1 TO WS-NP-RECS-READ
01031       ELSE
01032          IF NP-NOTFND
01033             MOVE 'Y' TO WS-NP-EOF-SW
01034          ELSE
01035             DISPLAY 'NPHE PRORATION FILE READ ERROR '
01036             DISPLAY 'FILE STATUS = ' NP-STATUS
01037             DISPLAY 'RETURN      = ' NP-RETURN
01038             DISPLAY 'FUNCTION    = ' NP-FUNCTION
01039             DISPLAY 'FEEDBACK    = ' NP-FEEDBACK
01040             MOVE 16 TO RETURN-CODE
01041             MOVE 'Y' TO SEVERE-ERR-SW.
01042
01043  2300-CITY-ROUTINE.
01044      PERFORM 2201-CHECK-CLS2
01045      IF HO-PRORATE < 1
01046         MOVE SPACES TO WS-STATUS
01047         PERFORM 2310-PRORATE-ROUTINE
01048      ELSE
01049         IF HO-NPHE-BSYR = ZERO AND
01050            HO-ASSDVAL   = ZERO AND
01051            HO-EQVAL     = ZERO
01052               PERFORM 2320-DIVISIONS-ROUTINE
01053         ELSE
01054            IF HO-NPHE-STATUS = 'SA' OR 'CN' OR 'ME'
01055               PERFORM 2330-SALES-ROUTINE
01056            ELSE
01057               IF AS-NPHE-STATUS = 'C' AND HO-NPHE-STATUS = 'TR'
01058                  PERFORM 2340-COFE-ROUTINE
01059 *                COMPUTE PREV-ADJ-BASE ROUNDED =
01060 *                   PREV-ADJ-BASE * PREV-MULTIPLY * PREV-MULTIPLY
01061 *                                 * PREV-MULTIPLY
01062                  IF PREV-ADJ-BASE < 0
01063                     MOVE 0 TO PREV-ADJ-BASE
01064                  END-IF
01065               ELSE
01066                  PERFORM 2350-OTHER-ROUTINE
01067               END-IF
01068            END-IF
01069         END-IF
01070      END-IF.
01071
01072  2310-PRORATE-ROUTINE.
01073       MOVE 'P' TO WS-STAT-SW
01074        MOVE HO-PROP TO NP-KEY
01075        PERFORM 2205-READ-NPHE-PRORAT-FILE
01076        IF AGE-YES
01077           COMPUTE PREV-BASE ROUNDED =
01078             NP-TOT-CURR-AV * PA-PREV-EQ
01079           MOVE NP-TOT-CURR-AV TO WS-PREV-BASE, WS-MVALUE
01080           IF (CITY-TOWN OR NORTHWEST)
01081 *            SUBTRACT 4500 FROM PREV-BASE
01082 *            SUBTRACT 5000 FROM PREV-BASE
01083              SUBTRACT WS-PRIOR-MINIMUM FROM PREV-BASE
01084              IF PREV-ADJ-BASE < 0
01085                 MOVE ZEROS TO PREV-ADJ-BASE
01086              END-IF
01087              COMPUTE PREV-ADJ-BASE ROUNDED =
01088                  PREV-BASE * PREV-MULTIPLY
01089              IF PREV-ADJ-BASE < 0
01090                 MOVE ZEROS TO PREV-ADJ-BASE
01091              END-IF
01092           ELSE
01093 *            SUBTRACT 5000 FROM PREV-BASE
01094              SUBTRACT WS-PRIOR-MINIMUM FROM PREV-BASE
01095              COMPUTE PREV-ADJ-BASE ROUNDED =
01096                 PREV-BASE * PREV-MULTIPLY
01097              IF PREV-ADJ-BASE < 0
01098                 MOVE ZEROS TO PREV-ADJ-BASE
01099              END-IF
01100           END-IF
01101           MOVE 'Y' TO WS-PRORATE-SW
01102           MOVE 'PR' TO HO-NPHE-STATUS WS-STATUS
01103           COMPUTE HO-NPHE-BSYR = PA-TAXYEAR - 1
01104           MOVE HO-NPHE-BSYR (3:2) TO HO-YRAPPL
01105           MOVE WS-MVALUE TO HO-ASSDVAL
01106        ELSE
01107           IF (HO-EQVAL > 0 AND (CITY-TOWN OR NORTHWEST
01108                              OR SOUTHWEST))
01109              COMPUTE PREV-ADJ-BASE ROUNDED =
01110                HO-EQVAL * PREV-MULTIPLY
01111              MOVE HO-EQVAL TO PREV-BASE
01112              IF PREV-ADJ-BASE < 0
01113                 MOVE 0 TO PREV-ADJ-BASE
01114              END-IF
01115              IF NPHE-PRORATE-FND
01116                 MOVE 'Y' TO WS-PRORATE-SW
01117                 MOVE 'PR' TO HO-NPHE-STATUS WS-STATUS
01118              END-IF
01119           ELSE
01120             IF NPHE-PRORATE-FND
01121                MOVE 'PR' TO HO-NPHE-STATUS WS-STATUS
01122                MOVE ZERO TO WS-PREV-BASE
01123                COMPUTE PREV-BASE ROUNDED =
01124                  NP-TOT-CURR-AV * PA-PREV-EQ
01125                MOVE NP-TOT-CURR-AV TO WS-PREV-BASE, WS-MVALUE
01126                MOVE 'Y' TO WS-PRORATE-SW
01127 *              SUBTRACT 5000 FROM PREV-BASE
01128                SUBTRACT WS-PRIOR-MINIMUM FROM PREV-BASE
01129                COMPUTE PREV-ADJ-BASE ROUNDED =
01130                   PREV-BASE * PREV-MULTIPLY
01131                IF PREV-ADJ-BASE < 0
01132                   MOVE 0 TO PREV-ADJ-BASE
01133                END-IF
01134                COMPUTE HO-NPHE-BSYR = PA-TAXYEAR - 1
01135                MOVE HO-NPHE-BSYR (3:2) TO HO-YRAPPL
01136                MOVE WS-MVALUE        TO HO-ASSDVAL
01137             ELSE
01138                DISPLAY 'PRORATION RECORD NOT FOUND ' HO-PROP
01139             END-IF
01140        END-IF.
01141
01142  2320-DIVISIONS-ROUTINE.
01143       MOVE 'D' TO WS-STAT-SW
01144       MOVE 'DV' TO HO-NPHE-STATUS
01145       MOVE 'N' TO WS-TYPE-5-SW.
01146       MOVE ZEROES TO WS-D5-OCCFAC.
01147       PERFORM 2321-CHECK-SEGS THRU 2321-EXIT
01148          VARYING SUB FROM 1 BY 1
01149             UNTIL SUB > M-DTL-QST-CTR OR TYPE-5-FOUND.
01150       IF TYPE-5-FOUND AND WS-D5-OCCFAC > 0
01151          COMPUTE WS-FULL-BASE ROUNDED =
01152             ((M-VALUE (+8) * 100) / WS-D5-OCCFAC)
01153          ADD M-VALUE (7) TO WS-FULL-BASE
01154          COMPUTE PREV-BASE ROUNDED =
01155             WS-FULL-BASE * PA-PREV-EQ
01156          MOVE WS-FULL-BASE TO WS-MVALUE
01157       ELSE
01158          COMPUTE PREV-BASE ROUNDED =
01159              M-VALUE (+9) * PA-PREV-EQ
01160          MOVE M-VALUE (+9) TO WS-MVALUE
01161       END-IF
01162 *     SUBTRACT 5000 FROM PREV-BASE
01163       SUBTRACT WS-PRIOR-MINIMUM FROM PREV-BASE
01164       COMPUTE PREV-ADJ-BASE =
01165          PREV-BASE * PREV-MULTIPLY
01166       IF PREV-ADJ-BASE < 0
01167          MOVE 0 TO PREV-ADJ-BASE
01168       END-IF
01169       MOVE WS-MVALUE        TO HO-ASSDVAL.
01170       COMPUTE HO-NPHE-BSYR = PA-TAXYEAR - 1.
01171       MOVE HO-NPHE-BSYR (3:2) TO HO-YRAPPL.
01172  2321-CHECK-SEGS.
01173       MOVE D1-TYP (SUB) TO EDIT-TYP
01174       MOVE D1-CLS (SUB) TO EDIT-CLS
01175       IF TYPE-5
01176          MOVE 'Y' TO WS-TYPE-5-SW
01177          MOVE D5-OCCFAC (SUB) TO WS-D5-OCCFAC
01178       END-IF.
01179       IF TYPE2-5 AND RES
01180          ADD 1 TO SUB
01181       END-IF.
01182  2321-EXIT.
01183      EXIT.
01184  2330-SALES-ROUTINE.
01185       MOVE HO-EQVAL TO PREV-ADJ-BASE.
01186       MOVE 'S' TO WS-STAT-SW.
01187
01188  2340-COFE-ROUTINE.
01189       COMPUTE PREV-BASE ROUNDED =
01190          AS-VALUE-1 (+9) * PA-COFE-EQ
01191       IF CITY-TOWN OR NORTHWEST
01192          SUBTRACT 4500 FROM PREV-BASE
01193       ELSE
01194          SUBTRACT 5000 FROM PREV-BASE
01195       END-IF
01196 ******COMPUTE PREV-ADJ-BASE ROUNDED =
01197 ******   PREV-BASE * PREV-MULTIPLY
01198 ******MOVE AS-VALUE-1 (+9) TO WS-MVALUE HO-ASSDVAL
01199       COMPUTE WS-CTR = PARM-TAXYEAR - PARM-BASE-YR
01200       MOVE PREV-BASE TO  PREV-ADJ-BASE
01201       PERFORM 2341-CALC-BASE VARYING SUB1 FROM +1 BY +1
01202          UNTIL SUB1 > WS-CTR
01203       IF AS-NPHE-STATUS = 'C'
01204          MOVE 'C' TO WS-STAT-SW
01205          MOVE 'CO' TO HO-NPHE-STATUS
01206       END-IF.
01207  2341-CALC-BASE.
01208       COMPUTE PREV-ADJ-BASE ROUNDED =
01209          PREV-BASE * PREV-MULTIPLY.
01210  2350-OTHER-ROUTINE.
01211       COMPUTE PREV-ADJ-BASE ROUNDED =
01212          HO-EQVAL * PREV-MULTIPLY
01213       IF PREV-ADJ-BASE < 0
01214          MOVE 0 TO PREV-ADJ-BASE
01215       END-IF
01216       MOVE HO-ASSDVAL TO WS-MVALUE
01217       MOVE 'E' TO WS-STAT-SW.
01218
01219  2375-NORTHWEST-ROUTINE.
01220      PERFORM 2201-CHECK-CLS2
01221      IF HO-PRORATE < 1
01222         MOVE SPACES TO WS-STATUS
01223         PERFORM 2310-PRORATE-ROUTINE
01224         IF PREV-ADJ-BASE < 0
01225            MOVE ZERO TO PREV-ADJ-BASE
01226         END-IF
01227      ELSE
01228         IF HO-NPHE-BSYR = ZERO AND
01229            HO-ASSDVAL   = ZERO AND
01230            HO-EQVAL     = ZERO
01231               PERFORM 2320-DIVISIONS-ROUTINE
01232         ELSE
01233            IF HO-NPHE-STATUS = 'SA' OR 'CN' OR 'ME'
01234               PERFORM 2330-SALES-ROUTINE
01235            ELSE
01236               IF AS-NPHE-STATUS = 'C' AND HO-NPHE-STATUS = 'TR'
01237                  PERFORM 2340-COFE-ROUTINE
01238 *****************COMPUTE PREV-ADJ-BASE ROUNDED =
01239 *****************   PREV-ADJ-BASE * PREV-MULTIPLY * PREV-MULTIPLY
01240                  IF PREV-ADJ-BASE < 0
01241                     MOVE 0 TO PREV-ADJ-BASE
01242                  END-IF
01243               ELSE
01244                  PERFORM 2350-OTHER-ROUTINE
01245               END-IF
01246            END-IF
01247         END-IF
01248      END-IF.
01249
01250  2400-SOUTHWEST-ROUTINE.
01251      PERFORM 2201-CHECK-CLS2
01252      IF HO-PRORATE < 1
01253         MOVE SPACES TO WS-STATUS
01254         PERFORM 2310-PRORATE-ROUTINE
01255         IF PREV-ADJ-BASE < 0
01256            MOVE ZERO TO PREV-ADJ-BASE
01257         END-IF
01258      ELSE
01259         IF HO-NPHE-BSYR = ZERO AND
01260            HO-ASSDVAL   = ZERO AND
01261            HO-EQVAL     = ZERO
01262               PERFORM 2320-DIVISIONS-ROUTINE
01263         ELSE
01264            IF HO-NPHE-STATUS = 'SA' OR 'CN' OR 'ME'
01265               PERFORM 2330-SALES-ROUTINE
01266            ELSE
01267               IF AS-NPHE-STATUS = 'C' AND HO-NPHE-STATUS = 'TR'
01268                  PERFORM 2340-COFE-ROUTINE
01269 *****************COMPUTE PREV-ADJ-BASE ROUNDED =
01270 *****************   PREV-ADJ-BASE * PREV-MULTIPLY
01271                  IF PREV-ADJ-BASE < 0
01272                     MOVE 0 TO PREV-ADJ-BASE
01273                  END-IF
01274               ELSE
01275                  PERFORM 2350-OTHER-ROUTINE
01276               END-IF
01277            END-IF
01278         END-IF
01279      END-IF.
01280
01281  2410-CALC-PREV-BASE.
01282      MOVE ZEROS TO WS-PREV-BASE
01283      IF AGE-YES
01284         COMPUTE  PREV-BASE ROUNDED =
01285            M-VALUE (+9) * PA-PREV-EQ
01286         MOVE M-VALUE (+9) TO WS-PREV-BASE WS-MVALUE
01287      ELSE
01288         COMPUTE  PREV-BASE ROUNDED =
01289                  AS-VALUE-1 (+9) * PA-PREV-EQ
01290         MOVE AS-VALUE-1 (+9) TO WS-PREV-BASE WS-MVALUE
01291      END-IF
01292 *    SUBTRACT 5000 FROM PREV-BASE
01293      SUBTRACT WS-PRIOR-MINIMUM FROM PREV-BASE
01294      COMPUTE PREV-ADJ-BASE ROUNDED =
01295         PREV-BASE * PREV-MULTIPLY
01296      IF PREV-ADJ-BASE < 0
01297         MOVE ZERO TO PREV-ADJ-BASE
01298      END-IF
01299      IF AS-NPHE-STATUS = 'C'
01300         MOVE 'CO' TO HO-NPHE-STATUS
01301         MOVE 'C'  TO WS-STAT-SW
01302      ELSE
01303         IF AS-NPHE-STATUS = 'H'
01304            MOVE 'HP' TO HO-NPHE-STATUS
01305            MOVE 'H'  TO WS-STAT-SW
01306         END-IF
01307      END-IF.
01308
01309 *4000-ERROR-REPORT-RTN.
01310 *    MOVE CMST-MATCH  TO DIS-KEY
01311 *    MOVE WS-MSG  TO DIS-MSG
01312 *    DISPLAY DISPLAY-LINE.
01313                          SKIP2
01314  4200-OUTPUT-HOMEOWNER-RTN.
01315      MOVE HO-REC  TO HO-REC2
01316      IF WS-PRORATE-SW = 'Y'
01317         MOVE SPACES TO WS-PRORATE-SW
01318         COMPUTE CURR-ADJ-BASE ROUNDED =
01319            NP-TOT-CURR-AV * PA-CURR-EQ
01320      ELSE
01321         MOVE ZEROS  TO  WS-CURR-AV
01322         MOVE M-VALUE (+9) TO WS-CURR-AV
01323         COMPUTE CURR-ADJ-BASE ROUNDED =
01324            M-VALUE (+9) * PA-CURR-EQ
01325      END-IF
01326      COMPUTE NPHE-AMOUNT = CURR-ADJ-BASE - PREV-ADJ-BASE
01327      MOVE NPHE-AMOUNT TO SAVE-NPHE-AMOUNT
01328 * *  IF HO-PROP = 13251000190000
01329 * *     DISPLAY 'NPHE '  NPHE-AMOUNT
01330 * *             'CUR  ' CURR-ADJ-BASE
01331 * *             'PREV ' PREV-ADJ-BASE
01332 * *  END-IF
01333      IF CITY-TOWN
01334         EVALUATE  TRUE
01335            WHEN NPHE-AMOUNT < NPHE-LO-VAL
01336               MOVE NPHE-LO-VAL  TO NPHE-AMOUNT
01337            WHEN NPHE-AMOUNT > NPHE-HI-VAL-CITY
01338               MOVE NPHE-HI-VAL-CITY  TO NPHE-AMOUNT
01339            WHEN NPHE-AMOUNT < 0
01340               MOVE NPHE-LO-VAL  TO NPHE-AMOUNT
01341         END-EVALUATE
01342 *       IF NPHE-AMOUNT < 30000
01343 *       IF NPHE-AMOUNT < 33000
01344 *          CONTINUE
01345 *       ELSE
01346 *          PERFORM 4500-CORRECT-NPHE
01347 *       END-IF
01348      ELSE
01349         IF NORTHWEST
01350            EVALUATE  TRUE
01351               WHEN NPHE-AMOUNT < NPHE-LO-VAL
01352                  MOVE NPHE-LO-VAL  TO NPHE-AMOUNT
01353               WHEN NPHE-AMOUNT > NPHE-HI-VAL-NRWS
01354                  MOVE NPHE-HI-VAL-NRWS TO NPHE-AMOUNT
01355               WHEN NPHE-AMOUNT < 0
01356                  MOVE NPHE-LO-VAL  TO NPHE-AMOUNT
01357            END-EVALUATE
01358         ELSE
01359            IF SOUTHWEST
01360               EVALUATE  TRUE
01361                  WHEN NPHE-AMOUNT < NPHE-LO-VAL
01362                     MOVE NPHE-LO-VAL TO NPHE-AMOUNT
01363                  WHEN NPHE-AMOUNT > NPHE-HI-VAL-SWWS
01364                     MOVE NPHE-HI-VAL-SWWS TO NPHE-AMOUNT
01365                  WHEN NPHE-AMOUNT < 0
01366                     MOVE NPHE-LO-VAL TO NPHE-AMOUNT
01367               END-EVALUATE
01368            END-IF
01369         END-IF
01370      END-IF
01371      EVALUATE  TRUE
01372         WHEN HO-REC-NOCHANGE
01373            CONTINUE
01374         WHEN HO-REC-ZERO
01375            MOVE ZEROS  TO HO-NPHE2  HO-NPHE-BSYR2
01376         WHEN HO-REC-NPHE
01377            MOVE NPHE-AMOUNT   TO HO-NPHE2
01378            MOVE PREV-ADJ-BASE TO HO-EQVAL2
01379            MOVE PA-CURR-EQ    TO HO-EQFCTR2
01380            IF WS-STAT-SW = 'C'
01381               MOVE PA-BASE-YR (3:2) TO HO-YRAPPL2
01382               MOVE PA-BASE-YR       TO HO-NPHE-BSYR2
01383            END-IF
01384      END-EVALUATE.
01385            MOVE HO-PROP      TO LDMK-PROP.
01386            MOVE PA-TAXYEAR   TO LDMK-YEAR.
01387            MOVE SPACES       TO LNDMRK-EOF-SW.
01388            PERFORM 4400-READ-LANDMARK
01389            IF LNDMRK-FND
01390               MOVE NPHE-LO-VAL TO HO-NPHE2, NPHE-AMOUNT
01391               MOVE 'LN'        TO HO-NPHE-STATUS-2
01392            END-IF
01393      IF M-VALUE (1) > 0
01394         MOVE NPHE-LO-VAL TO HO-NPHE2, NPHE-AMOUNT
01395         MOVE 'SF'        TO HO-NPHE-STATUS-2
01396      END-IF
01397      MOVE WS-VAL-CLS TO WK-VAL-CLS
01398      IF NOT VALID-CLASS
01399         MOVE NPHE-LO-VAL TO HO-NPHE2, NPHE-AMOUNT
01400      END-IF
01401      MOVE HO-NPHE-BSYR2 TO WS-BASE-YR
01402      WRITE HO-REC2
01403      ADD +1  TO HOMS-OUT-CTR.
01404                          SKIP2
01405  4300-OUTPUT-NPHE-RTN.
01406      INITIALIZE NPHE-REC
01407      MOVE M-PROP  TO PROP-NO
01408      MOVE WS-BASE-YR  TO BASE-YR
01409      MOVE HO-ASSDVAL     TO NP-BASE-ASSD-VAL
01410      MOVE PREV-ADJ-BASE  TO NP-BASE-ADJ-EAV
01411      MOVE CURR-ADJ-BASE  TO CURR-EAV
01412      MOVE ZEROS TO CURR-AV
01413      IF WS-STATUS = 'PR'
01414         MOVE NP-TOT-CURR-AV TO CURR-AV
01415         MOVE SPACES TO WS-STATUS
01416      ELSE
01417         MOVE WS-CURR-AV    TO CURR-AV
01418      END-IF
01419      MOVE PREV-BASE    TO NP-BASE-EAV
01420      MOVE NPHE-AMOUNT  TO NPHE-AMT
01421      MOVE M-CLS  TO NPHE-CLASS
01422      WRITE NPHE-REC
01423      ADD +1  TO NPHE-OUT-CTR.
01424                          SKIP2
01425  4400-READ-LANDMARK.
01426       READ LNDMRK-PROP-FILE.
01427       IF GOOD-LAND
01428          MOVE 'N' TO LNDMRK-EOF-SW
01429          ADD +1 TO LNDMRK-RECS-READ
01430       ELSE
01431          IF LAND-NOTFND
01432             MOVE 'Y' TO LNDMRK-EOF-SW
01433          ELSE
01434             DISPLAY 'LANDMARK FILE READ ERROR '
01435             DISPLAY 'FILE STATUS = ' LDMK-STATUS
01436             DISPLAY 'RETURN      = ' LF-RETURN
01437             DISPLAY 'FUNCTION    = ' LF-FUNCTION
01438             DISPLAY 'FEEDBACK    = ' LF-FEEDBACK
01439             MOVE 16 TO RETURN-CODE
01440             MOVE 'Y' TO SEVERE-ERR-SW.
01441
01442  4500-CORRECT-NPHE.
01443 *    IF HO-NPHE-BSYR = 2002
01444 *       MOVE HO-NPHE-BSYR (3:2) TO EQ-YEAR
01445 *    ELSE
01446 *       IF HO-TEMP-ASSD = ZERO
01447 *          MOVE HO-NPHE-BSYR (3:2) TO EQ-YEAR
01448 *       ELSE
01449 *          MOVE 02 TO EQ-YEAR
01450 *       END-IF
01451 *    END-IF
01452
01453 *    MOVE 1                  TO EQ-QUAD
01454      PERFORM 4600-GET-EQUAL-FACTOR
01455 * *  IF HO-PROP = 13251000190000
01456 * *  IF HO-PROP = 13251000190000 OR 13251090360000
01457 * *     DISPLAY 'ASSD EQ       '  HO-ASSDVAL  '  ' EQ-FACTOR
01458 * *     DISPLAY 'TMP ASSD NPHE '  HO-TEMP-ASSD ' ' NPHE-AMOUNT
01459 * *  END-IF
01460      IF WS-REC-FND
01461 *       IF HO-TEMP-ASSD = ZERO
01462 *          COMPUTE WS-BASE-EAV ROUNDED =
01463 *             HO-ASSDVAL  * EQ-FACTOR
01464 *       ELSE
01465 *          COMPUTE WS-BASE-EAV ROUNDED =
01466 *             HO-TEMP-ASSD  * EQ-FACTOR
01467 *       END-IF
01468         COMPUTE WS-MIN-R    ROUNDED =
01469            WS-BASE-EAV * WS-MIN
01470         COMPUTE WS-MAX-R    ROUNDED =
01471            WS-BASE-EAV * WS-MAX
01472 * *     IF HO-PROP = 13251000190000
01473 * *     IF HO-PROP = 13251000190000 OR 13251090360000
01474 * *       DISPLAY 'MIN/MAX 3  ' WS-MIN-R ' ' WS-MAX-R ' '
01475 * *       DISPLAY 'CURREAV BSEAV 3 ' CURR-ADJ-BASE ' ' WS-BASE-EA
01476 * *     END-IF
01477         IF CURR-ADJ-BASE < WS-MIN-R OR CURR-ADJ-BASE = WS-MIN-R
01478            CONTINUE
01479         ELSE
01480            IF CURR-ADJ-BASE < WS-MAX-R
01481 *             SUBTRACT 30000 FROM SAVE-NPHE-AMOUNT
01482               SUBTRACT 33000 FROM SAVE-NPHE-AMOUNT
01483 *             IF SAVE-NPHE-AMOUNT > 5000
01484               IF SAVE-NPHE-AMOUNT > 2000
01485 *                ADD 5000 TO NPHE-AMOUNT
01486                  ADD 2000 TO NPHE-AMOUNT
01487               ELSE
01488                  ADD SAVE-NPHE-AMOUNT TO NPHE-AMOUNT
01489               END-IF
01490            ELSE
01491 *             SUBTRACT 30000 FROM SAVE-NPHE-AMOUNT
01492               SUBTRACT 33000 FROM SAVE-NPHE-AMOUNT
01493 *             IF SAVE-NPHE-AMOUNT > 10000
01494               IF SAVE-NPHE-AMOUNT > 7000
01495 *                ADD 10000 TO NPHE-AMOUNT
01496                  ADD 7000 TO NPHE-AMOUNT
01497               ELSE
01498                  ADD SAVE-NPHE-AMOUNT TO NPHE-AMOUNT
01499               END-IF
01500 * *           IF HO-PROP = 13251000190000
01501 * *              DISPLAY 'SVNPHE       ' SAVE-NPHE-AMOUNT
01502 * *              DISPLAY 'NPHE         ' NPHE-AMOUNT
01503 * *           END-IF
01504         END-IF
01505      END-IF.
01506  4600-GET-EQUAL-FACTOR.
01507 *    READ EQUAL-FACTOR
01508 *    IF EQ-NORMAL-STATUS
01509 *       MOVE 'Y' TO WS-REC-FND-SW
01510 *    ELSE
01511 *       IF EQ-RECORD-NOT-FOUND
01512 *          MOVE 'N' TO WS-REC-FND-SW
01513 *       ELSE
01514 *          DISPLAY 'EQUALFCT READ FAILED ' EQ-STATUS
01515 *                  'RETURN               ' EQ-RETURN
01516 *                  'FUNCTION             ' EQ-FUNCTION
01517 *                  'FEEDBACK             ' EQ-FEEDBACK
01518 *          MOVE 16 TO RETURN-CODE
01519 *       END-IF
01520 *    END-IF.
01521  9000-PARM-CHECK-RTN.
01522      IF VALID-PARM-LGTH
01523         PERFORM 9010-DO-PARM-CHECK-RTN
01524      ELSE
01525         MOVE 16  TO RETURN-CODE
01526         DISPLAY 'PARM ERROR -- INVALID PARM LENGTH '
01527                 'IT MUST BE 23'
01528         DISPLAY 'PARM LIST: '  PARM-LIST
01529      END-IF.
01530                          SKIP2
01531  9010-DO-PARM-CHECK-RTN.
01532      IF PARM-PREV-EQ-X NUMERIC
01533         IF PARM-PREV-EQ = ZEROS
01534            MOVE 16  TO RETURN-CODE
01535            DISPLAY 'PARM ERROR -- PREV EQ FACTOR NOT > ZEROS'
01536            DISPLAY 'PARM PREF EQ FACTOR: '  PARM-PREV-EQ
01537         END-IF
01538      ELSE
01539         MOVE 16  TO RETURN-CODE
01540         DISPLAY 'PARM ERROR -- PREV EQ FACTOR NOT NUMERIC'
01541         DISPLAY 'PARM PREF EQ FACTOR: '  PARM-PREV-EQ-X
01542      END-IF
01543                          SKIP1
01544      IF PARM-CURR-EQ-X NUMERIC
01545         IF PARM-CURR-EQ = ZEROS
01546            MOVE 16  TO RETURN-CODE
01547            DISPLAY 'PARM ERROR -- CURR EQ FACTOR NOT > ZEROS'
01548            DISPLAY 'PARM CURR EQ FACTOR: '  PARM-CURR-EQ
01549         END-IF
01550      ELSE
01551         MOVE 16  TO RETURN-CODE
01552         DISPLAY 'PARM ERROR -- CURR EQ FACTOR NOT NUMERIC'
01553         DISPLAY 'PARM CURR EQ FACTOR: '  PARM-CURR-EQ-X
01554      END-IF
01555                          SKIP1
01556      IF PARM-COFE-EQ-X NUMERIC
01557         IF PARM-COFE-EQ = ZEROS
01558            MOVE 16  TO RETURN-CODE
01559            DISPLAY 'PARM ERROR -- COFE EQ FACTOR NOT > ZEROS'
01560            DISPLAY 'PARM COFE EQ FACTOR: '  PARM-COFE-EQ
01561         END-IF
01562      ELSE
01563         MOVE 16  TO RETURN-CODE
01564         DISPLAY 'PARM ERROR -- COFE EQ FACTOR NOT NUMERIC'
01565         DISPLAY 'PARM COFE EQ FACTOR: '  PARM-COFE-EQ-X
01566      END-IF
01567                          SKIP1
01568      IF PARM-BASE-YR-X NUMERIC
01569         IF PARM-BASE-YR = ZEROS
01570            MOVE 16  TO RETURN-CODE
01571            DISPLAY 'PARM ERROR -- BASE YEAR NOT > ZEROS'
01572            DISPLAY 'PARM BASE YEAR.....: '  PARM-BASE-YR
01573         END-IF
01574      ELSE
01575         MOVE 16  TO RETURN-CODE
01576         DISPLAY 'PARM ERROR -- BASE YEAR NOT NUMERIC'
01577         DISPLAY 'PARM BASE YEAR.....: '  PARM-BASE-YR-X
01578      END-IF.
01579      IF PARM-TAXYEAR-X NUMERIC
01580         IF PARM-TAXYEAR = ZEROS
01581            MOVE 16  TO RETURN-CODE
01582            DISPLAY 'PARM ERROR -- TAXYEAR NOT > ZEROS'
01583            DISPLAY 'PARM TAXYEAR.....: '  PARM-TAXYEAR
01584         END-IF
01585      ELSE
01586         MOVE 16  TO RETURN-CODE
01587         DISPLAY 'PARM ERROR -- TAXYEAR NOT NUMERIC'
01588         DISPLAY 'PARM TAXYEAR.....: '  PARM-TAXYEAR-X
01589      END-IF.
01590                          SKIP2
01591  9200-INITIALIZE-RTN.
01592      MOVE PARM-LIST  TO PARM-AREA
01593      DISPLAY '*****  PARM INFO  *****'
01594      MOVE PA-PREV-EQ  TO DISPLAY-EQ
01595      DISPLAY '   PARM PREV EQ FACTOR: '  DISPLAY-EQ
01596      MOVE PA-CURR-EQ  TO DISPLAY-EQ
01597      DISPLAY '   PARM CURR EQ FACTOR: '  DISPLAY-EQ
01598      MOVE PA-COFE-EQ  TO DISPLAY-EQ
01599      DISPLAY '   PARM COFE EQ FACTOR: '  DISPLAY-EQ
01600      DISPLAY '   PARM BASE YEAR.....: '  PA-BASE-YR
01601      DISPLAY '   PARM TAX YEAR......: '  PA-TAXYEAR
01602      DISPLAY SPACES.
01603                          SKIP2
01604  9300-DISPLAY-CTRS-RTN.
01605      DISPLAY SPACES
01606      DISPLAY '*****  COUNTERS  *****'
01607      MOVE HOMS-READ-CTR  TO CTR-DISPLAY
01608      DISPLAY 'HOMEOWNER RECORDS READ.....: '  CTR-DISPLAY
01609      MOVE CMST-READ-CTR  TO CTR-DISPLAY
01610      DISPLAY 'CURRENT MASTER RECORDS READ: '  CTR-DISPLAY
01611      MOVE PMST-READ-CTR  TO CTR-DISPLAY
01612      DISPLAY 'PRIOR MASTER RECORDS READ..: '  CTR-DISPLAY
01613      MOVE HOMS-OUT-CTR  TO CTR-DISPLAY
01614      DISPLAY 'HOMEOWNER RECORDS WRITTEN..: '  CTR-DISPLAY
01615      MOVE NO-CHG-CTR    TO CTR-DISPLAY
01616      DISPLAY 'HOMEOWNER RECORDS W/O CHG..: '  CTR-DISPLAY
01617      MOVE NPHE-OUT-CTR  TO CTR-DISPLAY
01618      DISPLAY 'NPHE RECORDS WRITTEN.......: '  CTR-DISPLAY
01619      MOVE AGE-1-CTR     TO CTR-DISPLAY
01620      DISPLAY 'RECORDS W/ AGE = 1 ........: '  CTR-DISPLAY
01621      MOVE PRIOR-CTR     TO CTR-DISPLAY
01622      DISPLAY 'RECORDS W/ PRIOR YEAR .....: '  CTR-DISPLAY
01623      MOVE NO-PRIOR-CTR  TO CTR-DISPLAY
01624      DISPLAY 'RECORDS W/O PRIOR YEAR ....: '  CTR-DISPLAY
01625      MOVE LNDMRK-RECS-READ TO CTR-DISPLAY
01626      DISPLAY 'LANDMARK RECORDS READ  ....: '  CTR-DISPLAY
01627      DISPLAY SPACES.
01628      MOVE WS-NP-RECS-READ TO CTR-DISPLAY
01629      DISPLAY 'NPHE PRORATE RECS READ ....: '  CTR-DISPLAY
01630      DISPLAY SPACES.
