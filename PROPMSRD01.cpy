00001 *     --REAL ESTATE PROPERTY MASTER--      *BYTES* *DESCRIPTION  *
00002      05 PM-BASE.
00003 *                                            1-29   FIXED SEGMENT
00004         10 PM-VOL             PIC 999      COMP-3.
00005 *                                            1-2    VOLUME NO.
00006         10 PM-KEY.
00007 *                                            3-13   RECORD KEY
00008            15 PM-PROP         PIC 9(15)    COMP-3.
00009 *                                             3-10  PROPERTY NO.
00010            15 PM-PROPX REDEFINES PM-PROP.
00011               20 PM-PROPR     PIC X(4).
00012               20 FILLER       PIC X(4).
00013            15 PM-TXTYP        PIC X.
00014 *                                             11    TAX TYPE
00015               88 CURRENT-TAX-TYPE      VALUE '0'.
00016               88 BACK-TAX-TYPE         VALUE '1'.
00017               88 ROLL-BACK-TAX-TYPE    VALUE '2'.
00018               88 AIR-POLL-TAX-TYPE     VALUE '3'.
00019               88 ARREARAGE-TAX-TYPE    VALUE 'D' 'M' '4'.
00020               88 CIRCULATOR-TAX-TYPE   VALUE '5'.
00021 *                                                                *
00022            15 PM-TXYR         PIC 99       COMP-3.
00023 *                                             12-13 TAX YEAR
00024         10 PM-KEYR REDEFINES PM-KEY.
00025 *                                            3-13   REDEFINED KEY
00026            15 PM-PROP-TYPE    PIC X(9).
00027 *                                             3-11  PROPERTY NO.
00028 *                                                   AND TAX TYPE
00029 *                                                                *
00030            15 FILLER          PIC XX.
00031 *                                             12-13 FILLER
00032         10 PM-BILLTYP         PIC 9.
00033 *                                           14      BILLABLE TYPE
00034            88 BILLABLE-TYPE-1          VALUE 1.
00035            88 BILLABLE-TYPE-2          VALUE 2.
00036            88 BILLABLE-TYPE-3          VALUE 3.
00037            88 BILLABLE-TYPE-4          VALUE 4.
00038            88 BILLABLE-TYPE-5          VALUE 5.
00039            88 BILLABLE-TYPE-6          VALUE 6.
00040            88 BILLABLE-TYPE-7          VALUE 7.
00041            88 BILLTYPES-EX-RR          VALUE 4 THRU 6.
00042            88 BILLABLE-TYPE-9          VALUE 9.
00043 *                                                                *
00044         10 PM-TXSTAT          PIC 9.
00045 *                                           15      TAX STATUS
00046            88 TAXABLE-PARCEL           VALUE 0.
00047            88 EXEMPT                   VALUE 1.
00048            88 RAILROAD                 VALUE 2.
00049            88 HOMESTEAD-EXEMPT         VALUE 3.
00050            88 VETERANS-EXEMPT          VALUE 4.
00051            88 COOP-HOMESTEAD-EXEMPT    VALUE 5.
00052            88 DIVIDED-PARCEL           VALUE 9.
00053 *                                                                *
00054         10 PM-TXCD            PIC 9(5)     COMP-3.
00055 *                                           16-18   TAX CODE
00056         10 PM-STRT            PIC 9(5)     COMP-3.
00057 *                                           19-21   STREET CODE
00058         10 PM-HSENO           PIC 9(5)     COMP-3.
00059 *                                           22-24   HOUSE NUMBER
00060         10 PM-CLASS           PIC 999      COMP-3.
00061 *                                           25-26   MAJOR MINOR
00062 *                                                    CLASS
00063            88  PM-RES-CLS              VALUE 202 THRU 212
00064                                              234 278 295.
00065            88  PM-RES-CLS-BILLS        VALUE 202 THRU 213
00066                                              234 278 295 299.
00067         10 PM-TXIND           PIC 9.
00068 *                                           27      TAX INDICATOR
00069            88 NO-TAX-INFO              VALUE 0.
00070            88 TAX-INFO-PRESENT         VALUE 1.
00071 *                                                                *
00072         10 PM-SEGCTR          PIC 999      COMP-3.
00073 *                                           28-29   MSTR SEG CNTR
00074            88 NO-SEGMENTS              VALUE 0.
00075            88 MAXIMUM-SEGMENTS         VALUE 40.
00076            88 SEGMENTS-PRESENT         VALUE 1 THRU 40.
00077 *----------------------------------------------------------------*
00078 *   T A X  I N F O R M A T I O N, IF PRESENT CONTAINS 46 BYTES   *
00079 *     DESCRIPTION RELATIVE TO 1ST BYTE IN EACH SEGMENT           *
00080 *                                                                *
00081      05 PM-TXINFO OCCURS 0 TO 1 TIMES DEPENDING ON PM-TXIND.
00082 *                                            1-46   TAX SEGMENT
00083         10 PM-ASSDVAL         PIC S9(9)    COMP-3.
00084 *                                           1-5     ASSESSED
00085 *                                                    VALUATION
00086         10 PM-EQVAL           PIC S9(9)    COMP-3.
00087 *                                            6-10   EQUALIZED
00088 *                                                    VALUATION
00089         10 PM-HOEXVAL         PIC S9(9)    COMP-3.
00090 *                                           11-15   HOMEOWNER
00091 *                                                    EXEMPTION
00092 *                                                    VALUATION
00093         10 PM-SCEXVAL         PIC S9(9)    COMP-3.
00094 *                                           16-20   SC
00095 *                                                    HOMESTEAD
00096 *                                                    EXEMPTION
00097 *                                                    VALUATION
00098         10 PM-TXRAT           PIC S99V999  COMP-3.
00099 *                                           21-23   TAX RATE
00100         10 PM-TXRAT-RD REDEFINES PM-TXRAT
00101                               PIC SV99999  COMP-3.
00102 *                                           21-23   TAX RATE FOR
00103 *                                                   ASREA723
00104         10 PM-TOTTX           PIC S9(9)V99 COMP-3.
00105 *                                           24-29   TOTAL
00106 *                                                    TAX AMOUNT
00107         10 PM-ESTTX           PIC S9(9)V99 COMP-3.
00108         10 PM-PNLTYAMT REDEFINES PM-ESTTX
00109                               PIC S9(9)V99 COMP-3.
00110 *                                           30-35   ESTIMATED
00111 *                                                    TAX AMOUNT
00112 *                                                   OR PENALTY-AMT
00113         10 PM-PRYR-EQVAL      PIC S9(9)    COMP-3.
00114 *                                           36-40   PRIOR YEAR
00115 *                                                    EQUALIZED
00116 *                                                    VALUATION
00117         10 PM-PRYR-TXRAT     PIC S99V999   COMP-3.
00118 *                                           41-43   PRIOR YEAR
00119 *                                                    TAX RATE
00120         10 PM-PRYR-TXCD      PIC 9(5)      COMP-3.
00121 *                                           44-46   PRIOR YEAR
00122 *                                                    TAX CODE
00123 *----------------------------------------------------------------*
00124 *                                                                *
00125 *                                                                *
00126 * P M - S E G M E N T S, ALL OTHER SEGMENTS (EXCEPT TAX-INFO)    *
00127 *  IF PRESENT, EACH CONTAINS 30 BYTES DESCRIPTION RELATIVE TO    *
00128 *  1ST BYTE IN EACH SEGMENT                                      *
00129 *                                                                *
00130      05 PM-SEGS.
00131        07 PM-MSTSEGS OCCURS 0 TO 40 TIMES DEPENDING ON PM-SEGCTR.
00132         10 PM-MSTSEG.
00133            15 PM-SEGTYP       PIC X.
00134            15 FILLER          PIC X(29).
00135 *                                                                *
00136 *----------------------------------------------------------------*
00137 * N A M E,  A D D R E S S,  AND  C I T Y  I N F O R M A T I O N  *
00138 *  IF NAME SEGMENT IS PRESENT 'N1', ADDRESS AND CITY SEGMENTS    *
00139 *  ARE ALWAYS THE NEXT 2 SEGMENTS                                *
00140 *                                                                *
00141         10 PM-NA-SEG REDEFINES PM-MSTSEG.
00142            15 PM-NA-TYP-CD.
00143               88 NAME-TYPE-CODE        VALUE 'N1'.
00144               88 ADDR-TYPE-CODE        VALUE 'N2'.
00145               88 CITY-TYPE-CODE        VALUE 'N3'.
00146 *                                                                *
00147               20 PM-NA-TYP    PIC X.
00148 *                                            1      TYPE N
00149                  88 NAME-ADDR-SEGMENT  VALUE 'N'.
00150 *                                                                *
00151               20 PM-NA-CD     PIC 9.
00152 *                                            2      CODE 1-2-3
00153                  88 NAME-CODE          VALUE 1.
00154                  88 ADDRESS-CODE       VALUE 2.
00155                  88 CITY-ST-CODE       VALUE 3.
00156 *                                                                *
00157            15 PM-NA-NAME.
00158               20 PM-NAME      PIC X(22).
00159 *                                            3-24   NAME
00160               20 FILLER       PIC X(6).
00161            15 PM-NA-ADDR REDEFINES PM-NA-NAME.
00162               20 PM-ADDR      PIC X(22).
00163 *                                            3-24   ADDRESS
00164               20 FILLER       PIC X(6).
00165            15 PM-NA-CSZ REDEFINES PM-NA-NAME.
00166               20 PM-CITY      PIC X(12).
00167 *                                            3-14   CITY
00168               20 PM-STATE     PIC XX.
00169 *                                           15-16   STATE
00170               20 PM-ZIP       PIC 9(9)     COMP-3.
00171 *                                           17-21   ZIPCODE
00172               20 PM-CHGDTE.
00173                  25 PM-MM     PIC 99.
00174                  25 FILLER    PIC X.
00175                  25 PM-DD     PIC 99.
00176                  25 FILLER    PIC X.
00177                  25 PM-YY     PIC 99.
00178 *                                           22-29   DATE OF NAME
00179 *                                                    OR ADDRESS
00180 *                                                    CHANGE
00181               20 FILLER       PIC X.
00182 *----------------------------------------------------------------*
00183 * P A Y M E N T  I N F O R M A T I O N                           *
00184 *                                                                *
00185         10 PM-PYMT-SEG REDEFINES PM-MSTSEG.
00186            15 PM-PYMT-TYP-NO.
00187               88 PAYMENT-TYPE-P0       VALUE 'P0'.
00188               88 PAYMENT-TYPE-P1       VALUE 'P1'.
00189               88 PAYMENT-TYPE-P2       VALUE 'P2'.
00190               88 PAYMENT-TYPE-F1       VALUE 'F1'.
00191               88 PAYMENT-TYPE-F2       VALUE 'F2'.
00192 *                                                                *
00193               20 PM-PYMT-TYP  PIC X.
00194 *                                            1      TYPE P
00195                  88 PAYMENT-SEGMENT    VALUE 'P'.
00196 *                                                                *
00197               20 PM-PYMT-NO   PIC 9.
00198 *                                            2      PAYMENT NO.
00199                  88 DUMMY-INSTALLMENT  VALUE 0.
00200                  88 ESTIMATED-INSTALL  VALUE 1.
00201                  88 FINAL-INSTALLMENT  VALUE 2.
00202 *                                                                *
00203
00204            15 PM-PYMT-DATE    PIC 9(7)     COMP-3.
00205 *                                            3-6    DATE (0YYMMDD)
00206            15 PM-PYMT-AMT     PIC S9(9)V99 COMP-3.
00207 *                                            7-12   AMOUNT
00208            15 PM-PYMT-INT     PIC S9(7)V99 COMP-3.
00209 *                                           13-17   INTEREST
00210            15 PM-PYMT-CST     PIC S9(5)V99 COMP-3.
00211 *                                           18-21   COST
00212            15 PM-PYMT-SERNO   PIC 9(12)    COMP-3.
00213 *                                           22-28  SERIAL NO.
00214            15 PM-PYMT-TYP-SRC PIC 9(3)     COMP-3.
00215 *                                           29-30  PYMT SOURCE
00216 *----------------------------------------------------------------*
00217 * C O N D E M N A T I O N S                                      *
00218 *                                                                *
00219         10 PM-CNDM-SEG REDEFINES PM-MSTSEG.
00220            15 PM-CNDM-TYP     PIC X.
00221 *                                            1      TYPE C
00222               88 CONDEMNATION-SEGMENT  VALUE 'C'.
00223 *                                                                *
00224            15 PM-CNDM-CSNO    PIC X(9).
00225 *                                            2-10   CASE NUMBER
00226            15 PM-CNDM-PRCLNO  PIC X(9).
00227 *                                           11-19   PARCEL NUMBER
00228            15 PM-CNDM-TAX-AMT PIC S9(7)V99       PACKED-DECIMAL.
00229 *                                           20-24   NEW TAX AMOUNT
00230            15 PM-CNDM-TEMP-SW PIC X.
00231 *                                           25-25   SWITCH STATING
00232 *                                                   NEW TAX BUILT
00233 *                                                   OFF LAST YEARS
00234 *                                                   TAXES.
00235            15 PM-CNDM-DATE    PIC 9(9) COMP-3.
00236 *                                           26-30   DATE
00237 *----------------------------------------------------------------*
00238 * A C Q U I S I T I O N S  (AQ1)                                 *
00239 *                                                                *
00240         10 PM-ACQSN-SEG1 REDEFINES PM-MSTSEG.
00241            15 PM-ACQSN-TYP1   PIC XXX.
00242 *                                            1-3    TYPE AQ1
00243               88 ACQUISITION-SEGMENT1     VALUE 'AQ1'.
00244 *                                                                *
00245            15 FILLER          PIC XX.
00246 *                                            4-5    FILLER
00247            15 PM-ACQSN-AGCY   PIC X(25).
00248 *                                            6-30   ACQUIRING
00249 *                                                   AGENCY
00250 *----------------------------------------------------------------*
00251 * A C Q U I S I T I O N S  (AQ2)                                 *
00252 *                                                                *
00253         10 PM-ACQSN-SEG2 REDEFINES PM-MSTSEG.
00254            15 PM-ACQSN-TYP2   PIC XXX.
00255 *                                            1-3    TYPE AQ2
00256               88 ACQUISITION-SEGMENT2     VALUE 'AQ2'.
00257 *                                                                *
00258            15 PM-ACQSN-DATE   PIC 9(7)     COMP-3.
00259 *                                            4-7    DATE (0MMDDYY)
00260            15 PM-ACQSN-DKTNO  PIC 9(9)     COMP-3.
00261 *                                            8-12   DOCKET NO.
00262 *                                                   (099999999)
00263            15 PM-ACQSN-EXAMT  PIC 9(7)V99  COMP-3.
00264 *                                           13-17   EXEMPT BALANCE
00265            15 PM-ACQSN-BALPCT PIC 999      COMP-3.
00266 *                                           18-19   EXEMPT BALANCE
00267 *                                                   PERCENT
00268            15 PM-ACQSN-NWASSV PIC 9(9)     COMP-3.
00269 *                                           20-24   NEW ASSESSED
00270 *                                                   VALUATION
00271            15 PM-ACQ-UPD-DATE PIC 9(7)     COMP-3.
00272 *                                           25-28   DATE RECORD
00273 *                                                   UPDATED
00274 *                                                   MMDDYY
00275            15 FILLER          PIC X(02).
00276 *                                           29-30
00277 *----------------------------------------------------------------*
00278 * H   R E F U N D S                                              *
00279 *                                                                *
00280         10 PM-RFND-SEG REDEFINES PM-MSTSEG.
00281            15 PM-RFND-TYP     PIC X.
00282 *                                            1      TYPE H
00283               88 H-REFUND-SEGMENT      VALUE 'H'.
00284 *                                                                *
00285            15 PM-RFND-DATE    PIC 9(7)     COMP-3.
00286 *                                            2-5    DATE (0MMDDYY)
00287            15 PM-RFND-AMT     PIC S9(9)V99 COMP-3.
00288 *                                            6-11   AMOUNT
00289            15 PM-RFND-CSNO    PIC X(10).
00290 *                                           12-21   CASE NUMBER
00291            15 FILLER          PIC X(9).
00292 *----------------------------------------------------------------*
00293 * A/Z  R E F U N D S                                             *
00294 *                                                                *
00295         10 PM-AZ-SEG REDEFINES PM-MSTSEG.
00296            15 PM-AZ-TYP-INSTL.
00297               88 A-Z-TYP-INSTALL-R1    VALUE 'R1'.
00298               88 A-Z-TYP-INSTALL-R2    VALUE 'R2'.
00299 *                                                                *
00300               20 PM-AZ-TYP    PIC X.
00301 *                                            1      TYPE R
00302                  88 A-Z-REFUND-SEGMENT VALUE 'R'.
00303 *                                                                *
00304               20 PM-AZ-INSTL  PIC 9.
00305 *                                            2      INSTALLMENT
00306                  88 A-Z-EST-INSTALL    VALUE 1.
00307                  88 A-Z-FINAL-INSTALL  VALUE 2.
00308 *                                                                *
00309            15 PM-AZ-DATE      PIC 9(7)     COMP-3.
00310 *                                            3-6    DATE REFUND
00311 *                                                       OR
00312 *                                                   D&O DATE
00313 *                                                    (0MMDDYY)
00314            15 PM-AZ-AMT       PIC S9(9)V99 COMP-3.
00315 *                                            7-12   AMOUNT REFUND
00316 *                                                       OR
00317 *                                                   TAX AMOUNT
00318            15 PM-AZ-NO        PIC X(7).
00319 *                                           13-19   REFUND NUMBER
00320 *                                                       OR
00321 *                                                   D&O NUMBER
00322            15 PM-AZ-PENALTY   PIC S9(7)V99 COMP-3.
00323 *                                           20-24   PENALTY AMT
00324            15 PM-AZ-COSTS     PIC S9(5)V99 COMP-3.
00325 *                                           25-28   COSTS
00326            15 PM-AZ-COMPLETED PIC 9.
00327 *                                           29-29   REFUND
00328 *                                                     COMPLETED
00329 *                                                     INDICATOR
00330            15 PM-AZ-WILLCALL  PIC X.
00331 *                                           30-30   WILL CALL
00332 *                                                     INDICATOR
00333 *----------------------------------------------------------------*
00334 * R-3  R E F U N D S                                             *
00335 *                                                                *
00336 * THIS SEGMENT CONTAINS THE SERIAL NO. OF THE P1 OR P2 SEGMENT   *
00337 * THAT A R1 OR R2 SEGMENT IS TO BE MATCHED AGAINST FOR INTEREST  *
00338 * CALCULATIONS.                                                  *
00339 *                                                                *
00340         10 PM-R3-SEG REDEFINES PM-MSTSEG.
00341            15 PM-R3-TYP-INSTL PIC X(2).
00342               88 PM-R3-INSTALL         VALUE 'R3'.
00343 *                                                                *
00344            15 FILLER REDEFINES PM-R3-TYP-INSTL.
00345               20 PM-R3-TYP    PIC X.
00346 *                                            1      TYPE R
00347                  88 PM-R3-REFUND-SEGMENT VALUE 'R'.
00348 *                                                                *
00349               20 PM-R3-INSTL  PIC 9.
00350 *                                            2      INSTALLMENT  *
00351                  88 PM-A-Z-R3-INSTALL    VALUE  3.
00352 *                                                                *
00353            15 PM-R3-FILL      PIC X(13).
00354 *                                            3-15
00355            15 PM-R3-SER-NO    PIC 9(12)    COMP-3.
00356 *                                           16-22   CORRESPOND
00357 *                                                   PAYMENT
00358 *                                                   SERIAL NO
00359            15 PM-R3-NO        PIC X(7).
00360 *                                           23-29   REFUND NUMBER
00361 *                                                       OR
00362 *                                                   D&O NUMBER
00363            15 PM-R3-FILL2     PIC X.
00364 *                                           30-30
00365 *----------------------------------------------------------------*
00366 * P R O T E S T                                                  *
00367 *                                                                *
00368         10 PM-PROT-SEG REDEFINES PM-MSTSEG.
00369            15 PM-PROT-TYP     PIC X.
00370 *                                            1      TYPE T
00371               88 PROTEST-SEGMENT       VALUE 'T'.
00372 *                                                                *
00373            15 PM-PROT-NO      PIC 9(7)     COMP-3.
00374 *                                            2-5    NUMBER
00375            15 PM-PROT-AMT     PIC S9(9)V99 COMP-3.
00376 *                                            6-11   AMOUNT
00377            15 FILLER          PIC X(19).
00378 *----------------------------------------------------------------*
00379 * M I S C E L L A N E O U S                                      *
00380 *                                                                *
00381         10 PM-MISC-SEG REDEFINES PM-MSTSEG.
00382            15 PM-MISC-TYP     PIC X.
00383 *                                            1      TYPE M
00384               88 MISCELLANEOUS-SEGMENT VALUE 'M'.
00385 *                                                                *
00386            15 PM-SLDFRT1.
00387               20 PM-SLDFRT-CD1 PIC 9.
00388 *                                            2      SOLD FOREFEIT
00389 *                                                    CODE
00390                  88  SOLD1             VALUE 1.
00391                  88  FORFEIT1          VALUE 2.
00392 *                                                                *
00393               20 PM-SLDFRT-YR1 PIC 99.
00394 *                                            3-4    SOLD FOREFEIT
00395 *                                                    YEAR
00396            15 PM-SLDFRT2.
00397               20 PM-SLDFRT-CD2 PIC 9.
00398 *                                            5      SOLD FOREFEIT
00399 *                                                    CODE
00400                  88  SOLD2             VALUE 1.
00401                  88  FORFEIT2          VALUE 2.
00402 *                                                                *
00403               20 PM-SLDFRT-YR2 PIC 99.
00404 *                                            6-7    SOLD FOREFEIT
00405 *                                                    YEAR
00406            15 PM-MISC-BKTXPCT PIC 999      COMP-3.
00407 *                                            8-9    BACK TAX
00408 *                                                    PERCENT
00409            15 PM-MISC-ROBKPCT PIC 999      COMP-3.
00410 *                                           10-11   ROLL BACK
00411 *                                                    PERCENT
00412            15 PM-MISC-ROBKIND PIC X.
00413 *                                           12      PARTIAL
00414 *                                                    INDICATOR
00415            15 PM-MISC-COENO   PIC 9(5)     COMP-3.
00416 *                                           13-15   CERTIFICATE
00417 *                                                    OF ERRORS
00418            15 PM-MISC-PNLTYDAT
00419                               PIC 9(7)     COMP-3.
00420 *                                           16-19   PENALTY DATE
00421 *                                                    (0MMDDYY)
00422            15 PM-MISC-ORIGPROP
00423                               PIC 9(15)    COMP-3.
00424 *                                           20-27   ORIGINAL
00425 *                                                    PROPERTY-NO.
00426            15 PM-MISC-ORIGPROP-R REDEFINES PM-MISC-ORIGPROP.
00427               20 PM-SCAV-SALE-YEAR
00428                               PIC 9(05)    COMP-3.
00429               20 PM-SCAV-ACTION
00430                               PIC 9.
00431               20 FILLER       PIC X(04).
00432            15 PM-MISC-EQFCTR  PIC S9V9(4)  COMP-3.
00433 *                                           28-30   EQUALIZATION
00434 *                                                   FACTOR FOR
00435 *                                                   BACKTAX AND
00436 *                                                   ROLLBACK TAX
00437 *                                                                *
00438 *----------------------------------------------------------------*
00439 * A B A T E M E N T                                              *
00440 *                                                                *
00441         10 PM-ABAT-SEG REDEFINES PM-MSTSEG.
00442            15 PM-ABAT-TYP     PIC XX.
00443 *                                            1-2    TYPE AB
00444               88 ABAT-SEGMENT       VALUE 'AB'.
00445 *                                                                *
00446            15 PM-ABAT-NO      PIC 9(9)     COMP-3.
00447 *                                            3-7    ABAT AGENCY
00448            15 PM-ABAT-TXYR    PIC 9(5)     COMP-3.
00449 *                                            8-10   BASE TAX YEAR
00450            15 PM-AGCY-AMT     PIC S9(9)V99 COMP-3.
00451 *                                           11-16   AGENCY TOTAL
00452 *                                                   TAX AMOUNT
00453            15 PM-ABAT-PCT     PIC S9(3)V99 COMP-3.
00454 *                                           17-19   ABAT PCT
00455            15 PM-ABAT-AMT     PIC S9(9)V99 COMP-3.
00456 *                                           20-25   ABATED
00457 *                                                   TAX AMOUNT
00458            15 PM-ABAT-PREVAMT PIC S9(7)V99 COMP-3.
00459 *                                           26-30   PREV ABATED
00460 *                                                   TAX AMOUNT
00461         10 PM-ABAT-SEG-RD REDEFINES PM-ABAT-SEG.
00462 *
00463 *** STARTING IN TAX YEAR 2003 THE AGENCY NO. WAS EXPANDED FROM ***
00464 *** 5 DIGITS TO 9. PRIOR YEAR 'AB' SEGMENTS WERE NOT CONVERTED ***
00465 *** AND THAT IS WHY THIS REDEFINITION OF THE SEGMENT WAS       ***
00466 *** CREATED FOR TAX YEARS 2002 AND PRIOR.                      ***
00467 *
00468            15 PM-ABAT-TYP2    PIC XX.
00469 *                                            1-2    TYPE AB
00470               88 ABAT-SEGMENT2      VALUE 'AB'.
00471 *                                                                *
00472            15 PM-ABAT-NO2     PIC 9(5)     COMP-3.
00473 *                                            3-5    ABAT AGENCY
00474            15 PM-ABAT-TXYR2   PIC 9(5)     COMP-3.
00475 *                                            6-8    BASE TAX YEAR
00476            15 PM-AGCY-AMT2    PIC S9(9)V99 COMP-3.
00477 *                                            9-14   AGENCY TOTAL
00478 *                                                   TAX AMOUNT
00479            15 PM-ABAT-PCT2    PIC S9(3)V99 COMP-3.
00480 *                                           15-17   ABAT PCT
00481            15 PM-ABAT-AMT2    PIC S9(9)V99 COMP-3.
00482 *                                           18-23   ABATED
00483 *                                                   TAX AMOUNT
00484            15 PM-ABAT-PREVAMT2 PIC S9(9)V99 COMP-3.
00485 *                                           24-29   PREV ABATED
00486 *                                                   TAX AMOUNT
00487            15 FILLER           PIC X.
00488 *                                           30-30   PREV ABATED
00489 *                                                   TAX AMOUNT
00490 *----------------------------------------------------------------*
00491 * S E N I O R   C I T I Z E N   T A X   D E F E R R A L          *
00492 *                                                                *
00493         10 PM-SC-SEG REDEFINES PM-MSTSEG.
00494            15 PM-SC-TYP       PIC XX.
00495 *                                            1-2    TYPE SC
00496               88 SENCIT-SEGMENT     VALUE 'SC'.
00497 *                                                                *
00498            15 PM-SC-VNO       PIC X(5).
00499 *                                            3-7    VOUCHER NUMBER
00500            15 PM-SC-DATE      PIC 9(6).
00501 *                                            8-13   VOUCHER DATE
00502 *                                                    (MMDDYY)
00503            15 FILLER          PIC X(17).
00504 *                                           14-30
00505 *----------------------------------------------------------------*
00506 * E N T E R P R I S E   Z O N E                                  *
00507 *                                                                *
00508         10 PM-EZ-SEG REDEFINES PM-MSTSEG.
00509            15 PM-EZ-TYP       PIC XX.
00510 *                                            1-2    TYPE EZ
00511               88 ENTZONE-SEGMENT    VALUE 'EZ'.
00512 *                                                                *
00513            15 PM-EZ-NO        PIC 9(9)     COMP-3.
00514 *                                            3-7    ABAT AGENCY
00515            15 PM-BASE-TXYR    PIC 9(5)     COMP-3.
00516 *                                            8-10   BASE TAX YEAR
00517            15 PM-EZ-ABATPCT   PIC S9(3)V99 COMP-3.
00518 *                                           11-13   ABAT PCT
00519            15 PM-EZ-ABATAMT   PIC S9(9)V99 COMP-3.
00520 *                                           14-19   ABAT AMT
00521            15 PM-EZ-PREVAMT   PIC S9(9)V99 COMP-3.
00522 *                                           20-25   PREV AMT
00523            15 FILLER          PIC X(5).
00524 *                                           26-30   FILLER
00525 *----------------------------------------------------------------*
00526 * S A L E  P A Y M E N T  I N F O R M A T I O N  *
00527 *                                                                *
00528         10 PM-SALPYMT-SEG REDEFINES PM-MSTSEG.
00529            15 PM-SALPYMT-TYP-NO.
00530               88 SALEPAYMENT-TYPE-S1     VALUE 'S1'.
00531               88 SALEPAYMENT-TYPE-S2     VALUE 'S2'.
00532 *                                                                *
00533               20 PM-SALPYMT-TYP  PIC X.
00534 *                                            1      TYPE S
00535                  88 SALEPAYMENT-SEGMENT  VALUE 'S'.
00536 *                                                                *
00537               20 PM-SALPYMT-NO   PIC 9.
00538 *                                            2      PAYMENT NO.
00539                  88 ESTIMATED-INSTALL1    VALUE 1.
00540                  88 FINAL-INSTALLMENT2    VALUE 2.
00541 *                                                                *
00542
00543            15 PM-SALPYMT-DATE    PIC 9(7)     COMP-3.
00544 *                                            3-6    DATE (0YYMMDD)
00545            15 PM-SALPYMT-AMT     PIC S9(9)V99 COMP-3.
00546 *                                            7-12   AMOUNT
00547            15 PM-SALPYMT-INT     PIC S9(7)V99 COMP-3.
00548 *                                           13-17   INTEREST
00549            15 PM-SALPYMT-CST     PIC S9(5)V99 COMP-3.
00550 *                                           18-21   COST
00551            15 PM-SALPYMT-SERNO   PIC 9(12)    COMP-3.
00552 *                                           22-28  SERIAL NO.
00553            15 PM-SALPYMT-TYP-SRC PIC 9(3)     COMP-3.
00554 *                                           29-30  PYMT SOURCE
00555 *----------------------------------------------------------------*
00556 * E X E M P T I O N                                              *
00557 *                                                                *
00558         10 PM-EX-SEG REDEFINES PM-MSTSEG.
00559            15 PM-EX-TYP          PIC XX.
00560 *                                            1-2    TYPE EX
00561               88 EXEMPTION-SEG           VALUE 'EX'.
00562 *                                                                *
00563            15 PM-EX-TRUNK        PIC 9(5)     COMP-3.
00564 *                                            3-5    TRUNK NO.
00565            15 PM-EX-SEQNO        PIC 9(3)     COMP-3.
00566 *                                            6-7    SEQUENCE NO.
00567            15 PM-EX-TYPE         PIC X.
00568 *                                            8-8    EXEMPT TYPE
00569               88 PM-EX-PRIV              VALUE 'E'.
00570               88 PM-EX-GOVT              VALUE 'G'.
00571 *                                                                *
00572            15 FILLER             PIC X(22).
00573 *                                            9-30   FILLER
00574 *----------------------------------------------------------------*
00575 * D E L I N Q U E N T   S P E C I A L   A S S E S S M E N T      *
00576 *                                                                *
00577         10 PM-DS-SEG REDEFINES PM-MSTSEG.
00578            15 PM-DS-TYP          PIC XX.
00579 *                                            1-2    TYPE DS
00580               88 DSA-SEG                 VALUE 'DS'.
00581 *                                                                *
00582            15 PM-DS-WARRANT      PIC X(8).
00583 *                                            3-10   WARRANT NO.
00584            15 PM-DS-INSTALL      PIC 99.
00585 *                                           11-12   INSTALL. NO.
00586            15 PM-DS-ITEM         PIC 9(5)     COMP-3.
00587 *                                           13-15   ITEM NO.
00588            15 PM-DS-AMT          PIC S9(7)V99 COMP-3.
00589 *                                           16-20   AMOUNT
00590            15 PM-DS-INT          PIC S9(7)V99 COMP-3.
00591 *                                           21-25   INTEREST
00592            15 PM-DS-DELQDATE     PIC 9(7)    COMP-3.
00593 *                                           26-29   DELQ DATE
00594 *                                                    (0MMDDYY)
00595            15 FILLER             PIC X.
00596 *                                           30-30   FILLER
00597 *----------------------------------------------------------------*
00598 * E S C R O W   C A S E                                          *
00599 *                                                                *
00600         10 PM-ES-SEG REDEFINES PM-MSTSEG.
00601            15 PM-ES-TYP          PIC XX.
00602 *                                            1-2    TYPE ES
00603               88 ES-SEG                  VALUE 'ES'.
00604 *                                                                *
00605            15 PM-ES-DATEPD       PIC 9(7)     COMP-3.
00606 *                                            3-6    DATE PAID
00607 *                                                    (0MMDDYY)
00608            15 PM-ES-CASENO       PIC X(10).
00609 *                                            7-16   CASE NO.
00610            15 PM-ES-AMTPD        PIC S9(9)V99 COMP-3.
00611 *                                           17-22   AMOUNT PAID
00612            15 FILLER             PIC X(8).
00613 *                                           23-30   FILLER
00614 *----------------------------------------------------------------*
00615 * J U D G M E N T   R E F U S E D                                *
00616 *                                                                *
00617         10 PM-JR-SEG REDEFINES PM-MSTSEG.
00618            15 PM-JR-TYP          PIC XX.
00619 *                                            1-2    TYPE JR
00620               88 JR-SEG                  VALUE 'JR'.
00621 *                                                                *
00622            15 PM-JR-JRNO         PIC 9(7)     COMP-3.
00623 *                                            3-6    JR NUMBER
00624            15 PM-JR-RUNDATE      PIC 9(7)     COMP-3.
00625 *                                            7-10   RUN DATE
00626 *                                                    (0MMDDYY)
00627            15 PM-JR-REVEQVAL     PIC 9(9)     COMP-3.
00628 *                                           11-15   REVISED
00629 *                                                    EQUALIZED
00630 *                                                    VALUATION
00631            15 PM-JR-JRAMT        PIC S9(9)V99 COMP-3.
00632 *                                           16-21   JR AMOUNT
00633            15 PM-JR-ADJDATE      PIC 9(7)     COMP-3.
00634 *                                           22-25   ADJUDICATION
00635 *                                                    DATE
00636 *                                                    (0MMDDYY)
00637            15 PM-JR-REASON       PIC XX.
00638 *                                           26-27   REASON
00639            15 FILLER             PIC XXX.
00640 *                                           28-30   FILLER
00641 *----------------------------------------------------------------*
00642 * I N Q U I R E   R E F U N D   S T A T U S                      *
00643 *                                                                *
00644         10 PM-IR-SEG REDEFINES PM-MSTSEG.
00645            15 PM-IR-TYP          PIC XX.
00646 *                                            1-2    TYPE IR
00647               88 IR-SEG                  VALUE 'IR'.
00648 *                                                                *
00649            15 PM-IR-INSTL        PIC 9.
00650 *                                            3-3    INSTALLMENT
00651            15 PM-IR-RFNDTYP      PIC XX.
00652 *                                            4-5    REFUND TYPE
00653            15 PM-IR-RFNDAPPDATE  PIC 9(7)     COMP-3.
00654 *                                            6-9    REFUND
00655 *                                                    APPLICATION
00656 *                                                    DATE
00657 *                                                    (0MMDDYY)
00658            15 PM-IR-RFNDSTAT     PIC 99.
00659 *                                           10-11   REFUND
00660 *                                                    STATUS
00661            15 PM-IR-LETTERDATE   PIC 9(7)     COMP-3.
00662 *                                           12-15   LETTER DATE
00663 *                                                    (0MMDDYY)
00664            15 PM-IR-RFNDDATE     PIC 9(7)     COMP-3.
00665 *                                           16-19   REFUND DATE
00666 *                                                    (0MMDDYY)
00667            15 PM-IR-CHKNO        PIC 9(7)     COMP-3.
00668 *                                           20-23   CHECK NO.
00669            15 PM-IR-RMRKS        PIC X(2).
00670 *                                           24-25   REMARKS
00671            15 FILLER             PIC X(5).
00672 *                                           26-30   FILLER
00673 *----------------------------------------------------------------*
00674 * H O M E O W N E R   E X E M P T I O N   W A I V E R            *
00675 *                                                                *
00676         10 PM-WH-SEG REDEFINES PM-MSTSEG.
00677            15 PM-WH-TYP          PIC XX.
00678 *                                            1-2    TYPE WH
00679               88 WH-SEG                  VALUE 'WH'.
00680 *                                                                *
00681            15 PM-WH-WAIVEDEQVAL  PIC 9(9)     COMP-3.
00682 *                                            3-7    WAIVED
00683 *                                                    EQUALIZED
00684 *                                                    VALUATION
00685            15 PM-WH-WAIVEDTXAMT  PIC S9(9)V99 COMP-3.
00686 *                                            8-13   WAIVED
00687 *                                                    TAX AMOUNT
00688            15 PM-WH-REVTOTTX     PIC S9(9)V99 COMP-3.
00689 *                                           14-19   REVISED
00690 *                                                    TOTAL
00691 *                                                    TAX AMOUNT
00692            15 PM-WH-UPDTDATE     PIC 9(7)     COMP-3.
00693 *                                           20-23   UPDATE DATE
00694 *                                                    (0MMDDYY)
00695            15 FILLER             PIC X(7).
00696 *                                           24-30   FILLER
00697 *----------------------------------------------------------------*
00698 * H O M E S T E A D   E X E M P T I O N   W A I V E R            *
00699 *                                                                *
00700         10 PM-SW-SEG REDEFINES PM-MSTSEG.
00701            15 PM-SW-TYP          PIC XX.
00702 *                                            1-2    TYPE SW
00703               88 SW-SEG                  VALUE 'SW'.
00704 *                                                                *
00705            15 PM-SW-WAIVEDEQVAL  PIC 9(9)     COMP-3.
00706 *                                            3-7    WAIVED
00707 *                                                    EQUALIZED
00708 *                                                    VALUATION
00709            15 PM-SW-WAIVEDTXAMT  PIC S9(9)V99 COMP-3.
00710 *                                            8-13   WAIVED
00711 *                                                    TAX AMOUNT
00712            15 PM-SW-REVTOTTX     PIC S9(9)V99 COMP-3.
00713 *                                           14-19   REVISED
00714 *                                                    TOTAL
00715 *                                                    TAX AMOUNT
00716            15 PM-SW-UPDTDATE     PIC 9(7)     COMP-3.
00717 *                                           20-23   UPDATE DATE
00718 *                                                    (0MMDDYY)
00719            15 FILLER             PIC X(7).
00720 *                                           24-30   FILLER
00721 *----------------------------------------------------------------*
00722 * S E N I O R  F R E E Z E   E X E M P T I O N   W A I V E R     *
00723 *                                                                *
00724         10 PM-FW-SEG REDEFINES PM-MSTSEG.
00725            15 PM-FW-TYP          PIC XX.
00726 *                                            1-2    TYPE FW
00727               88 FW-SEG                  VALUE 'FW'.
00728 *                                                                *
00729            15 PM-FW-WAIVEDEQVAL  PIC 9(9)     COMP-3.
00730 *                                            3-7    WAIVED
00731 *                                                    EQUALIZED
00732 *                                                    VALUATION
00733            15 PM-FW-WAIVEDTXAMT  PIC S9(9)V99 COMP-3.
00734 *                                            8-13   WAIVED
00735 *                                                    TAX AMOUNT
00736            15 PM-FW-REVTOTTX     PIC S9(9)V99 COMP-3.
00737 *                                           14-19   REVISED
00738 *                                                    TOTAL
00739 *                                                    TAX AMOUNT
00740            15 PM-FW-UPDTDATE     PIC 9(7)     COMP-3.
00741 *                                           20-23   UPDATE DATE
00742 *                                                    (0MMDDYY)
00743            15 FILLER             PIC X(7).
00744 *                                           24-30   FILLER
00745 *----------------------------------------------------------------*
00746 * L O N G T I M E  O W N E R   E X E M P T I O N   W A I V E R   *
00747 *                                                                *
00748         10 PM-LW-SEG REDEFINES PM-MSTSEG.
00749            15 PM-LW-TYP          PIC XX.
00750 *                                            1-2    TYPE LW
00751               88 LW-SEG                  VALUE 'LW'.
00752 *                                                                *
00753            15 PM-LW-WAIVEDEQVAL  PIC 9(9)     COMP-3.
00754 *                                            3-7    WAIVED
00755 *                                                    EQUALIZED
00756 *                                                    VALUATION
00757            15 PM-LW-WAIVEDTXAMT  PIC S9(9)V99 COMP-3.
00758 *                                            8-13   WAIVED
00759 *                                                    TAX AMOUNT
00760            15 PM-LW-REVTOTTX     PIC S9(9)V99 COMP-3.
00761 *                                           14-19   REVISED
00762 *                                                    TOTAL
00763 *                                                    TAX AMOUNT
00764            15 PM-LW-UPDTDATE     PIC 9(7)     COMP-3.
00765 *                                           20-23   UPDATE DATE
00766 *                                                    (0MMDDYY)
00767            15 FILLER             PIC X(7).
00768 *                                           24-30   FILLER
00769 *----------------------------------------------------------------*
00770 * V E T E R A N S / D I S A B L E D   E X E M P T  W A I V E R   *
00771 *                                                                *
00772         10 PM-VW-SEG REDEFINES PM-MSTSEG.
00773            15 PM-VW-TYP          PIC XX.
00774 *                                            1-2    TYPE VW
00775               88 VW-SEG                  VALUE 'VW'.
00776 *                                                                *
00777            15 PM-VW-WAIVEDEQVAL  PIC 9(9)     COMP-3.
00778 *                                            3-7    WAIVED
00779 *                                                    EQUALIZED
00780 *                                                    VALUATION
00781            15 PM-VW-WAIVEDTXAMT  PIC S9(9)V99 COMP-3.
00782 *                                            8-13   WAIVED
00783 *                                                    TAX AMOUNT
00784            15 PM-VW-REVTOTTX     PIC S9(9)V99 COMP-3.
00785 *                                           14-19   REVISED
00786 *                                                    TOTAL
00787 *                                                    TAX AMOUNT
00788            15 PM-VW-UPDTDATE     PIC 9(7)     COMP-3.
00789 *                                           20-23   UPDATE DATE
00790 *                                                    (0MMDDYY)
00791            15 FILLER             PIC X(7).
00792 *                                           24-30   FILLER
00793 *----------------------------------------------------------------*
00794 * B A N K R U P T C Y   C O U R T   O R D E R                    *
00795 *                                                                *
00796         10 PM-BK-SEG REDEFINES PM-MSTSEG.
00797            15 PM-BK-TYP          PIC XX.
00798 *                                            1-2    TYPE BK
00799               88 BK-SEG                  VALUE 'BK'.
00800 *                                                                *
00801            15 FILLER             PIC X(4).
00802 *                                            3-6    FILLER
00803            15 PM-BK-CASENO       PIC X(10).
00804 *                                            7-16   CASE NO.
00805            15 PM-BK-DSMSDTE      PIC 9(8).
00806 *                                           17-24   DISMISSAL
00807 *                                                   DATE
00808 *                                                   (MMDDYYYY)
00809            15 FILLER             PIC X(6).
00810 *                                           25-30   FILLER
00811 *----------------------------------------------------------------*
00812 * N O   C A S H   B I D   O N   S C A V E N G E R  S A L E       *
00813 *                                                                *
00814         10 PM-SS-SEG REDEFINES PM-MSTSEG.
00815            15 PM-SS-TYP          PIC XX.
00816 *                                            1-2    TYPE SS
00817               88 SS-SEG                  VALUE 'SS'.
00818 *                                                                *
00819            15 PM-SS-SEG1.
00820 *                                            3-30   SEGMENT 1
00821               20 PM-SS-SALEYR    PIC 9(5)     COMP-3.
00822 *                                               3-5 SALE YEAR
00823 *                                                        (0YYYY)
00824               20 PM-SS-ACTCD     PIC 9.
00825 *                                               6-6 ACTION CODE
00826               20 PM-SS-SALDATE   PIC 9(7)     COMP-3.
00827 *                                               7-10 SALE DATE
00828 *                                                        (0MMDDYY)
00829               20 PM-SS-FRTXYR    PIC 99.
00830 *                                              11-12 FROM TAX YEAR
00831               20 PM-SS-TOTXYR    PIC 99.
00832 *                                              13-14 TO TAX YEAR
00833               20 PM-SS-NOYRS     PIC 99.
00834 *                                              15-16 NO. OF YEARS
00835               20 PM-SS-TAXDUE    PIC S9(9)V99 COMP-3.
00836 *                                              17-22 TAX DUE
00837               20 PM-SS-INTDUE    PIC S9(9)V99 COMP-3.
00838 *                                              23-28 INTEREST DUE
00839               20 PM-SS-SEQNO1    PIC 9.
00840 *                                              29   SEQUENCE NO
00841               20 FILLER          PIC X.
00842 *                                              30   FILLER
00843            15 PM-SS-SEG2 REDEFINES PM-SS-SEG1.
00844 *                                            3-30   SEGMENT 2
00845               20 PM-SS-BUYERNO   PIC 9(7)     COMP-3.
00846 *                                               3-6 BUYER
00847 *                                                   NUMBER
00848               20 PM-SS-SALCERT   PIC 9(7)     COMP-3.
00849 *                                               7-10 SALE
00850 *                                                    CERTIFICATE
00851               20 PM-SS-BIDAMT    PIC S9(9)V99 COMP-3.
00852 *                                              11-16 BID AMOUNT
00853               20 PM-SS-INDFND    PIC S9(9)V99 COMP-3.
00854 *                                              17-22 INDEMNITY
00855 *                                                    FUND
00856               20 PM-SS-SALERR    PIC S9(9)V99 COMP-3.
00857 *                                              23-28 SALE IN ERROR
00858 *                                                    FUND
00859               20 PM-SS-SALERRAUTO REDEFINES PM-SS-SALERR
00860                                  PIC S9(9)V99 COMP-3.
00861 *                                              23-28 SALE IN ERROR
00862 *                                                    FUND PLUS
00863 *                                                    AUTO FEE
00864               20 PM-SS-SEQNO2    PIC 9.
00865 *                                              29   SEQUENCE NO
00866               20 FILLER          PIC X.
00867 *                                              30   FILLER
00868 *----------------------------------------------------------------*
00869 * S O L D / F O R F E I T E D                                    *
00870 *(SOLD OR FORFEITED OR NO CASH BID IN ANNUAL SALE)               *
00871         10 PM-SF-SEG REDEFINES PM-MSTSEG.
00872            15 PM-SF-TYP          PIC XX.
00873 *                                            1-2    TYPE SF
00874               88 SF-SEG                  VALUE 'SF'.
00875 *
00876            15 PM-SF-SEQNO        PIC 9.
00877 *                                            3-3    SEQUENCE
00878 *                                                     NUMBER
00879            15 PM-SF-SEG1.
00880 *                                            4-30   SEGMENT 1
00881               20 PM-SF-IND       PIC 9.
00882 *                                            4-4    INDICATOR
00883               20 PM-SF-DATE      PIC 9(7)     COMP-3.
00884 *                                            5-8    DATE
00885 *                                                     (0YYMMDD)
00886               20 PM-SF-BUYNO     PIC 9(7)     COMP-3.
00887 *                                            9-12   BUYER NO.
00888               20 PM-SF-BIDPCT    PIC 999V99   COMP-3.
00889 *                                           13-15   BID PERCENT
00890               20 PM-SF-CERTNO    PIC 9(7)     COMP-3.
00891 *                                           16-19   SALE
00892 *                                                     CERTIFICATE
00893 *                                                     NUMBER
00894               20 PM-SF-1-PENPCT  PIC 999V99   COMP-3.
00895 *                                           20-22   1ST INSTALL
00896 *                                                     PNLTY PCT
00897               20 PM-SF-2-PENPCT  PIC 999V99   COMP-3.
00898 *                                           23-25   2ND INSTALL
00899 *                                                     PNLTY PCT
00900               20 PM-SF-FEEIND    PIC 9.
00901 *                                           26-26   FEE
00902 *                                                     INDICATOR
00903               20 FILLER          PIC X(4).
00904 *                                           27-30   FILLER
00905            15 PM-SF-SEG2 REDEFINES PM-SF-SEG1.
00906 *                                            4-30   SEGMENT 2
00907               20 PM-SF-1-TAXDUE  PIC S9(9)V99 COMP-3.
00908 *                                            4-9    1ST INSTALL
00909 *                                                     TAX DUE
00910               20 PM-SF-1-PNLTY   PIC S9(9)V99 COMP-3.
00911 *                                           10-15   1ST INSTALL
00912 *                                                     PENALTY
00913               20 PM-SF-2-TAXDUE  PIC S9(9)V99 COMP-3.
00914 *                                           16-21   2ND INSTALL
00915 *                                                     TAX DUE
00916               20 PM-SF-2-PNLTY   PIC S9(9)V99 COMP-3.
00917 *                                           22-27   2ND INSTALL
00918 *                                                     PENALTY
00919               20 PM-SF-COSTS     PIC S9(3)V99 COMP-3.
00920 *                                           28-30   COSTS
00921 *----------------------------------------------------------------*
00922 * M U N I C I P A L   A C Q U I S I T I O N S                    *
00923 *                                                                *
00924         10 PM-MUACQSN-SEG REDEFINES PM-MSTSEG.
00925            15 PM-MUACQSN-TYP    PIC XX.
00926 *                                            1-2    TYPE GA
00927               88 MUACQUISITION-SEGMENT   VALUE 'GA'.
00928 *                                                                *
00929            15 PM-MUACQSN-DATE   PIC 9(8).
00930 *                                            3-10   ACQUISITION DA
00931 *                                                   (MMDDYYYY)
00932            15 PM-MUACQSN-EXPYR  PIC 9(4).
00933 *                                           11-14   EXPIRATION YEA
00934            15 PM-MUACQSN-UPDATE PIC 9(8).
00935 *                                           15-22   UPDATE DATE
00936            15 FILLER            PIC X(8).
00937 *                                           23-30   FILLER
00938 *----------------------------------------------------------------*
00939 * S E N I O R  F R E E Z E  E X E M P T I O N                    *
00940 *                                                                *
00941         10 PM-SRFRZ-SEG REDEFINES PM-MSTSEG.
00942            15 PM-SRFRZ-TYP      PIC XX.
00943 *                                            1-2    TYPE FE
00944               88 SRFRZ-SEGMENT           VALUE 'FE'.
00945 *                                                                *
00946            15 PM-SRFRZ-EXVAL    PIC S9(11)    COMP-3.
00947 *                                            3-8    EXEMPT
00948 *                                                   VALUATION
00949            15 PM-SRFRZ-BASYRVAL PIC S9(11)    COMP-3.
00950 *                                            9-14   BASE YEAR
00951 *                                                   VALUATION
00952            15 PM-SRFRZ-DEDAMT   PIC S9(9)V99  COMP-3.
00953 *                                           15-20   DEDUCTION
00954 *                                                   AMOUNT
00955            15 FILLER            PIC X(10).
00956 *                                           21-30   FILLER
00957 *----------------------------------------------------------------*
00958 * E X T E N D E D  O W N E R S H I P   E X E M P T I O N
00959 *
00960         10 PM-EXOWN-SEG REDEFINES PM-MSTSEG.
00961            15 PM-EXOWN-TYP      PIC XX.
00962 *                                            1-2    TYPE EO
00963               88 EXOWN-SEGMENT           VALUE 'EO'.
00964 *
00965            15 PM-EXOWN-EXVAL    PIC S9(11)    COMP-3.
00966 *                                            3-8    EXEMPT
00967 *                                                   VALUATION
00968            15 PM-EXOWN-BASYRVAL PIC S9(11)    COMP-3.
00969 *                                            9-14   BASE YEAR
00970 *                                                   VALUATION
00971            15 PM-EXOWN-DEDAMT   PIC S9(9)V99  COMP-3.
00972 *                                           15-20   DEDUCTION
00973 *                                                   AMOUNT
00974            15 PM-EXOWN-IND      PIC X(1).
00975 *             1 = < 75,000                  21-21   EXTENDED
00976 *             2 = > 75,000                          HOMEOWNER
00977 *             3 = SUNSET EXEMPTION                  INDICATOR
00978            15 FILLER            PIC X(09).
00979 *                                           22-30   FILLER
00980 *----------------------------------------------------------------
00981 * F O R F E I T U R E  I N F O R M A T I O N                     *
00982 *                                                                *
00983         10 FF-FORFEIT-SEG REDEFINES PM-MSTSEG.
00984            15 FF-PYMT-TYP-NO.
00985               88 FORFEITURE-TYP-F1 VALUE 'F1'.
00986               88 FORFEITURE-TYP-F2 VALUE 'F2'.
00987               20 FF-PYMT-TYP       PIC X.
00988 *                                            1-1    TYPE 'F'
00989               20 FF-PYMT-NO        PIC 9.
00990 *                                            2-2    NUMBER
00991 *                                                                *
00992            15 FF-PYMT-DATE      PIC 9(7)      COMP-3.
00993 *                                            3-6    DATE         *
00994 *                                                   (0YYMMDD)    *
00995            15 FF-PYMT-AMT       PIC S9(9)V99  COMP-3.
00996 *                                            7-12   AMOUNT
00997            15 FF-PYMT-INT       PIC S9(7)V99  COMP-3.
00998 *                                            13-17  INTEREST
00999            15 FF-PYMT-CST       PIC S9(5)V99  COMP-3.
01000 *                                            18-21  COST
01001            15 FF-PYMT-SERNO     PIC 9(12)     COMP-3.
01002 *                                            22-28  SERIAL NO.
01003            15 FF-PYMT-TYP-SRC   PIC 9(3)     COMP-3.
01004 *                                           29-30  PYMT SOURCE
01005 *----------------------------------------------------------------*
01006 * L A S T  Y E A R  T A X  A M O U N T  B E F O R E  C OF E      *
01007 *                                                                *
01008         10 PM-TAXAMOUNT-SEG REDEFINES PM-MSTSEG.
01009            15 PM-TAXAMT-TYPE        PIC XX.
01010               88 PM-TOTAL-TAX-TYPE    VALUE 'LT'.
01011 *                                            1-2    SEGMENT      *
01012 *                                                   TYPE         *
01013            15 PM-NON-COFE-TOTAL-TAX PIC 9(9)V99   COMP-3.
01014 *                                            3-8    TOTAL TAX    *
01015            15 PM-NON-COFE-EST-TAX   PIC 9(9)V99   COMP-3.
01016 *                                            9-14   ESTIMATED    *
01017 *                                                   TAX          *
01018            15 FILLER            PIC X(16).
01019 *                                            15-30  FILLER
01020 *----------------------------------------------------------------*
01021 * C E R T I F I C A T E   O F   E R R O R                        *
01022 *                                                                *
01023         10 PM-CERT-SEG REDEFINES PM-MSTSEG.
01024            15 PM-CERT-TYP    PIC X.
01025 *                                            1      TYPE O
01026               88 PM-CERT-SEGMENT VALUE 'O'.
01027 *                                                                *
01028            15 PM-CERT-COENO   PIC 9(7)     COMP-3.
01029 *                                            2-05   CERTIFICATE
01030 *                                                    OF ERRORS
01031            15 PM-CERT-FILLER  PIC X(25).
01032 *                                           06-30     FILLER
01033 *                                                                *
01034 *----------------------------------------------------------------*
01035 ******************************************************************
01036 * D I S A B L E D / V E T E R A N S   E X E M P T I O N          *
01037 *                                                                *
01038         10 PM-DISVET-SEG REDEFINES PM-MSTSEG.
01039            15 PM-DISVET-TYPE        PIC XX.
01040               88 DV-SEG               VALUE 'DV'.
01041 *                                            1-2    SEGMENT      *
01042 *                                                   TYPE         *
01043            15 PM-VET-DIS-IND        PIC X.
01044 *               1 = RETURNING VETERAN
01045 *               2 = DISABLED PERSON
01046 *               3 = DISABLED VETERAN 30-49%
01047 *               4 = DISABLED VETERAN 50-69%
01048 *               5 = RETURING VETERAN / DISABLED VETERAN  < 75
01049 *               6 = RETURNING VETERAN / DISABLED VETERAN > 75
01050 *               7 = RETURNING VETERAN / DISABLED PERSON
01051 *               8 = DISABLED VETERAN > 70%
01052 *                                            3      INDICATOR    *
01053            15 PM-RET-VET-EXEMPT     PIC 9(7)      COMP-3.
01054 *                                            4-7    RETURNING    *
01055 *                                                   VETERANS     *
01056            15 PM-DISABLE-EXEMPT     PIC 9(7)      COMP-3.
01057 *                                            8-11   DISABLED     *
01058 *                                                   PERSON       *
01059            15 PM-DISABLE-VET        PIC 9(7)      COMP-3.
01060 *                                           12-15   DISABLED     *
01061 *                                                   VETERAN      *
01062            15 PM-DIS-RET-EXEMPT     PIC 9(7)      COMP-3.
01063 *                                           16-19   COMBINED     *
01064 *                                                   DIS/VET      *
01065            15 FILLER                PIC X(11).
01066 *                                           20-30
01067 *----------------------------------------------------------------*
01068 ******************************************************************
