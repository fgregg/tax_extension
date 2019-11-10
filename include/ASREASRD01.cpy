00001      05  M-BASE.
00002 *                                              1-122 FIXED SEGMENT
00003          10  M-STAT1        PIC X.
00004              88  M-NON-ASSESSED       VALUE '0'.
00005              88  M-ASSESSED           VALUE '1'.
00006 *                                              1     STATUS-1
00007          10  M-VOLPROP.
00008 *                                              2-11  RECORD KEY
00009              15  M-VOL      PIC S9(3)    COMP-3.
00010                  88  M-RE-VOL         VALUE +001 THRU +601.
00011                  88  M-RR-VOL         VALUE +605.
00012 *                                              2-3     VOLUME
00013              15  M-PROP     PIC S9(15)   COMP-3.
00014 *                                              4-11    PROPERTY-NO
00015          10  M-TXTYP        PIC X.
00016 *                                             12-12  TAX TYPE
00017          10  FILLER         PIC X.
00018 *                                             13-13  FILLER
00019          10  M-TXCD         PIC S9(5)    COMP-3.
00020 *                                             14-16  TAX CODE
00021          10  M-STAT2        PIC X.
00022              88  M-TAXABLE-PARCEL     VALUE '0'.
00023              88  M-EXEMPT             VALUE '1'.
00024              88  M-RAILROAD           VALUE '2'.
00025              88  M-HOMESTEAD-NON-COOP VALUE '3'.
00026              88  M-VETERAN            VALUE '4'.
00027              88  M-HOMESTEAD-COOP     VALUE '5'.
00028 *                                             17     STATUS-2
00029          10  M-CLS          PIC S9(3)    COMP-3.
00030 *                                             18-19  CLASS (9-99)
00031 *                                                    (MAJOR-MINOR)
00032          10  M-NBHD         PIC S9(3)    COMP-3.
00033 *                                             20-21  NEIGHBORHD-CD
00034          10  M-STRT         PIC S9(5)    COMP-3.
00035 *                                             22-24  STREET CODE
00036          10  M-HSENO        PIC S9(5)    COMP-3.
00037 *                                             25-27  HOUSE-NO
00038          10  M-LNDDIM       PIC S9(7)    COMP-3.
00039 *                                             28-31  LND-DIMENSION
00040          10  M-LNDCD        PIC X.
00041 *                                             32     LAND CODE
00042          10  M-LNDSQFT      PIC S9(7)    COMP-3.
00043 *                                             33-36  LAND-SQ-FT
00044          10  M-IRREG        PIC X.
00045 *                                             37     IRREGULAR
00046          10  M-STAT3        PIC X.
00047 *                                             38     COMPLAINT ST
00048          10  M-CMPLNTNO     PIC S9(7)    COMP-3.
00049 *                                             39-42  B.A.COMPLAINT
00050          10  M-BA-ACTION.
00051 *                                             43-44  B.A. ACTION
00052              15  M-BA-YR    PIC X.
00053 *                                             43       BA-YEAR
00054              15  M-BA-REV   PIC X.
00055 *                                             44       BA-REVISION
00056          10  M-ASMT-ACTION OCCURS 4 TIMES.
00057 *                                             45-56  ASMT-ACTIONS
00058 *                                                    (YR-CHNG-REV)
00059              15  M-ASMT-YR  PIC X.
00060 *                                             45       AA-YEAR1
00061 *                                             48       AA-YEAR2
00062 *                                             51       AA-YEAR3
00063 *                                             54       AA-YEAR4
00064              15  M-ASMT-CHG PIC X.
00065 *                                             46       AA-CHANGE1
00066 *                                             49       AA-CHANGE2
00067 *                                             52       AA-CHANGE3
00068 *                                             55       AA-CHANGE4
00069              15  M-ASMT-REV PIC X.
00070 *                                             47       AA-REVSION1
00071 *                                             50       AA-REVSION2
00072 *                                             53       AA-REVSION3
00073 *                                             56       AA-REVSION4
00074          10  M-VALUE OCCURS 12 TIMES
00075                             PIC S9(9)    COMP-3.
00076 *                                             57-116 VALUE FIELDS
00077 *                                             57-61 PRIOR LAND/
00078 *                                                   SEN FRZ EX VAL
00079 *                                             62-66 PRIOR IMPRV/
00080 *                                                   PRORATION FCTR
00081 *                                                   * USE S9(9) AS
00082 *                                                   S9(3)V9(6) *
00083 *                                             67-71 PRIOR TOTAL/
00084 *                                                   ENTRPRS ZN VAL
00085 *                                             72-76 CURRENT-LAND/
00086 *                                                  LONG TERM EXVAL
00087 *                                             77-81 CURR-IMPRV/
00088 *                                                   OCC FACTOR
00089 *                                             82-86    CURR-TOTAL
00090 *                                             87-91    PROPOSE-LND
00091 *                                             92-96    PROP-IMPRV
00092 *                                             97-101   PROP-TOTAL
00093 *                                            102-106   FARM VALUE
00094 *                                            107-111   HOMEOWNER
00095 *                                                      VALUE
00096 *                                            112-116   NON-HOMOWNR
00097 *                                                      VALUE
00098          10  M-LT-IND       PIC X.
00099 *                                            117-117 LONG TERM
00100 *                                                     INDICATOR
00101          10  FILLER         PIC X.
00102 *                                            118-118 FILLER
00103          10  M-REC-CTRS.
00104 *                                            119-122 RECORD CNTRS
00105              15  M-SLS-CTR  PIC S9.
00106                  88  M-NO-SALES       VALUE +0.
00107                  88  M-SALES-PRESENT  VALUE +1.
00108 *                                            119       SALES-CNTR
00109              15  M-DTL-QST-CTR
00110                             PIC S9(3).
00111                  88  M-NO-DETAIL      VALUE +0.
00112                  88  M-DETAIL-PRESENT VALUE +1 THRU +350.
00113                  88  M-MAXIMUM-DETAIL VALUE +350.
00114 *                                            120-122   DETAIL-CNTR
00115 *----------------------------------------------------------------*
00116 *   S A L E S  S E G M E N T, IF PRESENT, CONTAINS 34 BYTES      *
00117 *    DESCRIPTIONS RELATIVE TO 1ST BYTE IN EACH SEGMENT           *
00118 *                                                                *
00119      05  M-SALES OCCURS 0 TO 1 TIMES DEPENDING ON M-SLS-CTR.
00120 *                                              1-34  SALES SEGMENT
00121          10  M-DEEDTYP      PIC XX.
00122 *                                              1-2   DEED-TYPE
00123          10  M-DEEDNO       PIC S9(9)    COMP-3.
00124 *                                              3-7   DEED-NUMBER
00125          10  M-SLS-DAT1     PIC S9(7)    COMP-3.
00126 *                                              8-11  SALE-DATE1
00127 *                                                      (YYMMDD)
00128          10  M-SLS-AMT1     PIC S9(9)    COMP-3.
00129 *                                             12-16  SALE-AMOUNT1
00130          10  M-SLS-DAT2     PIC S9(7)    COMP-3.
00131 *                                             17-20  SALE-DATE2
00132 *                                                      (YYMMDD)
00133          10  M-SLS-AMT2     PIC S9(9)    COMP-3.
00134 *                                             21-25  SALE-AMOUNT2
00135          10  M-SLS-DAT3     PIC S9(7)    COMP-3.
00136 *                                             26-29  SALE-DATE3
00137 *                                                      (YYMMDD)
00138          10  M-SLS-AMT3     PIC S9(9)    COMP-3.
00139 *                                             30-34  SALE-AMOUNT3
00140 *----------------------------------------------------------------*
00141 * D E T A I L  S E G M E N T S, IF PRESENT,EACH CONTAINS 53 BYTES
00142 *    DESCRIPTIONS RELATIVE TO 1ST BYTE IN EACH SEGMENT           *
00143 *                                                                *
00144      05  M-DTL-QST
00145              OCCURS 0 TO 350 TIMES DEPENDING ON M-DTL-QST-CTR.
00146          10  D-TYP1.
00147 *                                              1-53  TYPE1-LAND
00148              15  D1-MC      PIC S999     COMP-3.
00149 *                                              1-2   S1-MULTI-CODE
00150              15  D1-TYP     PIC X.
00151                  88  D1-TYPE1         VALUE '1'.
00152 *                                              3     S1-TYPE1
00153              15  D1-CD      PIC X.
00154 *                                              4     S1-CODE 0/1
00155 *                                                    (0=VACANT)
00156 *                                                    (1=IMPROVED)
00157              15  D1-DEC     PIC X.
00158 *                                              5     S1-DECIMAL
00159              15  D1-UM      PIC XX.
00160 *                                              6-7   S1-UNIT-MEAS
00161              15  D1-CLS     PIC S999     COMP-3.
00162 *                                              8-9   S1-CLASS
00163 *                                                    (MAJOR-MINOR)
00164              15  D1-EXRR    PIC X.
00165 *                                             10     S1-EX-RR
00166              15  D1-FF      PIC S9(7)    COMP-3.
00167 *                                             11-14  S1-FRONT-FT
00168              15  D1-DPTH    PIC S9(5)    COMP-3.
00169 *                                             15-17  S1-DEPTH
00170              15  D1-UPR     PIC S9(5)V99 COMP-3.
00171 *                                             18-21  S1-UNIT-PRICE
00172              15  D1-DFCTR   PIC S99V999  COMP-3.
00173 *                                             22-24  S1-DPTH-FACTR
00174              15  D1-CFCTR   PIC S9V9(4)  COMP-3.
00175 *                                             25-27  S1-CORNR-FCTR
00176              15  D1-ECFCTR  PIC SV9(5)   COMP-3.
00177 *                                             28-30  S1-EXTRA
00178 *                                                    CORNER FACTOR
00179              15  D1-PCASSD  PIC S99V9(5) COMP-3.
00180 *                                             31-34  S1-% ASSESSED
00181              15  D1-EI      PIC S99V9    COMP-3.
00182                  88  D1-ECON1         VALUE +10.0.
00183                  88  D1-ECON2         VALUE +13.0.
00184                  88  D1-ECON3         VALUE +15.0.
00185                  88  D1-ECON4         VALUE +16.0.
00186                  88  D1-ECON5         VALUE +20.0.
00187                  88  D1-ECON6         VALUE +25.0.
00188                  88  D1-ECON-IND      VALUE +10.0 +13.0 +15.0
00189                                             +16.0 +20.0 +25.0.
00190 *                                             35-36  S1-ECON-IND
00191              15  D1-VAL     PIC S9(9)    COMP-3.
00192 *                                             37-41  S1-VALUATION
00193              15  FILLER     PIC X(11).
00194 *                                             42-52  FILLER
00195              15  D1-UI      PIC X.
00196 *                                             53     S1-UNIT-IND
00197 *----------------------------------------------------------------*
00198          10  D-TYP2 REDEFINES D-TYP1.
00199 *                                              1-53  TYPE2-IMPRVMT
00200              15  D2-MC      PIC S999     COMP-3.
00201 *                                              1-2   S2-MULTI-CODE
00202              15  D2-TYP     PIC X.
00203                  88  D2-TYPE2         VALUE '2'.
00204                  88  D2-TYPE2-5       VALUE '2' THRU '5'.
00205 *                                              3     S2-TYPE2
00206              15  D2-CD      PIC X.
00207 *                                              4     S2-CODE 2/3
00208 *                                                    2=MAJOR-IMPRV
00209 *                                                    3=MINOR-IMPRV
00210              15  D2-DEC     PIC X.
00211 *                                              5     S2-DECIMAL
00212              15  D2-UM      PIC XX.
00213 *                                              6-7   S2-UNIT-MEAS
00214              15  D2-CLS     PIC S999     COMP-3.
00215                  88  D2-RES-CLASS     VALUE +202 THRU +212
00216                                             +234 +278 +295.
00217                  88  D2-QUES-CLASS    VALUE +202 THRU +212
00218                                             +234 +278 +295
00219                                             +402 THRU +412
00220                                             +434 +478 +495.
00221                  88  D2-CLS-288       VALUE +288.
00222                  88  D2-QCLS1-3       VALUE +210 +211 +212 +295
00223                                             +410 +411 +412 +495.
00224                  88  D2-QCLS1OR5      VALUE +202 +203 +204
00225                                             +402 +403 +404.
00226                  88  D2-QCLS2         VALUE +205 THRU +208 +278
00227                                             +405 THRU +408 +478.
00228                  88  D2-QCLS2-3       VALUE +209 +409.
00229                  88  D2-QCLS4         VALUE +234 +434.
00230                  88  D2-QCLS6         VALUE +593.
00231                  88  D2-QCLS7         VALUE +597.
00232 *                                              8-9   S2-CLASS
00233 *                                                    (MAJOR-MINOR)
00234              15  D2-CDU     PIC XX.
00235 *                                             10-11  S2-CDU
00236              15  D2-AREA    PIC S9(7)    COMP-3.
00237 *                                             12-15  S2-AREA
00238              15  D2-UPR     PIC S9(5)V99 COMP-3.
00239 *                                             16-19  S2-UNIT-PRICE
00240              15  D2-PRDCT   PIC S9(9)    COMP-3.
00241 *                                             20-24  S2-PRODUCT
00242              15  D2-AGE     PIC S999     COMP-3.
00243                  88  D2-0-AGE         VALUE +0.
00244                  88  D2-OVER-80       VALUE +081 THRU +999.
00245 *                                             25-26  S2-AGE
00246              15  D2-COND    PIC S99V9    COMP-3.
00247 *                                             27-28  S2-CONDITION
00248              15  D2-PCASSD  PIC S99V9(5) COMP-3.
00249 *                                             29-32  S2-% ASSESSED
00250              15  D2-BUFF    PIC S999     COMP-3.
00251 *                                             33-34  S2-BUFF-NO
00252              15  FILLER     PIC XX.
00253 *                                             35-36  FILLER
00254              15  D2-VAL     PIC S9(9)    COMP-3.
00255 *                                             37-41  S2-VALUATION
00256              15  D2-KEYPCL  PIC S9(15)   COMP-3.
00257 *                                             42-49  S2-KEY PARCEL
00258              15  D2-SC      PIC X.
00259 *                                             50     S2-SPLIT CODE
00260              15  FILLER     PIC X(3).
00261 *                                             51-53  FILLER
00262 *----------------------------------------------------------------*
00263          10  D-TYP3 REDEFINES D-TYP1.
00264 *                                              1-53  TYPE3-IMPRVMT
00265              15  D3-MC      PIC S999     COMP-3.
00266 *                                              1-2   S3-MULTI-CODE
00267              15  D3-TYP     PIC X.
00268                  88  D3-TYPE3         VALUE '3'.
00269 *                                              3     S3-TYPE3
00270              15  D3-CD      PIC X.
00271 *                                              4     S3-CODE 2/3
00272 *                                                    2=MAJOR-IMPRV
00273 *                                                    3=MINOR-IMPRV
00274              15  FILLER     PIC X(3).
00275 *                                              5-7   FILLER
00276              15  D3-CLS     PIC S999     COMP-3.
00277 *                                              8-9   S3-CLASS
00278 *                                                    (MAJOR-MINOR)
00279              15  D3-CDU     PIC XX.
00280 *                                             10-11  S3-CDU
00281              15  D3-REPCST  PIC S9(9)    COMP-3.
00282 *                                             12-16  S3-REPRODUCTV
00283 *                                                    COST
00284              15  FILLER     PIC X(6).
00285 *                                             17-22  FILLER
00286              15  D3-YR      PIC S999     COMP-3.
00287 *                                             23-24  S3-YEAR
00288              15  D3-AGE     PIC S999     COMP-3.
00289                  88  D3-0-AGE         VALUE +0.
00290                  88  D3-OVER-80       VALUE +081 THRU +999.
00291 *                                             25-26  S3-AGE
00292              15  D3-COND    PIC S99V9    COMP-3.
00293 *                                             27-28  S3-CONDITION
00294              15  D3-PCASSD  PIC S99V9(5) COMP-3.
00295 *                                             29-32  S3-% ASSESSED
00296              15  D3-BUFF    PIC S999     COMP-3.
00297 *                                             33-34  S3-BUFF-NO
00298              15  FILLER     PIC XX.
00299 *                                             35-36  FILLER
00300              15  D3-VAL     PIC S9(9)    COMP-3.
00301 *                                             37-41  S3-VALUATION
00302              15  D3-KEYPCL  PIC S9(15)   COMP-3.
00303 *                                             42-49  S3-KEY PARCEL
00304              15  D3-SC      PIC X.
00305 *                                             50     S3-SPLIT CODE
00306              15  FILLER     PIC X(3).
00307 *                                             51-53  FILLER
00308 *----------------------------------------------------------------*
00309          10  D-TYP4 REDEFINES D-TYP1.
00310 *                                              1-53  TYPE4-IMPRVMT
00311              15  D4-MC      PIC S999     COMP-3.
00312 *                                              1-2   S4-MULTI-CODE
00313              15  D4-TYP     PIC X.
00314                  88  D4-TYPE4         VALUE '4'.
00315 *                                              3     S4-TYPE4
00316              15  D4-CD      PIC X.
00317 *                                              4     S4-CODE4
00318              15  FILLER     PIC X(3).
00319 *                                              5-7   FILLER
00320              15  D4-CLS     PIC S999     COMP-3.
00321 *                                              8-9   S4-CLASS
00322 *                                                    (MAJOR-MINOR)
00323              15  D4-CDU     PIC XX.
00324 *                                             10-11  S4-CDU
00325              15  D4-REPCST  PIC S9(9)    COMP-3.
00326 *                                             12-16  S4-REPRODUCTV
00327 *                                                    COST
00328              15  D4-TOTVAL  PIC S9(9)    COMP-3.
00329 *                                             17-21  S4-TOTAL-VALU
00330              15  FILLER     PIC X.
00331 *                                             22     FILLER
00332              15  D4-OCCFAC  PIC S99V9    COMP-3.
00333 *                                             23-24  S4-OCCUPANCY
00334 *                                                    FACTOR
00335              15  D4-AGE     PIC S999     COMP-3.
00336                  88  D4-0-AGE         VALUE +0.
00337                  88  D4-OVER-80       VALUE +081 THRU +999.
00338 *                                             25-26  S4-AGE
00339              15  D4-COND    PIC S99V9    COMP-3.
00340 *                                             27-28  S4-CONDITION
00341              15  D4-PCASSD  PIC S99V9(5) COMP-3.
00342 *                                             29-32  S4-% ASSESSED
00343              15  D4-BUFF    PIC S999     COMP-3.
00344 *                                             33-34  S4-BUFF-NO
00345              15  FILLER     PIC XX.
00346 *                                             35-36  FILLER
00347              15  D4-VAL     PIC S9(9)    COMP-3.
00348 *                                             37-41  S4-VALUATION
00349              15  D4-KEYPCL  PIC S9(15)   COMP-3.
00350 *                                             42-49  S4-KEY PARCEL
00351              15  D4-SC      PIC X.
00352 *                                             50     S4-SPLIT CODE
00353              15  FILLER     PIC X(3).
00354 *                                             51-53  FILLER
00355 *----------------------------------------------------------------*
00356 *                                              1-53  TYPE5-IMPRVMT
00357          10  D-TYP5 REDEFINES D-TYP1.
00358              15  D5-MC      PIC S999     COMP-3.
00359 *                                              1-2   S5-MULTI-CODE
00360              15  D5-TYP     PIC X.
00361                  88  D5-TYPE5         VALUE '5'.
00362 *                                              3     S5-TYPE5
00363              15  D5-CD      PIC X.
00364 *                                              4     S5-CODE5
00365              15  FILLER     PIC X(3).
00366 *                                              5-7   FILLER
00367              15  D5-CLS     PIC S999     COMP-3.
00368 *                                              8-9   S5-CLASS
00369 *                                                    (MAJOR-MINOR)
00370              15  D5-CDU     PIC XX.
00371 *                                             10-11  S5-CDU
00372              15  D5-REPCST  PIC S9(9)    COMP-3.
00373 *                                             12-16  S5-REPRODUCTV
00374 *                                                    COST
00375              15  FILLER     PIC X(6).
00376 *                                             17-22  FILLER
00377              15  D5-OCCFAC  PIC S99V9    COMP-3.
00378 *                                             23-24  S5-OCCUPANCY
00379 *                                                    FACTOR
00380              15  D5-AGE     PIC S999     COMP-3.
00381                  88  D5-0-AGE         VALUE +0.
00382                  88  D5-OVER-80       VALUE +081 THRU +999.
00383 *                                             25-26  S5-AGE
00384              15  D5-COND    PIC S99V9    COMP-3.
00385 *                                             27-28  S5-CONDITION
00386              15  D5-PCASSD  PIC S99V9(5) COMP-3.
00387 *                                             29-32  S5-% ASSESSED
00388              15  D5-BUFF    PIC S999     COMP-3.
00389 *                                             33-34  S5-BUFF-NO
00390              15  FILLER     PIC XX.
00391 *                                             35-36  FILLER
00392              15  D5-VAL     PIC S9(9)    COMP-3.
00393 *                                             37-41  S5-VALUATION
00394              15  D5-KEYPCL  PIC S9(15)   COMP-3.
00395 *                                             42-49  S5-KEY PARCEL
00396              15  D5-SC      PIC X.
00397 *                                             50     S5-SPLIT CODE
00398              15  FILLER     PIC X(3).
00399 *                                             51-53  FILLER
00400 *----------------------------------------------------------------*
00401 * --  Q U E S T I O N N A I R E  S E G M E N T  -- IS PRESENT,   *
00402 *      IF THE PRECEDING DETAIL SEGMENT IS - TYPE = '2' THRU '5'  *
00403 *      AND CLASS = 202-212,234,278,295,402-412,434,478,495       *
00404 *                                                                *
00405          10  M-QST  REDEFINES D-TYP1.
00406 *                                              1-53  QUESTIONNAIRE
00407            12  RFILL1.
00408                15  R1       PIC 9.
00409                    88  R1-RESTYP1-5 VALUE 1 THRU 5.
00410                    88  R1-RESTYP1-3 VALUE 1 2 3 5.
00411                    88  R1-RESTYP1OR5 VALUE 1 5.
00412                    88  R1-RESTYP2   VALUE 2.
00413                    88  R1-RESTYP2-3 VALUE 2 3.
00414                    88  R1-RESTYP4   VALUE 4.
00415 *                                              1     TYPE-RESIDENC
00416                15  R2       PIC 9.
00417 *                                              2     USE
00418                15  R2A      PIC 9.
00419 *                                              3     NO. OF APTS.
00420 *                                   ---------- 4-5   EXTERIOR -
00421 *                                                     CONSTRUCTION
00422                15  R3       PIC 9.
00423 *                                              4     WALLS
00424                15  R4       PIC 9.
00425 *                                              5     ROOF
00426 *                                   ---------- 6-27  ROOMS,HEATING
00427 *                                                     AND PLUMBING
00428            12  RFILL2.
00429                15  R5A1     PIC S999     COMP-3.
00430 *                                              6-7   NO. OF ROOMS
00431            12  RFILL3.
00432                15  R5A2     PIC 99.
00433 *                                              8-9   NO. OF BEDRMS
00434                15  R5B1     PIC 9.
00435 *                                             10     BASEMENT-TYPE
00436                15  R5B2     PIC 9.
00437 *                                             11     BSMT-FINISH
00438                15  R5C1     PIC 9.
00439 *                                             12     CENTRAL-HEAT
00440                15  R5C2F    PIC 9.
00441 *                                             13     FLOOR-FURNACE
00442                15  R5C2U    PIC 9.
00443 *                                             14     UNIT-HEATER
00444                15  R5C2S    PIC 9.
00445 *                                             15     STOVE
00446                15  R5C2O    PIC 9.
00447 *                                             16     SOLAR
00448                15  R5C3     PIC 9.
00449 *                                             17     CENTRAL-AIR
00450                15  R5C4     PIC 9.
00451 *                                             18     NO. FIREPLACE
00452                15  RNCU     PIC 9.
00453 *                                             19     NO.COMMERCIAL
00454 *                                                    UNITS
00455                15  FILLER   PIC X(3).
00456 *                                             20-22  FILLER
00457                15  R5D1     PIC 9.
00458 *                                             23     ATTIC-TYPE
00459                15  R5D2     PIC 9.
00460 *                                             24     ATTIC-FINISH
00461                15  R5E1     PIC 99.
00462 *                                             25-26  NO. FULL-BATH
00463                15  R5E2     PIC 9.
00464 *                                             27     NO. HALF-BATH
00465                15  R6A      PIC 9.
00466 *                                             28     TYPE OF DESGN
00467 *                                                    ARCHITECT OR
00468 *                                                    STOCK PLAN
00469                15  R6B      PIC 9.
00470 *                                             29     TYPE OF DESGN
00471 *                                                    TYPICAL OR
00472 *                                                    ATYPICAL
00473                15  R7A      PIC 9.
00474 *                                             30     CONST QUALITY
00475 *                                                    1 = DELUXE
00476 *                                                    2 = AVERAGE
00477 *                                                    3 = POOR
00478                15  R7B      PIC 9.
00479 *                                             31     CONST QUALITY
00480 *                                                    1 = RENOVATED
00481                15  R8       PIC 9.
00482 *                                             32     SITE DESRBLTY
00483 *                                   ----------33-36  GARAGE - INFO
00484                15  R9A      PIC 9.
00485 *                                             33     GARAGE-SIZE
00486                15  R9B      PIC 9.
00487 *                                             34     GARAGE-CONST.
00488                15  R9C      PIC 9.
00489 *                                             35     GARAGE-ATTACH
00490                15  R9D      PIC 9.
00491 *                                             36     GRAGE IN AREA
00492 *                                   ----------37-40  2ND GRGE INFO
00493                15  R10A     PIC 9.
00494 *                                             37     GARAGE-SIZE
00495                15  R10B     PIC 9.
00496 *                                             38     GARAGE-CONST.
00497                15  R10C     PIC 9.
00498 *                                             39     GARAGE-ATTACH
00499                15  R10D     PIC 9.
00500 *                                             40     GRAGE IN AREA
00501                15  R11      PIC 9.
00502 *                                             41     PORCH
00503            12  RFILL4.
00504                15  R12      PIC S9(7)    COMP-3.
00505 *                                   ----------42-45  OTHER -
00506 *                                                     IMPROVEMENTS
00507                15  R13      PIC S9(7)    COMP-3.
00508 *                                             46-49  BLDG-SQ-FT
00509            12  RFILL5.
00510                15  R16      PIC 9.
00511 *                                             50     ST OF REPAIR
00512                15  R17      PIC 9.
00513 *                                             51     SPECIAL COND
00514            12  RFILL6.
00515                15  FILLER   PIC XX.
00516 *                                             52-53  FILLER
00517 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
