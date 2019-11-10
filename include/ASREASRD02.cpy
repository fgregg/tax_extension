00001      05  M-BASE.
00002 *                                              1-122 FIXED SEGMENT
00003          10  M-STAT1        PIC X.
00004              88  MR-NON-ASSESSED      VALUE '0'.
00005              88  MR-ASSESSED          VALUE '1'.
00006 *                                              1     STATUS-1
00007          10  M-VOLPROP.
00008 *                                              2-11  RECORD KEY
00009              15  M-VOL      PIC S9(3)    COMP-3.
00010                  88  MR-RE-VOL        VALUE +001 THRU +601.
00011                  88  MR-RR-VOL        VALUE +605.
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
00022              88  MR-TAXABLE-PARCEL    VALUE '0'.
00023              88  MR-EXEMPT            VALUE '1'.
00024              88  MR-RAILROAD          VALUE '2'.
00025              88  MR-HOMESTEAD-NON-COOP VALUE '3'.
00026              88  MR-VETERAN           VALUE '4'.
00027              88  MR-HOMESTEAD-COOP    VALUE '5'.
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
00074          10  M-VALUE-1  OCCURS 12 TIMES
00075                             PIC S9(9)    COMP-3.
00076 *                                             57-116 VALUE FIELDS
00077 *                                             57-61    PRIOR LAND
00078 *                                             62-66    PRIOR IMPRV
00079 *                                             67-71    PRIOR TOTAL
00080 *                                             72-76    CURRNT-LAND
00081 *                                             77-81    CURR-IMPRV
00082 *                                             82-86    CURR-TOTAL
00083 *                                             87-91    PROPOSE-LND
00084 *                                             92-96    PROP-IMPRV
00085 *                                             97-101   PROP-TOTAL
00086 *                                            102-106   PRIOR-REG/
00087 *                                                      COMPL DATE
00088 *                                            107-111   CURRENT-REG
00089 *                                            112-116   PRIOR-PROP
00090          10  M-LT-IND       PIC X.
00091 *                                            117-117 LONG TERM
00092 *                                                     INDICATOR
00093          10  FILLER         PIC X.
00094 *                                            118-118 FILLER
00095          10  M-REC-CTRS-1.
00096 *                                            119-122 RECORD CNTRS
00097              15  M-SLS-CTR-1
00098                             PIC S9.
00099                  88  MR-NO-SALES      VALUE +0.
00100                  88  MR-SALES-PRESENT VALUE +1.
00101 *                                            119       SALES-CNTR
00102              15  M-DTL-QST-CTR-1
00103                             PIC S9(3).
00104                  88  MR-NO-DETAIL     VALUE +0.
00105                  88  MR-DETAIL-PRESENT VALUE +1 THRU +350.
00106                  88  MR-MAXIMUM-DETAIL VALUE +350.
00107 *                                            120-122   DETAIL-CNTR
00108 *----------------------------------------------------------------*
00109 *   S A L E S  S E G M E N T, IF PRESENT, CONTAINS 34 BYTES      *
00110 *    DESCRIPTIONS RELATIVE TO 1ST BYTE IN EACH SEGMENT           *
00111 *                                                                *
00112      05  M-SALES-1 OCCURS 0 TO 1 TIMES DEPENDING ON M-SLS-CTR-1.
00113 *                                              1-34  SALES SEGMENT
00114          10  M-DEEDTYP-1    PIC XX.
00115 *                                              1-2   DEED-TYPE
00116          10  M-DEEDNO-1     PIC S9(9)    COMP-3.
00117 *                                              3-7   DEED-NUMBER
00118          10  M-SLS-DAT1-1   PIC S9(7)    COMP-3.
00119 *                                              8-11  SALE-DATE1
00120 *                                                      (YYMMDD)
00121          10  M-SLS-AMT1-1   PIC S9(9)    COMP-3.
00122 *                                             12-16  SALE-AMOUNT1
00123          10  M-SLS-DAT2-1   PIC S9(7)    COMP-3.
00124 *                                             17-20  SALE-DATE2
00125 *                                                      (YYMMDD)
00126          10  M-SLS-AMT2-1   PIC S9(9)    COMP-3.
00127 *                                             21-25  SALE-AMOUNT2
00128          10  M-SLS-DAT3-1   PIC S9(7)    COMP-3.
00129 *                                             26-29  SALE-DATE3
00130 *                                                      (YYMMDD)
00131          10  M-SLS-AMT3-1   PIC S9(9)    COMP-3.
00132 *                                             30-34  SALE-AMOUNT3
00133 *----------------------------------------------------------------*
00134 * D E T A I L  S E G M E N T S, IF PRESENT,EACH CONTAINS 53 BYTES
00135 *    DESCRIPTIONS RELATIVE TO 1ST BYTE IN EACH SEGMENT           *
00136 *                                                                *
00137      05  M-DTL-QST-1
00138              OCCURS 0 TO 350 TIMES DEPENDING ON M-DTL-QST-CTR-1.
00139          10  D-TYP1.
00140 *                                              1-53  TYPE1-LAND
00141              15  D1-MC      PIC S999     COMP-3.
00142 *                                              1-2   S1-MULTI-CODE
00143              15  D1-TYP     PIC X.
00144                  88  D1R-TYPE1        VALUE '1'.
00145 *                                              3     S1-TYPE1
00146              15  D1-CD      PIC X.
00147 *                                              4     S1-CODE 0/1
00148 *                                                    (0=VACANT)
00149 *                                                    (1=IMPROVED)
00150              15  D1-DEC     PIC X.
00151 *                                              5     S1-DECIMAL
00152              15  D1-UM      PIC XX.
00153 *                                              6-7   S1-UNIT-MEAS
00154              15  D1-CLS     PIC S999     COMP-3.
00155 *                                              8-9   S1-CLASS
00156 *                                                    (MAJOR-MINOR)
00157              15  D1-EXRR    PIC X.
00158 *                                             10     S1-EX-RR
00159              15  D1-FF      PIC S9(7)    COMP-3.
00160 *                                             11-14  S1-FRONT-FT
00161              15  D1-DPTH    PIC S9(5)    COMP-3.
00162 *                                             15-17  S1-DEPTH
00163              15  D1-UPR     PIC S9(5)V99 COMP-3.
00164 *                                             18-21  S1-UNIT-PRICE
00165              15  D1-DFCTR   PIC S99V999  COMP-3.
00166 *                                             22-24  S1-DPTH-FACTR
00167              15  D1-CFCTR   PIC S9V9(4)  COMP-3.
00168 *                                             25-27  S1-CORNR-FCTR
00169              15  D1-ECFCTR  PIC SV9(5)   COMP-3.
00170 *                                             28-30  S1-EXTRA
00171 *                                                    CORNER FACTOR
00172              15  D1-PCASSD  PIC S99V9(5) COMP-3.
00173 *                                             31-34  S1-% ASSESSED
00174              15  D1-EI      PIC S99V9    COMP-3.
00175                  88  D1R-ECON1        VALUE +20.0.
00176                  88  D1R-ECON2        VALUE +16.0.
00177                  88  D1R-ECON3        VALUE +33.0.
00178                  88  D1R-ECON4        VALUE +30.0.
00179                  88  D1R-ECON5        VALUE +36.0.
00180                  88  D1R-ECON6        VALUE +38.0.
00181                  88  D1R-ECON-IND  VALUE +10.0 +13.0 +15.0 +16.0
00182                                          +20.0 +25.0.
00183 *                88  D1R-ECON-IND  VALUE +10.0 +16.0 +20.0 +25.0
00184 *                                        +30.0 +33.0 +36.0 +38.0.
00185 *                                             35-36  S1-ECON-IND
00186              15  D1-VAL     PIC S9(9)    COMP-3.
00187 *                                             37-41  S1-VALUATION
00188              15  FILLER     PIC X(11).
00189 *                                             42-52  FILLER
00190              15  D1-UI      PIC X.
00191 *                                             53     S1-UNIT-IND
00192 *----------------------------------------------------------------*
00193          10  D-TYP2 REDEFINES D-TYP1.
00194 *                                              1-53  TYPE2-IMPRVMT
00195              15  D2-MC      PIC S999     COMP-3.
00196 *                                              1-2   S2-MULTI-CODE
00197              15  D2-TYP     PIC X.
00198                  88  D2R-TYPE2        VALUE '2'.
00199                  88  D2R-TYPE2-5      VALUE '2' THRU '5'.
00200 *                                              3     S2-TYPE2
00201              15  D2-CD      PIC X.
00202 *                                              4     S2-CODE 2/3
00203 *                                                    2=MAJOR-IMPRV
00204 *                                                    3=MINOR-IMPRV
00205              15  D2-DEC     PIC X.
00206 *                                              5     S2-DECIMAL
00207              15  D2-UM      PIC XX.
00208 *                                              6-7   S2-UNIT-MEAS
00209              15  D2-CLS     PIC S999     COMP-3.
00210                  88  D2R-RES-CLASS    VALUE +202 THRU +212
00211                                             +234 +278 +295.
00212                  88  D2R-QUES-CLASS   VALUE +202 THRU +212
00213                                             +234 +278 +295
00214                                             +402 THRU +412
00215                                             +434 +478 +495.
00216                  88  D2R-CLS-288      VALUE +288.
00217                  88  D2R-QCLS1-3      VALUE +210 +211 +212 +295
00218                                             +410 +411 +412 +495.
00219                  88  D2R-QCLS1OR5     VALUE +202 +203 +204
00220                                             +402 +403 +404.
00221                  88  D2R-QCLS2        VALUE +205 THRU +208 +278
00222                                             +405 THRU +408 +478.
00223                  88  D2R-QCLS2-3      VALUE +209 +409.
00224                  88  D2R-QCLS4        VALUE +234 +434.
00225 *                                              8-9   S2-CLASS
00226 *                                                    (MAJOR-MINOR)
00227              15  D2-CDU     PIC XX.
00228 *                                             10-11  S2-CDU
00229              15  D2-AREA    PIC S9(7)    COMP-3.
00230 *                                             12-15  S2-AREA
00231              15  D2-UPR     PIC S9(5)V99 COMP-3.
00232 *                                             16-19  S2-UNIT-PRICE
00233              15  D2-PRDCT   PIC S9(9)    COMP-3.
00234 *                                             20-24  S2-PRODUCT
00235              15  D2-AGE     PIC S999     COMP-3.
00236                  88  D2-0-AGE         VALUE +0.
00237                  88  D2-OVER-80       VALUE +081 THRU +999.
00238 *                                             25-26  S2-AGE
00239              15  D2-COND    PIC S99V9    COMP-3.
00240 *                                             27-28  S2-CONDITION
00241              15  D2-PCASSD  PIC S99V9(5) COMP-3.
00242 *                                             29-32  S2-% ASSESSED
00243              15  D2-BUFF    PIC S999     COMP-3.
00244 *                                             33-34  S2-BUFF-NO
00245              15  FILLER     PIC XX.
00246 *                                             35-36  FILLER
00247              15  D2-VAL     PIC S9(9)    COMP-3.
00248 *                                             37-41  S2-VALUATION
00249              15  D2-KEYPCL  PIC S9(15)   COMP-3.
00250 *                                             42-49  S2-KEY PARCEL
00251              15  D2-SC      PIC X.
00252 *                                             50     S2-SPLIT CODE
00253              15  FILLER     PIC X(3).
00254 *                                             51-53  FILLER
00255 *----------------------------------------------------------------*
00256          10  D-TYP3 REDEFINES D-TYP1.
00257 *                                              1-53  TYPE3-IMPRVMT
00258              15  D3-MC      PIC S999     COMP-3.
00259 *                                              1-2   S3-MULTI-CODE
00260              15  D3-TYP     PIC X.
00261                  88  D3R-TYPE3        VALUE '3'.
00262 *                                              3     S3-TYPE3
00263              15  D3-CD      PIC X.
00264 *                                              4     S3-CODE 2/3
00265 *                                                    2=MAJOR-IMPRV
00266 *                                                    3=MINOR-IMPRV
00267              15  FILLER     PIC X(3).
00268 *                                              5-7   FILLER
00269              15  D3-CLS     PIC S999     COMP-3.
00270 *                                              8-9   S3-CLASS
00271 *                                                    (MAJOR-MINOR)
00272              15  D3-CDU     PIC XX.
00273 *                                             10-11  S3-CDU
00274              15  D3-REPCST  PIC S9(9)    COMP-3.
00275 *                                             12-16  S3-REPRODUCTV
00276 *                                                    COST
00277              15  FILLER     PIC X(6).
00278 *                                             17-22  FILLER
00279              15  D3-YR      PIC S999     COMP-3.
00280 *                                             23-24  S3-YEAR
00281              15  D3-AGE     PIC S999     COMP-3.
00282                  88  D3-0-AGE         VALUE +0.
00283                  88  D3-OVER-80       VALUE +081 THRU +999.
00284 *                                             25-26  S3-AGE
00285              15  D3-COND    PIC S99V9    COMP-3.
00286 *                                             27-28  S3-CONDITION
00287              15  D3-PCASSD  PIC S99V9(5) COMP-3.
00288 *                                             29-32  S3-% ASSESSED
00289              15  D3-BUFF    PIC S999     COMP-3.
00290 *                                             33-34  S3-BUFF-NO
00291              15  FILLER     PIC XX.
00292 *                                             35-36  FILLER
00293              15  D3-VAL     PIC S9(9)    COMP-3.
00294 *                                             37-41  S3-VALUATION
00295              15  D3-KEYPCL  PIC S9(15)   COMP-3.
00296 *                                             42-49  S3-KEY PARCEL
00297              15  D3-SC      PIC X.
00298 *                                             50     S3-SPLIT CODE
00299              15  FILLER     PIC X(3).
00300 *                                             51-53  FILLER
00301 *----------------------------------------------------------------*
00302          10  D-TYP4 REDEFINES D-TYP1.
00303 *                                              1-53  TYPE4-IMPRVMT
00304              15  D4-MC      PIC S999     COMP-3.
00305 *                                              1-2   S4-MULTI-CODE
00306              15  D4-TYP     PIC X.
00307                  88  D4R-TYPE4        VALUE '4'.
00308 *                                              3     S4-TYPE4
00309              15  D4-CD      PIC X.
00310 *                                              4     S4-CODE4
00311              15  FILLER     PIC X(3).
00312 *                                              5-7   FILLER
00313              15  D4-CLS     PIC S999     COMP-3.
00314 *                                              8-9   S4-CLASS
00315 *                                                    (MAJOR-MINOR)
00316              15  D4-CDU     PIC XX.
00317 *                                             10-11  S4-CDU
00318              15  D4-REPCST  PIC S9(9)    COMP-3.
00319 *                                             12-16  S4-REPRODUCTV
00320 *                                                    COST
00321              15  D4-TOTVAL  PIC S9(9)    COMP-3.
00322 *                                             17-21  S4-TOTAL-VALU
00323              15  FILLER     PIC X.
00324 *                                             22     FILLER
00325              15  D4-OCCFAC  PIC S99V9    COMP-3.
00326 *                                             23-24  S4-OCCUPANCY
00327 *                                                    FACTOR
00328              15  D4-AGE     PIC S999     COMP-3.
00329                  88  D4-0-AGE         VALUE +0.
00330                  88  D4-OVER-AGE      VALUE +081 THRU +999.
00331 *                                             25-26  S4-AGE
00332              15  D4-COND    PIC S99V9    COMP-3.
00333 *                                             27-28  S4-CONDITION
00334              15  D4-PCASSD  PIC S99V9(5) COMP-3.
00335 *                                             29-32  S4-% ASSESSED
00336              15  D4-BUFF    PIC S999     COMP-3.
00337 *                                             33-34  S4-BUFF-NO
00338              15  FILLER     PIC XX.
00339 *                                             35-36  FILLER
00340              15  D4-VAL     PIC S9(9)    COMP-3.
00341 *                                             37-41  S4-VALUATION
00342              15  D4-KEYPCL  PIC S9(15)   COMP-3.
00343 *                                             42-49  S4-KEY PARCEL
00344              15  D4-SC      PIC X.
00345 *                                             50     S4-SPLIT CODE
00346              15  FILLER     PIC X(3).
00347 *                                             51-53  FILLER
00348 *----------------------------------------------------------------*
00349 *                                              1-53  TYPE5-IMPRVMT
00350          10  D-TYP5 REDEFINES D-TYP1.
00351              15  D5-MC      PIC S999     COMP-3.
00352 *                                              1-2   S5-MULTI-CODE
00353              15  D5-TYP     PIC X.
00354                  88  D5R-TYPE5        VALUE '5'.
00355 *                                              3     S5-TYPE5
00356              15  D5-CD      PIC X.
00357 *                                              4     S5-CODE5
00358              15  FILLER     PIC X(3).
00359 *                                              5-7   FILLER
00360              15  D5-CLS     PIC S999     COMP-3.
00361 *                                              8-9   S5-CLASS
00362 *                                                    (MAJOR-MINOR)
00363              15  D5-CDU     PIC XX.
00364 *                                             10-11  S5-CDU
00365              15  D5-REPCST  PIC S9(9)    COMP-3.
00366 *                                             12-16  S5-REPRODUCTV
00367 *                                                    COST
00368              15  FILLER     PIC X(6).
00369 *                                             17-22  FILLER
00370              15  D5-OCCFAC  PIC S99V9    COMP-3.
00371 *                                             23-24  S5-OCCUPANCY
00372 *                                                    FACTOR
00373              15  D5-AGE     PIC S999     COMP-3.
00374                  88  D5-0-AGE         VALUE +0.
00375                  88  D5-OVER-80       VALUE +081 THRU +999.
00376 *                                             25-26  S5-AGE
00377              15  D5-COND    PIC S99V9    COMP-3.
00378 *                                             27-28  S5-CONDITION
00379              15  D5-PCASSD  PIC S99V9(5) COMP-3.
00380 *                                             29-32  S5-% ASSESSED
00381              15  D5-BUFF    PIC S999     COMP-3.
00382 *                                             33-34  S5-BUFF-NO
00383              15  FILLER     PIC XX.
00384 *                                             35-36  FILLER
00385              15  D5-VAL     PIC S9(9)    COMP-3.
00386 *                                             37-41  S5-VALUATION
00387              15  D5-KEYPCL  PIC S9(15)   COMP-3.
00388 *                                             42-49  S5-KEY PARCEL
00389              15  D5-SC      PIC X.
00390 *                                             50     S5-SPLIT CODE
00391              15  FILLER     PIC X(3).
00392 *                                             51-53  FILLER
00393 *----------------------------------------------------------------*
00394 * --  Q U E S T I O N N A I R E  S E G M E N T  -- IS PRESENT,   *
00395 *      IF THE PRECEDING DETAIL SEGMENT IS - TYPE = '2' THRU '5'  *
00396 *      AND CLASS = 202-212,234,278,295,405-412,434,478,495       *
00397          10  M-QST  REDEFINES D-TYP1.
00398 *                                              1-53  QUESTIONNAIRE
00399            12  RFILL1.
00400                15  R1       PIC 9.
00401                  88  R1R-RESTYP1-5    VALUE 1 THRU 5.
00402                  88  R1R-RESTYP1-3    VALUE 1 2 3 5.
00403                  88  R1R-RESTYP1OR5   VALUE 1 5.
00404                  88  R1R-RESTYP2      VALUE 2.
00405                  88  R1R-RESTYP2-3    VALUE 2 3.
00406                  88  R1R-RESTYP4      VALUE 4.
00407 *                                              1     TYPE-RESIDENC
00408                15  R2       PIC 9.
00409 *                                              2     USE
00410                15  R2A      PIC 9.
00411 *                                              3     NO. OF APTS.
00412 *                                   ---------- 4-5   EXTERIOR -
00413 *                                                     CONSTRUCTION
00414                15  R3       PIC 9.
00415 *                                              4     WALLS
00416                15  R4       PIC 9.
00417 *                                              5     ROOF
00418 *                                   ---------- 6-27  ROOMS,HEATING
00419 *                                                     AND PLUMBING
00420            12  RFILL2.
00421                15  R5A1     PIC S999     COMP-3.
00422 *                                              6-7   NO. OF ROOMS
00423            12  RFILL3.
00424                15  R5A2     PIC 99.
00425 *                                              8-9   NO. OF BEDRMS
00426                15  R5B1     PIC 9.
00427 *                                             10     BASEMENT-TYPE
00428                15  R5B2     PIC 9.
00429 *                                             11     BSMT-FINISH
00430                15  R5C1     PIC 9.
00431 *                                             12     CENTRAL-HEAT
00432                15  R5C2F    PIC 9.
00433 *                                             13     FLOOR-FURNACE
00434                15  R5C2U    PIC 9.
00435 *                                             14     UNIT-HEATER
00436                15  R5C2S    PIC 9.
00437 *                                             15     STOVE
00438                15  R5C2O    PIC 9.
00439 *                                             16     SOLAR
00440                15  R5C3     PIC 9.
00441 *                                             17     CENTRAL-AIR
00442                15  R5C4     PIC 9.
00443 *                                             18     NO. FIREPLACE
00444                15  R5C41    PIC 9.
00445 *                                             19     1ST FLR FIREP
00446                15  R5C42    PIC 9.
00447 *                                             20     2ND FLR FIREP
00448                15  R5C43    PIC 9.
00449 *                                             21     3RD FLR FIREP
00450                15  R5C4B    PIC 9.
00451 *                                             22     BSMT FIREPLCE
00452                15  R5D1     PIC 9.
00453 *                                             23     ATTIC-TYPE
00454                15  R5D2     PIC 9.
00455 *                                             24     ATTIC-FINISH
00456                15  R5E1     PIC 99.
00457 *                                             25-26  NO. FULL-BATH
00458                15  R5E2     PIC 9.
00459 *                                             27     NO. HALF-BATH
00460                15  R6A      PIC 9.
00461 *                                             28     TYPE OF DESGN
00462 *                                                    ARCHITECT OR
00463 *                                                    STOCK PLAN
00464                15  R6B      PIC 9.
00465 *                                             29     TYPE OF DESGN
00466 *                                                    TYPICAL OR
00467 *                                                    ATYPICAL
00468                15  R7A      PIC 9.
00469 *                                             30     CONST QUALITY
00470 *                                                    1 = DELUXE
00471 *                                                    2 = AVERAGE
00472 *                                                    3 = POOR
00473                15  R7B      PIC 9.
00474 *                                             31     CONST QUALITY
00475 *                                                    1 = RENOVATED
00476                15  R8       PIC 9.
00477 *                                             32     SITE DESRBLTY
00478 *                                   ----------33-36  GARAGE - INFO
00479                15  R9A      PIC 9.
00480 *                                             33     GARAGE-SIZE
00481                15  R9B      PIC 9.
00482 *                                             34     GARAGE-CONST.
00483                15  R9C      PIC 9.
00484 *                                             35     GARAGE-ATTACH
00485                15  R9D      PIC 9.
00486 *                                             36     GRAGE IN AREA
00487 *                                   ----------37-40  2ND GRGE INFO
00488                15  R10A     PIC 9.
00489 *                                             37     GARAGE-SIZE
00490                15  R10B     PIC 9.
00491 *                                             38     GARAGE-CONST.
00492                15  R10C     PIC 9.
00493 *                                             39     GARAGE-ATTACH
00494                15  R10D     PIC 9.
00495 *                                             40     GRAGE IN AREA
00496                15  R11      PIC 9.
00497 *                                             41     PORCH
00498            12  RFILL4.
00499                15  R12      PIC S9(7)    COMP-3.
00500 *                                   ----------42-45  OTHER -
00501 *                                                     IMPROVEMENTS
00502                15  R13      PIC S9(7)    COMP-3.
00503 *                                             46-49  BLDG-SQ-FT
00504            12  RFILL5.
00505                15  R16      PIC 9.
00506 *                                             50     ST OF REPAIR
00507                15  R17      PIC 9.
00508 *                                             51     SPECIAL COND
00509            12  RFILL6.
00510                15  FILLER   PIC XX.
00511 *                                             52-53  FILLER
00512 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
