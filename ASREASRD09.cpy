00001      05  AS-BASE.
00002 *                                              1-122 FIXED SEGMENT
00003 ******************************************************************
00004 **  RECORD LAYOUT FOR A.AS.HMA.EXPR&YR&TOWN GENERATED DATASET
00005 ******************************************************************
00006          10  AS-STAT1       PIC X.
00007              88  AS-NON-ASSESSED      VALUE '0'.
00008              88  AS-ASSESSED          VALUE '1'.
00009 *                                              1     STATUS-1
00010          10  AS-VOLPROP.
00011 *                                              2-11  RECORD KEY
00012              15  AS-VOL     PIC S9(3)    COMP-3.
00013                  88  AS-RE-VOL        VALUE +001 THRU +601.
00014                  88  AS-RR-VOL        VALUE +605.
00015 *                                              2-3     VOLUME
00016              15  AS-PROP    PIC S9(15)   COMP-3.
00017 *                                              4-11    PROPERTY-NO
00018          10  AS-PERMITIND   PIC X.
00019 *                                             12-12  PERMIT IND
00020          10  AS-TXTYP       PIC X.
00021 *                                             13-13  TAX TYPE
00022          10  AS-TXCD        PIC S9(5)    COMP-3.
00023 *                                             14-16  TAX CODE
00024          10  AS-STAT2       PIC X.
00025              88  AS-TAXABLE-PARCEL    VALUE '0'.
00026              88  AS-EXEMPT            VALUE '1'.
00027              88  AS-RAILROAD          VALUE '2'.
00028              88  AS-HOMESTEAD-NON-COOP VALUE '3'.
00029              88  AS-VETERAN           VALUE '4'.
00030              88  AS-HOMESTEAD-COOP    VALUE '5'.
00031 *                                             17     STATUS-2
00032          10  AS-CLS         PIC S9(3)    COMP-3.
00033 *                                             18-19  CLASS (9-99)
00034 *                                                    (MAJOR-MINOR)
00035          10  AS-NBHD        PIC S9(3)    COMP-3.
00036 *                                             20-21  NEIGHBORHD-CD
00037          10  AS-STRT        PIC S9(5)    COMP-3.
00038 *                                             22-24  STREET CODE
00039          10  AS-HSENO       PIC S9(5)    COMP-3.
00040 *                                             25-27  HOUSE-NO
00041          10  AS-LNDDIM      PIC S9(7)    COMP-3.
00042 *                                             28-31  LND-DIMENSION
00043          10  AS-LNDCD       PIC X.
00044 *                                             32     LAND CODE
00045          10  AS-NPHE-STATUS REDEFINES AS-LNDCD  PIC X.
00046 *                                             32     NPHE STATUS
00047 *                                                       FIELD
00048          10  AS-LNDSQFT     PIC S9(7)    COMP-3.
00049 *                                             33-36  LAND-SQ-FT
00050          10  AS-IRREG       PIC X.
00051 *                                             37     IRREGULAR
00052          10  AS-STAT3       PIC X.
00053 *                                             38     COMPLAINT ST
00054          10  AS-CMPLNTNO    PIC S9(7)    COMP-3.
00055 *                                             39-42  B.A.COMPLAINT
00056          10  AS-BA-ACTION.
00057 *                                             43-44  B.A. ACTION
00058              15  AS-BA-YR   PIC X.
00059 *                                             43       BA-YEAR
00060              15  AS-BA-REV  PIC X.
00061 *                                             44       BA-REVISION
00062          10  AS-ASMT-ACTION OCCURS 4 TIMES.
00063 *                                             45-56  ASMT-ACTIONS
00064 *                                                    (YR-CHNG-REV)
00065              15  AS-ASMT-YR PIC X.
00066 *                                             45       AA-YEAR1
00067 *                                             48       AA-YEAR2
00068 *                                             51       AA-YEAR3
00069 *                                             54       AA-YEAR4
00070              15  AS-ASMT-CHG PIC X.
00071 *                                             46       AA-CHANGE1
00072 *                                             49       AA-CHANGE2
00073 *                                             52       AA-CHANGE3
00074 *                                             55       AA-CHANGE4
00075              15  AS-ASMT-REV PIC X.
00076 *                                             47       AA-REVSION1
00077 *                                             50       AA-REVSION2
00078 *                                             53       AA-REVSION3
00079 *                                             56       AA-REVSION4
00080          10  AS-VALUE-1 OCCURS 12 TIMES
00081                             PIC S9(9)    COMP-3.
00082 *                                             57-116 VALUE FIELDS
00083 *                                             57-61    PRIOR LAND
00084 *                                             62-66    PRIOR IMPRV
00085 *                                             67-71    PRIOR TOTAL
00086 *                                             72-76    CURRNT-LAND
00087 *                                             77-81    CURR-IMPRV
00088 *                                             82-86    CURR-TOTAL
00089 *                                             87-91    PROPOSE-LND
00090 *                                             92-96    PROP-IMPRV
00091 *                                             97-101   PROP-TOTAL
00092 *                                            102-106   PRIOR-REG/
00093 *                                                      COMPL DATE
00094 *                                            107-111   CURRENT-REG
00095 *                                            112-116   PRIOR-PROP
00096          10  AS-LT-IND      PIC X.
00097 *                                            117-117 LONGTERM INDI
00098          10  FILLER          PIC X.
00099 *                                            118-118 FILLER
00100          10  AS-REC-CTRS-1.
00101 *                                            119-122 RECORD CNTRS
00102              15  AS-SLS-CTR-1
00103                             PIC S9.
00104                  88  AS-NO-SALES      VALUE +0.
00105                  88  AS-SALES-PRESENT VALUE +1.
00106 *                                            119       SALES-CNTR
00107              15  AS-DTL-QST-CTR-1
00108                             PIC S9(3).
00109                  88  AS-NO-DETAIL     VALUE +0.
00110                  88  AS-DETAIL-PRESENT VALUE +1 THRU +350.
00111                  88  AS-MAXIMUM-DETAIL VALUE +350.
00112 *                                            120-122   DETAIL-CNTR
00113 *----------------------------------------------------------------*
00114 *   S A L E S  S E G M E N T, IF PRESENT, CONTAINS 34 BYTES      *
00115 *    DESCRIPTIONS RELATIVE TO 1ST BYTE IN EACH SEGMENT           *
00116 *                                                                *
00117      05  AS-SALES-1 OCCURS 0 TO 1 TIMES DEPENDING ON AS-SLS-CTR-1.
00118 *                                              1-34  SALES SEGMENT
00119          10  AS-DEEDTYP-1   PIC XX.
00120 *                                              1-2   DEED-TYPE
00121          10  AS-DEEDNO-1    PIC S9(9)    COMP-3.
00122 *                                              3-7   DEED-NUMBER
00123          10  AS-SLS-DAT1-1  PIC S9(7)    COMP-3.
00124 *                                              8-11  SALE-DATE1
00125 *                                                      (YYMMDD)
00126          10  AS-SLS-AMT1-1  PIC S9(9)    COMP-3.
00127 *                                             12-16  SALE-AMOUNT1
00128          10  AS-SLS-DAT2-1  PIC S9(7)    COMP-3.
00129 *                                             17-20  SALE-DATE2
00130 *                                                      (YYMMDD)
00131          10  AS-SLS-AMT2-1  PIC S9(9)    COMP-3.
00132 *                                             21-25  SALE-AMOUNT2
00133          10  AS-SLS-DAT3-1  PIC S9(7)    COMP-3.
00134 *                                             26-29  SALE-DATE3
00135 *                                                      (YYMMDD)
00136          10  AS-SLS-AMT3-1  PIC S9(9)    COMP-3.
00137 *                                             30-34  SALE-AMOUNT3
00138 *----------------------------------------------------------------*
00139 * D E T A I L  S E G M E N T S, IF PRESENT,EACH CONTAINS 53 BYTES
00140 *    DESCRIPTIONS RELATIVE TO 1ST BYTE IN EACH SEGMENT           *
00141 *                                                                *
00142      05  AS-DTL-QST-1
00143              OCCURS 0 TO 350 TIMES DEPENDING ON AS-DTL-QST-CTR-1.
00144          10  AS1-TYP1.
00145 *                                              1-53  TYPE1-LAND
00146              15  AS1-MC     PIC S999     COMP-3.
00147 *                                              1-2   S1-MULTI-CODE
00148              15  AS1-TYP    PIC X.
00149                  88  AS1-TYPE1        VALUE '1'.
00150 *                                              3     S1-TYPE1
00151              15  AS1-CD     PIC X.
00152 *                                              4     S1-CODE 0/1
00153 *                                                    (0=VACANT)
00154 *                                                    (1=IMPROVED)
00155              15  AS1-DEC    PIC X.
00156 *                                              5     S1-DECIMAL
00157              15  AS1-UM     PIC XX.
00158 *                                              6-7   S1-UNIT-MEAS
00159              15  AS1-CLS    PIC S999     COMP-3.
00160 *                                              8-9   S1-CLASS
00161 *                                                    (MAJOR-MINOR)
00162              15  AS1-EXRR   PIC X.
00163 *                                             10     S1-EX-RR
00164              15  AS1-FF     PIC S9(7)    COMP-3.
00165 *                                             11-14  S1-FRONT-FT
00166              15  AS1-DPTH   PIC S9(5)    COMP-3.
00167 *                                             15-17  S1-DEPTH
00168              15  AS1-UPR    PIC S9(5)V99 COMP-3.
00169 *                                             18-21  S1-UNIT-PRICE
00170              15  AS1-DFCTR  PIC S99V999  COMP-3.
00171 *                                             22-24  S1-DPTH-FACTR
00172              15  AS1-CFCTR  PIC S9V9(4)  COMP-3.
00173 *                                             25-27  S1-CORNR-FCTR
00174              15  AS1-ECFCTR PIC SV9(5)   COMP-3.
00175 *                                             28-30  S1-EXTRA
00176 *                                                    CORNER FACTOR
00177              15  AS1-PCASSD PIC S99V9(5) COMP-3.
00178 *                                             31-34  S1-% ASSESSED
00179              15  AS1-EI     PIC S99V9    COMP-3.
00180                  88  AS1-ECON1        VALUE +22.0.
00181                  88  AS1-ECON2        VALUE +16.0.
00182                  88  AS1-ECON3        VALUE +33.0.
00183                  88  AS1-ECON4        VALUE +30.0.
00184                  88  AS1-ECON5        VALUE +37.0.
00185                  88  AS1-ECON6        VALUE +38.5.
00186                  88  AS1-ECON-IND     VALUE +16.0 +22.0 +30.0
00187                                             +33.0 +37.0 +38.5.
00188 *                                             35-36  S1-ECON-IND
00189              15  AS1-VAL    PIC S9(9)    COMP-3.
00190 *                                             37-41  S1-VALUATION
00191              15  FILLER     PIC X(11).
00192 *                                             42-52  FILLER
00193              15  AS1-UI     PIC X.
00194 *                                             53     S1-UNIT-IND
00195 *----------------------------------------------------------------*
00196          10  AS2-TYP2 REDEFINES AS1-TYP1.
00197 *                                              1-53  TYPE2-IMPRVMT
00198              15  AS2-MC     PIC S999     COMP-3.
00199 *                                              1-2   S2-MULTI-CODE
00200              15  AS2-TYP    PIC X.
00201                  88  AS2-TYPE2        VALUE '2'.
00202                  88  AS2-TYPE2-5      VALUE '2' THRU '5'.
00203 *                                              3     S2-TYPE2
00204              15  AS2-CD     PIC X.
00205 *                                              4     S2-CODE 2/3
00206 *                                                    2=MAJOR-IMPRV
00207 *                                                    3=MINOR-IMPRV
00208              15  AS2-DEC    PIC X.
00209 *                                              5     S2-DECIMAL
00210              15  AS2-UM     PIC XX.
00211 *                                              6-7   S2-UNIT-MEAS
00212              15  AS2-CLS    PIC S999     COMP-3.
00213                  88  AS2-QUES-CLASS   VALUE +202 THRU +212
00214                                             +234 +278 +295
00215                                             +402 THRU +412
00216                                             +434 +478 +495.
00217                  88  AS2-QCLS1-3      VALUE +210 +211 +212 +295
00218                                             +410 +411 +412 +495.
00219                  88  AS2-QCLS1OR5     VALUE +202 +203 +204
00220                                             +402 +403 +404.
00221                  88  AS2-QCLS2        VALUE +205 THRU +208 +278
00222                                             +405 THRU +408 +478.
00223                  88  AS2-QCLS2-3      VALUE +209 +409.
00224                  88  AS2-QCLS4        VALUE +234 +434.
00225                  88  AS2-QCLS6        VALUE +593.
00226                  88  AS2-QCLS7        VALUE +597.
00227 *                                              8-9   S2-CLASS
00228 *                                                    (MAJOR-MINOR)
00229              15  AS2-CDU    PIC XX.
00230 *                                             10-11  S2-CDU
00231              15  AS2-AREA   PIC S9(7)    COMP-3.
00232 *                                             12-15  S2-AREA
00233              15  AS2-UPR    PIC S9(5)V99 COMP-3.
00234 *                                             16-19  S2-UNIT-PRICE
00235              15  AS2-PRDCT  PIC S9(9)    COMP-3.
00236 *                                             20-24  S2-PRODUCT
00237              15  AS2-AGE    PIC S999     COMP-3.
00238                  88  AS2-0-AGE        VALUE +0.
00239                  88  AS2-OVER-80      VALUE +081 THRU +999.
00240 *                                             25-26  S2-AGE
00241              15  AS2-COND   PIC S99V9    COMP-3.
00242 *                                             27-28  S2-CONDITION
00243              15  AS2-PCASSD PIC S99V9(5) COMP-3.
00244 *                                             29-32  S2-% ASSESSED
00245              15  AS2-BUFF   PIC S999     COMP-3.
00246 *                                             33-34  S2-BUFF-NO
00247              15  FILLER     PIC XX.
00248 *                                             35-36  FILLER
00249              15  AS2-VAL    PIC S9(9)    COMP-3.
00250 *                                             37-41  S2-VALUATION
00251              15  AS2-KEYPCL PIC S9(15)   COMP-3.
00252 *                                             42-49  S2-KEY PARCEL
00253              15  AS2-SC     PIC X.
00254 *                                             50     S2-SPLIT CODE
00255              15  FILLER     PIC X(3).
00256 *                                             51-53  FILLER
00257 *----------------------------------------------------------------*
00258          10  AS3-TYP3 REDEFINES AS1-TYP1.
00259 *                                              1-53  TYPE3-IMPRVMT
00260              15  AS3-MC     PIC S999     COMP-3.
00261 *                                              1-2   S3-MULTI-CODE
00262              15  AS3-TYP    PIC X.
00263                  88  AS3-TYPE3        VALUE '3'.
00264 *                                              3     S3-TYPE3
00265              15  AS3-CD     PIC X.
00266 *                                              4     S3-CODE 2/3
00267 *                                                    2=MAJOR-IMPRV
00268 *                                                    3=MINOR-IMPRV
00269              15  FILLER     PIC X(3).
00270 *                                              5-7   FILLER
00271              15  AS3-CLS    PIC S999     COMP-3.
00272 *                                              8-9   S3-CLASS
00273 *                                                    (MAJOR-MINOR)
00274              15  AS3-CDU    PIC XX.
00275 *                                             10-11  S3-CDU
00276              15  AS3-REPCST PIC S9(9)    COMP-3.
00277 *                                             12-16  S3-REPRODUCTV
00278 *                                                    COST
00279              15  FILLER     PIC X(6).
00280 *                                             17-22  FILLER
00281              15  AS3-YR     PIC S999     COMP-3.
00282 *                                             23-24  S3-YEAR
00283              15  AS3-AGE    PIC S999     COMP-3.
00284                  88  AS3-0-AGE        VALUE +0.
00285                  88  AS3-OVER-80      VALUE +081 THRU +999.
00286 *                                             25-26  S3-AGE
00287              15  AS3-COND   PIC S99V9    COMP-3.
00288 *                                             27-28  S3-CONDITION
00289              15  AS3-PCASSD PIC S99V9(5) COMP-3.
00290 *                                             29-32  S3-% ASSESSED
00291              15  AS3-BUFF   PIC S999     COMP-3.
00292 *                                             33-34  S3-BUFF-NO
00293              15  FILLER     PIC XX.
00294 *                                             35-36  FILLER
00295              15  AS3-VAL    PIC S9(9)    COMP-3.
00296 *                                             37-41  S3-VALUATION
00297              15  AS3-KEYPCL PIC S9(15)   COMP-3.
00298 *                                             42-49  S3-KEY PARCEL
00299              15  AS3-SC     PIC X.
00300 *                                             50     S3-SPLIT CODE
00301              15  FILLER     PIC X(3).
00302 *                                             51-53  FILLER
00303 *----------------------------------------------------------------*
00304          10  AS4-TYP4 REDEFINES AS1-TYP1.
00305 *                                              1-53  TYPE4-IMPRVMT
00306              15  AS4-MC     PIC S999     COMP-3.
00307 *                                              1-2   S4-MULTI-CODE
00308              15  AS4-TYP    PIC X.
00309                  88  AS4-TYPE4        VALUE '4'.
00310 *                                              3     S4-TYPE4
00311              15  AS4-CD     PIC X.
00312 *                                              4     S4-CODE4
00313              15  FILLER     PIC X(3).
00314 *                                              5-7   FILLER
00315              15  AS4-CLS    PIC S999     COMP-3.
00316 *                                              8-9   S4-CLASS
00317 *                                                    (MAJOR-MINOR)
00318              15  AS4-CDU    PIC XX.
00319 *                                             10-11  S4-CDU
00320              15  AS4-REPCST PIC S9(9)    COMP-3.
00321 *                                             12-16  S4-REPRODUCTV
00322 *                                                    COST
00323              15  AS4-TOTVAL PIC S9(9)    COMP-3.
00324 *                                             17-21  S4-TOTAL-VALU
00325              15  FILLER     PIC X.
00326 *                                             22     FILLER
00327              15  AS4-OCCFAC PIC S99V9    COMP-3.
00328 *                                             23-24  S4-OCCUPANCY
00329 *                                                    FACTOR
00330              15  AS4-AGE    PIC S999     COMP-3.
00331                  88  AS4-0-AGE        VALUE +0.
00332                  88  AS4-OVER-AGE     VALUE +081 THRU +999.
00333 *                                             25-26  S4-AGE
00334              15  AS4-COND   PIC S99V9    COMP-3.
00335 *                                             27-28  S4-CONDITION
00336              15  AS4-PCASSD PIC S99V9(5) COMP-3.
00337 *                                             29-32  S4-% ASSESSED
00338              15  AS4-BUFF   PIC S999     COMP-3.
00339 *                                             33-34  S4-BUFF-NO
00340              15  FILLER     PIC XX.
00341 *                                             35-36  FILLER
00342              15  AS4-VAL    PIC S9(9)    COMP-3.
00343 *                                             37-41  S4-VALUATION
00344              15  AS4-KEYPCL PIC S9(15)   COMP-3.
00345 *                                             42-49  S4-KEY PARCEL
00346              15  AS4-SC     PIC X.
00347 *                                             50     S4-SPLIT CODE
00348              15  FILLER     PIC X(3).
00349 *                                             51-53  FILLER
00350 *----------------------------------------------------------------*
00351 *                                              1-53  TYPE5-IMPRVMT
00352          10  AS5-TYP5 REDEFINES AS1-TYP1.
00353              15  AS5-MC     PIC S999     COMP-3.
00354 *                                              1-2   S5-MULTI-CODE
00355              15  AS5-TYP    PIC X.
00356                  88  AS5-TYPE5        VALUE '5'.
00357 *                                              3     S5-TYPE5
00358              15  AS5-CD     PIC X.
00359 *                                              4     S5-CODE5
00360              15  FILLER     PIC X(3).
00361 *                                              5-7   FILLER
00362              15  AS5-CLS    PIC S999     COMP-3.
00363 *                                              8-9   S5-CLASS
00364 *                                                    (MAJOR-MINOR)
00365              15  AS5-CDU    PIC XX.
00366 *                                             10-11  S5-CDU
00367              15  AS5-REPCST PIC S9(9)    COMP-3.
00368 *                                             12-16  S5-REPRODUCTV
00369 *                                                    COST
00370              15  FILLER     PIC X(6).
00371 *                                             17-22  FILLER
00372              15  AS5-OCCFAC PIC S99V9    COMP-3.
00373 *                                             23-24  S5-OCCUPANCY
00374 *                                                    FACTOR
00375              15  AS5-AGE    PIC S999     COMP-3.
00376                  88  AS5-0-AGE        VALUE +0.
00377                  88  AS5-OVER-80      VALUE +081 THRU +999.
00378 *                                             25-26  S5-AGE
00379              15  AS5-COND   PIC S99V9    COMP-3.
00380 *                                             27-28  S5-CONDITION
00381              15  AS5-PCASSD PIC S99V9(5) COMP-3.
00382 *                                             29-32  S5-% ASSESSED
00383              15  AS5-BUFF   PIC S999     COMP-3.
00384 *                                             33-34  S5-BUFF-NO
00385              15  FILLER     PIC XX.
00386 *                                             35-36  FILLER
00387              15  AS5-VAL    PIC S9(9)    COMP-3.
00388 *                                             37-41  S5-VALUATION
00389              15  AS5-KEYPCL PIC S9(15)   COMP-3.
00390 *                                             42-49  S5-KEY PARCEL
00391              15  AS5-SC     PIC X.
00392 *                                             50     S5-SPLIT CODE
00393              15  FILLER     PIC X(3).
00394 *                                             51-53  FILLER
00395 *----------------------------------------------------------------*
00396 * --  Q U E S T I O N N A I R E  S E G M E N T  -- IS PRESENT,   *
00397 *      IF THE PRECEDING DETAIL SEGMENT IS - TYPE = '2' THRU '5'  *
00398 *      AND CLASS = 202-212,234,278,295,405-412,434,478,495       *
00399          10  AS-QST REDEFINES AS1-TYP1.
00400 *                                              1-53  QUESTIONNAIRE
00401            12  AS-QMFILL1.
00402                15  AS-QM1   PIC 9.
00403                  88  AS-QM1R-RESTYP1-5 VALUE 1 THRU 5.
00404                  88  AS-QM1R-RESTYP1-3 VALUE 1 2 3 5.
00405                  88  AS-QM1R-RESTYP1OR5 VALUE 1 5.
00406                  88  AS-QM1R-RESTYP2  VALUE 2.
00407                  88  AS-QM1R-RESTYP2-3 VALUE 2 3.
00408                  88  AS-QM1R-RESTYP4  VALUE 4.
00409 *                                              1     TYPE-RESIDENC
00410                15  AS-QM2   PIC 9.
00411 *                                              2     USE
00412                15  AS-QM2A  PIC 9.
00413 *                                              3     NO. OF APTS.
00414 *                                   ---------- 4-5   EXTERIOR -
00415 *                                                     CONSTRUCTION
00416                15  AS-QM3   PIC 9.
00417 *                                              4     WALLS
00418                15  AS-QM4   PIC 9.
00419 *                                              5     ROOF
00420 *                                   ---------- 6-27  ROOMS,HEATING
00421 *                                                     AND PLUMBING
00422            12  AS-QMFILL2.
00423                15  AS-QM5A1 PIC S999     COMP-3.
00424 *                                              6-7   NO. OF ROOMS
00425            12  AS-QMFILL3.
00426                15  AS-QM5A2 PIC 99.
00427 *                                              8-9   NO. OF BEDRMS
00428                15  AS-QM5B1 PIC 9.
00429 *                                             10     BASEMENT-TYPE
00430                15  AS-QM5B2 PIC 9.
00431 *                                             11     BSMT-FINISH
00432                15  AS-QM5C1 PIC 9.
00433 *                                             12     CENTRAL-HEAT
00434                15  AS-QM5C2F PIC 9.
00435 *                                             13     FLOOR-FURNACE
00436                15  AS-QM5C2U PIC 9.
00437 *                                             14     UNIT-HEATER
00438                15  AS-QM5C2S PIC 9.
00439 *                                             15     STOVE
00440                15  AS-QM5C2O PIC 9.
00441 *                                             16     SOLAR
00442                15  AS-QM5C3 PIC 9.
00443 *                                             17     CENTRAL-AIR
00444                15  AS-QM5C4 PIC 9.
00445 *                                             18     NO. FIREPLACE
00446                15  AS-QM5C41 PIC 9.
00447 *                                             19     1ST FLR FIREP
00448                15  AS-QM5C42 PIC 9.
00449 *                                             20     2ND FLR FIREP
00450                15  AS-QM5C43 PIC 9.
00451 *                                             21     3RD FLR FIREP
00452                15  AS-QM5C4B PIC 9.
00453 *                                             22     BSMT FIREPLCE
00454                15  AS-QM5D1 PIC 9.
00455 *                                             23     ATTIC-TYPE
00456                15  AS-QM5D2 PIC 9.
00457 *                                             24     ATTIC-FINISH
00458                15  AS-QM5E1 PIC 99.
00459 *                                             25-26  NO. FULL-BATH
00460                15  AS-QM5E2 PIC 9.
00461 *                                             27     NO. HALF-BATH
00462                15  AS-QM6A  PIC 9.
00463 *                                             28     TYPE OF DESGN
00464 *                                                    ARCHITECT OR
00465 *                                                    STOCK PLAN
00466                15  AS-QM6B  PIC 9.
00467 *                                             29     TYPE OF DESGN
00468 *                                                    TYPICAL OR
00469 *                                                    ATYPICAL
00470                15  AS-QM7A  PIC 9.
00471 *                                             30     CONST QUALITY
00472 *                                                    1 = DELUXE
00473 *                                                    2 = AVERAGE
00474 *                                                    3 = POOR
00475                15  AS-QM7B  PIC 9.
00476 *                                             31     CONST QUALITY
00477 *                                                    1 = RENOVATED
00478                15  AS-QM8   PIC 9.
00479 *                                             32     SITE DESRBLTY
00480 *                                   ----------33-36  GARAGE - INFO
00481                15  AS-QM9A  PIC 9.
00482 *                                             33     GARAGE-SIZE
00483                15  AS-QM9B  PIC 9.
00484 *                                             34     GARAGE-CONST.
00485                15  AS-QM9C  PIC 9.
00486 *                                             35     GARAGE-ATTACH
00487                15  AS-QM9D  PIC 9.
00488 *                                             36     GRAGE IN AREA
00489 *                                   ----------37-40  2ND GRGE INFO
00490                15  AS-QM10A PIC 9.
00491 *                                             37     GARAGE-SIZE
00492                15  AS-QM10B PIC 9.
00493 *                                             38     GARAGE-CONST.
00494                15  AS-QM10C PIC 9.
00495 *                                             39     GARAGE-ATTACH
00496                15  AS-QM10D PIC 9.
00497 *                                             40     GRAGE IN AREA
00498                15  AS-QM11  PIC 9.
00499 *                                             41     PORCH
00500            12  AS-QMFILL4.
00501                15  AS-QM12  PIC S9(7)    COMP-3.
00502 *                                   ----------42-45  OTHER -
00503 *                                                     IMPROVEMENTS
00504                15  AS-QM13  PIC S9(7)    COMP-3.
00505 *                                             46-49  BLDG-SQ-FT
00506            12  AS-QMFILL5.
00507                15  AS-QM16  PIC 9.
00508 *                                             50     ST OF REPAIR
00509                15  AS-QM17  PIC 9.
00510 *                                             51     SPECIAL COND
00511            12  AS-QMFILL6.
00512                15  FILLER   PIC XX.
00513 *                                             52-53  FILLER
00514 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
