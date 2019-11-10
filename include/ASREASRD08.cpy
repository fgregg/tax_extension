00001      05  AS-BASE.
00002 *                                              1-122 FIXED SEGMENT
00003          10  AS-STAT1       PIC X.
00004              88  AS-NON-ASSESSED      VALUE '0'.
00005              88  AS-ASSESSED          VALUE '1'.
00006 *                                              1     STATUS-1
00007          10  AS-VOLPROP.
00008 *                                              2-11  RECORD KEY
00009              15  AS-VOL     PIC S9(3)    COMP-3.
00010                  88  AS-RE-VOL        VALUE +001 THRU +601.
00011                  88  AS-RR-VOL        VALUE +605.
00012 *                                              2-3     VOLUME
00013              15  AS-PROP    PIC S9(15)   COMP-3.
00014 *                                              4-11    PROPERTY-NO
00015          10  AS-TXTYP       PIC X.
00016 *                                             12-12  TAX TYPE
00017          10  AS-PERMITIND   PIC X.
00018 *                                             13-13  PERMIT IND
00019          10  AS-TXCD        PIC S9(5)    COMP-3.
00020 *                                             14-16  TAX CODE
00021          10  AS-STAT2       PIC X.
00022              88  AS-TAXABLE-PARCEL    VALUE '0'.
00023              88  AS-EXEMPT            VALUE '1'.
00024              88  AS-RAILROAD          VALUE '2'.
00025              88  AS-HOMESTEAD-NON-COOP VALUE '3'.
00026              88  AS-VETERAN           VALUE '4'.
00027              88  AS-HOMESTEAD-COOP    VALUE '5'.
00028 *                                             17     STATUS-2
00029          10  AS-CLS         PIC S9(3)    COMP-3.
00030 *                                             18-19  CLASS (9-99)
00031 *                                                    (MAJOR-MINOR)
00032          10  AS-NBHD        PIC S9(3)    COMP-3.
00033 *                                             20-21  NEIGHBORHD-CD
00034          10  AS-STRT        PIC S9(5)    COMP-3.
00035 *                                             22-24  STREET CODE
00036          10  AS-HSENO       PIC S9(5)    COMP-3.
00037 *                                             25-27  HOUSE-NO
00038          10  AS-LNDDIM      PIC S9(7)    COMP-3.
00039 *                                             28-31  LND-DIMENSION
00040          10  AS-LNDCD       PIC X.
00041 *                                             32     LAND CODE
00042          10  AS-NPHE-STATUS REDEFINES AS-LNDCD  PIC X.
00043 *                                             32     NPHE STATUS
00044 *                                                       FIELD
00045          10  AS-LNDSQFT     PIC S9(7)    COMP-3.
00046 *                                             33-36  LAND-SQ-FT
00047          10  AS-IRREG       PIC X.
00048 *                                             37     IRREGULAR
00049          10  AS-STAT3       PIC X.
00050 *                                             38     COMPLAINT ST
00051          10  AS-CMPLNTNO    PIC S9(7)    COMP-3.
00052 *                                             39-42  B.A.COMPLAINT
00053          10  AS-BA-ACTION.
00054 *                                             43-44  B.A. ACTION
00055              15  AS-BA-YR   PIC X.
00056 *                                             43       BA-YEAR
00057              15  AS-BA-REV  PIC X.
00058 *                                             44       BA-REVISION
00059          10  AS-ASMT-ACTION OCCURS 4 TIMES.
00060 *                                             45-56  ASMT-ACTIONS
00061 *                                                    (YR-CHNG-REV)
00062              15  AS-ASMT-YR PIC X.
00063 *                                             45       AA-YEAR1
00064 *                                             48       AA-YEAR2
00065 *                                             51       AA-YEAR3
00066 *                                             54       AA-YEAR4
00067              15  AS-ASMT-CHG PIC X.
00068 *                                             46       AA-CHANGE1
00069 *                                             49       AA-CHANGE2
00070 *                                             52       AA-CHANGE3
00071 *                                             55       AA-CHANGE4
00072              15  AS-ASMT-REV PIC X.
00073 *                                             47       AA-REVSION1
00074 *                                             50       AA-REVSION2
00075 *                                             53       AA-REVSION3
00076 *                                             56       AA-REVSION4
00077          10  AS-VALUE-1 OCCURS 12 TIMES
00078                             PIC S9(9)    COMP-3.
00079 *                                             57-116 VALUE FIELDS
00080 *                                             57-61    PRIOR LAND
00081 *                                             62-66    PRIOR IMPRV
00082 *                                             67-71    PRIOR TOTAL
00083 *                                             72-76    CURRNT-LAND
00084 *                                             77-81    CURR-IMPRV
00085 *                                             82-86    CURR-TOTAL
00086 *                                             87-91    PROPOSE-LND
00087 *                                             92-96    PROP-IMPRV
00088 *                                             97-101   PROP-TOTAL
00089 *                                            102-106   PRIOR-REG/
00090 *                                                      COMPL DATE
00091 *                                            107-111   CURRENT-REG
00092 *                                            112-116   PRIOR-PROP
00093          10  AS-LT-IND      PIC X.
00094 *                                            117-117 LONGTERM INDI
00095          10  FILLER          PIC X.
00096 *                                            118-118 FILLER
00097          10  AS-REC-CTRS-1.
00098 *                                            119-122 RECORD CNTRS
00099              15  AS-SLS-CTR-1
00100                             PIC S9.
00101                  88  AS-NO-SALES      VALUE +0.
00102                  88  AS-SALES-PRESENT VALUE +1.
00103 *                                            119       SALES-CNTR
00104              15  AS-DTL-QST-CTR-1
00105                             PIC S9(3).
00106                  88  AS-NO-DETAIL     VALUE +0.
00107                  88  AS-DETAIL-PRESENT VALUE +1 THRU +350.
00108                  88  AS-MAXIMUM-DETAIL VALUE +350.
00109 *                                            120-122   DETAIL-CNTR
00110 *----------------------------------------------------------------*
00111 *   S A L E S  S E G M E N T, IF PRESENT, CONTAINS 34 BYTES      *
00112 *    DESCRIPTIONS RELATIVE TO 1ST BYTE IN EACH SEGMENT           *
00113 *                                                                *
00114      05  AS-SALES-1 OCCURS 0 TO 1 TIMES DEPENDING ON AS-SLS-CTR-1.
00115 *                                              1-34  SALES SEGMENT
00116          10  AS-DEEDTYP-1   PIC XX.
00117 *                                              1-2   DEED-TYPE
00118          10  AS-DEEDNO-1    PIC S9(9)    COMP-3.
00119 *                                              3-7   DEED-NUMBER
00120          10  AS-SLS-DAT1-1  PIC S9(7)    COMP-3.
00121 *                                              8-11  SALE-DATE1
00122 *                                                      (YYMMDD)
00123          10  AS-SLS-AMT1-1  PIC S9(9)    COMP-3.
00124 *                                             12-16  SALE-AMOUNT1
00125          10  AS-SLS-DAT2-1  PIC S9(7)    COMP-3.
00126 *                                             17-20  SALE-DATE2
00127 *                                                      (YYMMDD)
00128          10  AS-SLS-AMT2-1  PIC S9(9)    COMP-3.
00129 *                                             21-25  SALE-AMOUNT2
00130          10  AS-SLS-DAT3-1  PIC S9(7)    COMP-3.
00131 *                                             26-29  SALE-DATE3
00132 *                                                      (YYMMDD)
00133          10  AS-SLS-AMT3-1  PIC S9(9)    COMP-3.
00134 *                                             30-34  SALE-AMOUNT3
00135 *----------------------------------------------------------------*
00136 * D E T A I L  S E G M E N T S, IF PRESENT,EACH CONTAINS 53 BYTES
00137 *    DESCRIPTIONS RELATIVE TO 1ST BYTE IN EACH SEGMENT           *
00138 *                                                                *
00139      05  AS-DTL-QST-1
00140              OCCURS 0 TO 350 TIMES DEPENDING ON AS-DTL-QST-CTR-1.
00141          10  AS1-TYP1.
00142 *                                              1-53  TYPE1-LAND
00143              15  AS1-MC     PIC S999     COMP-3.
00144 *                                              1-2   S1-MULTI-CODE
00145              15  AS1-TYP    PIC X.
00146                  88  AS1-TYPE1        VALUE '1'.
00147 *                                              3     S1-TYPE1
00148              15  AS1-CD     PIC X.
00149 *                                              4     S1-CODE 0/1
00150 *                                                    (0=VACANT)
00151 *                                                    (1=IMPROVED)
00152              15  AS1-DEC    PIC X.
00153 *                                              5     S1-DECIMAL
00154              15  AS1-UM     PIC XX.
00155 *                                              6-7   S1-UNIT-MEAS
00156              15  AS1-CLS    PIC S999     COMP-3.
00157 *                                              8-9   S1-CLASS
00158 *                                                    (MAJOR-MINOR)
00159              15  AS1-EXRR   PIC X.
00160 *                                             10     S1-EX-RR
00161              15  AS1-FF     PIC S9(7)    COMP-3.
00162 *                                             11-14  S1-FRONT-FT
00163              15  AS1-DPTH   PIC S9(5)    COMP-3.
00164 *                                             15-17  S1-DEPTH
00165              15  AS1-UPR    PIC S9(5)V99 COMP-3.
00166 *                                             18-21  S1-UNIT-PRICE
00167              15  AS1-DFCTR  PIC S99V999  COMP-3.
00168 *                                             22-24  S1-DPTH-FACTR
00169              15  AS1-CFCTR  PIC S9V9(4)  COMP-3.
00170 *                                             25-27  S1-CORNR-FCTR
00171              15  AS1-ECFCTR PIC SV9(5)   COMP-3.
00172 *                                             28-30  S1-EXTRA
00173 *                                                    CORNER FACTOR
00174              15  AS1-PCASSD PIC S99V9(5) COMP-3.
00175 *                                             31-34  S1-% ASSESSED
00176              15  AS1-EI     PIC S99V9    COMP-3.
00177                  88  AS1-ECON1        VALUE +22.0.
00178                  88  AS1-ECON2        VALUE +16.0.
00179                  88  AS1-ECON3        VALUE +33.0.
00180                  88  AS1-ECON4        VALUE +30.0.
00181                  88  AS1-ECON5        VALUE +37.0.
00182                  88  AS1-ECON6        VALUE +38.5.
00183                  88  AS1-ECON-IND     VALUE +16.0 +22.0 +30.0
00184                                             +33.0 +37.0 +38.5.
00185 *                                             35-36  S1-ECON-IND
00186              15  AS1-VAL    PIC S9(9)    COMP-3.
00187 *                                             37-41  S1-VALUATION
00188              15  FILLER     PIC X(11).
00189 *                                             42-52  FILLER
00190              15  AS1-UI     PIC X.
00191 *                                             53     S1-UNIT-IND
00192 *----------------------------------------------------------------*
00193          10  AS2-TYP2 REDEFINES AS1-TYP1.
00194 *                                              1-53  TYPE2-IMPRVMT
00195              15  AS2-MC     PIC S999     COMP-3.
00196 *                                              1-2   S2-MULTI-CODE
00197              15  AS2-TYP    PIC X.
00198                  88  AS2-TYPE2        VALUE '2'.
00199                  88  AS2-TYPE2-5      VALUE '2' THRU '5'.
00200 *                                              3     S2-TYPE2
00201              15  AS2-CD     PIC X.
00202 *                                              4     S2-CODE 2/3
00203 *                                                    2=MAJOR-IMPRV
00204 *                                                    3=MINOR-IMPRV
00205              15  AS2-DEC    PIC X.
00206 *                                              5     S2-DECIMAL
00207              15  AS2-UM     PIC XX.
00208 *                                              6-7   S2-UNIT-MEAS
00209              15  AS2-CLS    PIC S999     COMP-3.
00210                  88  AS2-QUES-CLASS   VALUE +202 THRU +212
00211                                             +234 +278 +295
00212                                             +402 THRU +412
00213                                             +434 +478 +495.
00214                  88  AS2-QCLS1-3      VALUE +210 +211 +212 +295
00215                                             +410 +411 +412 +495.
00216                  88  AS2-QCLS1OR5     VALUE +202 +203 +204
00217                                             +402 +403 +404.
00218                  88  AS2-QCLS2        VALUE +205 THRU +208 +278
00219                                             +405 THRU +408 +478.
00220                  88  AS2-QCLS2-3      VALUE +209 +409.
00221                  88  AS2-QCLS4        VALUE +234 +434.
00222                  88  AS2-QCLS6        VALUE +593.
00223                  88  AS2-QCLS7        VALUE +597.
00224 *                                              8-9   S2-CLASS
00225 *                                                    (MAJOR-MINOR)
00226              15  AS2-CDU    PIC XX.
00227 *                                             10-11  S2-CDU
00228              15  AS2-AREA   PIC S9(7)    COMP-3.
00229 *                                             12-15  S2-AREA
00230              15  AS2-UPR    PIC S9(5)V99 COMP-3.
00231 *                                             16-19  S2-UNIT-PRICE
00232              15  AS2-PRDCT  PIC S9(9)    COMP-3.
00233 *                                             20-24  S2-PRODUCT
00234              15  AS2-AGE    PIC S999     COMP-3.
00235                  88  AS2-0-AGE        VALUE +0.
00236                  88  AS2-OVER-80      VALUE +081 THRU +999.
00237 *                                             25-26  S2-AGE
00238              15  AS2-COND   PIC S99V9    COMP-3.
00239 *                                             27-28  S2-CONDITION
00240              15  AS2-PCASSD PIC S99V9(5) COMP-3.
00241 *                                             29-32  S2-% ASSESSED
00242              15  AS2-BUFF   PIC S999     COMP-3.
00243 *                                             33-34  S2-BUFF-NO
00244              15  FILLER     PIC XX.
00245 *                                             35-36  FILLER
00246              15  AS2-VAL    PIC S9(9)    COMP-3.
00247 *                                             37-41  S2-VALUATION
00248              15  AS2-KEYPCL PIC S9(15)   COMP-3.
00249 *                                             42-49  S2-KEY PARCEL
00250              15  AS2-SC     PIC X.
00251 *                                             50     S2-SPLIT CODE
00252              15  FILLER     PIC X(3).
00253 *                                             51-53  FILLER
00254 *----------------------------------------------------------------*
00255          10  AS3-TYP3 REDEFINES AS1-TYP1.
00256 *                                              1-53  TYPE3-IMPRVMT
00257              15  AS3-MC     PIC S999     COMP-3.
00258 *                                              1-2   S3-MULTI-CODE
00259              15  AS3-TYP    PIC X.
00260                  88  AS3-TYPE3        VALUE '3'.
00261 *                                              3     S3-TYPE3
00262              15  AS3-CD     PIC X.
00263 *                                              4     S3-CODE 2/3
00264 *                                                    2=MAJOR-IMPRV
00265 *                                                    3=MINOR-IMPRV
00266              15  FILLER     PIC X(3).
00267 *                                              5-7   FILLER
00268              15  AS3-CLS    PIC S999     COMP-3.
00269 *                                              8-9   S3-CLASS
00270 *                                                    (MAJOR-MINOR)
00271              15  AS3-CDU    PIC XX.
00272 *                                             10-11  S3-CDU
00273              15  AS3-REPCST PIC S9(9)    COMP-3.
00274 *                                             12-16  S3-REPRODUCTV
00275 *                                                    COST
00276              15  FILLER     PIC X(6).
00277 *                                             17-22  FILLER
00278              15  AS3-YR     PIC S999     COMP-3.
00279 *                                             23-24  S3-YEAR
00280              15  AS3-AGE    PIC S999     COMP-3.
00281                  88  AS3-0-AGE        VALUE +0.
00282                  88  AS3-OVER-80      VALUE +081 THRU +999.
00283 *                                             25-26  S3-AGE
00284              15  AS3-COND   PIC S99V9    COMP-3.
00285 *                                             27-28  S3-CONDITION
00286              15  AS3-PCASSD PIC S99V9(5) COMP-3.
00287 *                                             29-32  S3-% ASSESSED
00288              15  AS3-BUFF   PIC S999     COMP-3.
00289 *                                             33-34  S3-BUFF-NO
00290              15  FILLER     PIC XX.
00291 *                                             35-36  FILLER
00292              15  AS3-VAL    PIC S9(9)    COMP-3.
00293 *                                             37-41  S3-VALUATION
00294              15  AS3-KEYPCL PIC S9(15)   COMP-3.
00295 *                                             42-49  S3-KEY PARCEL
00296              15  AS3-SC     PIC X.
00297 *                                             50     S3-SPLIT CODE
00298              15  FILLER     PIC X(3).
00299 *                                             51-53  FILLER
00300 *----------------------------------------------------------------*
00301          10  AS4-TYP4 REDEFINES AS1-TYP1.
00302 *                                              1-53  TYPE4-IMPRVMT
00303              15  AS4-MC     PIC S999     COMP-3.
00304 *                                              1-2   S4-MULTI-CODE
00305              15  AS4-TYP    PIC X.
00306                  88  AS4-TYPE4        VALUE '4'.
00307 *                                              3     S4-TYPE4
00308              15  AS4-CD     PIC X.
00309 *                                              4     S4-CODE4
00310              15  FILLER     PIC X(3).
00311 *                                              5-7   FILLER
00312              15  AS4-CLS    PIC S999     COMP-3.
00313 *                                              8-9   S4-CLASS
00314 *                                                    (MAJOR-MINOR)
00315              15  AS4-CDU    PIC XX.
00316 *                                             10-11  S4-CDU
00317              15  AS4-REPCST PIC S9(9)    COMP-3.
00318 *                                             12-16  S4-REPRODUCTV
00319 *                                                    COST
00320              15  AS4-TOTVAL PIC S9(9)    COMP-3.
00321 *                                             17-21  S4-TOTAL-VALU
00322              15  FILLER     PIC X.
00323 *                                             22     FILLER
00324              15  AS4-OCCFAC PIC S99V9    COMP-3.
00325 *                                             23-24  S4-OCCUPANCY
00326 *                                                    FACTOR
00327              15  AS4-AGE    PIC S999     COMP-3.
00328                  88  AS4-0-AGE        VALUE +0.
00329                  88  AS4-OVER-AGE     VALUE +081 THRU +999.
00330 *                                             25-26  S4-AGE
00331              15  AS4-COND   PIC S99V9    COMP-3.
00332 *                                             27-28  S4-CONDITION
00333              15  AS4-PCASSD PIC S99V9(5) COMP-3.
00334 *                                             29-32  S4-% ASSESSED
00335              15  AS4-BUFF   PIC S999     COMP-3.
00336 *                                             33-34  S4-BUFF-NO
00337              15  FILLER     PIC XX.
00338 *                                             35-36  FILLER
00339              15  AS4-VAL    PIC S9(9)    COMP-3.
00340 *                                             37-41  S4-VALUATION
00341              15  AS4-KEYPCL PIC S9(15)   COMP-3.
00342 *                                             42-49  S4-KEY PARCEL
00343              15  AS4-SC     PIC X.
00344 *                                             50     S4-SPLIT CODE
00345              15  FILLER     PIC X(3).
00346 *                                             51-53  FILLER
00347 *----------------------------------------------------------------*
00348 *                                              1-53  TYPE5-IMPRVMT
00349          10  AS5-TYP5 REDEFINES AS1-TYP1.
00350              15  AS5-MC     PIC S999     COMP-3.
00351 *                                              1-2   S5-MULTI-CODE
00352              15  AS5-TYP    PIC X.
00353                  88  AS5-TYPE5        VALUE '5'.
00354 *                                              3     S5-TYPE5
00355              15  AS5-CD     PIC X.
00356 *                                              4     S5-CODE5
00357              15  FILLER     PIC X(3).
00358 *                                              5-7   FILLER
00359              15  AS5-CLS    PIC S999     COMP-3.
00360 *                                              8-9   S5-CLASS
00361 *                                                    (MAJOR-MINOR)
00362              15  AS5-CDU    PIC XX.
00363 *                                             10-11  S5-CDU
00364              15  AS5-REPCST PIC S9(9)    COMP-3.
00365 *                                             12-16  S5-REPRODUCTV
00366 *                                                    COST
00367              15  FILLER     PIC X(6).
00368 *                                             17-22  FILLER
00369              15  AS5-OCCFAC PIC S99V9    COMP-3.
00370 *                                             23-24  S5-OCCUPANCY
00371 *                                                    FACTOR
00372              15  AS5-AGE    PIC S999     COMP-3.
00373                  88  AS5-0-AGE        VALUE +0.
00374                  88  AS5-OVER-80      VALUE +081 THRU +999.
00375 *                                             25-26  S5-AGE
00376              15  AS5-COND   PIC S99V9    COMP-3.
00377 *                                             27-28  S5-CONDITION
00378              15  AS5-PCASSD PIC S99V9(5) COMP-3.
00379 *                                             29-32  S5-% ASSESSED
00380              15  AS5-BUFF   PIC S999     COMP-3.
00381 *                                             33-34  S5-BUFF-NO
00382              15  FILLER     PIC XX.
00383 *                                             35-36  FILLER
00384              15  AS5-VAL    PIC S9(9)    COMP-3.
00385 *                                             37-41  S5-VALUATION
00386              15  AS5-KEYPCL PIC S9(15)   COMP-3.
00387 *                                             42-49  S5-KEY PARCEL
00388              15  AS5-SC     PIC X.
00389 *                                             50     S5-SPLIT CODE
00390              15  FILLER     PIC X(3).
00391 *                                             51-53  FILLER
00392 *----------------------------------------------------------------*
00393 * --  Q U E S T I O N N A I R E  S E G M E N T  -- IS PRESENT,   *
00394 *      IF THE PRECEDING DETAIL SEGMENT IS - TYPE = '2' THRU '5'  *
00395 *      AND CLASS = 202-212,234,278,295,405-412,434,478,495       *
00396          10  AS-QST REDEFINES AS1-TYP1.
00397 *                                              1-53  QUESTIONNAIRE
00398            12  AS-QMFILL1.
00399                15  AS-QM1   PIC 9.
00400                  88  AS-QM1R-RESTYP1-5 VALUE 1 THRU 5.
00401                  88  AS-QM1R-RESTYP1-3 VALUE 1 2 3 5.
00402                  88  AS-QM1R-RESTYP1OR5 VALUE 1 5.
00403                  88  AS-QM1R-RESTYP2  VALUE 2.
00404                  88  AS-QM1R-RESTYP2-3 VALUE 2 3.
00405                  88  AS-QM1R-RESTYP4  VALUE 4.
00406 *                                              1     TYPE-RESIDENC
00407                15  AS-QM2   PIC 9.
00408 *                                              2     USE
00409                15  AS-QM2A  PIC 9.
00410 *                                              3     NO. OF APTS.
00411 *                                   ---------- 4-5   EXTERIOR -
00412 *                                                     CONSTRUCTION
00413                15  AS-QM3   PIC 9.
00414 *                                              4     WALLS
00415                15  AS-QM4   PIC 9.
00416 *                                              5     ROOF
00417 *                                   ---------- 6-27  ROOMS,HEATING
00418 *                                                     AND PLUMBING
00419            12  AS-QMFILL2.
00420                15  AS-QM5A1 PIC S999     COMP-3.
00421 *                                              6-7   NO. OF ROOMS
00422            12  AS-QMFILL3.
00423                15  AS-QM5A2 PIC 99.
00424 *                                              8-9   NO. OF BEDRMS
00425                15  AS-QM5B1 PIC 9.
00426 *                                             10     BASEMENT-TYPE
00427                15  AS-QM5B2 PIC 9.
00428 *                                             11     BSMT-FINISH
00429                15  AS-QM5C1 PIC 9.
00430 *                                             12     CENTRAL-HEAT
00431                15  AS-QM5C2F PIC 9.
00432 *                                             13     FLOOR-FURNACE
00433                15  AS-QM5C2U PIC 9.
00434 *                                             14     UNIT-HEATER
00435                15  AS-QM5C2S PIC 9.
00436 *                                             15     STOVE
00437                15  AS-QM5C2O PIC 9.
00438 *                                             16     SOLAR
00439                15  AS-QM5C3 PIC 9.
00440 *                                             17     CENTRAL-AIR
00441                15  AS-QM5C4 PIC 9.
00442 *                                             18     NO. FIREPLACE
00443                15  AS-QM5C41 PIC 9.
00444 *                                             19     1ST FLR FIREP
00445                15  AS-QM5C42 PIC 9.
00446 *                                             20     2ND FLR FIREP
00447                15  AS-QM5C43 PIC 9.
00448 *                                             21     3RD FLR FIREP
00449                15  AS-QM5C4B PIC 9.
00450 *                                             22     BSMT FIREPLCE
00451                15  AS-QM5D1 PIC 9.
00452 *                                             23     ATTIC-TYPE
00453                15  AS-QM5D2 PIC 9.
00454 *                                             24     ATTIC-FINISH
00455                15  AS-QM5E1 PIC 99.
00456 *                                             25-26  NO. FULL-BATH
00457                15  AS-QM5E2 PIC 9.
00458 *                                             27     NO. HALF-BATH
00459                15  AS-QM6A  PIC 9.
00460 *                                             28     TYPE OF DESGN
00461 *                                                    ARCHITECT OR
00462 *                                                    STOCK PLAN
00463                15  AS-QM6B  PIC 9.
00464 *                                             29     TYPE OF DESGN
00465 *                                                    TYPICAL OR
00466 *                                                    ATYPICAL
00467                15  AS-QM7A  PIC 9.
00468 *                                             30     CONST QUALITY
00469 *                                                    1 = DELUXE
00470 *                                                    2 = AVERAGE
00471 *                                                    3 = POOR
00472                15  AS-QM7B  PIC 9.
00473 *                                             31     CONST QUALITY
00474 *                                                    1 = RENOVATED
00475                15  AS-QM8   PIC 9.
00476 *                                             32     SITE DESRBLTY
00477 *                                   ----------33-36  GARAGE - INFO
00478                15  AS-QM9A  PIC 9.
00479 *                                             33     GARAGE-SIZE
00480                15  AS-QM9B  PIC 9.
00481 *                                             34     GARAGE-CONST.
00482                15  AS-QM9C  PIC 9.
00483 *                                             35     GARAGE-ATTACH
00484                15  AS-QM9D  PIC 9.
00485 *                                             36     GRAGE IN AREA
00486 *                                   ----------37-40  2ND GRGE INFO
00487                15  AS-QM10A PIC 9.
00488 *                                             37     GARAGE-SIZE
00489                15  AS-QM10B PIC 9.
00490 *                                             38     GARAGE-CONST.
00491                15  AS-QM10C PIC 9.
00492 *                                             39     GARAGE-ATTACH
00493                15  AS-QM10D PIC 9.
00494 *                                             40     GRAGE IN AREA
00495                15  AS-QM11  PIC 9.
00496 *                                             41     PORCH
00497            12  AS-QMFILL4.
00498                15  AS-QM12  PIC S9(7)    COMP-3.
00499 *                                   ----------42-45  OTHER -
00500 *                                                     IMPROVEMENTS
00501                15  AS-QM13  PIC S9(7)    COMP-3.
00502 *                                             46-49  BLDG-SQ-FT
00503            12  AS-QMFILL5.
00504                15  AS-QM16  PIC 9.
00505 *                                             50     ST OF REPAIR
00506                15  AS-QM17  PIC 9.
00507 *                                             51     SPECIAL COND
00508            12  AS-QMFILL6.
00509                15  FILLER   PIC XX.
00510 *                                             52-53  FILLER
00511 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
