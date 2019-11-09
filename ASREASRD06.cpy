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
00015          10  FILLER         PIC X.
00016 *                                             12     FILLER
00017          10  M-PERMIND      PIC X.
00018 *                                             13       PERMIT IND
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
00059              15  M-ASMT-YR PIC X.
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
00074          10  M-DIVNO-PROP   PIC 9(14)    COMP-3.
00075 *                                             57-64    DIVISION NO
00076 *                                                      PROPERTY NO
00077          10  FILLER         PIC XX.
00078 *                                             65-66    FILLER
00079          10  M-VALUE   OCCURS 10 TIMES
00080                             PIC S9(9)    COMP-3.
00081 *                                             67-116 VALUE FIELDS
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
00093          10  FILLER         PIC XX.
00094 *                                            117-118 FILLER
00095          10  M-REC-CTRS-1.
00096 *                                            119-122 RECORD CNTRS
00097              15  M-SLS-CTR-1
00098                             PIC S9.
00099                  88  M-NO-SALES       VALUE +0.
00100                  88  M-SALES-PRESENT VALUE +1.
00101 *                                            119       SALES-CNTR
00102              15  M-DTL-QST-CTR
00103                             PIC S9(3).
00104                  88  M-NO-DETAIL      VALUE +0.
00105                  88  M-DETAIL-PRESENT VALUE +1 THRU +350.
00106                  88  M-MAXIMUM-DETAIL VALUE +350.
00107 *                                            120-122   DETAIL-CNTR
00108 *----------------------------------------------------------------*
00109 *   S A L E S  S E G M E N T, IF PRESENT, CONTAINS 34 BYTES      *
00110 *    DESCRIPTIONS RELATIVE TO 1ST BYTE IN EACH SEGMENT           *
00111 *                                                                *
00112      05  M-SALES   OCCURS 0 TO 1 TIMES DEPENDING ON M-SLS-CTR-1.
00113 *                                              1-34  SALES SEGMENT
00114          10  M-DEEDTYP      PIC XX.
00115 *                                              1-2   DEED-TYPE
00116          10  M-DEEDNO       PIC S9(9)    COMP-3.
00117 *                                              3-7   DEED-NUMBER
00118          10  M-SLS-DAT1     PIC S9(7)    COMP-3.
00119 *                                              8-11  SALE-DATE1
00120 *                                                      (YYMMDD)
00121          10  M-SLS-AMT1     PIC S9(9)    COMP-3.
00122 *                                             12-16  SALE-AMOUNT1
00123          10  M-SLS-DAT2     PIC S9(7)    COMP-3.
00124 *                                             17-20  SALE-DATE2
00125 *                                                      (YYMMDD)
00126          10  M-SLS-AMT2     PIC S9(9)    COMP-3.
00127 *                                             21-25  SALE-AMOUNT2
00128          10  M-SLS-DAT3     PIC S9(7)    COMP-3.
00129 *                                             26-29  SALE-DATE3
00130 *                                                      (YYMMDD)
00131          10  M-SLS-AMT3     PIC S9(9)    COMP-3.
00132 *                                             30-34  SALE-AMOUNT3
00133 *----------------------------------------------------------------*
00134 * D E T A I L  S E G M E N T S, IF PRESENT,EACH CONTAINS 53 BYTES
00135 *    DESCRIPTIONS RELATIVE TO 1ST BYTE IN EACH SEGMENT           *
00136 *                                                                *
00137      05  M-DTL-QST
00138              OCCURS 0 TO 350 TIMES DEPENDING ON M-DTL-QST-CTR.
00139          10  DM-TYP1.
00140 *                                              1-53  TYPE1-LAND
00141              15  D1-MC      PIC S999     COMP-3.
00142 *                                              1-2   S1-MULTI-CODE
00143              15  D1-TYP     PIC X.
00144                  88  D1-TYPE1         VALUE '1'.
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
00169              15  D1-ECFCTR PIC SV9(5)    COMP-3.
00170 *                                             28-30  S1-EXTRA
00171 *                                                    CORNER FACTOR
00172              15  D1-PCASSD PIC S99V9(5) COMP-3.
00173 *                                             31-34  S1-% ASSESSED
00174              15  D1-EI      PIC S99V9    COMP-3.
00175                  88  D1-ECON1         VALUE +22.0.
00176                  88  D1-ECON2         VALUE +16.0.
00177                  88  D1-ECON3         VALUE +33.0.
00178                  88  D1-ECON4         VALUE +30.0.
00179                  88  D1-ECON5         VALUE +37.0.
00180                  88  D1-ECON6         VALUE +38.5.
00181                  88  D1-ECON-IND      VALUE +16.0 +22.0 +30.0
00182                                             +33.0 +37.0 +38.5.
00183 *                                             35-36  S1-ECON-IND
00184              15  D1-VAL     PIC S9(9)    COMP-3.
00185 *                                             37-41  S1-VALUATION
00186              15  FILLER     PIC X(11).
00187 *                                             42-52  FILLER
00188              15  D1-UI      PIC X.
00189 *                                             53     S1-UNIT-IND
00190 *----------------------------------------------------------------*
00191          10  DM-TYP2 REDEFINES DM-TYP1.
00192 *                                              1-53  TYPE2-IMPRVMT
00193              15  D2-MC      PIC S999     COMP-3.
00194 *                                              1-2   S2-MULTI-CODE
00195              15  D2-TYP     PIC X.
00196                  88  D2-TYPE2         VALUE '2'.
00197                  88  D2-TYPE2-5       VALUE '2' THRU '5'.
00198 *                                              3     S2-TYPE2
00199              15  D2-CD      PIC X.
00200 *                                              4     S2-CODE 2/3
00201 *                                                    2=MAJOR-IMPRV
00202 *                                                    3=MINOR-IMPRV
00203              15  D2-DEC     PIC X.
00204 *                                              5     S2-DECIMAL
00205              15  D2-UM      PIC XX.
00206 *                                              6-7   S2-UNIT-MEAS
00207              15  D2-CLS     PIC S999     COMP-3.
00208                  88  D2-QUES-CLASS    VALUE +202 THRU +212
00209                                             +234 +278 +295
00210                                             +402 THRU +412
00211                                             +434 +478 +495.
00212                  88  D2-QCLS1-3       VALUE +210 +211 +212 +295
00213                                             +410 +411 +412 +495.
00214                  88  D2-QCLS1OR5      VALUE +202 +203 +204
00215                                             +402 +403 +404.
00216                  88  D2-QCLS2         VALUE +205 THRU +208 +278
00217                                             +405 THRU +408 +478.
00218                  88  D2-QCLS2-3       VALUE +209 +409.
00219                  88  D2-QCLS4         VALUE +234 +434.
00220                  88  D2-QCLS6         VALUE +593.
00221                  88  D2-QCLS7         VALUE +597.
00222 *                                              8-9   S2-CLASS
00223 *                                                    (MAJOR-MINOR)
00224              15  D2-CDU     PIC XX.
00225 *                                             10-11  S2-CDU
00226              15  D2-AREA    PIC S9(7)    COMP-3.
00227 *                                             12-15  S2-AREA
00228              15  D2-UPR     PIC S9(5)V99 COMP-3.
00229 *                                             16-19  S2-UNIT-PRICE
00230              15  D2-PRDCT   PIC S9(9)    COMP-3.
00231 *                                             20-24  S2-PRODUCT
00232              15  D2-AGE     PIC S999     COMP-3.
00233                  88  D2-0-AGE         VALUE +0.
00234                  88  D2-OVER-80       VALUE +081 THRU +999.
00235 *                                             25-26  S2-AGE
00236              15  D2-COND    PIC S99V9    COMP-3.
00237 *                                             27-28  S2-CONDITION
00238              15  D2-PCASSD PIC S99V9(5) COMP-3.
00239 *                                             29-32  S2-% ASSESSED
00240              15  D2-BUFF    PIC S999     COMP-3.
00241 *                                             33-34  S2-BUFF-NO
00242              15  FILLER     PIC XX.
00243 *                                             35-36  FILLER
00244              15  D2-VAL     PIC S9(9)    COMP-3.
00245 *                                             37-41  S2-VALUATION
00246              15  D2-KEYPCL PIC S9(15)    COMP-3.
00247 *                                             42-49  S2-KEY PARCEL
00248              15  D2-SC      PIC X.
00249 *                                             50     S2-SPLIT CODE
00250              15  FILLER     PIC X(3).
00251 *                                             51-53  FILLER
00252 *----------------------------------------------------------------*
00253          10  DM-TYP3 REDEFINES DM-TYP1.
00254 *                                              1-53  TYPE3-IMPRVMT
00255              15  D3-MC      PIC S999     COMP-3.
00256 *                                              1-2   S3-MULTI-CODE
00257              15  D3-TYP     PIC X.
00258                  88  D3-TYPE3         VALUE '3'.
00259 *                                              3     S3-TYPE3
00260              15  D3-CD      PIC X.
00261 *                                              4     S3-CODE 2/3
00262 *                                                    2=MAJOR-IMPRV
00263 *                                                    3=MINOR-IMPRV
00264              15  D3-PI      PIC X.
00265 *                                              5     PERMIT IND
00266 *                                                    1 OR BLANK=
00267 *                                                      NO MAT PERM
00268 *                                                    2=MAT PERM
00269 *                                                      WITH BLDG
00270 *                                                    3=MAT PERM
00271              15  FILLER     PIC XX.
00272 *                                              6-7   FILLER
00273              15  D3-CLS     PIC S999     COMP-3.
00274 *                                              8-9   S3-CLASS
00275 *                                                    (MAJOR-MINOR)
00276              15  D3-CDU     PIC XX.
00277 *                                             10-11  S3-CDU
00278              15  D3-REPCST PIC S9(9)     COMP-3.
00279 *                                             12-16  S3-REPRODUCTV
00280 *                                                    COST
00281              15  FILLER     PIC X(6).
00282 *                                             17-22  FILLER
00283              15  D3-YR      PIC S999     COMP-3.
00284 *                                             23-24  S3-YEAR
00285              15  D3-AGE     PIC S999     COMP-3.
00286                  88  D3-0-AGE         VALUE +0.
00287                  88  D3-OVER-80       VALUE +081 THRU +999.
00288 *                                             25-26  S3-AGE
00289              15  D3-COND    PIC S99V9    COMP-3.
00290 *                                             27-28  S3-CONDITION
00291              15  D3-PCASSD PIC S99V9(5) COMP-3.
00292 *                                             29-32  S3-% ASSESSED
00293              15  D3-BUFF    PIC S999     COMP-3.
00294 *                                             33-34  S3-BUFF-NO
00295              15  FILLER     PIC XX.
00296 *                                             35-36  FILLER
00297              15  D3-VAL     PIC S9(9)    COMP-3.
00298 *                                             37-41  S3-VALUATION
00299              15  D3-KEYPCL PIC S9(15)    COMP-3.
00300 *                                             42-49  S3-KEY PARCEL
00301              15  D3-SC      PIC X.
00302 *                                             50     S3-SPLIT CODE
00303              15  FILLER     PIC X(3).
00304 *                                             51-53  FILLER
00305 *----------------------------------------------------------------*
00306          10  DM-TYP4 REDEFINES DM-TYP1.
00307 *                                              1-53  TYPE4-IMPRVMT
00308              15  D4-MC      PIC S999     COMP-3.
00309 *                                              1-2   S4-MULTI-CODE
00310              15  D4-TYP     PIC X.
00311                  88  D4-TYPE4         VALUE '4'.
00312 *                                              3     S4-TYPE4
00313              15  D4-CD      PIC X.
00314 *                                              4     S4-CODE4
00315              15  FILLER     PIC X(3).
00316 *                                              5-7   FILLER
00317              15  D4-CLS     PIC S999     COMP-3.
00318 *                                              8-9   S4-CLASS
00319 *                                                    (MAJOR-MINOR)
00320              15  D4-CDU     PIC XX.
00321 *                                             10-11  S4-CDU
00322              15  D4-REPCST PIC S9(9)     COMP-3.
00323 *                                             12-16  S4-REPRODUCTV
00324 *                                                    COST
00325              15  D4-TOTVAL PIC S9(9)     COMP-3.
00326 *                                             17-21  S4-TOTAL-VALU
00327              15  FILLER     PIC X.
00328 *                                             22     FILLER
00329              15  D4-OCCFAC PIC S99V9     COMP-3.
00330 *                                             23-24  S4-OCCUPANCY
00331 *                                                    FACTOR
00332              15  D4-AGE     PIC S999     COMP-3.
00333                  88  D4-0-AGE         VALUE +0.
00334                  88  D4-OVER-AGE      VALUE +081 THRU +999.
00335 *                                             25-26  S4-AGE
00336              15  D4-COND    PIC S99V9    COMP-3.
00337 *                                             27-28  S4-CONDITION
00338              15  D4-PCASSD PIC S99V9(5) COMP-3.
00339 *                                             29-32  S4-% ASSESSED
00340              15  D4-BUFF    PIC S999     COMP-3.
00341 *                                             33-34  S4-BUFF-NO
00342              15  FILLER     PIC XX.
00343 *                                             35-36  FILLER
00344              15  D4-VAL     PIC S9(9)    COMP-3.
00345 *                                             37-41  S4-VALUATION
00346              15  D4-KEYPCL PIC S9(15)    COMP-3.
00347 *                                             42-49  S4-KEY PARCEL
00348              15  D4-SC      PIC X.
00349 *                                             50     S4-SPLIT CODE
00350              15  FILLER     PIC X(3).
00351 *                                             51-53  FILLER
00352 *----------------------------------------------------------------*
00353 *                                              1-53  TYPE5-IMPRVMT
00354          10  DM-TYP5 REDEFINES DM-TYP1.
00355              15  D5-MC      PIC S999     COMP-3.
00356 *                                              1-2   S5-MULTI-CODE
00357              15  D5-TYP     PIC X.
00358                  88  D5-TYPE5         VALUE '5'.
00359 *                                              3     S5-TYPE5
00360              15  D5-CD      PIC X.
00361 *                                              4     S5-CODE5
00362              15  FILLER     PIC X(3).
00363 *                                              5-7   FILLER
00364              15  D5-CLS     PIC S999     COMP-3.
00365 *                                              8-9   S5-CLASS
00366 *                                                    (MAJOR-MINOR)
00367              15  D5-CDU     PIC XX.
00368 *                                             10-11  S5-CDU
00369              15  D5-REPCST PIC S9(9)     COMP-3.
00370 *                                             12-16  S5-REPRODUCTV
00371 *                                                    COST
00372              15  FILLER     PIC X(6).
00373 *                                             17-22  FILLER
00374              15  D5-OCCFAC PIC S99V9     COMP-3.
00375 *                                             23-24  S5-OCCUPANCY
00376 *                                                    FACTOR
00377              15  D5-AGE     PIC S999     COMP-3.
00378                  88  D5-0-AGE         VALUE +0.
00379                  88  D5-OVER-80       VALUE +081 THRU +999.
00380 *                                             25-26  S5-AGE
00381              15  D5-COND    PIC S99V9    COMP-3.
00382 *                                             27-28  S5-CONDITION
00383              15  D5-PCASSD PIC S99V9(5) COMP-3.
00384 *                                             29-32  S5-% ASSESSED
00385              15  D5-BUFF    PIC S999     COMP-3.
00386 *                                             33-34  S5-BUFF-NO
00387              15  FILLER     PIC XX.
00388 *                                             35-36  FILLER
00389              15  D5-VAL     PIC S9(9)    COMP-3.
00390 *                                             37-41  S5-VALUATION
00391              15  D5-KEYPCL PIC S9(15)    COMP-3.
00392 *                                             42-49  S5-KEY PARCEL
00393              15  D5-SC      PIC X.
00394 *                                             50     S5-SPLIT CODE
00395              15  FILLER     PIC X(3).
00396 *                                             51-53  FILLER
00397 *----------------------------------------------------------------*
00398 * --  Q U E S T I O N N A I R E  S E G M E N T  -- IS PRESENT,   *
00399 *      IF THE PRECEDING DETAIL SEGMENT IS - TYPE = '2' THRU '5'  *
00400 *      AND CLASS = 202-212,234,278,295,405-412,434,478,495       *
00401          10  M-QST REDEFINES DM-TYP1.
00402 *                                              1-53  QUESTIONNAIRE
00403            12  QMFILL1.
00404                15  QM1      PIC 9.
00405                  88  QM1R-RESTYP1-5   VALUE 1 THRU 5.
00406                  88  QM1R-RESTYP1-3   VALUE 1 2 3 5.
00407                  88  QM1R-RESTYP1OR5  VALUE 1 5.
00408                  88  QM1R-RESTYP2     VALUE 2.
00409                  88  QM1R-RESTYP2-3   VALUE 2 3.
00410                  88  QM1R-RESTYP4     VALUE 4.
00411 *                                              1     TYPE-RESIDENC
00412                15  QM2      PIC 9.
00413 *                                              2     USE
00414                15  QM2A     PIC 9.
00415 *                                              3     NO. OF APTS.
00416 *                                   ---------- 4-5   EXTERIOR -
00417 *                                                     CONSTRUCTION
00418                15  QM3      PIC 9.
00419 *                                              4     WALLS
00420                15  QM4      PIC 9.
00421 *                                              5     ROOF
00422 *                                   ---------- 6-27  ROOMS,HEATING
00423 *                                                     AND PLUMBING
00424            12  QMFILL2.
00425                15  QM5A1    PIC S999     COMP-3.
00426 *                                              6-7   NO. OF ROOMS
00427            12  QMFILL3.
00428                15  QM5A2    PIC 99.
00429 *                                              8-9   NO. OF BEDRMS
00430                15  QM5B1    PIC 9.
00431 *                                             10     BASEMENT-TYPE
00432                15  QM5B2    PIC 9.
00433 *                                             11     BSMT-FINISH
00434                15  QM5C1    PIC 9.
00435 *                                             12     CENTRAL-HEAT
00436                15  QM5C2F   PIC 9.
00437 *                                             13     FLOOR-FURNACE
00438                15  QM5C2U   PIC 9.
00439 *                                             14     UNIT-HEATER
00440                15  QM5C2S   PIC 9.
00441 *                                             15     STOVE
00442                15  QM5C2O   PIC 9.
00443 *                                             16     SOLAR
00444                15  QM5C3    PIC 9.
00445 *                                             17     CENTRAL-AIR
00446                15  QM5C4    PIC 9.
00447 *                                             18     NO. FIREPLACE
00448                15  QM5C41   PIC 9.
00449 *                                             19     1ST FLR FIREP
00450                15  QM5C42   PIC 9.
00451 *                                             20     2ND FLR FIREP
00452                15  QM5C43   PIC 9.
00453 *                                             21     3RD FLR FIREP
00454                15  QM5C4B   PIC 9.
00455 *                                             22     BSMT FIREPLCE
00456                15  QM5D1    PIC 9.
00457 *                                             23     ATTIC-TYPE
00458                15  QM5D2    PIC 9.
00459 *                                             24     ATTIC-FINISH
00460                15  QM5E1    PIC 99.
00461 *                                             25-26  NO. FULL-BATH
00462                15  QM5E2    PIC 9.
00463 *                                             27     NO. HALF-BATH
00464                15  QM6A     PIC 9.
00465 *                                             28     TYPE OF DESGN
00466 *                                                    ARCHITECT OR
00467 *                                                    STOCK PLAN
00468                15  QM6B     PIC 9.
00469 *                                             29     TYPE OF DESGN
00470 *                                                    TYPICAL OR
00471 *                                                    ATYPICAL
00472                15  QM7A     PIC 9.
00473 *                                             30     CONST QUALITY
00474 *                                                    1 = DELUXE
00475 *                                                    2 = AVERAGE
00476 *                                                    3 = POOR
00477                15  QM7B     PIC 9.
00478 *                                             31     CONST QUALITY
00479 *                                                    1 = RENOVATED
00480                15  QM8      PIC 9.
00481 *                                             32     SITE DESRBLTY
00482 *                                   ----------33-36  GARAGE - INFO
00483                15  QM9A     PIC 9.
00484 *                                             33     GARAGE-SIZE
00485                15  QM9B     PIC 9.
00486 *                                             34     GARAGE-CONST.
00487                15  QM9C     PIC 9.
00488 *                                             35     GARAGE-ATTACH
00489                15  QM9D     PIC 9.
00490 *                                             36     GRAGE IN AREA
00491 *                                   ----------37-40  2ND GRGE INFO
00492                15  QM10A    PIC 9.
00493 *                                             37     GARAGE-SIZE
00494                15  QM10B    PIC 9.
00495 *                                             38     GARAGE-CONST.
00496                15  QM10C    PIC 9.
00497 *                                             39     GARAGE-ATTACH
00498                15  QM10D    PIC 9.
00499 *                                             40     GRAGE IN AREA
00500                15  QM11     PIC 9.
00501 *                                             41     PORCH
00502            12  QMFILL4.
00503                15  QM12     PIC S9(7)    COMP-3.
00504 *                                   ----------42-45  OTHER -
00505 *                                                     IMPROVEMENTS
00506                15  QM13     PIC S9(7)    COMP-3.
00507 *                                             46-49  BLDG-SQ-FT
00508            12  QMFILL5.
00509                15  QM16     PIC 9.
00510 *                                             50     ST OF REPAIR
00511                15  QM17     PIC 9.
00512 *                                             51     SPECIAL COND
00513            12  QMFILL6.
00514                15  FILLER   PIC XX.
00515 *                                             52-53  FILLER
00516 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
