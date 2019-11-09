00001      05  MA-BASE.
00002 *                                              1-122 FIXED SEGMENT
00003          10  MA-STAT1       PIC X.
00004              88  MA-NON-ASSESSED      VALUE '0'.
00005              88  MA-ASSESSED          VALUE '1'.
00006 *                                              1     STATUS-1
00007          10  MA-VOLPROP.
00008 *                                              2-11  RECORD KEY
00009              15  MA-VOL     PIC S9(3)    COMP-3.
00010                  88  MA-RE-VOL        VALUE +001 THRU +601.
00011                  88  MA-RR-VOL        VALUE +605.
00012 *                                              2-3     VOLUME
00013              15  MA-PROP    PIC S9(15)   COMP-3.
00014 *                                              4-11    PROPERTY-NO
00015          10  MA-TXTYP       PIC X.
00016 *                                             12-12  TAX TYPE
00017          10  MA-SF-IND      PIC X.
00018 *                                             13-13  SEN FRZ IND
00019          10  MA-TXCD        PIC S9(5)    COMP-3.
00020 *                                             14-16  TAX CODE
00021          10  MA-STAT2       PIC X.
00022              88  MA-TAXABLE-PARCEL    VALUE '0'.
00023              88  MA-EXEMPT            VALUE '1'.
00024              88  MA-RAILROAD          VALUE '2'.
00025              88  MA-HOMESTEAD-NON-COOP VALUE '3'.
00026              88  MA-VETERAN           VALUE '4'.
00027              88  MA-HOMESTEAD-COOP    VALUE '5'.
00028 *                                             17     STATUS-2
00029          10  MA-CLS         PIC S9(3)    COMP-3.
00030 *                                             18-19  CLASS (9-99)
00031 *                                                    (MAJOR-MINOR)
00032          10  MA-NBHD        PIC S9(3)    COMP-3.
00033 *                                             20-21  NEIGHBORHD-CD
00034          10  MA-STRT        PIC S9(5)    COMP-3.
00035 *                                             22-24  STREET CODE
00036          10  MA-HSENO       PIC S9(5)    COMP-3.
00037 *                                             25-27  HOUSE-NO
00038          10  MA-LNDDIM      PIC S9(7)    COMP-3.
00039 *                                             28-31  LND-DIMENSION
00040          10  MA-LNDCD       PIC X.
00041 *                                             32     LAND CODE
00042          10  MA-NPHE-STATUS REDEFINES MA-LNDCD    PIC X.
00043 *                                             32     NPHE STATUS
00044 *                                                       FIELD
00045          10  MA-LNDSQFT     PIC S9(7)    COMP-3.
00046 *                                             33-36  LAND-SQ-FT
00047          10  MA-IRREG       PIC X.
00048 *                                             37     IRREGULAR
00049          10  MA-STAT3       PIC X.
00050 *                                             38     COMPLAINT ST
00051          10  MA-CMPLNTNO    PIC S9(7)    COMP-3.
00052 *                                             39-42  B.A.COMPLAINT
00053          10  MA-BA-ACTION.
00054 *                                             43-44  B.A. ACTION
00055              15  MA-BA-YR   PIC X.
00056 *                                             43       BA-YEAR
00057              15  MA-BA-REV  PIC X.
00058 *                                             44       BA-REVISION
00059          10  MA-ASMT-ACTION OCCURS 4 TIMES.
00060 *                                             45-56  ASMT-ACTIONS
00061 *                                                    (YR-CHNG-REV)
00062              15  MA-ASMT-YR PIC X.
00063 *                                             45       AA-YEAR1
00064 *                                             48       AA-YEAR2
00065 *                                             51       AA-YEAR3
00066 *                                             54       AA-YEAR4
00067              15  MA-ASMT-CHG PIC X.
00068 *                                             46       AA-CHANGE1
00069 *                                             49       AA-CHANGE2
00070 *                                             52       AA-CHANGE3
00071 *                                             55       AA-CHANGE4
00072              15  MA-ASMT-REV PIC X.
00073 *                                             47       AA-REVSION1
00074 *                                             50       AA-REVSION2
00075 *                                             53       AA-REVSION3
00076 *                                             56       AA-REVSION4
00077          10  MA-VALUE-1 OCCURS 12 TIMES
00078                             PIC S9(9)    COMP-3.
00079 *                                             57-116 VALUE FIELDS
00080 *                                             57-61 PRIOR LAND/
00081 *                                                   SEN FRZ EX VAL
00082 *                                             62-66 PRIOR IMPRV/
00083 *                                                   PRORATION FCTR
00084 *                                                   * USE S9(9) AS
00085 *                                                   S9(3)V9(6)  *
00086 *                                             67-71 PRIOR TOTAL/
00087 *                                                   ENTRPRS ZN VAL
00088 *                                             72-76 CURRNT-LAND
00089 *                                                 LONG TERM EX VAL
00090 *                                             77-81 CURR-IMPRV/
00091 *                                                   OCC FACTOR
00092 *                                             82-86    CURR-TOTAL
00093 *                                             87-91    PROPOSE-LND
00094 *                                             92-96    PROP-IMPRV
00095 *                                             97-101   PROP-TOTAL
00096 *                                            102-106   PRIOR-REG/
00097 *                                                      COMPL DATE
00098 *                                            107-111   CURRENT-REG
00099 *                                            112-116   PRIOR-PROP
00100          10  MA-NO-CALC-SF  PIC X.
00101 *                                            117     SEN FREEZE
00102 *                                                    NO CALC IND.
00103          10  MA-HO-RESPONSE PIC 9.
00104 *                                            118     HOMEOWNER
00105 *                                                    RESPONSE
00106          10  MA-REC-CTRS-1.
00107 *                                            119-122 RECORD CNTRS
00108              15  MA-SLS-CTR-1
00109                             PIC S9.
00110                  88  MA-NO-SALES      VALUE +0.
00111                  88  MA-SALES-PRESENT VALUE +1.
00112 *                                            119       SALES-CNTR
00113              15  MA-DTL-QST-CTR-1
00114                             PIC S9(3).
00115                  88  MA-NO-DETAIL     VALUE +0.
00116                  88  MA-DETAIL-PRESENT VALUE +1 THRU +350.
00117                  88  MA-MAXIMUM-DETAIL VALUE +350.
00118 *                                            120-122   DETAIL-CNTR
00119 *----------------------------------------------------------------*
00120 *   S A L E S  S E G M E N T, IF PRESENT, CONTAINS 34 BYTES      *
00121 *    DESCRIPTIONS RELATIVE TO 1ST BYTE IN EACH SEGMENT           *
00122 *                                                                *
00123      05  MA-SALES-1 OCCURS 0 TO 1 TIMES DEPENDING ON MA-SLS-CTR-1.
00124 *                                              1-34  SALES SEGMENT
00125          10  MA-DEEDTYP-1   PIC XX.
00126 *                                              1-2   DEED-TYPE
00127          10  MA-DEEDNO-1    PIC S9(9)    COMP-3.
00128 *                                              3-7   DEED-NUMBER
00129          10  MA-SLS-DAT1-1  PIC S9(7)    COMP-3.
00130 *                                              8-11  SALE-DATE1
00131 *                                                      (YYMMDD)
00132          10  MA-SLS-AMT1-1  PIC S9(9)    COMP-3.
00133 *                                             12-16  SALE-AMOUNT1
00134          10  MA-SLS-DAT2-1  PIC S9(7)    COMP-3.
00135 *                                             17-20  SALE-DATE2
00136 *                                                      (YYMMDD)
00137          10  MA-SLS-AMT2-1  PIC S9(9)    COMP-3.
00138 *                                             21-25  SALE-AMOUNT2
00139          10  MA-SLS-DAT3-1  PIC S9(7)    COMP-3.
00140 *                                             26-29  SALE-DATE3
00141 *                                                      (YYMMDD)
00142          10  MA-SLS-AMT3-1  PIC S9(9)    COMP-3.
00143 *                                             30-34  SALE-AMOUNT3
00144 *----------------------------------------------------------------*
00145 * D E T A I L  S E G M E N T S, IF PRESENT,EACH CONTAINS 53 BYTES
00146 *    DESCRIPTIONS RELATIVE TO 1ST BYTE IN EACH SEGMENT           *
00147 *                                                                *
00148      05  MA-DTL-QST-1
00149              OCCURS 0 TO 350 TIMES DEPENDING ON MA-DTL-QST-CTR-1.
00150          10  DM-TYP1.
00151 *                                              1-53  TYPE1-LAND
00152              15  D1M-MC     PIC S999     COMP-3.
00153 *                                              1-2   S1-MULTI-CODE
00154              15  D1M-TYP    PIC X.
00155                  88  D1M-TYPE1        VALUE '1'.
00156 *                                              3     S1-TYPE1
00157              15  D1M-CD     PIC X.
00158 *                                              4     S1-CODE 0/1
00159 *                                                    (0=VACANT)
00160 *                                                    (1=IMPROVED)
00161              15  D1M-DEC    PIC X.
00162 *                                              5     S1-DECIMAL
00163              15  D1M-UM     PIC XX.
00164 *                                              6-7   S1-UNIT-MEAS
00165              15  D1M-CLS    PIC S999     COMP-3.
00166 *                                              8-9   S1-CLASS
00167 *                                                    (MAJOR-MINOR)
00168              15  D1M-EXRR   PIC X.
00169 *                                             10     S1-EX-RR
00170              15  D1M-FF     PIC S9(7)    COMP-3.
00171 *                                             11-14  S1-FRONT-FT
00172              15  D1M-DPTH   PIC S9(5)    COMP-3.
00173 *                                             15-17  S1-DEPTH
00174              15  D1M-UPR    PIC S9(5)V99 COMP-3.
00175 *                                             18-21  S1-UNIT-PRICE
00176              15  D1M-DFCTR  PIC S99V999  COMP-3.
00177 *                                             22-24  S1-DPTH-FACTR
00178              15  D1M-CFCTR  PIC S9V9(4)  COMP-3.
00179 *                                             25-27  S1-CORNR-FCTR
00180              15  D1M-ECFCTR PIC SV9(5)   COMP-3.
00181 *                                             28-30  S1-EXTRA
00182 *                                                    CORNER FACTOR
00183              15  D1M-PCASSD PIC S99V9(5) COMP-3.
00184 *                                             31-34  S1-% ASSESSED
00185              15  D1M-EI     PIC S99V9    COMP-3.
00186                  88  D1M-ECON1        VALUE +22.0.
00187                  88  D1M-ECON2        VALUE +16.0.
00188                  88  D1M-ECON3        VALUE +33.0.
00189                  88  D1M-ECON4        VALUE +30.0.
00190                  88  D1M-ECON5        VALUE +37.0.
00191                  88  D1M-ECON6        VALUE +38.5.
00192                  88  D1M-ECON-IND     VALUE +16.0 +22.0 +30.0
00193                                             +33.0 +37.0 +38.5.
00194 *                                             35-36  S1-ECON-IND
00195              15  D1M-VAL    PIC S9(9)    COMP-3.
00196 *                                             37-41  S1-VALUATION
00197              15  FILLER     PIC X(11).
00198 *                                             42-52  FILLER
00199              15  D1M-UI     PIC X.
00200 *                                             53     S1-UNIT-IND
00201 *----------------------------------------------------------------*
00202          10  DM-TYP2 REDEFINES DM-TYP1.
00203 *                                              1-53  TYPE2-IMPRVMT
00204              15  D2M-MC     PIC S999     COMP-3.
00205 *                                              1-2   S2-MULTI-CODE
00206              15  D2M-TYP    PIC X.
00207                  88  D2M-TYPE2        VALUE '2'.
00208                  88  D2M-TYPE2-5      VALUE '2' THRU '5'.
00209 *                                              3     S2-TYPE2
00210              15  D2M-CD     PIC X.
00211 *                                              4     S2-CODE 2/3
00212 *                                                    2=MAJOR-IMPRV
00213 *                                                    3=MINOR-IMPRV
00214              15  D2M-DEC    PIC X.
00215 *                                              5     S2-DECIMAL
00216              15  D2M-UM     PIC XX.
00217 *                                              6-7   S2-UNIT-MEAS
00218              15  D2M-CLS    PIC S999     COMP-3.
00219                  88  D2M-QUES-CLASS   VALUE +202 THRU +212
00220                                             +234 +278 +295
00221                                             +402 THRU +412
00222                                             +434 +478 +495.
00223                  88  D2M-QCLS1-3      VALUE +210 +211 +212 +295
00224                                             +410 +411 +412 +495.
00225                  88  D2M-QCLS1OR5     VALUE +202 +203 +204
00226                                             +402 +403 +404.
00227                  88  D2M-QCLS2        VALUE +205 THRU +208 +278
00228                                             +405 THRU +408 +478.
00229                  88  D2M-QCLS2-3      VALUE +209 +409.
00230                  88  D2M-QCLS4        VALUE +234 +434.
00231                  88  D2M-QCLS6        VALUE +593.
00232                  88  D2M-QCLS7        VALUE +597.
00233 *                                              8-9   S2-CLASS
00234 *                                                    (MAJOR-MINOR)
00235              15  D2M-CDU    PIC XX.
00236 *                                             10-11  S2-CDU
00237              15  D2M-AREA   PIC S9(7)    COMP-3.
00238 *                                             12-15  S2-AREA
00239              15  D2M-UPR    PIC S9(5)V99 COMP-3.
00240 *                                             16-19  S2-UNIT-PRICE
00241              15  D2M-PRDCT  PIC S9(9)    COMP-3.
00242 *                                             20-24  S2-PRODUCT
00243              15  D2M-AGE    PIC S999     COMP-3.
00244                  88  D2M-0-AGE        VALUE +0.
00245                  88  D2M-OVER-80      VALUE +081 THRU +999.
00246 *                                             25-26  S2-AGE
00247              15  D2M-COND   PIC S99V9    COMP-3.
00248 *                                             27-28  S2-CONDITION
00249              15  D2M-PCASSD PIC S99V9(5) COMP-3.
00250 *                                             29-32  S2-% ASSESSED
00251              15  D2M-BUFF   PIC S999     COMP-3.
00252 *                                             33-34  S2-BUFF-NO
00253              15  FILLER     PIC XX.
00254 *                                             35-36  FILLER
00255              15  D2M-VAL    PIC S9(9)    COMP-3.
00256 *                                             37-41  S2-VALUATION
00257              15  D2M-KEYPCL PIC S9(15)   COMP-3.
00258 *                                             42-49  S2-KEY PARCEL
00259              15  D2M-SC     PIC X.
00260 *                                             50     S2-SPLIT CODE
00261              15  FILLER     PIC X(3).
00262 *                                             51-53  FILLER
00263 *----------------------------------------------------------------*
00264          10  DM-TYP3 REDEFINES DM-TYP1.
00265 *                                              1-53  TYPE3-IMPRVMT
00266              15  D3M-MC     PIC S999     COMP-3.
00267 *                                              1-2   S3-MULTI-CODE
00268              15  D3M-TYP    PIC X.
00269                  88  D3M-TYPE3        VALUE '3'.
00270 *                                              3     S3-TYPE3
00271              15  D3M-CD     PIC X.
00272 *                                              4     S3-CODE 2/3
00273 *                                                    2=MAJOR-IMPRV
00274 *                                                    3=MINOR-IMPRV
00275              15  FILLER     PIC X(3).
00276 *                                              5-7   FILLER
00277              15  D3M-CLS    PIC S999     COMP-3.
00278 *                                              8-9   S3-CLASS
00279 *                                                    (MAJOR-MINOR)
00280              15  D3M-CDU    PIC XX.
00281 *                                             10-11  S3-CDU
00282              15  D3M-REPCST PIC S9(9)    COMP-3.
00283 *                                             12-16  S3-REPRODUCTV
00284 *                                                    COST
00285              15  FILLER     PIC X(6).
00286 *                                             17-22  FILLER
00287              15  D3M-YR     PIC S999     COMP-3.
00288 *                                             23-24  S3-YEAR
00289              15  D3M-AGE    PIC S999     COMP-3.
00290                  88  D3M-0-AGE        VALUE +0.
00291                  88  D3M-OVER-80      VALUE +081 THRU +999.
00292 *                                             25-26  S3-AGE
00293              15  D3M-COND   PIC S99V9    COMP-3.
00294 *                                             27-28  S3-CONDITION
00295              15  D3M-PCASSD PIC S99V9(5) COMP-3.
00296 *                                             29-32  S3-% ASSESSED
00297              15  D3M-BUFF   PIC S999     COMP-3.
00298 *                                             33-34  S3-BUFF-NO
00299              15  FILLER     PIC XX.
00300 *                                             35-36  FILLER
00301              15  D3M-VAL    PIC S9(9)    COMP-3.
00302 *                                             37-41  S3-VALUATION
00303              15  D3M-KEYPCL PIC S9(15)   COMP-3.
00304 *                                             42-49  S3-KEY PARCEL
00305              15  D3M-SC     PIC X.
00306 *                                             50     S3-SPLIT CODE
00307              15  FILLER     PIC X(3).
00308 *                                             51-53  FILLER
00309 *----------------------------------------------------------------*
00310          10  DM-TYP4 REDEFINES DM-TYP1.
00311 *                                              1-53  TYPE4-IMPRVMT
00312              15  D4M-MC     PIC S999     COMP-3.
00313 *                                              1-2   S4-MULTI-CODE
00314              15  D4M-TYP    PIC X.
00315                  88  D4M-TYPE4        VALUE '4'.
00316 *                                              3     S4-TYPE4
00317              15  D4M-CD     PIC X.
00318 *                                              4     S4-CODE4
00319              15  FILLER     PIC X(3).
00320 *                                              5-7   FILLER
00321              15  D4M-CLS    PIC S999     COMP-3.
00322 *                                              8-9   S4-CLASS
00323 *                                                    (MAJOR-MINOR)
00324              15  D4M-CDU    PIC XX.
00325 *                                             10-11  S4-CDU
00326              15  D4M-REPCST PIC S9(9)    COMP-3.
00327 *                                             12-16  S4-REPRODUCTV
00328 *                                                    COST
00329              15  D4M-TOTVAL PIC S9(9)    COMP-3.
00330 *                                             17-21  S4-TOTAL-VALU
00331              15  FILLER     PIC X.
00332 *                                             22     FILLER
00333              15  D4M-OCCFAC PIC S99V9    COMP-3.
00334 *                                             23-24  S4-OCCUPANCY
00335 *                                                    FACTOR
00336              15  D4M-AGE    PIC S999     COMP-3.
00337                  88  D4M-0-AGE        VALUE +0.
00338                  88  D4M-OVER-AGE     VALUE +081 THRU +999.
00339 *                                             25-26  S4-AGE
00340              15  D4M-COND   PIC S99V9    COMP-3.
00341 *                                             27-28  S4-CONDITION
00342              15  D4M-PCASSD PIC S99V9(5) COMP-3.
00343 *                                             29-32  S4-% ASSESSED
00344              15  D4M-BUFF   PIC S999     COMP-3.
00345 *                                             33-34  S4-BUFF-NO
00346              15  FILLER     PIC XX.
00347 *                                             35-36  FILLER
00348              15  D4M-VAL    PIC S9(9)    COMP-3.
00349 *                                             37-41  S4-VALUATION
00350              15  D4M-KEYPCL PIC S9(15)   COMP-3.
00351 *                                             42-49  S4-KEY PARCEL
00352              15  D4M-SC     PIC X.
00353 *                                             50     S4-SPLIT CODE
00354              15  FILLER     PIC X(3).
00355 *                                             51-53  FILLER
00356 *----------------------------------------------------------------*
00357 *                                              1-53  TYPE5-IMPRVMT
00358          10  DM-TYP5 REDEFINES DM-TYP1.
00359              15  D5M-MC     PIC S999     COMP-3.
00360 *                                              1-2   S5-MULTI-CODE
00361              15  D5M-TYP    PIC X.
00362                  88  D5M-TYPE5        VALUE '5'.
00363 *                                              3     S5-TYPE5
00364              15  D5M-CD     PIC X.
00365 *                                              4     S5-CODE5
00366              15  FILLER     PIC X(3).
00367 *                                              5-7   FILLER
00368              15  D5M-CLS    PIC S999     COMP-3.
00369 *                                              8-9   S5-CLASS
00370 *                                                    (MAJOR-MINOR)
00371              15  D5M-CDU    PIC XX.
00372 *                                             10-11  S5-CDU
00373              15  D5M-REPCST PIC S9(9)    COMP-3.
00374 *                                             12-16  S5-REPRODUCTV
00375 *                                                    COST
00376              15  FILLER     PIC X(6).
00377 *                                             17-22  FILLER
00378              15  D5M-OCCFAC PIC S99V9    COMP-3.
00379 *                                             23-24  S5-OCCUPANCY
00380 *                                                    FACTOR
00381              15  D5M-AGE    PIC S999     COMP-3.
00382                  88  D5M-0-AGE        VALUE +0.
00383                  88  D5M-OVER-80      VALUE +081 THRU +999.
00384 *                                             25-26  S5-AGE
00385              15  D5M-COND   PIC S99V9    COMP-3.
00386 *                                             27-28  S5-CONDITION
00387              15  D5M-PCASSD PIC S99V9(5) COMP-3.
00388 *                                             29-32  S5-% ASSESSED
00389              15  D5M-BUFF   PIC S999     COMP-3.
00390 *                                             33-34  S5-BUFF-NO
00391              15  FILLER     PIC XX.
00392 *                                             35-36  FILLER
00393              15  D5M-VAL    PIC S9(9)    COMP-3.
00394 *                                             37-41  S5-VALUATION
00395              15  D5M-KEYPCL PIC S9(15)   COMP-3.
00396 *                                             42-49  S5-KEY PARCEL
00397              15  D5M-SC     PIC X.
00398 *                                             50     S5-SPLIT CODE
00399              15  FILLER     PIC X(3).
00400 *                                             51-53  FILLER
00401 *----------------------------------------------------------------*
00402 * --  Q U E S T I O N N A I R E  S E G M E N T  -- IS PRESENT,   *
00403 *      IF THE PRECEDING DETAIL SEGMENT IS - TYPE = '2' THRU '5'  *
00404 *      AND CLASS = 202-212,234,278,295,405-412,434,478,495       *
00405          10  MA-QST REDEFINES DM-TYP1.
00406 *                                              1-53  QUESTIONNAIRE
00407            12  QMFILL1.
00408                15  QM1      PIC 9.
00409                  88  QM1R-RESTYP1-5   VALUE 1 THRU 5.
00410                  88  QM1R-RESTYP1-3   VALUE 1 2 3 5.
00411                  88  QM1R-RESTYP1OR5  VALUE 1 5.
00412                  88  QM1R-RESTYP2     VALUE 2.
00413                  88  QM1R-RESTYP2-3   VALUE 2 3.
00414                  88  QM1R-RESTYP4     VALUE 4.
00415 *                                              1     TYPE-RESIDENC
00416                15  QM2      PIC 9.
00417 *                                              2     USE
00418                15  QM2A     PIC 9.
00419 *                                              3     NO. OF APTS.
00420 *                                   ---------- 4-5   EXTERIOR -
00421 *                                                     CONSTRUCTION
00422                15  QM3      PIC 9.
00423 *                                              4     WALLS
00424                15  QM4      PIC 9.
00425 *                                              5     ROOF
00426 *                                   ---------- 6-27  ROOMS,HEATING
00427 *                                                     AND PLUMBING
00428            12  QMFILL2.
00429                15  QM5A1    PIC S999     COMP-3.
00430 *                                              6-7   NO. OF ROOMS
00431            12  QMFILL3.
00432                15  QM5A2    PIC 99.
00433 *                                              8-9   NO. OF BEDRMS
00434                15  QM5B1    PIC 9.
00435 *                                             10     BASEMENT-TYPE
00436                15  QM5B2    PIC 9.
00437 *                                             11     BSMT-FINISH
00438                15  QM5C1    PIC 9.
00439 *                                             12     CENTRAL-HEAT
00440                15  QM5C2F   PIC 9.
00441 *                                             13     FLOOR-FURNACE
00442                15  QM5C2U   PIC 9.
00443 *                                             14     UNIT-HEATER
00444                15  QM5C2S   PIC 9.
00445 *                                             15     STOVE
00446                15  QM5C2O   PIC 9.
00447 *                                             16     SOLAR
00448                15  QM5C3    PIC 9.
00449 *                                             17     CENTRAL-AIR
00450                15  QM5C4    PIC 9.
00451 *                                             18     NO. FIREPLACE
00452                15  QM5C41   PIC 9.
00453 *                                             19     1ST FLR FIREP
00454                15  QM5C42   PIC 9.
00455 *                                             20     2ND FLR FIREP
00456                15  QM5C43   PIC 9.
00457 *                                             21     3RD FLR FIREP
00458                15  QM5C4B   PIC 9.
00459 *                                             22     BSMT FIREPLCE
00460                15  QM5D1    PIC 9.
00461 *                                             23     ATTIC-TYPE
00462                15  QM5D2    PIC 9.
00463 *                                             24     ATTIC-FINISH
00464                15  QM5E1    PIC 99.
00465 *                                             25-26  NO. FULL-BATH
00466                15  QM5E2    PIC 9.
00467 *                                             27     NO. HALF-BATH
00468                15  QM6A     PIC 9.
00469 *                                             28     TYPE OF DESGN
00470 *                                                    ARCHITECT OR
00471 *                                                    STOCK PLAN
00472                15  QM6B     PIC 9.
00473 *                                             29     TYPE OF DESGN
00474 *                                                    TYPICAL OR
00475 *                                                    ATYPICAL
00476                15  QM7A     PIC 9.
00477 *                                             30     CONST QUALITY
00478 *                                                    1 = DELUXE
00479 *                                                    2 = AVERAGE
00480 *                                                    3 = POOR
00481                15  QM7B     PIC 9.
00482 *                                             31     CONST QUALITY
00483 *                                                    1 = RENOVATED
00484                15  QM8      PIC 9.
00485 *                                             32     SITE DESRBLTY
00486 *                                   ----------33-36  GARAGE - INFO
00487                15  QM9A     PIC 9.
00488 *                                             33     GARAGE-SIZE
00489                15  QM9B     PIC 9.
00490 *                                             34     GARAGE-CONST.
00491                15  QM9C     PIC 9.
00492 *                                             35     GARAGE-ATTACH
00493                15  QM9D     PIC 9.
00494 *                                             36     GRAGE IN AREA
00495 *                                   ----------37-40  2ND GRGE INFO
00496                15  QM10A    PIC 9.
00497 *                                             37     GARAGE-SIZE
00498                15  QM10B    PIC 9.
00499 *                                             38     GARAGE-CONST.
00500                15  QM10C    PIC 9.
00501 *                                             39     GARAGE-ATTACH
00502                15  QM10D    PIC 9.
00503 *                                             40     GRAGE IN AREA
00504                15  QM11     PIC 9.
00505 *                                             41     PORCH
00506            12  QMFILL4.
00507                15  QM12     PIC S9(7)    COMP-3.
00508 *                                   ----------42-45  OTHER -
00509 *                                                     IMPROVEMENTS
00510                15  QM13     PIC S9(7)    COMP-3.
00511 *                                             46-49  BLDG-SQ-FT
00512            12  QMFILL5.
00513                15  QM16     PIC 9.
00514 *                                             50     ST OF REPAIR
00515                15  QM17     PIC 9.
00516 *                                             51     SPECIAL COND
00517            12  QMFILL6.
00518                15  FILLER   PIC XX.
00519 *                                             52-53  FILLER
00520 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
