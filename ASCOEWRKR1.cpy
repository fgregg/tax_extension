00001 ******************************************************************
00002 * MODIFICATION: 29 APR 92. SEE SPEC.                             *
00003 ******************************************************************
00004 ******************************************************************
00005 * MODIFICATION: 11/04/2004. TJM. INCREASE SIZE OF C500-BACERTNO  *
00006 ******************************************************************
00007 *----------------------------------------------------------------*
00008      05  C500-TOWN          PIC 999       COMP-3.
00009 *                                              1-2   TOWN
00010      05  C500-VOL           PIC 999       COMP-3.
00011 *                                              3-4   VOLUME
00012      05  C500-PROP          PIC 9(15)     COMP-3.
00013 *                                              5-12  PIN
00014      05  C500-PROCYR        PIC 99.
00015 *                                             13-14  PROCESS YEAR
00016      05  C500-TXYR          PIC 99.
00017 *                                             15-16  TAX YEAR
00018      05  C500-TXTYP         PIC 9.
00019 *                                             17-17  TAX TYPE
00020      05  C500-TXSTAT        PIC 9.
00021 *                                             18-18  TAX STATUS
00022      05  C500-BACLS         PIC 9(7)      COMP-3.
00023 *                                             19-22  BD/APPLS
00024 *                                                      CLASS
00025      05  C500-AS1ACT        PIC 9.
00026 *                                             23-23  ASSESSOR
00027 *                                                      ACTION 1
00028      05  C500-AS2ACT        PIC 9.
00029 *                                             24-24  ASSESSOR
00030 *                                                      ACTION 2
00031      05  C500-BAACT         PIC 9.
00032 *                                             25-25  BD/APPLS
00033 *                                                      ACTION
00034      05  C500-BALAND        PIC 9(9)      COMP-3.
00035 *                                             26-30  BD/APPLS
00036 *                                                      LAND VAL
00037      05  C500-BAIMP         PIC 9(9)      COMP-3.
00038 *                                             31-35  BD/APPLS
00039 *                                                      IMP VAL
00040      05  C500-BATOT         PIC 9(9)      COMP-3.
00041 *                                             36-40  BD/APPLS
00042 *                                                      TOT VAL
00043      05  C500-BACERTNO      PIC 9(7)      COMP-3.
00044 *                                             41-44  CERTIFICATE
00045 *                                                      NUMBER
00046      05  C500-ISSDTE        PIC 9(9)      COMP-3.
00047 *                                             45-49  DATE ISSUED
00048 *                                                      (0MMDDYYYY)
00049      05  C500-CTLNUM        PIC 9(5)      COMP-3.
00050 *                                             50-52  CONTROL
00051 *                                                      NUMBER
00052      05  C500-ISSBY         PIC X(8).
00053 *                                             53-60  CERTIFICATE
00054 *                                                      ISSUED BY
00055      05  C500-OCCFAC        PIC 9(4)V9    COMP-3.
00056 *                                             61-63  CERTIFIED
00057 *                                                    HOMEOWNER
00058 *                                                    OCCUP FCT
00059      05  C500-PROFCTR       PIC 9V9(6)    COMP-3.
00060 *                                             64-67  CERTIFIED
00061 *                                                    HOMEOWNER
00062 *                                                    PRORATE FCT
00063      05  C500-EQVAL         PIC 9(9)      COMP-3.
00064 *                                             68-72  CERTIFIED
00065 *                                                    1977 BASE YR
00066 *                                                    VALUE
00067      05  C500-SEGCTR        PIC 999.
00068 *                                             73-75  SEGMENT
00069 *                                                      COUNTER
00070      05  FILLER             PIC X(05).
00071 *                                             76-80  FILLER
00072      05  C500-COFEACTION
00073              OCCURS 0 TO 50 TIMES DEPENDING ON C500-SEGCTR.
00074          10  C500-ACTNO     PIC 999.
00075 *                                              1-3   KEY -
00076 *                                                     ACTION NO.
00077          10  C500-ACTNO-RD  REDEFINES
00078              C500-ACTNO     PIC XXX.
00079 *
00080          10  C500-TYPE      PIC 99.
00081 *                                              4-5   C OF E TYPE
00082          10  C500-STAT      PIC 9.
00083 *                                              6-6   C OF E STATUS
00084          10  C500-REASON    PIC 99.
00085 *                                              7-8   C OF E REASON
00086          10  C500-PRTIND    PIC 9.
00087 *                                              9-9   PRINT
00088 *                                                     INDICATOR
00089          10  C500-ADJIND    PIC 9.
00090 *                                             10-10  ADJUDICATED
00091 *                                                     INDICATOR
00092          10  C500-TXCD      PIC 9(5)      COMP-3.
00093 *                                             11-13  TAX CODE
00094          10  C500-TXRTE     PIC 9(6)V9(3) COMP-3.
00095 *                                             14-18  TAX RATE
00096          10  C500-AMTDUE    PIC 9(11)V99  COMP-3.
00097 *                                             19-25  AMOUNT DUE
00098          10  C500-AMTPD     PIC 9(11)V99  COMP-3.
00099 *                                             26-32  AMOUNT PAID
00100          10  C500-APPLNAME  PIC X(22).
00101 *                                             33-54  APPLICANT'S
00102 *                                                     NAME
00103          10  C500-APPLADDR  PIC X(22).
00104 *                                             55-76  APPLICANT'S
00105 *                                                     ADDRESS
00106          10  C500-CITY      PIC X(12).
00107 *                                             77-88  CITY
00108          10  C500-ST        PIC XX.
00109 *                                             89-90  STATE
00110          10  C500-ZIP       PIC 9(9)      COMP-3.
00111 *                                             91-95  ZIPCODE
00112          10  C500-APPLPHONO PIC X(11).
00113 *                                             96-106 APPLICANT'S
00114 *                                                     PHONE NO.
00115          10  C500-SECAPPLNM PIC X(22).
00116 *                                            107-128 SECONDARY
00117 *                                                     APPLICANT'S
00118 *                                                     NAME
00119          10  C500-ASSCERTVALS.
00120 *                                            129-152 ASSESSOR'S
00121 *                                                     CERTIFIED
00122 *                                                     VALUES
00123              15  C500-CASSVAL   PIC 9(13)     COMP-3.
00124 *                                            129-135 CERTIFIED
00125 *                                                     ASSESSED
00126 *                                                     VALUE
00127              15  C500-CHOEXAMT  PIC 9(13)     COMP-3.
00128 *                                            136-142 CERTIFIED
00129 *                                                     HOMEOWNER
00130 *                                                     EXEMPTION
00131 *                                                     AMOUNT
00132              15  C500-CHCOOPQTY PIC 9(5)      COMP-3.
00133 *                                            143-145 CERTIFIED
00134 *                                                     HOMESTEAD
00135 *                                                     COOP QTY
00136              15  C500-CHEXAMT   PIC 9(13)     COMP-3.
00137 *                                            146-152 CERTIFIED
00138 *                                                     HOMESTEAD
00139 *                                                     EXEMPTION
00140 *                                                     AMOUNT
00141          10  C500-ASSRECVALS.
00142 *                                            153-202 ASSESSOR'S
00143 *                                                     RECOMMENDED
00144 *                                                     VALUES
00145              15  C500-RASSVAL   PIC 9(13)     COMP-3.
00146 *                                            153-159 RECOMMENDED
00147 *                                                     ASSESSED
00148 *                                                     VALUE
00149              15  C500-RORGEQVAL PIC 9(13)     COMP-3.
00150 *                                            160-166 RECOMMENDED
00151 *                                                     ORIGINAL
00152 *                                                     EQUALIZED
00153 *                                                     VALUE
00154              15  C500-R77HOBSVAL PIC 9(13)    COMP-3.
00155 *                                            167-173 RECOMMENDED
00156 *                                                     1977
00157 *                                                     HOMEOWNER
00158 *                                                     BASE VALUE
00159              15  C500-RHOPRO    PIC 9V9(6)    COMP-3.
00160 *                                            174-177 RECOMMENDED
00161 *                                                     HOMEOWNER
00162 *                                                     PRORATION
00163              15  C500-RHOOCCFAC PIC 9(4)V9    COMP-3.
00164 *                                            178-180 RECOMMENDED
00165 *                                                     HOMEOWNER
00166 *                                                     OCCUPANCY
00167 *                                                     FACTOR
00168              15  C500-RHOCOOPQTY PIC 9(5)     COMP-3.
00169 *                                            181-183 RECOMMENDED
00170 *                                                     HOMEOWNER
00171 *                                                     COOP QTY
00172              15  C500-RHOEXAMT  PIC 9(13)     COMP-3.
00173 *                                            184-190 RECOMMENDED
00174 *                                                     HOMEOWNER
00175 *                                                     EXEMPTION
00176 *                                                     AMOUNT
00177              15  C500-RHCOOPQTY PIC 9(5)      COMP-3.
00178 *                                            191-193 RECOMMENDED
00179 *                                                     HOMESTEAD
00180 *                                                     COOP QTY
00181              15  C500-RTXAMT    PIC 9(11)V99  COMP-3.
00182 *                                            194-200 RECOMMENDED
00183 *                                                     TAX AMOUNT
00184          10  C500-ADJVALS.
00185 *                                            201-221 ADJUDICATED
00186 *                                                     VALUES
00187              15  C500-ANEWASSVAL PIC 9(13)    COMP-3.
00188 *                                            201-207 ADJUDICATED
00189 *                                                     NEW ASSESSED
00190 *                                                     VALUE
00191              15  C500-ANEWEQVAL PIC 9(13)     COMP-3.
00192 *                                            208-214 ADJUDICATED
00193 *                                                     NEW
00194 *                                                     EQUALIZED
00195 *                                                     VALUE
00196              15  C500-AADJTXAMT PIC 9(11)V99  COMP-3.
00197 *                                            215-221 ADJUDICATED
00198 *                                                     ADJUSTED
00199 *                                                     TAX AMOUNT
00200          10  C500-TOBADTE       PIC 9(9)      COMP-3.
00201 *                                            222-226 DATE TO B/A
00202 *                                                     FROM ASSR
00203 *                                                     (0MMDDYYYY)
00204          10  C500-FRBADTE       PIC 9(9)      COMP-3.
00205 *                                            227-231 DATE FROM
00206 *                                                     B/A TO ASSR
00207 *                                                     (0MMDDYYYY)
00208          10  C500-TOSADTE       PIC 9(9)      COMP-3.
00209 *                                            232-236 DATE TO S.A.
00210 *                                                     FROM ASSR
00211 *                                                     (0MMDDYYYY)
00212          10  C500-FRSADTE       PIC 9(9)      COMP-3.
00213 *                                            237-241 DATE FROM
00214 *                                                     S.A. TO ASSR
00215 *                                                     (0MMDDYYYY)
00216          10  C500-TOTRSDTE      PIC 9(9)      COMP-3.
00217 *                                            242-246 DATE TO
00218 *                                                     TREASURER
00219 *                                                     (0MMDDYYYY)
00220          10  C500-ADJDTE        PIC 9(9)      COMP-3.
00221 *                                            247-251 ADJUDICATION
00222 *                                                     DATE
00223 *                                                     (0MMDDYYYY)
00224          10  C500-AMNDMTNO      PIC 999       COMP-3.
00225 *                                            252-253 AMENDMENT
00226 *                                                     NUMBER
00227          10  C500-AMNDMTDTE     PIC 9(9)      COMP-3.
00228 *                                            254-258 AMENDMENT
00229 *                                                     DATE
00230 *                                                     (0MMDDYYYY)
00231          10  C500-UPDTDTE       PIC 9(9)      COMP-3.
00232 *                                            259-263 UPDATE DATE
00233 *                                                     (0MMDDYYYY)
00234          10  C500-UPDTID        PIC X(8).
00235 *                                            264-271 UPDATE I.D.
00236          10  C500-RFNDDTE       PIC 9(9)      COMP-3.
00237 *                                            272-276 REFUND DATE
00238 *                                                     (0MMDDYYYY)
00239          10  C500-ACCTNO        PIC 999       COMP-3.
00240 *                                            277-278 ACCOUNT NO.
00241          10  C500-DONO          PIC X(7).
00242 *                                            279-285 D & O NO.
00243          10  C500-EXVAL         PIC 9(13)     COMP-3.
00244 *                                            286-292 EXCESS
00245 *                                                     VALUATION
00246          10  C500-JRCR          PIC 9(11)V99  COMP-3.
00247 *                                            293-299 AMOUNT OF
00248 *                                                     JR CREDIT
00249          10  C500-RFND          PIC 9(11)V99  COMP-3.
00250 *                                            300-306 REFUND
00251 *                                                     AMOUNT
00252          10  C500-INT-DATE      PIC 9(7)      COMP-3.
00253 *                                            307-310 INTEREST
00254 *                                                      DATE
00255          10  C500-INT-PERC      PIC 99V9(3)   COMP-3.
00256 *                                            311-313 INTEREST
00257 *                                                     PERCENT
00258          10  C500-SSN           PIC 9(9)      COMP-3.
00259 *                                            314-318 SOCIAL
00260 *                                                     SECURITY
00261 *                                                     NUMBER
00262          10  C500-WCNAME        PIC X(22).
00263 *                                            319-340 WILL
00264 *                                                     CALL
00265 *                                                     NAME
00266          10  C500-WCPHONE       PIC 9(11)     COMP-3.
00267 *                                            341-346 WILL
00268 *                                                     CALL
00269 *                                                     PHONE
00270 *                                                     NUMBER
00271          10  C500-FEDID         PIC 9(9)      COMP-3.
00272 *                                            347-351 FEDERAL
00273 *                                                     I.D.
00274 *                                                     NUMBER
00275          10  C500-INT-AMT       PIC 9(9)V99  COMP-3.
00276 *                                            352-357 INTEREST
00277 *                                                    AMOUNT
00278          10  C500-RSF-RESP      PIC X.
00279 *                                            358-358 RSF RESPONSE
00280          10  C500-RSF-VALUE     PIC S9(9)     COMP-3.
00281 *                                            359-363 RSF VALUE
00282          10  C500-ISSUE-DTE     PIC 9(9)      COMP-3.
00283 *                                            364-368 ACTION
00284 *                                                    ISSUE DATE
00285 *                                                    (0MMDDYYYY)
00286          10  C500-TRCK-DATE     PIC 9(9)      COMP-3.
00287 *                                            369-373 TRACK DATE
00288 *                                                    (0MMDDYYYY)
00289          10  C500-TRCK-DISP     PIC 99.
00290 *                                            374-375 TRACKING
00291 *                                                    DISPOSITION
00292          10  C500-CSF-RESP      PIC X.
00293 *                                            376-376 CERTIFIED
00294 *                                                    SEN FREEZE
00295 *                                                    RESPONSE
00296          10  C500-CLANDVAL      PIC 9(9)      COMP-3.
00297 *                                            377-381 CERTIFIED
00298 *                                                    LAND
00299 *                                                    VALUATION
00300          10  C500-CBLDGVAL      PIC 9(9)      COMP-3.
00301 *                                            382-386 CERTIFIED
00302 *                                                    BUILDING
00303 *                                                    VALUATION
00304          10  C500-RLANDVAL      PIC 9(9)      COMP-3.
00305 *                                            387-391 RECOMMENDED
00306 *                                                    LAND
00307 *                                                    VALUATION
00308          10  C500-RBLDGVAL      PIC 9(9)      COMP-3.
00309 *                                            392-396 RECOMMENDED
00310 *                                                    BUILDING
00311 *                                                    VALUATION
00312          10  C500-CSF-VALUE     PIC 9(9)      COMP-3.
00313 *                                            397-401 CERTIFIED
00314 *                                                    SENIOR FREEZE
00315 *                                                    VALUATION
00316          10  C500-INT-DTE       PIC 9(9)      COMP-3.
00317 *                                            402-406 INTEREST
00318 *                                                    DATE
00319          10  C500-CHK-DATE      PIC 9(9)      COMP-3.
00320 *                                            407-411 CHECK
00321 *                                                    DATE
00322          10  C500-CHK-NO        PIC 9(7)      COMP-3.
00323 *                                            412-415 CHECK
00324 *                                                    NUMBER
00325          10  C500-WCADDR        PIC X(22).
00326 *                                            416-437 WILL CALL
00327 *                                                    ADDRESS
00328          10  FILLER REDEFINES C500-WCADDR.
00329              15  C500-CHOBASE       PIC 9(9)  COMP-3.
00330 *                                            416-420 CERT HOME
00331 *                                                    ASSD VALUE
00332              15  C500-TRS-TRKDTE    PIC 9(9)  COMP-3.
00333 *                                            421-425 TREASURER
00334 *                                                  TRACKING DATE
00335              15  C500-CDISEXM    PIC 9(5)     COMP-3.
00336 *                                            426-428 CERTIFIED
00337 *                                               DISABLED/EXEMPTION
00338              15  C500-RDISEXM    PIC 9(5)     COMP-3.
00339 *                                            429-431 RECOMMENDED
00340 *                                               DISABLED/EXEMPTION
00341              15  FILLER         PIC X(06).
00342 *                                            432-437 FILLER
00343 *
00344          10  C500-WCCITY        PIC X(12).
00345 *                                            438-449 WILL CALL
00346 *                                                    CITY
00347          10  FILLER REDEFINES C500-WCCITY.
00348              15  C500-CHOPRO        PIC 9V9(6)   COMP-3.
00349 *                                            438-441 CERTIFIED
00350 *                                                    HOMEOWNER
00351 *                                                    PRORATION
00352              15  C500-CHOCOOPQTY    PIC 9(5)     COMP-3.
00353 *                                            442-444 CERTIFIED
00354 *                                                    HOMEOWNER
00355 *                                                    COOP QTY
00356              15  C500-CHOOCCFAC     PIC 9(4)V9   COMP-3.
00357 *                                            445-447 CERTIFIED
00358 *                                                    HOMEOWNER
00359 *                                                    OCC FACTOR
00360              15  FILLER             PIC XX.
00361 *                                            448-449 FILLER
00362          10  C500-WCST          PIC XX.
00363 *                                            450-451 WILL CALL
00364 *                                                    STATE
00365          10  FILLER REDEFINES C500-WCST.
00366              15  C500-CHO-RESP  PIC X.
00367              15  C500-CHS-RESP  PIC X.
00368 *
00369          10  C500-WCZIP         PIC 9(9)      COMP-3.
00370 *                                            452-456 WILL CALL
00371 *                                                    ZIP CODE
00372          10  C500-RFNDIND       PIC X.
00373 *                                            457-457 REFUND
00374 *                                                     INDICATOR
00375          10  C500-HEXAMT        PIC 9(13)     COMP-3.
00376 *                                            458-464 RECOMMENDED
00377 *                                                     HOMESTEAD
00378 *                                                     EXEMPTION
00379 *                                                     AMOUNT
00380          10  C500-RHORESP       PIC X.
00381 *                                            465-465 HOMEOWNER
00382 *                                                     RESPONSE
00383          10  C500-RHSRESP       PIC X.
00384 *                                            466-466 HOMESTEAD
00385 *                                                     RESPONSE
00386          10  C500-CEQVAL        PIC 9(13)     COMP-3.
00387 *                                            467-473 CERTIFIED
00388 *                                                     EQUALIZED
00389 *                                                     VALUATION
00390          10  C500-JRNO          PIC X(6).
00391 *                                            474-479 JRNO
00392          10  C500-AMNDIND       PIC X.
00393 *                                            480-480 AMENDMENT
00394 *                                                    INDICATOR
00395          10  C500-RLT-RESP      PIC X.
00396 *                                            481-481 RECOMMENDED
00397 *                                                    LT RESPONSE
00398          10  C500-RLT-VALUE     PIC 9(9)      COMP-3.
00399 *                                            482-486 RECOMMENDED
00400 *                                                    LT VALUE
00401          10  C500-CLT-RESP      PIC X.
00402 *                                            487-487 CERTIFIED
00403 *                                                    LT RESPONSE
00404          10  C500-CLT-VALUE     PIC 9(9)      COMP-3.
00405 *                                            488-492 CERTIFIED
00406 *                                                    LT VALUE
00407          10  FILLER             PIC X(38).
00408 *                                            493-530 AVAILABLE
00409 *----------------------------------------------------------------*
