00001  IDENTIFICATION DIVISION.
00002      SKIP1
00003  PROGRAM-ID. ASHMA828.
00004 *AUTHORS. SQUILLACE-MOLIS.
00005 *DATE-WRITTEN. MAY 20,1995.
00006 *REMARKS. THIS PROGRAM UPDATES THE DETAIL SEGMENTS OF THE
00007 *         PROPERTY INFORMATION DATA BASE FROM A FILE
00008 *         CONTAINING QUALIFYING DATA FROM THE ASSESSOR'S
00009 *         OFFICE.
00010 *         MODIFICATIONS MADE ON DEC 13,1995 - SQUILLACE
00011 *         MODIFICATIONS MADE ON APR 08,1996 - SQUILLACE
00012 *         MODIFICATIONS MADE ON JUN 07,1996 - SQUILLACE
00013      SKIP1
00014  ENVIRONMENT DIVISION.
00015  CONFIGURATION SECTION.
00016      SKIP1
00017  SOURCE-COMPUTER. IBM-370.
00018  OBJECT-COMPUTER. IBM-370.
00019      SKIP1
00020  INPUT-OUTPUT SECTION.
00021  FILE-CONTROL.
00022      SKIP1
00023      SELECT CNTRL-FILE    ASSIGN TO UT-S-CNTRLFLE.
00024      SKIP1
00025      SELECT MSGTBL-FILE     ASSIGN TO DA-MSGTBL
00026                             ORGANIZATION IS INDEXED
00027                             ACCESS MODE IS RANDOM
00028                             RECORD KEY IS  DT-KEY
00029                             FILE STATUS IS FILE-STATUS.
00030
00031      SKIP1
00032      SELECT UPDATE-FILE   ASSIGN TO UT-S-UPDATEF.
00033      SELECT UPDATE-REPORT ASSIGN TO UT-S-UPDATER.
00034      SELECT UPD-ERR-REP   ASSIGN TO UT-S-UPDATEER.
00035      SKIP1
00036  DATA DIVISION.
00037  FILE SECTION.
00038  FD  CNTRL-FILE
00039      LABEL RECORDS ARE STANDARD
00040      BLOCK CONTAINS 0 RECORDS
00041      RECORDING MODE IS F
00042      RECORD CONTAINS 80 CHARACTERS
00043      DATA RECORD IS CNTRL-CARD.
00044  01  CNTRL-CARD.
00045      05  CK-PT-CNTL-VAL     PIC X(4).
00046      05  CC-CICV.
00047          10 CC-CICV-N       PIC 9(4).
00048      05  C-SPACE            PIC X(72).
00049      SKIP1
00050  FD  MSGTBL-FILE
00051      BLOCK CONTAINS 0 RECORDS
00052      RECORD CONTAINS 68  CHARACTERS
00053      LABEL RECORDS ARE STANDARD
00054      DATA RECORD IS DP-TABLE-RECORD.
00055  COPY DPMSGTBLRD.
00056      EJECT
00057  FD  UPDATE-FILE
00058      BLOCK CONTAINS 0 RECORDS
00059      RECORDING MODE IS F
00060      RECORD CONTAINS 976 CHARACTERS
00061      LABEL RECORDS ARE STANDARD
00062      DATA RECORD IS DP-JOURNAL-REC.
00063  01  DP-JOURNAL-REC.
00064 *                                            1-31    DP JOURNAL
00065 *                                                  MASTER RECORD
00066      05  DP-JOURNAL-CODE    PIC X.
00067 *                                            1       'J'
00068      05  DP-TRANS-DATE      PIC 9(5) PACKED-DECIMAL.
00069 *                                            2-4     (YYDDD)
00070      05  DP-RECORD-NO       PIC 9(3) PACKED-DECIMAL.
00071 *                                            5-6     RECORD NO.
00072      05  DP-PREFIX-LENGTH   PIC 9(4) COMP.
00073 *                                            7-8     PREFIX
00074      05  FILLER             PIC 9(4) COMP.
00075 *                                            9-10
00076      05  DP-FLAG            PIC X.
00077 *                                           11       FLAG
00078      05  DP-TASK-NO         PIC 9(5) PACKED-DECIMAL.
00079 *                                           12-14    TASK NO.
00080      05  DP-REQUEST-TIME    PIC 9(7) PACKED-DECIMAL.
00081 *                                           15-18    (HHMMSS)
00082      05  DP-TRANS-ID        PIC X(4).
00083 *                                           19-22    TRANS. ID
00084      05  DP-TERMINAL-ID     PIC X(4).
00085 *                                           23-26    TERMINAL ID
00086      05  DP-FILE-ID         PIC X(2).
00087 *                                           27-28    FILE ID.
00088      05  DP-TYPE-ID         PIC X(2).
00089 *                                           29-30    TYPE ID.
00090      05  DP-TRANS-CODE      PIC X.
00091 *                                           31       TRANS CODE
00092 *                   'N' - NONE BEFORE ADDING A DETAIL SEGMENT
00093 *                   'W' - ADD A DETAIL SEGMENT
00094 *                   'G' - READ A DETAIL SEGMENT FOR UPDATE
00095 *                   'P' - REWRITE A DETAIL SEGMENT
00096 *-------------------------------------------------------
00097      05  ASAIS-RECORD.
00098 *                                            1-95    ASAIS
00099 *                                                  JOURNAL REC
00100          10  AS-TRANS-CODE    PIC X(4).
00101 *                                            1-4     TRANS. CODE
00102          10  AS-USER-ID       PIC X(8).
00103 *                                            5-12    USER ID
00104          10  AS-PROPERTY-NO   PIC X(14).
00105 *                                           13-26    PROP. NUMBER
00106          10  AS-PROCESS-YEAR  PIC X(2).
00107 *                                           27-28    PROCESS YR.
00108          10  AS-TAX-YEAR      PIC X(2).
00109 *                                           29-30    TAX YEAR
00110          10  AS-TAX-TYPE      PIC X.
00111 *                                           31       TAX TYPE
00112          10  AS-RECORD-CODE   PIC X.
00113 *                                           32       RECORD CODE
00114          10  FILLER           PIC X(63).
00115 *                                           33-95    FILLER
00116 *-------------------------------------------------------
00117 *               *  SENIOR FREEZE - DETAIL SEGMENT  *
00118      05  C150-U-SENFRZDETAIL.
00119 *                                              1-850 SENIOR
00120 *                                                     FREEZE
00121 *                                                     DETAIL
00122 *                                                     SEGMENT
00123          10  C150-U-BIRTHDTE PIC X(8).
00124 *                                              1-8   KEY -
00125 *                                                     BIRTH DATE
00126 *                                                     (MMDDCCYY)
00127          10  C150-U-APPLSTNAM PIC X(20).
00128 *                                              9-28  APPLICANT
00129 *                                                     LAST NAME
00130          10  C150-U-APPFSTNAM PIC X(15).
00131 *                                             29-43  APPLICANT
00132 *                                                     FIRST NAME
00133          10  C150-U-APPMI   PIC X.
00134 *                                             44-44  APPLICANT
00135 *                                                     MIDDLE INIT
00136          10  C150-U-APPTITLE PIC XX.
00137 *                                             45-46  APPLICANT
00138 *                                                     TITLE
00139          10  C150-U-APPOLDNAM PIC X(22).
00140 *                                             47-68  APPLICANT
00141 *                                                     OLD NAME
00142          10  C150-U-APPMAIL.
00143 *                                             69-113 APPLICANT
00144 *                                                     MAILING
00145 *                                                     ADDRESS
00146              15  C150-U-APPADDR PIC X(22).
00147 *                                             69-90  ADDRESS
00148              15  C150-U-APPCITY PIC X(12).
00149 *                                             91-102 CITY
00150              15  C150-U-APPSTATE PIC XX.
00151 *                                            103-104 STATE
00152              15  C150-U-APPZIP PIC 9(9).
00153 *                                            105-113 ZIP CODE
00154          10  C150-U-MAINTIND PIC 9.
00155 *                                            114-114 MAINTENANCE
00156 *                                                     INDICATOR
00157 *                                                     FOR ABOVE
00158 *                                                     5 FIELDS
00159          10  C150-U-SSN     PIC 9(11).
00160 *                                            115-125 SOCIAL
00161 *                                                     SECURITY
00162 *                                                     NUMBER
00163          10  C150-U-AGE     PIC 999.
00164 *                                            126-128 AGE
00165          10  C150-U-PHONE   PIC 9(10).
00166 *                                            129-138 TELEPHONE
00167 *                                                     NUMBER
00168          10  C150-U-MARSTAT PIC X.
00169 *                                            139-139 MARITAL
00170 *                                                     STATUS
00171          10  C150-U-SPSREDIND PIC X.
00172 *                                            150-U-150 SPOUSE
00173 *                                                     RESIDENTIAL
00174 *                                                     INDICATOR
00175          10  C150-U-SPSLSTNAM PIC X(20).
00176 *                                            141-160 SPOUSE
00177 *                                                     LAST NAME
00178          10  C150-U-SPSFSTNAM PIC X(15).
00179 *                                            161-175 SPOUSE
00180 *                                                     FIRST NAME
00181          10  C150-U-SPSMI   PIC X.
00182 *                                            176-176 SPOUSE
00183 *                                                     MIDDLE INIT
00184          10  C150-U-SPSTITLE PIC XX.
00185 *                                            177-178 SPOUSE
00186 *                                                     TITLE
00187          10  C150-U-SPSMAIL.
00188 *                                            179-250 SPOUSE
00189 *                                                     MAILING
00190 *                                                     ADDRESS
00191              15  C150-U-SPSHSENO PIC X(5).
00192 *                                            179-183 HOUSE NO.
00193              15  C150-U-SPSDIR PIC XX.
00194 *                                            184-185 DIRECTION
00195              15  C150-U-SPSSTREET PIC X(22).
00196 *                                            186-207 STREET
00197              15  C150-U-SPSSUFF PIC X(4).
00198 *                                            208-211 SUFFIX
00199              15  C150-U-SPSCITY PIC X(28).
00200 *                                            212-239 CITY
00201              15  C150-U-SPSSTATE PIC XX.
00202 *                                            240-241 STATE
00203              15  C150-U-SPSZIP PIC 9(9).
00204 *                                            242-250 ZIP CODE
00205          10  FILLER         PIC X(3).
00206 *                                            251-253 FILLER
00207          10  C150-U-SFSTAT  PIC X.
00208 *                                            254-254 STATUS
00209          10  C150-U-DENIALDTE PIC 9(8).
00210 *                                            255-262 DENIAL
00211 *                                                     DATE
00212 *                                                     (CCYYMMDD)
00213          10  C150-U-FSTAPPDTE PIC 9(8).
00214 *                                            263-270 FIRST APP
00215 *                                                     RECEIVED
00216 *                                                     DATE
00217 *                                                     (CCYYMMDD)
00218          10  C150-U-LSTAPPDTE PIC 9(8).
00219 *                                            271-278 LAST APP
00220 *                                                     RECEIVED
00221 *                                                     DATE
00222 *                                                     (CCYYMMDD)
00223          10  C150-U-QUALDTE PIC 9(8).
00224 *                                            279-286 QUALIFIED
00225 *                                                     DATE
00226 *                                                     (CCYYMMDD)
00227          10  C150-U-RTNDDTE PIC 9(8).
00228 *                                            287-294 RETURNED
00229 *                                                     DATE
00230 *                                                     (CCYYMMDD)
00231          10  C150-U-MANRTNCODES.
00232 *                                            295-314 MANUAL
00233 *                                                     RETURN CODE
00234              15  C150-U-MANRTNCD PIC 99 OCCURS 10 TIMES.
00235 *
00236          10  C150-U-RMRKS     PIC X(40).
00237 *                                            315-354 REMARKS
00238          10  C150-U-MISRTNCODES.
00239 *                                            355-374 MIS
00240 *                                                     RETURN CODE
00241              15  C150-U-MISRTNCD PIC 99 OCCURS 10 TIMES.
00242 *
00243          10  C150-U-COOPSENSHRS PIC 9(6).
00244 *                                            375-380 COOP
00245 *                                                     SENIR SHARE
00246          10  C150-U-PCTSENSHRS PIC V9(6).
00247 *                                            381-386 PERCENT
00248 *                                                     SENIR SHARE
00249          10  C150-U-LIFECARE  PIC X.
00250 *                                            387-387 LIFECARE
00251 *                                                     FACILITY
00252          10  FILLER           PIC X(18).
00253 *                                            388-405 FILLER
00254          10  C150-U-NOINCOME  PIC X.
00255 *                                            406-406 NO INCOME
00256 *                                                     INDICATOR
00257          10  C150-U-SSINC     PIC 9(7)V99.
00258 *                                            407-415 SOCIAL
00259 *                                                     SECURITY
00260 *                                                     INCOME
00261          10  C150-U-RRBEN     PIC 9(7)V99.
00262 *                                            416-424 RAILROAD
00263 *                                                     BENEFITS
00264          10  C150-U-CSBEN     PIC 9(7)V99.
00265 *                                            425-433 CIVIL SERVIC
00266 *                                                     BENEFITS
00267          10  C150-U-OTHBEN    PIC 9(7)V99.
00268 *                                            434-442 OTHER
00269 *                                                     BENEFITS
00270          10  C150-U-VETBEN    PIC 9(7)V99.
00271 *                                            443-451 VETERANS
00272 *                                                     BENEFITS
00273          10  C150-U-PUBAID    PIC 9(7)V99.
00274 *                                            452-460 PUBLIC AID
00275          10  C150-U-WAGES     PIC 9(7)V99.
00276 *                                            461-469 WAGES
00277          10  C150-U-INT       PIC 9(7)V99.
00278 *                                            470-478 INTEREST
00279          10  C150-U-NETRENT   PIC S9(7)V99.
00280 *                                            479-487 NET RENTAL
00281 *                                                     INCOME
00282          10  C150-U-NETCAPGAIN PIC S9(7)V99.
00283 *                                            488-496 NET CAPITAL
00284 *                                                     GAINS
00285          10  C150-U-OTHINC    PIC S9(7)V99.
00286 *                                            497-505 OTHER
00287 *                                                     INCOME
00288          10  C150-U-TOTINC    PIC S9(7)V99.
00289 *                                            506-514 TOTAL
00290 *                                                     INCOME
00291          10  C150-U-SIGNED  PIC X.
00292 *                                            515-515 SIGNED
00293          10  C150-U-NOTARIZED PIC X.
00294 *                                            516-516 NOTARIZED
00295          10  FILLER         PIC X(15).
00296 *                                            517-531 FILLER
00297          10  C150-U-BATCH   PIC 9(5).
00298 *                                            532-536 BATCH
00299          10  C150-U-ORIGDTE PIC 9(8).
00300 *                                            537-544 ORIGINATION
00301 *                                                     DATE
00302 *                                                     (CCYYMMDD)
00303          10  C150-U-KEYTIME PIC 9(7).
00304 *                                            545-551 TIME KEYED
00305 *                                                     (0HHMMSS)
00306          10  C150-U-ORIGEMPNO PIC X(8).
00307 *                                            552-559 ORIGINATION
00308 *                                                     ENTRY
00309 *                                                     EMPLOYEE NO
00310          10  C150-U-TERMID  PIC X(4).
00311 *                                            560-563 TERMINAL I.D
00312          10  C150-U-LSTUPID PIC X(8).
00313 *                                            564-571 LAST
00314 *                                                     UPDATE I.D.
00315          10  C150-U-LSTUPDTE PIC 9(8).
00316 *                                            572-579 LAST UPDATE
00317 *                                                     DATE
00318 *                                                     (CCYYMMDD)
00319          10  C150-U-LSTUPTIM PIC 9(7).
00320 *                                            580-586 LAST UPDATE
00321 *                                                     TIME
00322 *                                                     (0HHMMSS)
00323          10  FILLER         PIC X(15).
00324 *                                            587-601 FILLER
00325          10  C150-U-HSSTAT  PIC X.
00326 *                                            602-602 HOMESTEAD
00327 *                                                     STATUS
00328          10  C150-U-SHARES  PIC 9(6).
00329 *                                            603-608 SHARES
00330          10  C150-U-PCTSHARES PIC 999V999.
00331 *                                            609-614 PERCENT
00332 *                                                     OF SHARES
00333          10  C150-U-HSBATCH PIC 9(5).
00334 *                                            615-619 HOMESTEAD
00335 *                                                     BATCH
00336          10  C150-U-HSORIGEMPNO PIC X(8).
00337 *                                            620-627 HOMESTEAD
00338 *                                                     ORIGINATION
00339 *                                                     ENTRY
00340 *                                                     EMPLOYEE NO
00341          10  C150-U-HSORIGDTE PIC 9(8).
00342 *                                            628-635 HOMESTEAD
00343 *                                                     ORIGINATION
00344 *                                                     DATE
00345 *                                                     (CCYYMMDD)
00346          10  C150-U-HSKEYTIME PIC 9(7).
00347 *                                            636-642 HOMESTEAD
00348 *                                                     TIME KEYED
00349 *                                                     (0HHMMSS)
00350          10  C150-U-HSTERMID PIC X(4).
00351 *                                            643-646 HOMRSTEAD
00352 *                                                     TERMINAL I.
00353          10  C150-U-HSLSTUPD PIC X(8).
00354 *                                            647-654 HOMESTEAD
00355 *                                                    LAST
00356 *                                                     UPD DATE
00357          10  C150-U-HSLSTUPDTE PIC 9(8).
00358 *                                            655-662 HOMESTEAD
00359 *                                                     LAST UPDATE
00360 *                                                     DATE
00361 *                                                     (CCYYMMDD)
00362          10  C150-U-HSLSTUPTIM PIC 9(7).
00363 *                                            663-669 HOMESTEAD
00364 *                                                     LAST UPDATE
00365 *                                                     TIME
00366 *                                                     (0HHMMSS)
00367          10  C150-U-HSYRAPPLD PIC 9(4).
00368 *                                            670-673 HOMESTEAD
00369 *                                                    YEAR APPLIED
00370          10  FILLER         PIC X(26).
00371 *                                            674-699 FILLER
00372          10  C150-U-HOSTAT  PIC X.
00373 *                                            700-700 HOMEOWNER
00374 *                                                     STATUS
00375          10  C150-U-HOBASYR PIC 9(4).
00376 *                                            701-704 HOMEOWNER
00377 *                                                     BASE YEAR
00378          10  C150-U-HOBYEQFAC PIC 9V9(4).
00379 *                                            705-709 HOMEOWNER
00380 *                                                     BASE YEAR
00381 *                                                     EQUALIZATIO
00382 *                                                     FACTOR
00383          10  C150-U-HOBYAV  PIC 9(9).
00384 *                                            710-718 HOMEOWNER
00385 *                                                     BASE YEAR
00386 *                                                     ASSESSED
00387 *                                                     VALUATION
00388          10  C150-U-HOBYEV  PIC 9(9).
00389 *                                            719-727 HOMEOWNER
00390 *                                                     BASE YEAR
00391 *                                                     EQUALIZED
00392 *                                                     VALUATION
00393          10  C150-U-HOTERMID PIC X(4).
00394 *                                            728-731 HOMEOWNER
00395 *                                                     TERMINAL I.
00396          10  C150-U-ELGIND  PIC 9.
00397 *                                            732-732 HOMEOWNER
00398 *                                                    ELIG. IND.
00399          10  FILLER         PIC XXX.
00400 *                                            733-735 FILLER
00401          10  C150-U-HOLSTUPD PIC X(8).
00402 *                                            736-743 HOMEOWNER
00403 *                                                     LAST
00404 *                                                     UPDATE I.D.
00405          10  C150-U-HOLSTUPDTE PIC 9(8).
00406 *                                            744-751 HOMEOWNER
00407 *                                                     LAST UPDATE
00408 *                                                     DATE
00409 *                                                     (CCYYMMDD)
00410          10  C150-U-HOLSTUPTIM PIC 9(7).
00411 *                                            752-758 HOMEOWNER
00412 *                                                     LAST UPDATE
00413 *                                                     TIME
00414 *                                                     (0HHMMSS)
00415          10  C150-U-MAINTIND  PIC 9.
00416 *                                            759-759 MAINTENANCE
00417 *                                                     INDICATOR
00418          10  C150-U-MTTERMID PIC X(4).
00419 *                                            760-763 MAINTENANCE
00420 *                                                     TERMINAL I.
00421          10  C150-U-MTUPID  PIC X(8).
00422 *                                            764-771 MAINTENANCE
00423 *                                                     UPDATE I.D.
00424          10  C150-U-MTUPDTE PIC 9(8).
00425 *                                            772-779 MAINTENANCE
00426 *                                                     UPDATE
00427 *                                                     DATE
00428 *                                                     (CCYYMMDD)
00429          10  C150-U-MTUPTIM PIC 9(7).
00430 *                                            780-786 MAINTENANCE
00431 *                                                     UPDATE
00432 *                                                     TIME
00433 *                                                     (0HHMMSS)
00434          10  C150-U-SFPCT   PIC 99.
00435          10  C150-U-SFPCT-R REDEFINES C150-U-SFPCT PIC 9V9.
00436 *                                            787-788  SENIOR FREE
00437 *                                                     INCOME
00438 *                                                     PERCENTAGE
00439          10  FILLER         PIC X(62).
00440 *                                            789-850 FILLER
00441 *----------------------------------------------------------------
00442      SKIP3
00443  FD  UPDATE-REPORT
00444      RECORDING MODE IS F
00445      RECORD CONTAINS 133 CHARACTERS
00446      BLOCK CONTAINS 0 RECORDS
00447      LABEL RECORDS ARE STANDARD
00448      DATA RECORD IS UPDATE-REP.
00449  01  UPDATE-REP             PIC X(133).
00450      SKIP3
00451  FD  UPD-ERR-REP
00452      RECORDING MODE IS F
00453      RECORD CONTAINS 133 CHARACTERS
00454      BLOCK CONTAINS 0 RECORDS
00455      LABEL RECORDS ARE STANDARD
00456      DATA RECORD IS UPD-ERR-REC.
00457  01  UPD-ERR-REC            PIC X(133).
00458      EJECT
00459  WORKING-STORAGE SECTION.
00460      SKIP1
00461  77  ERR-SW                 PIC X     VALUE 'N'.
00462      88  ERR                          VALUE 'Y'.
00463  77  CC-EOF-SW              PIC X     VALUE 'N'.
00464      88  CC-EOF                       VALUE 'Y'.
00465  77  TUPD-RECS-RD           PIC S9(7) VALUE +0   PACKED-DECIMAL.
00466  77  TDET-SEGS-UPDT         PIC S9(7) VALUE +0   PACKED-DECIMAL.
00467  77  PCT-SEN-SHRS           PIC V9(06) VALUE 0.
00468  77  TUPD-RECS-REJ          PIC S9(7) VALUE +0   PACKED-DECIMAL.
00469  77  LINE-CNT               PIC S999  VALUE +60  PACKED-DECIMAL.
00470  77  LINE-CNTE              PIC S999  VALUE +60  PACKED-DECIMAL.
00471  77  PAGE-CNT               PIC S9(5) VALUE +0   PACKED-DECIMAL.
00472  77  PAGE-CNTE              PIC S9(5) VALUE +0   PACKED-DECIMAL.
00473  77  BLNK                   PIC X     VALUE SPACE.
00474  77  UPD-EOF-SW             PIC X     VALUE 'N'.
00475      88  UPD-EOF                      VALUE 'Y'.
00476  77  VALID-RECORD           PIC X     VALUE 'N'.
00477      88  VALID-REC                    VALUE 'Y'.
00478  77  WS-TXYR-COMPTN         PIC 99    VALUE ZEROS.
00479  77  CIC-CNTR               PIC S9(5) VALUE +0   PACKED-DECIMAL.
00480  77  SUB                    PIC S9(4) VALUE +0  COMP.
00481  77  SUB2                   PIC S9(4) VALUE +0  COMP.
00482      EJECT
00483  01  WORK-AREA.
00484      SKIP1
00485      05  CHECK-PT-ID.
00486          10  FILLER         PIC XX     VALUE 'AS'.
00487          10  CKPT-ID        PIC 9(6)   VALUE ZERO.
00488      SKIP1
00489      05  CURR-UPD-KEY.
00490          10  CU-PROP.
00491              15 CU-PROP-R   PIC 9(14).
00492          10  CU-PROCYR      PIC XX.
00493          10  CU-TXYR        PIC XX.
00494          10  CU-TXTYP       PIC X.
00495          10  CU-RECCD       PIC X.
00496          10  CU-BIRTH       PIC X(8).
00497      SKIP1
00498      05  PREV-UPD-KEY       PIC X(28)  VALUE LOW-VALUES.
00499      SKIP1
00500      05  WS-PROP            PIC X(14).
00501      05  FILLER REDEFINES WS-PROP.
00502          10  WS-PROP1       PIC XX.
00503          10  WS-PROP2       PIC XX.
00504          10  WS-PROP3       PIC XXX.
00505          10  WS-PROP4       PIC XXX.
00506          10  WS-PROP5       PIC XXXX.
00507      SKIP1
00508      05  WS-TXCD            PIC 9(5).
00509      05  FILLER REDEFINES WS-TXCD.
00510          10  WS-TOWN        PIC 99.
00511          10  FILLER         PIC XXX.
00512      SKIP1
00513      05  WS-ASSMT-KEY       PIC X(5).
00514      05  FILLER REDEFINES WS-ASSMT-KEY.
00515          10  WS-PROCYR      PIC XX.
00516          10  WS-TXYR        PIC XX.
00517          10  WS-TXTYP       PIC X.
00518      SKIP1
00519      05  DSP-TIME                     PIC 99,99.
00520      05  DSP-DATE.
00521          10  DSP-MO                   PIC 99.
00522          10  FILLER                   PIC X       VALUE '/'.
00523          10  DSP-DA                   PIC 99.
00524          10  FILLER                   PIC X       VALUE '/'.
00525          10  DSP-YR                   PIC 99.
00526      05  ACPT-DATE                    PIC 9(6).
00527      05  ACPT-DATE-X REDEFINES ACPT-DATE.
00528          10  ACPT-YR                  PIC 99.
00529          10  ACPT-MO                  PIC 99.
00530          10  ACPT-DA                  PIC 99.
00531      05  ACPT-TIME-HOLD               PIC 9(8).
00532      05  ACPT-TIME-HOLD-X REDEFINES ACPT-TIME-HOLD.
00533          10  ACPT-TIME                PIC 9(4).
00534          10  FILLER                   PIC 9(4).
00535          SKIP1
00536      05  CURR-TIME-X.
00537          10  CURR-TIME              PIC  9(6).
00538          10  CURR-TIME-2            PIC  9(2).
00539      05  CURR-TIME-N   REDEFINES    CURR-TIME-X PIC 9(8).
00540          SKIP1
00541      05  WS-DATE            PIC 9(8).
00542      05  FILLER REDEFINES WS-DATE.
00543          10  WS-MM          PIC 99.
00544          10  WS-DD          PIC 99.
00545          10  WS-CC          PIC 99.
00546          10  WS-YY          PIC 99.
00547          SKIP1
00548      05  WS-DATE2           PIC 9(8).
00549      05  FILLER REDEFINES WS-DATE2.
00550          10  WS-CC2         PIC 99.
00551          10  WS-YY2         PIC 99.
00552          10  WS-MM2         PIC 99.
00553          10  WS-DD2         PIC 99.
00554          SKIP1
00555      05  MO-DAY-YR          PIC 9(6)  VALUE ZERO.
00556          SKIP1
00557      05 WS-CLASS            PIC 9(7).
00558      05 FILLER REDEFINES WS-CLASS.
00559          10 WS-ZEROS        PIC 99.
00560          SKIP1
00561      05  FILE-STATUS            PIC 99.
00562          88 NORMAL-STATUS                 VALUE 00.
00563          88 RECORD-FOUND                  VALUE 00.
00564          88 VSAM-EOF                      VALUE 10.
00565          88 RECORD-NOTFND                 VALUE 23.
00566      05  FILE-STATUS-2                  BINARY.
00567          10  VSAM-RETURN     PIC 99      VALUE 0.
00568          10  VSAM-FUNCTION   PIC 9       VALUE 0.
00569          10  VSAM-FEEDBACK   PIC 999     VALUE 0.
00570      EJECT
00571  01  DATE-CHECK.
00572  COPY VALIDDATE2.
00573      SKIP3
00574  COPY VALIDCYMD2.
00575      EJECT
00576  01  PRINT-LINES.
00577      SKIP1
00578      05  HL-1.
00579          10  FILLER         PIC XXX   VALUE SPACE.
00580          10  H1-DATE        PIC 99B99B99.
00581          10  FILLER         PIC X(39) VALUE SPACE.
00582          10  FILLER         PIC X(69) VALUE
00583          'OFFICE OF THE COOK COUNTY ASSESSOR'.
00584          10  FILLER         PIC X(5)  VALUE 'PAGE'.
00585          10  H1-PAGE        PIC ZZ,ZZ9.
00586          SKIP1
00587      05  HL-2.
00588          10  FILLER         PIC XXX   VALUE SPACE.
00589          10  FILLER         PIC X(08) VALUE 'ASHMA828'.
00590          10  FILLER         PIC X(21) VALUE SPACE.
00591          10  FILLER         PIC X(57) VALUE
00592      'SEN. FRZ. DATA BASE UPDATE RPT. - MANUAL QUAL. - DENIAL -'.
00593          10  FILLER         PIC X(22) VALUE
00594      'RETURN   UPDATE REPORT'.
00595          SKIP1
00596      05  HL-3.
00597          10  FILLER         PIC XX.
00598          10  FILLER         PIC X(42) VALUE
00599          'PERMANENT INDEX   PC TX TX RC RC       FZ'.
00600          10  FILLER         PIC X(53) VALUE
00601          'APPR.DTE   Q-D-R  RET.  BATCH ORIGINAL  TIME   ENTRY'.
00602          SKIP1
00603      05  HL-4.
00604          10  FILLER         PIC X(7)  VALUE SPACE.
00605          10  FILLER         PIC X(49) VALUE
00606          'NUMBER       YR YR TP CD BRTHDATE ST /COOP SHR'.
00607          10  FILLER         PIC X(43) VALUE
00608          'DATE  CODES  NO.     DATE   KEYED  EMPLOYEE'.
00609          SKIP1
00610      05  DET-LN.
00611          10  FILLER         PIC X.
00612          10  DL-PROP1       PIC 99.
00613          10  DL-DSH1        PIC X.
00614          10  DL-PROP2       PIC 99.
00615          10  DL-DSH2        PIC X.
00616          10  DL-PROP3       PIC 999.
00617          10  DL-DSH3        PIC X.
00618          10  DL-PROP4       PIC 999.
00619          10  DL-DSH4        PIC X.
00620          10  DL-PROP5       PIC 9999.
00621          10  FILLER         PIC X     VALUE SPACE.
00622          10  DL-PC-YR       PIC 99.
00623          10  FILLER         PIC X     VALUE SPACE.
00624          10  DL-TX-YR       PIC 99.
00625          10  FILLER         PIC XX    VALUE SPACE.
00626          10  DL-TX-TP       PIC 9.
00627          10  FILLER         PIC XX    VALUE SPACE.
00628          10  DL-RC-CD       PIC 9.
00629          10  FILLER         PIC X     VALUE SPACE.
00630          10  DL-BIRTH-DTE   PIC 9(8).
00631          10  FILLER         PIC XX    VALUE SPACE.
00632          10  DL-FZ-ST       PIC X.
00633          10  FILLER         PIC X     VALUE SPACE.
00634          10  DL-COOP-SHR    PIC 9(8).
00635          10  FILLER         PIC X     VALUE SPACE.
00636          10  DL-QUAL-DTE    PIC 9(8).
00637          10  FILLER         PIC X     VALUE SPACE.
00638          10  DL-RT-CD1      PIC 99.
00639          10  FILLER         PIC X     VALUE SPACE.
00640          10  DL-RT-CD2      PIC 99.
00641          10  FILLER         PIC X     VALUE SPACE.
00642          10  DL-BATCH-NO    PIC 99999.
00643          10  FILLER         PIC XX    VALUE SPACE.
00644          10  DL-ORIG-DTE    PIC 9(8).
00645          10  FILLER         PIC X     VALUE SPACE.
00646          10  DL-TIME-KEY    PIC 999999.
00647          10  FILLER         PIC X     VALUE SPACE.
00648          10  DL-ENT-EMPL    PIC 99999999.
00649          10  FILLER         PIC X     VALUE SPACE.
00650          10  DL-SFPCT       PIC 9V9.
00651      SKIP2
00652      05  HL-1E.
00653          10  FILLER         PIC XXX   VALUE SPACE.
00654          10  HE-DATE        PIC 99B99B99.
00655          10  FILLER         PIC X(39) VALUE SPACE.
00656          10  FILLER         PIC X(69) VALUE
00657          'OFFICE OF THE COOK COUNTY ASSESSOR'.
00658          10  FILLER         PIC X(5)  VALUE 'PAGE'.
00659          10  HE-PAGE        PIC ZZ,ZZ9.
00660          SKIP1
00661      05  HL-2E.
00662          10  FILLER         PIC XXX   VALUE SPACE.
00663          10  FILLER         PIC X(08) VALUE 'ASHMA828'.
00664          10  FILLER         PIC X(21) VALUE SPACE.
00665          10  FILLER         PIC X(58) VALUE
00666      'SEN. FRZ. DATA BASE UPDATE RPT. - MANUAL QUAL. - DENIAL -'.
00667          10  FILLER         PIC X(21) VALUE
00668      'RETURN   ERROR REPORT'.
00669          SKIP1
00670      05  HL-3E.
00671          10  FILLER         PIC XX.
00672          10  FILLER         PIC X(42) VALUE
00673          'PERMANENT INDEX   PC TX TX RC RC       FZ'.
00674          10  FILLER         PIC X(53) VALUE
00675          'APPR.DTE   Q-D-R  RET.  BATCH ORIGINAL  TIME   ENTRY'.
00676          SKIP1
00677      05  HL-4E.
00678          10  FILLER         PIC X(7)  VALUE SPACE.
00679          10  FILLER         PIC X(49) VALUE
00680          'NUMBER       YR YR TP CD BRTHDATE ST /COOP SHR'.
00681          10  FILLER         PIC X(43) VALUE
00682          'DATE  CODES  NO.     DATE   KEYED  EMPLOYEE'.
00683          SKIP1
00684      05  DET-LNE.
00685          10  FILLER         PIC X.
00686          10  DE-PROP1       PIC XX.
00687          10  DE-DSH1        PIC X.
00688          10  DE-PROP2       PIC XX.
00689          10  DE-DSH2        PIC X.
00690          10  DE-PROP3       PIC XXX.
00691          10  DE-DSH3        PIC X.
00692          10  DE-PROP4       PIC XXX.
00693          10  DE-DSH4        PIC X.
00694          10  DE-PROP5       PIC XXXX.
00695          10  FILLER         PIC X     VALUE SPACE.
00696          10  DE-PC-YR       PIC XX.
00697          10  FILLER         PIC X     VALUE SPACE.
00698          10  DE-TX-YR       PIC XX.
00699          10  FILLER         PIC XX    VALUE SPACE.
00700          10  DE-TX-TP       PIC X.
00701          10  FILLER         PIC XX    VALUE SPACE.
00702          10  DE-RC-CD       PIC X.
00703          10  FILLER         PIC X     VALUE SPACE.
00704          10  DE-BIRTH-DTE   PIC X(8).
00705          10  FILLER         PIC XX    VALUE SPACE.
00706          10  DE-FZ-ST       PIC X.
00707          10  FILLER         PIC X     VALUE SPACE.
00708          10  DE-COOP-SHR    PIC X(8).
00709          10  FILLER         PIC X     VALUE SPACE.
00710          10  DE-QUAL-DTE    PIC X(8).
00711          10  FILLER         PIC X     VALUE SPACE.
00712          10  DE-RT-CD1      PIC XX.
00713          10  FILLER         PIC X     VALUE SPACE.
00714          10  DE-RT-CD2      PIC XX.
00715          10  FILLER         PIC X     VALUE SPACE.
00716          10  DE-BATCH-NO    PIC XXXXX.
00717          10  FILLER         PIC XX    VALUE SPACE.
00718          10  DE-ORIG-DTE    PIC X(8).
00719          10  FILLER         PIC X     VALUE SPACE.
00720          10  DE-TIME-KEY    PIC XXXXXX.
00721          10  FILLER         PIC X     VALUE SPACE.
00722          10  DE-ENT-EMPL    PIC XXXXXXXX.
00723          10  FILLER         PIC X     VALUE SPACE.
00724          10  DE-SFPCT       PIC 9V9.
00725          10  FILLER         PIC XX    VALUE SPACE.
00726          10  DE-MSG         PIC X(29).
00727          SKIP1
00728      05  TOT-LN.
00729          10  FILLER     PIC XX    VALUE SPACE.
00730          10  TL-TOT     PIC Z,ZZZ,ZZ9.
00731          10  FILLER     PIC XX    VALUE SPACE.
00732          10  TL-MSG     PIC X(29).
00733          EJECT
00734  01  UPDT-ERROR-TABLE.
00735      05  UPDT-ERR-MSG1      PIC X(47) VALUE
00736      'PROPERTY NO. MUST BE NUMERIC'.
00737      05  UPDT-ERR-MSG2      PIC X(47) VALUE
00738      'PROCESS YEAR MUST BE NUMERIC'.
00739      05  UPDT-ERR-MSG3      PIC X(47) VALUE
00740      'TAX YEAR MUST BE NUMERIC'.
00741      05  UPDT-ERR-MSG4      PIC X(47) VALUE
00742      'TAX TYPE MUST BE NUMERIC'.
00743      05  UPDT-ERR-MSG5      PIC X(47) VALUE
00744      'RECORD CODE MUST BE EQUAL 1 OR 2'.
00745      05  UPDT-ERR-MSG6      PIC X(47) VALUE
00746      'BIRTHDATE MUST BE VALID DATE'.
00747      05  UPDT-ERR-MSG7      PIC X(47) VALUE
00748      'FRZ STATUS MUST BE Q, D, R, OR M'.
00749      05  UPDT-ERR-MSG8      PIC X(47) VALUE
00750      'APPROVED DATE MUST BE VALID DATE'.
00751      05  UPDT-ERR-MSG9      PIC X(47) VALUE
00752      'DENIAL DATE MUST BE VALID DATE'.
00753      05  UPDT-ERR-MSG10     PIC X(47) VALUE
00754      'QUALIFY DATE MUST BE VALID DATE'.
00755      05  UPDT-ERR-MSG11     PIC X(47) VALUE
00756      'MIS RETURN 10 MUST BE EQUAL TO 49'.
00757      05  UPDT-ERR-MSG12     PIC X(47) VALUE
00758      'BATCH NO. GR THAN ZERO'.
00759      05  UPDT-ERR-MSG13     PIC X(47) VALUE
00760      'ORIGINAL DATE MUST BE VALID DATE'.
00761      05  UPDT-ERR-MSG14     PIC X(47) VALUE
00762      'TIME KEYED MUST BE NUMERIC'.
00763      05  UPDT-ERR-MSG15     PIC X(47) VALUE
00764      'EMPLOYEE MUST BE GR SPACE'.
00765      05  UPDT-ERR-MSG16     PIC X(47) VALUE
00766      'NO MATCHING ROOT SEGMENT'.
00767      05  UPDT-ERR-MSG17     PIC X(47) VALUE
00768      'NO MATCHING ASSESSMENT DATA SEG'.
00769      05  UPDT-ERR-MSG18     PIC X(47) VALUE
00770      'NO MATCHING MASTER SEG'.
00771      05  UPDT-ERR-MSG19     PIC X(47) VALUE
00772      'NO MATCHING DETAIL SEG'.
00773      05  UPDT-ERR-MSG20     PIC X(47) VALUE
00774      'DETAIL SEGMENT UPDATED'.
00775      05  UPDT-ERR-MSG21     PIC X(47) VALUE
00776      'INVALID RETURN CODE'.
00777      05  UPDT-ERR-MSG22     PIC X(47) VALUE
00778      'MISSING ADDITIONAL RETURN CODE'.
00779      05  UPDT-ERR-MSG23     PIC X(47) VALUE
00780      'RETURNED DATE MUST BE VALID DATE'.
00781      05  UPDT-ERR-MSG24     PIC X(47) VALUE
00782      'RECORD IS QUAL - UPD REJECTED'.
00783      05  UPDT-ERR-MSG25     PIC X(47) VALUE
00784      'RECORD IS DENIED - UPD REJECTED'.
00785      05  UPDT-ERR-MSG26     PIC X(47) VALUE
00786      'COOPSHRS MUST BE GREATER ZEROS'.
00787      05  UPDT-ERR-MSG27     PIC X(47) VALUE
00788      'BLDG SHRS = 0, RECORD REJECTED'.
00789      05  UPDT-ERR-MSG28     PIC X(47) VALUE
00790      'SENIOR FREEZE % NOT NUMERIC   '.
00791      05  UPDT-ERR-MSG29     PIC X(47) VALUE
00792      'SENIOR FREEZE % NOT VALID     '.
00793  01  FILLER REDEFINES UPDT-ERROR-TABLE.
00794      05  UPDT-ERRORS OCCURS 29 TIMES.
00795          10  UPDT-ERR       PIC X(35).
00796      SKIP1
00797  01  HOLD-TABLE.
00798      05  HOLD-ERRORS OCCURS 29 TIMES.
00799          10  FILLER         PIC X(35).
00800      EJECT
00801  01  IO-AREA.
00802      SKIP1
00803  COPY PIROOTSEGM.
00804      SKIP1
00805  COPY PIASSESSMT.
00806      SKIP1
00807  01  IO-AREA145-150.
00808  COPY ASAIS145SG.
00809      SKIP1
00810  COPY ASAIS150SG.
00811      EJECT
00812 **************************************************************
00813 *             ASSESSOR ASSESSMENT INFORMATION                *
00814 *                       DATA BASE                            *
00815 *             PATH TO: 1) ROOT SEGMENT                       *
00816 *                      2) ASSESSMENT/TAX INFORMATION SEGMENT *
00817 *                      3) EXEMPTION MASTER SEGMENT           *
00818 *                      4) EXEMPTION DETAIL SEGMENT           *
00819 *                      5) SOCIAL SECURITY SEGMENT            *
00820 **************************************************************
00821 *
00822  01  SSAS-29.
00823 *--------------------- QUALIFIED SSA'S ----------------------*
00824      05  LVL1-QUAL-SSA.
00825          10  LVL1-SEGNAME        PIC X(8)   VALUE 'PROPSEG '.
00826          10  FILLER              PIC X      VALUE '('.
00827          10  FILLER              PIC X(8)   VALUE 'PPROP   '.
00828          10  LVL1-RO             PIC XX     VALUE 'EQ'.
00829          10  LVL1-ARG            PIC 9(15)    COMP-3.
00830          10  FILLER              PIC X      VALUE ')'.
00831      05  LVL2-QUAL-SSA.
00832          10  LVL2-SEGNAME        PIC X(8)   VALUE 'ASSMTSEG'.
00833          10  FILLER              PIC X      VALUE '('.
00834          10  FILLER              PIC X(8)   VALUE 'PYSRCH  '.
00835          10  LVL2-RO             PIC XX     VALUE 'EQ'.
00836          10  LVL2-ARG.
00837              15  LVL2-PROCYR     PIC 99.
00838              15  LVL2-TXYR       PIC 99.
00839              15  LVL2-TXTYP      PIC 9.
00840          10  FILLER              PIC X      VALUE ')'.
00841      05  LVL3-QUAL-SSA.
00842          10  LVL3-SEGNAME        PIC X(8)   VALUE 'EXMASTSG'.
00843          10  FILLER              PIC X      VALUE '('.
00844          10  FILLER              PIC X(8)   VALUE 'EXRECCOD'.
00845          10  LVL3-RO             PIC XX     VALUE 'EQ'.
00846          10  LVL3-ARG            PIC X.
00847          10  FILLER              PIC X      VALUE ')'.
00848      05  LVL4-QUAL-SSA.
00849          10  LVL4-SEGNAME        PIC X(8)   VALUE 'EXDETLSG'.
00850          10  FILLER              PIC X      VALUE '('.
00851          10  FILLER              PIC X(8)   VALUE 'EXBRTHDT'.
00852          10  LVL4-RO             PIC XX     VALUE 'EQ'.
00853          10  LVL4-ARG            PIC X(8).
00854          10  FILLER              PIC X      VALUE ')'.
00855      05  LVL5-QUAL-SSA.
00856          10  LVL5-SEGNAME        PIC X(8)   VALUE 'EXSOSCSG'.
00857          10  FILLER              PIC X      VALUE '('.
00858          10  FILLER              PIC X(8)   VALUE 'EXSEQNUM'.
00859          10  LVL5-RO             PIC XX     VALUE 'EQ'.
00860          10  LVL5-ARG            PIC XX.
00861          10  FILLER              PIC X      VALUE ')'.
00862 *-------------------- UNQUALIFIED SSA'S ---------------------*
00863      05  LVL1-UNQUAL-SSA         PIC X(9)   VALUE 'PROPSEG'.
00864      05  LVL2-UNQUAL-SSA         PIC X(9)   VALUE 'ASSMTSEG'.
00865      05  LVL3-UNQUAL-SSA         PIC X(9)   VALUE 'EXMASTSG'.
00866      05  LVL4-UNQUAL-SSA         PIC X(9)   VALUE 'EXDETLSG'.
00867      05  LVL5-UNQUAL-SSA         PIC X(9)   VALUE 'EXSOSCSG'.
00868 *-------------------- PATH  SSA'S   -------------------------*
00869      05  LVL1-QUAL-SSA-PATH.
00870          10  FILLER              PIC X(10)  VALUE 'PROPSEG *D'.
00871          10  FILLER              PIC X      VALUE '('.
00872          10  FILLER              PIC X(8)   VALUE 'PPROP   '.
00873          10  LVL1-RO-P           PIC XX     VALUE 'EQ'.
00874          10  LVL1-ARG-P          PIC 9(15)    COMP-3.
00875          10  FILLER              PIC X      VALUE ')'.
00876      05  LVL2-QUAL-SSA-PATH.
00877          10  FILLER              PIC X(10)  VALUE 'ASSMTSEG*D'.
00878          10  FILLER              PIC X      VALUE '('.
00879          10  FILLER              PIC X(8)   VALUE 'PYSRCH  '.
00880          10  LVL2-RO-P           PIC XX     VALUE 'EQ'.
00881          10  LVL2-ARG-P.
00882              15  LVL2-PROCYR-P   PIC 99.
00883              15  LVL2-TXYR-P     PIC 99.
00884              15  LVL2-TXTYP-P    PIC 9.
00885      05  LVL3-QUAL-SSA-PATH.
00886          10  FILLER              PIC X(10)  VALUE 'EXMASTSG*D'.
00887          10  FILLER              PIC X      VALUE '('.
00888          10  FILLER              PIC X(8)   VALUE 'EXRECCOD'.
00889          10  LVL3-RO-P           PIC XX     VALUE 'EQ'.
00890          10  LVL3-ARG-P          PIC X.
00891          10  FILLER              PIC X      VALUE ')'.
00892      05  LVL3-UNQUAL-SSA-PATH.
00893          10  FILLER              PIC X(08)  VALUE 'EXMASTSG'.
00894          10  FILLER              PIC X      VALUE '*'.
00895          10  FILLER              PIC XXX    VALUE 'DN '.
00896      05  LVL4-QUAL-SSA-PATH.
00897          10  FILLER              PIC X(10)  VALUE 'EXDETLSG*D'.
00898          10  FILLER              PIC X      VALUE '('.
00899          10  FILLER              PIC X(8)   VALUE 'EXBRTHDT'.
00900          10  LVL4-RO-P           PIC XX     VALUE 'EQ'.
00901          10  LVL4-ARG-P          PIC X(8).
00902          10  FILLER              PIC X      VALUE ')'.
00903      SKIP3
00904  COPY IMSCALLS.
00905      EJECT
00906  LINKAGE SECTION.
00907      SKIP1
00908  01  IO-PCB.
00909      05  IP-TERM-NAME              PIC X(8).
00910      05  FILLER                    PIC X(2).
00911      05  IP-STATUS-CODE            PIC X(2).
00912          88  IP-GOOD-STATUS                    VALUE SPACES.
00913      05  IP-CURR-DATE-TIME         PIC X(8).
00914      05  IP-MSG-SEQ-NO             PIC X(4).
00915      05  IP-MSG-OUT-DES-NAME       PIC X(4).
00916      05  IP-USER-ID                PIC X(8).
00917      SKIP1
00918  01  PI-PCB.
00919  COPY PIPCB1DESC.
00920          10  PI-PROP-NO            PIC 9(15)  PACKED-DECIMAL.
00921          10  PI-ASS-KEY            PIC 9(5).
00922          10  PI-MSTR-KEY           PIC X.
00923          10  PI-DET-KEY            PIC X(8).
00924      EJECT
00925  PROCEDURE DIVISION.
00926      SKIP1
00927  000-START.
00928      ENTRY 'DLITCBL' USING IO-PCB PI-PCB
00929      OPEN INPUT CNTRL-FILE
00930      SKIP1
00931      PERFORM 650-READ-EDIT-CC
00932      IF ERR
00933         MOVE 16 TO RETURN-CODE
00934         CLOSE  CNTRL-FILE
00935         GOBACK.
00936      SKIP1
00937      IF RETURN-CODE NOT EQUAL 16
00938         OPEN INPUT MSGTBL-FILE
00939         IF  NORMAL-STATUS
00940             OPEN INPUT   UPDATE-FILE
00941                  OUTPUT  UPDATE-REPORT UPD-ERR-REP
00942         ELSE
00943             DISPLAY 'MESSAGE TABLE FILE OPEN FAILED'
00944             DISPLAY '      FILE  STATUS     ' FILE-STATUS
00945             DISPLAY '      FILE  RETURN     ' VSAM-RETURN
00946             DISPLAY '      FILE  FUNCTION   ' VSAM-FUNCTION
00947             DISPLAY '      FILE  FEEDBACK   ' VSAM-FEEDBACK
00948             MOVE 16 TO RETURN-CODE
00949         END-IF
00950      END-IF.
00951      SKIP1
00952      ACCEPT   CURR-TIME-N   FROM  TIME
00953      ACCEPT    ACPT-DATE    FROM  DATE
00954      MULTIPLY  ACPT-DATE    BY    100.0001
00955        GIVING  MO-DAY-YR
00956      MOVE      MO-DAY-YR    TO  HE-DATE H1-DATE
00957      INSPECT   HE-DATE   REPLACING  ALL  ' '  BY '/'
00958      INSPECT   H1-DATE   REPLACING  ALL  ' '  BY '/'.
00959      SKIP1
00960      WRITE UPDATE-REP  FROM BLNK AFTER ADVANCING PAGE
00961      WRITE UPD-ERR-REC FROM BLNK AFTER ADVANCING PAGE.
00962      SKIP1
00963      PERFORM 720-INQUIRE-CHECKPT.
00964      SKIP1
00965      PERFORM 100-MAINLINE UNTIL UPD-EOF
00966                              OR ERR.
00967      SKIP1
00968      IF LINE-CNT GREATER THAN +55
00969         PERFORM 570-UPDT-HEADING-RTN.
00970      IF LINE-CNTE GREATER THAN +55
00971         PERFORM 600-ERR-HEADING-RTN.
00972      MOVE TUPD-RECS-RD TO TL-TOT
00973      MOVE 'TOTAL FREEZE RECORDS READ' TO TL-MSG
00974      WRITE UPDATE-REP  FROM TOT-LN AFTER ADVANCING 3
00975      WRITE UPD-ERR-REC FROM TOT-LN AFTER ADVANCING 3
00976      SKIP1
00977      MOVE TDET-SEGS-UPDT  TO TL-TOT
00978      MOVE 'TOTAL DETAIL SEGMENTS UPDATED' TO TL-MSG
00979      WRITE UPDATE-REP  FROM TOT-LN AFTER ADVANCING 1
00980      WRITE UPD-ERR-REC FROM TOT-LN AFTER ADVANCING 1
00981      SKIP1
00982      MOVE TUPD-RECS-REJ   TO TL-TOT
00983      MOVE 'TOTAL FREEZE RECORDS REJECTED' TO TL-MSG
00984      WRITE UPDATE-REP  FROM TOT-LN AFTER ADVANCING 1.
00985      WRITE UPD-ERR-REC FROM TOT-LN AFTER ADVANCING 1.
00986      SKIP1
00987      DISPLAY 'TOTAL FREEZE RECORDS READ     ' TUPD-RECS-RD
00988      DISPLAY 'TOTAL DETAIL SEGMENTS UPDATED ' TDET-SEGS-UPDT
00989      DISPLAY 'TOTAL FREEZE RECORDS REJECTED ' TUPD-RECS-REJ.
00990      SKIP1
00991      IF ERR
00992         MOVE 16 TO RETURN-CODE.
00993      SKIP1
00994      CLOSE CNTRL-FILE UPDATE-FILE UPDATE-REPORT
00995                                   UPD-ERR-REP
00996      GOBACK.
00997      SKIP2
00998  100-MAINLINE.
00999      PERFORM 150-READ-UPDATE-FILE
01000      IF NOT UPD-EOF
01001         PERFORM 200-EDIT-PRINT-ERR
01002         IF VALID-REC
01003            PERFORM 300-GET-REPL-DET-SEG.
01004          SKIP2
01005  150-READ-UPDATE-FILE.
01006      READ UPDATE-FILE AT END
01007           MOVE 'Y' TO UPD-EOF-SW.
01008      IF NOT UPD-EOF
01009         ADD +1 TO TUPD-RECS-RD.
01010 *       MOVE AS-PROPERTY-NO TO CU-PROP
01011 *       MOVE AS-PROCESS-YEAR TO CU-PROCYR
01012 *       MOVE AS-TAX-YEAR TO CU-TXYR
01013 *       MOVE AS-TAX-TYPE TO CU-TXTYP
01014 *       MOVE AS-RECORD-CODE TO CU-RECCD
01015 *       MOVE C150-U-BIRTHDTE TO CU-BIRTH
01016 *       IF CURR-UPD-KEY NOT GREATER THAN PREV-UPD-KEY
01017 *          DISPLAY 'UPDATE FILE OUT OF SEQUENCE'
01018 *          DISPLAY 'CURRENT KEY = ' CU-PROP SPACE CU-PROCYR
01019 *                   SPACE CU-TXYR SPACE CU-TXTYP SPACE CU-RECCD
01020 *                   SPACE CU-BIRTH
01021 *          DISPLAY 'PREVIOUS KEY = ' PREV-UPD-KEY
01022 *          MOVE 'N' TO ERR-SW
01023 *       ELSE
01024 *          MOVE CURR-UPD-KEY TO PREV-UPD-KEY.
01025      SKIP2
01026  200-EDIT-PRINT-ERR.
01027      MOVE 'Y' TO VALID-RECORD
01028      MOVE ZEROS TO SUB
01029      IF AS-PROPERTY-NO NUMERIC
01030         CONTINUE
01031      ELSE
01032         ADD +1 TO SUB
01033         MOVE UPDT-ERR-MSG1 TO HOLD-ERRORS (SUB).
01034         SKIP1
01035      IF AS-PROCESS-YEAR NUMERIC
01036         CONTINUE
01037      ELSE
01038         ADD +1 TO SUB
01039         MOVE UPDT-ERR-MSG2 TO HOLD-ERRORS (SUB).
01040         SKIP1
01041      IF AS-TAX-YEAR NUMERIC
01042         CONTINUE
01043      ELSE
01044         ADD +1 TO SUB
01045         MOVE UPDT-ERR-MSG3 TO HOLD-ERRORS (SUB).
01046      SKIP1
01047      IF AS-TAX-TYPE NUMERIC
01048         CONTINUE
01049      ELSE
01050         ADD +1 TO SUB
01051         MOVE UPDT-ERR-MSG4 TO HOLD-ERRORS (SUB).
01052      SKIP1
01053      IF (AS-RECORD-CODE NUMERIC) AND
01054         (AS-RECORD-CODE = 1 OR
01055          AS-RECORD-CODE = 2)
01056         CONTINUE
01057      ELSE
01058         ADD +1 TO SUB
01059         MOVE UPDT-ERR-MSG5 TO HOLD-ERRORS (SUB).
01060      SKIP1
01061      MOVE C150-U-BIRTHDTE TO VALIDATE-DATE2
01062      IF VALIDATE-DATE2  NUMERIC
01063         AND
01064           ((VAL-ALL-MTHS2 AND VAL-ALL-DAYS2)
01065             OR
01066           (VAL-FEB2 AND VAL-LEAP-DAYS2 AND VAL-LEAP-YRS2)
01067             OR
01068           (VAL-4-MTHS2 AND VAL-4-DAYS2)
01069             OR
01070           (VAL-OTHER-MTHS2 AND VAL-OTHER-DAYS2))
01071         AND
01072           (VAL-CENT2)
01073         AND
01074           (VAL-CURR-YRS2
01075            OR
01076            VAL-LEAP-YRS2)
01077            CONTINUE
01078      ELSE
01079         IF VALIDATE-DATE2 NOT = '00000000'
01080            ADD +1 TO SUB
01081            MOVE UPDT-ERR-MSG6 TO HOLD-ERRORS (SUB).
01082      SKIP1
01083      IF (C150-U-SFSTAT = 'Q')
01084         PERFORM 260-CHECK-Q
01085      ELSE
01086      IF (C150-U-SFSTAT = 'D')
01087         PERFORM 270-CHECK-D
01088      ELSE
01089      IF (C150-U-SFSTAT = 'R')
01090         PERFORM 280-CHECK-R
01091      ELSE
01092      IF (C150-U-SFSTAT = 'M')
01093         PERFORM 290-CHECK-M
01094      ELSE
01095         ADD +1 TO SUB
01096         MOVE UPDT-ERR-MSG7 TO HOLD-ERRORS (SUB).
01097      SKIP1
01098 *****IF (C150-U-BATCH NUMERIC AND
01099 ******   C150-U-BATCH GREATER THAN ZERO)
01100 ******   CONTINUE
01101 *****ELSE
01102 *****   ADD +1 TO SUB
01103 *****   MOVE UPDT-ERR-MSG12 TO HOLD-ERRORS (SUB).
01104      SKIP1
01105      MOVE C150-U-ORIGDTE TO WS-DATE2 VALIDATE-DATE
01106      IF VALIDATE-DATE NUMERIC
01107         AND
01108           ((VAL-ALL-MTHS AND VAL-ALL-DAYS)
01109             OR
01110           (VAL-FEB  AND VAL-LEAP-DAYS AND VAL-LEAP-YRS)
01111             OR
01112           (VAL-4-MTHS AND VAL-4-DAYS)
01113             OR
01114           (VAL-OTHER-MTHS AND VAL-OTHER-DAYS))
01115         CONTINUE
01116      ELSE
01117         ADD +1 TO SUB
01118         MOVE UPDT-ERR-MSG13 TO HOLD-ERRORS (SUB).
01119      SKIP1
01120      IF C150-U-ORIGEMPNO GREATER THAN SPACES
01121          CONTINUE
01122      ELSE
01123         ADD +1 TO SUB
01124         MOVE UPDT-ERR-MSG15 TO HOLD-ERRORS (SUB).
01125      IF C150-U-SFPCT NUMERIC
01126          CONTINUE
01127      ELSE
01128         ADD +1 TO SUB
01129         MOVE UPDT-ERR-MSG28 TO HOLD-ERRORS (SUB).
01130      IF C150-U-SFPCT = (10 OR 08 OR 06 OR 04 OR 02)
01131          CONTINUE
01132      ELSE
01133         ADD +1 TO SUB
01134         MOVE UPDT-ERR-MSG29 TO HOLD-ERRORS (SUB).
01135      SKIP1
01136      IF SUB IS GREATER THAN 0
01137         ADD +1 TO TUPD-RECS-REJ
01138         MOVE 'N' TO VALID-RECORD
01139         PERFORM 250-PRINT-ERR-RECS-REPORT
01140                     VARYING SUB2 FROM +1 BY +1
01141                       UNTIL SUB2 GREATER THAN SUB.
01142 *    ELSE
01143 *       PERFORM 500-UPDT-LINE-LISTING
01144 *       WRITE UPDATE-REP FROM DET-LN AFTER ADVANCING 2
01145 *       ADD +2 TO LINE-CNT.
01146          SKIP2
01147  250-PRINT-ERR-RECS-REPORT.
01148      IF LINE-CNTE
01149         GREATER THAN +55
01150         PERFORM 600-ERR-HEADING-RTN.
01151      IF SUB2 EQUAL 1
01152         PERFORM 550-ERR-LINE-LISTING
01153         MOVE HOLD-ERRORS (SUB2) TO DE-MSG
01154         WRITE UPD-ERR-REC FROM DET-LNE AFTER ADVANCING 2
01155         ADD +2 TO LINE-CNTE
01156      ELSE
01157         MOVE HOLD-ERRORS (SUB2) TO DE-MSG
01158         WRITE UPD-ERR-REC FROM DET-LNE AFTER ADVANCING 1
01159         ADD +1 TO LINE-CNTE.
01160      MOVE SPACES TO DET-LNE.
01161      SKIP2
01162  260-CHECK-Q.
01163      MOVE C150-U-FSTAPPDTE TO WS-DATE2 VALIDATE-DATE
01164      IF VALIDATE-DATE  NUMERIC
01165         AND
01166           ((VAL-ALL-MTHS  AND VAL-ALL-DAYS )
01167             OR
01168           (VAL-FEB  AND VAL-LEAP-DAYS  AND VAL-LEAP-YRS )
01169             OR
01170           (VAL-4-MTHS  AND VAL-4-DAYS )
01171             OR
01172           (VAL-OTHER-MTHS  AND VAL-OTHER-DAYS ))
01173         CONTINUE
01174      ELSE
01175         ADD +1 TO SUB
01176         MOVE UPDT-ERR-MSG8 TO HOLD-ERRORS (SUB).
01177      SKIP1
01178      MOVE C150-U-QUALDTE TO WS-DATE2 VALIDATE-DATE
01179      IF VALIDATE-DATE NUMERIC
01180         AND
01181           ((VAL-ALL-MTHS AND VAL-ALL-DAYS)
01182             OR
01183           (VAL-FEB  AND VAL-LEAP-DAYS AND VAL-LEAP-YRS)
01184             OR
01185           (VAL-4-MTHS AND VAL-4-DAYS)
01186             OR
01187           (VAL-OTHER-MTHS AND VAL-OTHER-DAYS))
01188         CONTINUE
01189      ELSE
01190         ADD +1 TO SUB
01191         MOVE UPDT-ERR-MSG10 TO HOLD-ERRORS (SUB).
01192      SKIP1
01193      IF (C150-U-MISRTNCD (10) = 49)
01194         CONTINUE
01195      ELSE
01196         ADD +1 TO SUB
01197         MOVE UPDT-ERR-MSG11 TO HOLD-ERRORS (SUB).
01198      SKIP1
01199      IF (C150-U-BATCH NUMERIC AND
01200          C150-U-BATCH GREATER THAN ZERO)
01201          CONTINUE
01202      ELSE
01203         ADD +1 TO SUB
01204         MOVE UPDT-ERR-MSG12 TO HOLD-ERRORS (SUB).
01205      SKIP1
01206      IF ((C150-U-KEYTIME NUMERIC)
01207            OR
01208          (C150-U-KEYTIME EQUAL SPACES))
01209          CONTINUE
01210      ELSE
01211         ADD +1 TO SUB
01212         MOVE UPDT-ERR-MSG14 TO HOLD-ERRORS (SUB).
01213      SKIP2
01214  270-CHECK-D.
01215      MOVE C150-U-DENIALDTE TO WS-DATE2 VALIDATE-DATE
01216      IF VALIDATE-DATE  NUMERIC
01217         AND
01218           ((VAL-ALL-MTHS  AND VAL-ALL-DAYS )
01219             OR
01220           (VAL-FEB  AND VAL-LEAP-DAYS  AND VAL-LEAP-YRS )
01221             OR
01222           (VAL-4-MTHS  AND VAL-4-DAYS )
01223             OR
01224           (VAL-OTHER-MTHS  AND VAL-OTHER-DAYS ))
01225         CONTINUE
01226      ELSE
01227         ADD +1 TO SUB
01228         MOVE UPDT-ERR-MSG9 TO HOLD-ERRORS (SUB).
01229      SKIP1
01230      IF (C150-U-MISRTNCD (10) = 49)
01231          CONTINUE
01232      ELSE
01233         ADD +1 TO SUB
01234         MOVE UPDT-ERR-MSG11 TO HOLD-ERRORS (SUB).
01235         SKIP1
01236      IF ((C150-U-MISRTNCD (01) NOT EQUAL 00)     AND
01237          (C150-U-MISRTNCD (01) NOT EQUAL 49))    OR
01238         ((C150-U-MANRTNCD (01) NOT EQUAL 00)     AND
01239          (C150-U-MANRTNCD (01) NOT EQUAL 49))
01240           PERFORM 900-CHECK-MIS-CODES-DEN
01241           PERFORM 910-CHECK-MAN-CODES-DEN
01242      ELSE
01243         ADD +1 TO SUB
01244         MOVE UPDT-ERR-MSG21 TO HOLD-ERRORS (SUB).
01245         SKIP1
01246      IF (C150-U-BATCH NUMERIC AND
01247          C150-U-BATCH GREATER THAN ZERO)
01248          CONTINUE
01249      ELSE
01250         ADD +1 TO SUB
01251         MOVE UPDT-ERR-MSG12 TO HOLD-ERRORS (SUB).
01252         SKIP1
01253      IF ((C150-U-KEYTIME NUMERIC)
01254           OR
01255          (C150-U-KEYTIME EQUAL SPACES))
01256          CONTINUE
01257      ELSE
01258         ADD +1 TO SUB
01259         MOVE UPDT-ERR-MSG14 TO HOLD-ERRORS (SUB).
01260         SKIP3
01261  280-CHECK-R.
01262      MOVE C150-U-RTNDDTE   TO WS-DATE2 VALIDATE-DATE
01263      IF VALIDATE-DATE  NUMERIC
01264         AND
01265           ((VAL-ALL-MTHS  AND VAL-ALL-DAYS )
01266             OR
01267           (VAL-FEB  AND VAL-LEAP-DAYS  AND VAL-LEAP-YRS )
01268             OR
01269           (VAL-4-MTHS  AND VAL-4-DAYS )
01270             OR
01271           (VAL-OTHER-MTHS  AND VAL-OTHER-DAYS ))
01272         CONTINUE
01273      ELSE
01274         ADD +1 TO SUB
01275         MOVE UPDT-ERR-MSG23 TO HOLD-ERRORS (SUB).
01276      SKIP1
01277      IF (C150-U-MISRTNCD (10) = 49)
01278         CONTINUE
01279      ELSE
01280         ADD +1 TO SUB
01281         MOVE UPDT-ERR-MSG11 TO HOLD-ERRORS (SUB).
01282     SKIP1
01283      IF ((C150-U-MISRTNCD (01) NOT EQUAL 00)     OR
01284          (C150-U-MISRTNCD (01) NOT EQUAL 49)     OR
01285          (C150-U-MANRTNCD (01) NOT EQUAL 00)     OR
01286          (C150-U-MANRTNCD (01) NOT EQUAL 49))
01287           PERFORM 900-CHECK-MIS-CODES-DEN
01288           PERFORM 910-CHECK-MAN-CODES-DEN
01289      ELSE
01290         ADD +1 TO SUB
01291         MOVE UPDT-ERR-MSG21 TO HOLD-ERRORS (SUB).
01292     SKIP1
01293      IF (C150-U-BATCH NUMERIC AND
01294          C150-U-BATCH GREATER THAN ZERO)
01295          CONTINUE
01296      ELSE
01297         ADD +1 TO SUB
01298         MOVE UPDT-ERR-MSG12 TO HOLD-ERRORS (SUB).
01299     SKIP1
01300      IF ((C150-U-KEYTIME NUMERIC)
01301           OR
01302          (C150-U-KEYTIME EQUAL SPACES))
01303          CONTINUE
01304      ELSE
01305         ADD +1 TO SUB
01306         MOVE UPDT-ERR-MSG14 TO HOLD-ERRORS (SUB).
01307     SKIP3
01308  290-CHECK-M.
01309      SKIP1
01310      IF (C150-U-COOPSENSHRS NUMERIC AND
01311          C150-U-COOPSENSHRS GREATER THAN ZEROS)
01312          CONTINUE
01313      ELSE
01314         ADD +1 TO SUB
01315         MOVE UPDT-ERR-MSG26 TO HOLD-ERRORS (SUB).
01316     SKIP3
01317  300-GET-REPL-DET-SEG.
01318      SKIP1
01319      MOVE AS-PROPERTY-NO  TO CU-PROP
01320      MOVE AS-PROCESS-YEAR TO CU-PROCYR
01321      MOVE AS-TAX-YEAR     TO CU-TXYR
01322      MOVE AS-TAX-TYPE     TO CU-TXTYP
01323 *****MOVE AS-RECORD-CODE  TO CU-RECCD
01324      MOVE C150-U-BIRTHDTE TO CU-BIRTH
01325      SKIP1
01326      MOVE CU-PROP-R TO LVL1-ARG
01327      MOVE CU-PROCYR TO LVL2-PROCYR
01328      MOVE CU-TXYR   TO LVL2-TXYR
01329      MOVE CU-TXTYP  TO LVL2-TXTYP
01330      MOVE CU-BIRTH  TO LVL4-ARG
01331      SKIP1
01332      CALL 'CBLTDLI' USING GHU
01333                           PI-PCB
01334                           IO-AREA145-150
01335                           LVL1-QUAL-SSA
01336                           LVL2-QUAL-SSA
01337                           LVL3-UNQUAL-SSA-PATH
01338                           LVL4-QUAL-SSA
01339      IF PI-DB-GOOD-STATUS
01340         IF ((C150-SFSTAT = 'Q')  AND
01341            (C150-U-SFSTAT = 'R' OR
01342             C150-U-SFSTAT = 'D'))
01343             MOVE SPACES TO DET-LNE
01344             ADD +1 TO TUPD-RECS-REJ
01345             MOVE UPDT-ERR-MSG24 TO DE-MSG
01346             PERFORM 550-ERR-LINE-LISTING
01347             WRITE UPD-ERR-REC FROM DET-LNE
01348                   AFTER ADVANCING 2
01349             ADD +2 TO LINE-CNTE
01350         ELSE
01351            IF ((C150-SFSTAT = 'D')  AND
01352               (C150-U-SFSTAT = 'Q' OR
01353                C150-U-SFSTAT = 'R'))
01354                MOVE SPACES TO DET-LNE
01355                ADD +1 TO TUPD-RECS-REJ
01356                MOVE UPDT-ERR-MSG25 TO DE-MSG
01357                PERFORM 550-ERR-LINE-LISTING
01358                WRITE UPD-ERR-REC FROM DET-LNE
01359                      AFTER ADVANCING 2
01360                ADD +2 TO LINE-CNTE
01361            ELSE
01362               IF (C150-U-SFSTAT = 'Q' OR
01363                   C150-U-SFSTAT = 'D' OR
01364                   C150-U-SFSTAT = 'R')
01365                   PERFORM 350-REPL-DETL-SEGMENT
01366               ELSE
01367                  IF (C150-U-SFSTAT = 'M')
01368                      PERFORM 400-M-REPL-DETL-SEGMENT
01369                  ELSE
01370                     NEXT SENTENCE
01371      ELSE
01372         IF PI-SEG-NOTFND
01373            IF PI-SEGMENT-LVL = '00'
01374               MOVE SPACES TO DET-LNE
01375               ADD +1 TO TUPD-RECS-REJ
01376               MOVE UPDT-ERR-MSG16 TO DE-MSG
01377               PERFORM 550-ERR-LINE-LISTING
01378               WRITE UPD-ERR-REC FROM DET-LNE
01379                     AFTER ADVANCING 2
01380               ADD +2 TO LINE-CNTE
01381            ELSE
01382               IF PI-DB-1ST-SEG-LEVEL
01383                  MOVE SPACES TO DET-LNE
01384                  ADD +1 TO TUPD-RECS-REJ
01385                  MOVE UPDT-ERR-MSG17 TO DE-MSG
01386                  PERFORM 550-ERR-LINE-LISTING
01387                  WRITE UPD-ERR-REC FROM DET-LNE
01388                        AFTER ADVANCING 2
01389                  ADD +2 TO LINE-CNTE
01390               ELSE
01391                  IF PI-DB-2ND-SEG-LEVEL
01392                     MOVE SPACES TO DET-LNE
01393                     ADD +1 TO TUPD-RECS-REJ
01394                     MOVE UPDT-ERR-MSG18 TO DE-MSG
01395                     PERFORM 550-ERR-LINE-LISTING
01396                     WRITE UPD-ERR-REC FROM DET-LNE
01397                           AFTER ADVANCING 2
01398                     ADD +2 TO LINE-CNTE
01399                  ELSE
01400                     MOVE SPACES TO DET-LNE
01401                     ADD +1 TO TUPD-RECS-REJ
01402                     MOVE UPDT-ERR-MSG19 TO DE-MSG
01403                     PERFORM 550-ERR-LINE-LISTING
01404                     WRITE UPD-ERR-REC FROM DET-LNE
01405                           AFTER ADVANCING 2
01406                     ADD +2 TO LINE-CNTE
01407         ELSE
01408             PERFORM 800-DISPLAY-PI-PCB.
01409      SKIP3
01410  350-REPL-DETL-SEGMENT.
01411      MOVE C150-U-SFSTAT    TO C150-SFSTAT
01412      MOVE C150-U-MISRTNCODES TO C150-MISRTNCODES
01413      MOVE C150-U-BATCH       TO C150-BATCH
01414      MOVE C150-U-ORIGDTE     TO C150-ORIGDTE
01415      MOVE C150-U-KEYTIME     TO C150-KEYTIME
01416      MOVE C150-U-ORIGEMPNO   TO C150-ORIGEMPNO
01417      MOVE 'ASHMA828'         TO C150-LSTUPID
01418      ACCEPT ACPT-DATE   FROM DATE
01419      MOVE ACPT-MO       TO WS-MM
01420      MOVE ACPT-DA       TO WS-DD
01421      MOVE ACPT-YR       TO WS-YY
01422      MOVE 20            TO WS-CC
01423      MOVE WS-DATE       TO C150-LSTUPDTE
01424      ACCEPT ACPT-TIME-HOLD FROM TIME
01425      MOVE ACPT-TIME     TO C150-LSTUPTIM
01426      IF C150-U-SFSTAT = 'Q'
01427          MOVE C150-U-FSTAPPDTE TO C150-FSTAPPDTE
01428          MOVE C150-U-QUALDTE TO C150-QUALDTE.
01429      IF C150-U-SFSTAT = 'D'
01430          MOVE C150-U-DENIALDTE TO C150-DENIALDTE.
01431      IF C150-U-SFPCT = (10 OR 08 OR 06 OR 04 OR 02)
01432          MOVE C150-U-SFPCT     TO C150-SFPCT.
01433         SKIP1
01434      CALL 'CBLTDLI' USING REPL
01435                           PI-PCB
01436                           IO-AREA145-150
01437                           LVL3-UNQUAL-SSA-PATH
01438                           LVL4-UNQUAL-SSA
01439      IF PI-DB-GOOD-STATUS
01440         PERFORM 500-UPDT-LINE-LISTING
01441         WRITE UPDATE-REP FROM DET-LN AFTER ADVANCING 2
01442         ADD +2 TO LINE-CNT
01443         ADD +2 TO CIC-CNTR
01444         ADD +1 TO TDET-SEGS-UPDT
01445         IF CIC-CNTR GREATER CC-CICV-N
01446             PERFORM 720-INQUIRE-CHECKPT
01447         ELSE
01448             NEXT SENTENCE
01449      ELSE
01450          PERFORM 800-DISPLAY-PI-PCB.
01451      SKIP3
01452  400-M-REPL-DETL-SEGMENT.
01453      MOVE 'ASHMA828'         TO C150-LSTUPID
01454      ACCEPT ACPT-DATE   FROM DATE
01455      MOVE ACPT-MO       TO WS-MM
01456      MOVE ACPT-DA       TO WS-DD
01457      MOVE ACPT-YR       TO WS-YY
01458      MOVE 20            TO WS-CC
01459      MOVE WS-DATE       TO C150-LSTUPDTE
01460      ACCEPT ACPT-TIME-HOLD FROM TIME
01461      MOVE ACPT-TIME     TO C150-LSTUPTIM
01462         SKIP1
01463      MOVE C150-U-COOPSENSHRS TO C150-COOPSENSHRS
01464 *****MOVE ZEROS              TO C150-PCTSENSHRS
01465      IF C145-BLDGSHARES GREATER THAN ZEROS
01466          DIVIDE C150-U-COOPSENSHRS BY C145-BLDGSHARES
01467          GIVING PCT-SEN-SHRS
01468          MOVE   PCT-SEN-SHRS TO C150-PCTSENSHRS
01469      ELSE
01470         ADD +1 TO TUPD-RECS-REJ
01471         MOVE UPDT-ERR-MSG27 TO DE-MSG
01472         PERFORM 550-ERR-LINE-LISTING
01473         WRITE UPD-ERR-REC FROM DET-LNE
01474               AFTER ADVANCING 2
01475         ADD +2 TO LINE-CNTE.
01476         SKIP1
01477      CALL 'CBLTDLI' USING REPL
01478                           PI-PCB
01479                           IO-AREA145-150
01480                           LVL3-UNQUAL-SSA-PATH
01481                           LVL4-UNQUAL-SSA
01482      IF PI-DB-GOOD-STATUS
01483         PERFORM 500-UPDT-LINE-LISTING
01484         WRITE UPDATE-REP FROM DET-LN
01485               AFTER ADVANCING 2
01486         ADD +2 TO LINE-CNT
01487         ADD +2 TO CIC-CNTR
01488         ADD +1 TO TDET-SEGS-UPDT
01489         IF CIC-CNTR GREATER CC-CICV-N
01490             PERFORM 720-INQUIRE-CHECKPT
01491         ELSE
01492             NEXT SENTENCE
01493      ELSE
01494          PERFORM 800-DISPLAY-PI-PCB.
01495      SKIP3
01496  500-UPDT-LINE-LISTING.
01497      IF LINE-CNT
01498         GREATER THAN +55
01499         PERFORM 570-UPDT-HEADING-RTN.
01500      IF C150-U-SFSTAT = 'Q'
01501         MOVE C150-U-QUALDTE TO DL-COOP-SHR
01502      ELSE
01503         MOVE C150-U-COOPSENSHRS TO DL-COOP-SHR.
01504      MOVE AS-PROPERTY-NO TO WS-PROP
01505      MOVE WS-PROP1      TO DL-PROP1
01506      MOVE WS-PROP2      TO DL-PROP2
01507      MOVE WS-PROP3      TO DL-PROP3
01508      MOVE WS-PROP4      TO DL-PROP4
01509      MOVE WS-PROP5      TO DL-PROP5
01510      MOVE '-'  TO DL-DSH1 DL-DSH2 DL-DSH3 DL-DSH4
01511      MOVE AS-PROCESS-YEAR TO DL-PC-YR
01512      MOVE AS-TAX-YEAR   TO DL-TX-YR
01513      MOVE AS-TAX-TYPE   TO DL-TX-TP
01514      MOVE AS-RECORD-CODE TO DL-RC-CD
01515      MOVE C150-U-BIRTHDTE TO DL-BIRTH-DTE
01516      MOVE C150-U-SFSTAT TO DL-FZ-ST
01517 *****MOVE C150-U-FSTAPPDTE TO DL-APPR-DTE
01518      MOVE C150-U-QUALDTE TO DL-QUAL-DTE
01519      MOVE C150-U-MISRTNCD (10) TO DL-RT-CD1
01520      MOVE C150-U-BATCH  TO  DL-BATCH-NO
01521      MOVE C150-U-ORIGDTE TO DL-ORIG-DTE
01522      MOVE C150-U-KEYTIME TO DL-TIME-KEY
01523      MOVE C150-U-ORIGEMPNO TO DL-ENT-EMPL.
01524      MOVE C150-U-SFPCT     TO DL-SFPCT.
01525      SKIP3
01526  550-ERR-LINE-LISTING.
01527      IF LINE-CNTE
01528         GREATER THAN +55
01529         PERFORM 600-ERR-HEADING-RTN.
01530      IF C150-U-SFSTAT = 'Q'
01531         MOVE C150-U-QUALDTE TO DE-COOP-SHR
01532      ELSE
01533         MOVE C150-U-COOPSENSHRS TO DE-COOP-SHR.
01534      MOVE AS-PROPERTY-NO TO WS-PROP
01535      MOVE WS-PROP1      TO DE-PROP1
01536      MOVE WS-PROP2      TO DE-PROP2
01537      MOVE WS-PROP3      TO DE-PROP3
01538      MOVE WS-PROP4      TO DE-PROP4
01539      MOVE WS-PROP5      TO DE-PROP5
01540      MOVE '-'  TO DE-DSH1 DE-DSH2 DE-DSH3 DE-DSH4
01541      MOVE AS-PROCESS-YEAR TO DE-PC-YR
01542      MOVE AS-TAX-YEAR   TO DE-TX-YR
01543      MOVE AS-TAX-TYPE   TO DE-TX-TP
01544      MOVE AS-RECORD-CODE TO DE-RC-CD
01545      MOVE C150-U-BIRTHDTE TO DE-BIRTH-DTE
01546      MOVE C150-U-SFSTAT TO DE-FZ-ST
01547 *****MOVE C150-U-FSTAPPDTE TO DE-APPR-DTE
01548      MOVE C150-U-QUALDTE TO DE-QUAL-DTE
01549      MOVE C150-U-MISRTNCD (10) TO DE-RT-CD1
01550      MOVE C150-U-BATCH  TO  DE-BATCH-NO
01551      MOVE C150-U-ORIGDTE TO DE-ORIG-DTE
01552      MOVE C150-U-KEYTIME TO DE-TIME-KEY
01553      MOVE C150-U-ORIGEMPNO TO DE-ENT-EMPL.
01554      MOVE C150-U-SFPCT     TO DE-SFPCT.
01555      SKIP3
01556  570-UPDT-HEADING-RTN.
01557      ADD +1 TO PAGE-CNT
01558      MOVE PAGE-CNT TO H1-PAGE
01559      WRITE UPDATE-REP  FROM HL-1 AFTER ADVANCING PAGE
01560      WRITE UPDATE-REP  FROM HL-2 AFTER ADVANCING 2
01561      WRITE UPDATE-REP  FROM HL-3 AFTER ADVANCING 2
01562      WRITE UPDATE-REP  FROM HL-4 AFTER ADVANCING 1
01563      MOVE +6 TO LINE-CNT.
01564      SKIP2
01565  600-ERR-HEADING-RTN.
01566      ADD +1 TO PAGE-CNTE
01567      MOVE PAGE-CNTE TO HE-PAGE
01568      WRITE UPD-ERR-REC FROM HL-1E AFTER ADVANCING PAGE
01569      WRITE UPD-ERR-REC FROM HL-2E AFTER ADVANCING 2
01570      WRITE UPD-ERR-REC FROM HL-3E AFTER ADVANCING 2
01571      WRITE UPD-ERR-REC FROM HL-4E AFTER ADVANCING 1
01572      MOVE +6 TO LINE-CNTE.
01573      SKIP2
01574  650-READ-EDIT-CC.
01575      PERFORM 670-READ-CARD-FILE
01576      DISPLAY 'CONTROL CARD  '  CNTRL-CARD
01577      IF NOT CC-EOF
01578         PERFORM 690-CNTRL-EDIT
01579      ELSE
01580         DISPLAY 'CONTROL CARD IS MISSING'
01581         MOVE 'Y' TO ERR-SW
01582      END-IF.
01583      SKIP2
01584  670-READ-CARD-FILE.
01585      READ CNTRL-FILE
01586        AT END
01587          MOVE 'Y' TO CC-EOF-SW.
01588      SKIP2
01589  690-CNTRL-EDIT.
01590      IF CK-PT-CNTL-VAL EQUAL 'CICV'
01591         IF CC-CICV NUMERIC AND
01592            CC-CICV-N GREATER THAN ZERO
01593            DISPLAY 'CONTROL CARD VALUES ARE: '
01594            DISPLAY 'CHECK POINT CONTROL VALUE '
01595                    CK-PT-CNTL-VAL
01596            DISPLAY 'CICV NUMBER IS ' CC-CICV-N
01597         ELSE
01598             DISPLAY 'INVALID CONTROL CARD'
01599             DISPLAY 'POS 1-4 MUST BE EQUAL TO CICV'
01600             DISPLAY 'POS 5-8 MUST BE NUMERIC'
01601             MOVE 16 TO RETURN-CODE
01602         END-IF
01603      ELSE
01604          DISPLAY 'INVALID CONTROL CARD'
01605          DISPLAY 'POS 1-4 MUST BE EQUAL TO CICV'
01606          DISPLAY 'POS 5-8 MUST BE NUMERIC'
01607          MOVE 16 TO RETURN-CODE
01608      END-IF.
01609      SKIP3
01610  720-INQUIRE-CHECKPT.
01611      IF CIC-CNTR GREATER CC-CICV-N
01612         ADD +1 TO CKPT-ID
01613         MOVE +0 TO CIC-CNTR
01614         PERFORM 730-ISSUE-CHECKPT.
01615      SKIP3
01616  730-ISSUE-CHECKPT.
01617      CALL 'CBLTDLI' USING CHKP
01618                           IO-PCB
01619                           CHECK-PT-ID
01620      IF NOT IP-GOOD-STATUS
01621         DISPLAY 'ERROR ON ISSUE OF CHECKPOINT'
01622         PERFORM 820-DISPLAY-IO-PCB
01623      END-IF.
01624      SKIP3
01625  800-DISPLAY-PI-PCB.
01626      SKIP1
01627      MOVE 'N' TO ERR-SW
01628      MOVE 16  TO RETURN-CODE
01629      SKIP1
01630      DISPLAY 'THE DBD NAME IS                        '
01631        PI-DBD-NAME
01632      DISPLAY 'THE SEGMENT LEVEL IS                   '
01633        PI-SEGMENT-LVL
01634      DISPLAY 'THE STATUS CODE IS                     '
01635        PI-STATUS-CODE
01636      DISPLAY 'THE PROCESSING OPTIONS ARE             '
01637        PI-PROC-OPT
01638      DISPLAY 'THE SEGMENT NAME IS                    '
01639        PI-NAME-FDBK
01640      DISPLAY 'THE LENGTH OF THE KEY FEEDBACK AREA IS '
01641        PI-KEY-FDBK-LNG
01642      DISPLAY 'THE NUMBER OF SENSITIVE SEGMENTS IS    ' PI-SEN-SEG
01643      DISPLAY 'THE KEY-FEEDBACK AREA IS               '
01644      PI-PROP-NO  PI-ASS-KEY  PI-MSTR-KEY PI-DET-KEY
01645      PERFORM 830-ISSUE-ROLL.
01646      SKIP3
01647  820-DISPLAY-IO-PCB.
01648      SKIP1
01649      MOVE 'N' TO ERR-SW
01650      MOVE 16  TO RETURN-CODE
01651      SKIP1
01652      DISPLAY 'THE LOGICAL TERMINAL NAME IS           '
01653        IP-TERM-NAME
01654      DISPLAY 'THE STATUS CODE IS                     '
01655        IP-STATUS-CODE
01656      DISPLAY 'THE CURRENT DATE AND TIME ARE          '
01657        IP-CURR-DATE-TIME.
01658      DISPLAY 'THE INPUT MESSAGE SEQUENCE NUMBER IS   '
01659        IP-MSG-SEQ-NO.
01660      DISPLAY 'THE MSG OUTPUT DESCRIPTOR NAME IS      '
01661        IP-MSG-OUT-DES-NAME.
01662      DISPLAY 'THE USER ID IS                         '
01663        IP-USER-ID.
01664      PERFORM 830-ISSUE-ROLL.
01665      SKIP3
01666  830-ISSUE-ROLL.
01667      CALL 'CBLTDLI' USING ROLL.
01668      SKIP3
01669  900-CHECK-MIS-CODES-DEN.
01670      SKIP1
01671      IF (C150-U-MISRTNCD (01) NOT EQUAL  49 OR
01672         C150-U-MISRTNCD (01) NOT EQUAL  00)
01673         MOVE     'RC'             TO   DT-TYPE-CODE
01674         MOVE C150-U-MISRTNCD (01) TO   DT-ENTRY-CODE
01675         PERFORM 950-READ-MSG-TABLE
01676         IF RECORD-FOUND
01677            MOVE     DT-TC2       TO  DL-RT-CD2
01678         ELSE
01679            ADD +1 TO SUB
01680            MOVE C150-U-MISRTNCD (01) TO DE-RT-CD2
01681            MOVE UPDT-ERR-MSG22 TO HOLD-ERRORS (SUB)
01682         END-IF
01683      END-IF.
01684      SKIP3
01685  910-CHECK-MAN-CODES-DEN.
01686      IF C150-U-MANRTNCD (01) NOT EQUAL  49 OR
01687         C150-U-MANRTNCD (01) NOT EQUAL  00
01688         MOVE     'RC'           TO   DT-TYPE-CODE
01689         MOVE C150-U-MANRTNCD (01) TO   DT-ENTRY-CODE
01690         PERFORM 950-READ-MSG-TABLE
01691         IF RECORD-FOUND
01692            MOVE     DT-TC2       TO  DL-RT-CD2
01693         ELSE
01694            ADD +1 TO SUB
01695            MOVE C150-U-MANRTNCD (01) TO DE-RT-CD2
01696            MOVE UPDT-ERR-MSG22 TO HOLD-ERRORS (SUB)
01697         END-IF
01698      END-IF.
01699       SKIP3
01700  950-READ-MSG-TABLE.
01701      READ MSGTBL-FILE
01702      IF NORMAL-STATUS
01703         CONTINUE
01704      ELSE
01705         IF RECORD-NOTFND
01706            NEXT SENTENCE
01707         ELSE
01708            DISPLAY 'INVALID READ OF MSG TABLE  FILE'
01709            DISPLAY '      FILE  STATUS     ' FILE-STATUS
01710            DISPLAY '      FILE  RETURN     ' VSAM-RETURN
01711            DISPLAY '      FILE  FUNCTION   ' VSAM-FUNCTION
01712            DISPLAY '      FILE  FEEDBACK   ' VSAM-FEEDBACK
01713            MOVE 16 TO RETURN-CODE
01714         END-IF
01715      END-IF.
