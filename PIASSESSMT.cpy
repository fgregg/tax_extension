00001 *----------------------------------------------------------------*
00002 *                   *  A S S E S S M E N T   D A T A  *
00003      05  PY-ASMTDATA.
00004 *                                              1-160 ASSMT-DATA
00005          10  PY-KEY.
00006 *                                              1-5   SEGMENT KEY
00007              15  PY-YEAR    PIC 99.
00008 *                                              1-2     YEAR
00009              15  PY-TXYR    PIC 99.
00010 *                                              3-4     TAX YEAR
00011              15  PY-TXTYPE  PIC 9.
00012                  88  PY-CURRENT-TXTYP     VALUE 0.
00013                  88  PY-BACKTAX-TXTYP     VALUE 1.
00014                  88  PY-ROLLBACK-TXTYP    VALUE 2.
00015                  88  PY-AIR-POLL-TXTYP    VALUE 3.
00016                  88  PY-CIRCULATOR-TXTYP  VALUE 5.
00017 *                                              5       TAX TYPE
00018 *                                                                *
00019          10  PY-BLTYPE      PIC 9.
00020              88  PY-BILTYP-1        VALUE 1.
00021              88  PY-BILTYP-2        VALUE 2.
00022              88  PY-BILTYP-3        VALUE 3.
00023              88  PY-BILTYP-4        VALUE 4.
00024              88  PY-BILTYP-5        VALUE 5.
00025              88  PY-BILTYP-6        VALUE 6.
00026              88  PY-BILTYP-EX-RR    VALUE 4 THRU 6.
00027 *                                              6     BILLABLE TYPE
00028          10  PY-TXSTAT      PIC 9.
00029              88  PY-TAXABLE-PARCEL  VALUE 0.
00030              88  PY-EXEMPT          VALUE 1.
00031              88  PY-RAILROAD        VALUE 2.
00032              88  PY-HOMESTEAD-EXMPT VALUE 3.
00033              88  PY-VETERANS-EXMPT  VALUE 4.
00034              88  PY-COOPHMSTD-EXMPT VALUE 5.
00035              88  PY-DIVIDED-PARCEL  VALUE 9.
00036 *                                              7     TAX STATUS
00037          10  PY-TXCD        PIC 9(5)     COMP-3.
00038 *                                              8-10  TAX CODE
00039          10  PY-AS-CLS      PIC 9(7)     COMP-3.
00040              88  PY-AS-EX-CLS       VALUE 0000001.
00041              88  PY-AS-RR-CLS       VALUE 0000002.
00042 *                                             11-14  ASSESSOR
00043 *                                                    CLASS (MAJOR-
00044 *                                                    MINOR-SUFFIX)
00045 *                                                     (Z99-99-99)
00046          10  PY-BA-CLS      PIC 9(7)     COMP-3.
00047              88  PY-BA-EX-CLS       VALUE 0000001.
00048              88  PY-BA-RR-CLS       VALUE 0000002.
00049              88  PY-BA-NON-RES      VALUE 0030000 THRU 0099999.
00050              88  PY-BA-200-CLS      VALUE 0020200 THRU 0021299
00051                                           0023400 THRU 0023499
00052                                           0027800 THRU 0027899
00053                                           0029500 THRU 0029599.
00054 *                                             15-18  BD. OF APPEAL
00055 *                                                    CLASS (MAJOR-
00056 *                                                    MINOR-SUFFIX)
00057 *                                                     (Z99-99-99)
00058          10  PY-AS-STAT     PIC 9.
00059              88  PY-NON-ASSESSED    VALUE 0.
00060              88  PY-ASSESSED        VALUE 1.
00061 *                                             19     ASMT-STATUS
00062          10  PY-PRT-IND     PIC 9.
00063              88  PY-NOT-PRINTED     VALUE 0.
00064              88  PY-PRINTED         VALUE 1.
00065 *                                             20     PRINT
00066 *                                                     INDICATOR
00067          10  PY-NBHD        PIC 9(3)     COMP-3.
00068 *                                             21-22  NEIGHBORHD-CD
00069          10  PY-STRT        PIC 9(5)     COMP-3.
00070 *                                             23-25  STREET CODE
00071          10  PY-HSENO       PIC 9(5)     COMP-3.
00072 *                                             26-28  HOUSE-NO
00073          10  PY-LNDDIM      PIC 9(7)     COMP-3.
00074 *                                             29-32  LND-DIMENSION
00075          10  PY-LNDCD       PIC X.
00076              88  PY-LND-ACRES       VALUE 'A'.
00077              88  PY-LND-BKLOT       VALUE 'B'.
00078              88  PY-LND-IRREG       VALUE 'N'.
00079 *                                             33     LAND CODE
00080          10  PY-LNDSQFT     PIC 9(9)     COMP-3.
00081 *                                             34-38  LAND-SQ-FT
00082          10  PY-IRREG       PIC 9.
00083              88  PY-NONIRREG-LOT    VALUE 0.
00084              88  PY-IRREG-LOT       VALUE 1.
00085 *                                             39     IRREGULAR
00086 *                                                                *
00087 *                               *  40-42  ASMT/BA ACTION FIELDS  *
00088          10  PY-AS1-ACT     PIC 9.
00089              88  PY-AS1-VALID-2-9   VALUE  2 THRU 9.
00090              88  PY-AS1-PERMIT      VALUE 2.
00091              88  PY-AS1-DIVISION    VALUE 3.
00092              88  PY-AS1-REVISION    VALUE 4.
00093 *                                             40     ASMT-ACTION1
00094          10  PY-AS2-ACT     PIC 9.
00095              88  PY-AS2-VALID-2-9   VALUE  2 THRU 9.
00096              88  PY-AS2-PERMIT      VALUE 2.
00097              88  PY-AS2-DIVISION    VALUE 3.
00098              88  PY-AS2-REVISION    VALUE 4.
00099 *                                             41     ASMT-ACTION2
00100          10  PY-BA-ACT      PIC 9.
00101              88  PY-BA-REVISION     VALUE 1.
00102 *                                             42     B.A.-ACTION
00103 *                                                                *
00104          10  PY-PASS        PIC 9.
00105              88  PY-BA-PASS         VALUE 1.
00106              88  PY-2ND-AS-PASS     VALUE 2.
00107              88  PY-1ST-AS-PASS     VALUE 3.
00108 *                                             43     PASS
00109 *                                                                *
00110 *                               *  44-58  ASMT VALUATION FIELDS  *
00111          10  PY-AS-LANDVAL  PIC 9(9)     COMP-3.
00112 *                                             44-48  AS-LAND-VALUE
00113          10  PY-AS-IMPRVAL  PIC 9(9)     COMP-3.
00114 *                                             49-53  AS-IMPROVEMNT
00115          10  PY-AS-TOTLVAL  PIC 9(9)     COMP-3.
00116 *                                             54-58  AS-TOTAL-VALU
00117 *                                                                *
00118 *                               *  59-73  B.A. VALUATION FIELDS  *
00119          10  PY-BA-LANDVAL  PIC 9(9)     COMP-3.
00120 *                                             59-63  BA-LAND-VALUE
00121          10  PY-BA-IMPRVAL  PIC 9(9)     COMP-3.
00122 *                                             64-68  BA-IMPROVEMNT
00123          10  PY-BA-TOTLVAL  PIC 9(9)     COMP-3.
00124 *                                             69-73  BA-TOTAL-VALU
00125 *                                                                *
00126          10  PY-N-F-ASDVAL  PIC 9(9)     COMP-3.
00127 *                                             74-78  NON-FARM
00128 *                                                    ASSESSED-VALU
00129          10  PY-FARM-ASDVAL PIC 9(9)     COMP-3.
00130 *                                             79-83  FARM
00131 *                                                    ASSESSED-VALU
00132          10  PY-MRKTVAL     PIC 9(9)     COMP-3.
00133 *                                             84-88  MARKET-VALUE
00134 *                                                                *
00135 *                               *  89-98  DIVISION FIELDS        *
00136          10  PY-DIVNO       PIC 9(7)     COMP-3.
00137 *                                             89-92  DIVISION NO.
00138          10  PY-DIVCODE     PIC X.
00139              88  PY-DIV-ADD         VALUE 'A'.
00140              88  PY-DIV-CHANGE      VALUE 'C'.
00141              88  PY-DIV-DELETE      VALUE 'D'.
00142 *                                             93     DIV. CODE
00143          10  PY-DIVSTAT     PIC 9.
00144              88  PY-DIV-ASSESSABLE  VALUE 0.
00145              88  PY-DIV-EXEMPT      VALUE 1.
00146              88  PY-DIV-RR          VALUE 2.
00147 *                                             94     DIV. STATUS
00148          10  PY-DIVDTE      PIC 9(7)     COMP-3.
00149 *                                             95-98  DIVISION DATE
00150 *                                                     POSTED
00151          10  PY-TAX-TOTLVAL PIC 9(9)     COMP-3.
00152 *                                             99-103 TAXABLE
00153 *                                                    ASSESSED
00154 *                                                    TOTAL VAL
00155          10  PY-TAX-EQLVAL  PIC 9(9)     COMP-3.
00156 *                                            104-108 TAXABLE
00157 *                                                    EQUALIZED
00158 *                                                    VALUATION
00159          10  PY-HMOWN-EXVAL PIC 9(9)     COMP-3.
00160 *                                            109-113 HOMEOWNER
00161 *                                                    EXEMPTION
00162 *                                                    VALUATION
00163          10  PY-HMSTD-EXVAL PIC 9(9)     COMP-3.
00164 *                                            114-118 HOMESTEAD
00165 *                                                    EXEMPTION
00166 *                                                    VALUATION
00167          10  PY-TAX-RATE    PIC 9(4)V999 COMP-3.
00168 *                                            119-122 TAX RATE
00169          10  PY-TAX-AMOUNT  PIC 9(9)V99  COMP-3.
00170 *                                            123-128 TOTAL TAX
00171 *                                                    AMOUNT
00172          10  PY-EST-TAXPEN  PIC 9(9)V99  COMP-3.
00173 *                                            129-134 ESTIMATE
00174 *                                                    TAX AMOUNT
00175 *                                                    OR PENALTY
00176          10  PY-CITY-CDE    PIC 9(5)     COMP-3.
00177 *                                            135-137 CITY CODE
00178          10  PY-LT-EXVAL REDEFINES PY-CITY-CDE  PIC 9(5) COMP-3.
00179 *                                            135-137 LONG TERM
00180 *                                                    EXEMPTION
00181 *                                                    AMOUNT
00182          10  PY-STRT-CDE    PIC 9(5)     COMP-3.
00183 *                                            138-140 STREET CODE
00184          10  PY-LT-IND      PIC X(01).
00185 *                                            141-141 LONG TERM
00186 *                                                    INDICATOR
00187          10  PY-ADDR        PIC X(09).
00188 *                                            142-150 ADDRESS
00189          10  PY-BATCH-NO    PIC 9(5)     COMP-3.
00190 *                                            151-153 BATCH NUMBER
00191          10  PY-AGCY-RRNO   PIC 9(5)     COMP-3.
00192 *                                            154-156 AGENCY NUMBER
00193 *                                                    OR RAILROAD
00194 *                                                    NUMBER
00195          10  PY-BA-COMP-NO  PIC S9(7)     COMP-3.
00196 *                                            157-160 COMPLAINT
00197 *                                                    NUMBER
00198 *----------------------------------------------------------------*
