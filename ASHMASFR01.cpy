00001 *----------------------------------------------------------------*
00002 *               *  SENIOR FREEZE - SEQUENTIAL FILE *
00003      05  SF-REC.
00004 *                                              1-400 SENIOR
00005 *                                                     FREEZE
00006 *                                                     SEGMENT
00007          10  SF-KEY.
00008 *
00009              15  SF-TOWN        PIC 9(02).
00010 *                                              1-2   TOWN NO.
00011              15  SF-VOL         PIC 9(03).
00012 *                                              3-5   VOLUME
00013              15  SF-PROP        PIC 9(15)  PACKED-DECIMAL.
00014 *                                              6-13  PROPERTY NO.
00015          10  SF-PROC-YR     PIC X(4).
00016 *                                             14-17  PROCESS YEAR
00017          10  SF-TAX-YR      PIC X(4).
00018 *                                             18-21  TAX YEAR
00019          10  SF-TAX-TYPE    PIC X(1).
00020 *                                             22-22  TAX TYPE
00021 *                                                     UNITS
00022          10  SF-RECCODE     PIC X(1).
00023 *                                             23-23  RECCODE
00024          10  SF-BLDGUNITS   PIC 9(5).
00025 *                                             24-28
00026 *
00027          10  SF-BLDGSHARES  PIC 9(6).
00028 *                                             29-34
00029 *
00030          10  SF-PROPCT      PIC 9V9(6).
00031 *                                             35-41
00032          10  SF-KEYPCL      PIC 9(14).
00033 *                                             42-55
00034          10  SF-SPLTCD      PIC 9.
00035 *                                             56-56  SPLIT CODE
00036          10  SF-OCCFAC      PIC 9(4)V9.
00037 *                                             57-61  OCCUPANCY
00038 *                                                     FACTOR
00039          10  SF-288EXPAV    PIC 9(9).
00040 *                                             62-70  288
00041 *                                                     EXPIRATION
00042 *                                                     ASSESSED
00043 *                                                     VALUATION
00044          10  SF-288EXPEV    PIC 9(9).
00045 *                                             71-79  288
00046 *                                                     EXPIRATION
00047 *                                                     EQUALIZED
00048 *                                                     VALUATION
00049          10  SF-288OVRLIMAV PIC 9(9).
00050 *                                             80-88  288
00051 *                                                     OVER LIMIT
00052 *                                                     ASSESSED
00053 *                                                     VALUATION
00054          10  SF-288OVRLIMEV PIC 9(9).
00055 *                                             89-97  288
00056 *                                                     OVER LIMIT
00057 *                                                     EQUALIZED
00058 *                                                     VALUATION
00059          10  SF-BVYNOCALCIND PIC X.
00060 *                                             98-98  BASE VALUE
00061 *                                                     YEAR
00062 *                                                     CANNOT BE
00063 *                                                     CALCULATED
00064 *                                                     BY PROGRAM
00065 *                                                     INDICATOR
00066          10  SF-BVYMANCALCIND PIC X.
00067 *                                             99-99  BASE VALUE
00068 *                                                     YEAR MANUAL
00069 *                                                     CALCULATION
00070 *                                                     INDICATOR
00071          10  SF-BASVALYR     PIC 9(4).
00072 *                                            100-103 BASE VALUE
00073 *                                                     YEAR
00074          10  SF-BVYCLS       PIC 9(6).
00075 *                                            104-109 BASE VALUE
00076 *                                                     YEAR CLASS
00077          10  SF-BVYFULLAV    PIC 9(9).
00078 *                                            110-118 BASE VALUE
00079 *                                                     YEAR FULL
00080 *                                                     ASSESSED
00081 *                                                     VALUATION
00082          10  SF-BVYEV        PIC 9(9).
00083 *                                            119-127 BASE VALUE
00084 *                                                     YEAR
00085 *                                                     EQUALIZED
00086 *                                                     VALUATION
00087          10  SF-BVYELCOMPFULLAV PIC 9(9).
00088 *                                            128-136 BASE VALUE
00089 *                                                     YEAR ELIGIBL
00090 *                                                     COMPUTED
00091 *                                                     FULL
00092 *                                                     ASSESSED
00093 *                                                     VALUATION
00094          10  SF-BVYTOTELCOMPEV PIC 9(9).
00095 *                                            137-145 BASE VALUE
00096 *                                                     YEAR TOTAL
00097 *                                                     ELIGIBLE
00098 *                                                     COMPUTED
00099 *                                                     EQUALIZED
00100 *                                                     ASSESSED
00101          10  SF-CYCLS        PIC 9(6).
00102 *                                            146-151 CURRENT YEAR
00103 *                                                     CLASS
00104          10  SF-CYFARMIND      PIC X.
00105 *                                            152-152 CURRENT YEAR
00106 *                                                     FARM
00107 *                                                     INDICATOR
00108          10  SF-CYFULLAV     PIC 9(9).
00109 *                                            153-161 CURRENT YEAR
00110 *                                                     FULL
00111 *                                                     ASSESSED
00112 *                                                     VALUATION
00113          10  SF-CYELCOMPAV PIC 9(9).
00114 *                                            162-170 CURRENT YEAR
00115 *                                                     ELIGIBLE
00116 *                                                     COMPUTED
00117 *                                                     ASSESSED
00118 *                                                     VALUATION
00119          10  SF-CYELCOMPEV PIC 9(9).
00120 *                                            171-179 CURRENT YEAR
00121 *                                                     ELIGIBLE
00122 *                                                     COMPUTED
00123 *                                                     EQUALIZED
00124 *                                                     VALUATION
00125          10  SF-CYNOTELAV    PIC 9(9).
00126 *                                            180-188 CURRENT YEAR
00127 *                                                     NOT ELIGIBLE
00128 *                                                     ASSESSED
00129 *                                                     VALUATION
00130          10  SF-CYNOTELEV    PIC 9(9).
00131 *                                            189-197 CURRENT YEAR
00132 *                                                     NOT ELIGIBLE
00133 *                                                     EQUALIZED
00134 *                                                     VALUATION
00135          10  SF-CYFNLEVDIFF PIC 9(9).
00136 *                                            198-206 CURRENT YEAR
00137 *                                                     FINAL
00138 *                                                     EQUALIZED
00139 *                                                     VALUATION
00140 *                                                     FOR TAX
00141 *                                                     COMPUTATION
00142          10  SF-HOUNITS       PIC 9(5).
00143 *                                            207-211 NUMBER OF
00144 *                                                     UNITS WITH
00145 *                                                     HOMEOWNER
00146 *                                                     EXEMPTIONS
00147          10  SF-HSUNITS       PIC 9(5).
00148 *                                            212-216 NUMBER OF
00149 *                                                     UNITS WITH
00150 *                                                     HOMESTEAD
00151 *                                                     EXEMPTIONS
00152          10  SF-SFSHARES      PIC 9(6).
00153 *                                            217-222 NUMBER OF
00154 *                                                     SHARES WITH
00155 *                                                     SENIOR FREEZ
00156          10  SF-CYFULLEV     PIC 9(9).
00157 *                                            223-231 CURRENT YEAR
00158 *                                                     FULL
00159 *                                                     EQUALIZED
00160 *                                                     VALUATION
00161          10  SF-CALC-TYP   PIC X.
00162 *                                            232-232 CALCULATION
00163 *                                                     TYPE
00164 *----------------------------------------------------------------*
00165          10  SF-BIRTHDTE    PIC X(8).
00166 *                                            233-240 KEY -
00167 *                                                     BIRTH DATE
00168 *                                                     (MMDDCCYY)
00169          10  SF-SFSTAT      PIC X.
00170 *                                            241-241 STATUS
00171          10  SF-DENIALDTE PIC 9(8).
00172 *                                            242-249 DENIAL
00173 *                                                     DATE
00174 *                                                     (CCYYMMDD)
00175          10  SF-FSTAPPDTE PIC 9(8).
00176 *                                            250-257 FIRST APP
00177 *                                                     RECEIVED
00178 *                                                     DATE
00179 *                                                     (CCYYMMDD)
00180          10  SF-LSTAPPDTE PIC 9(8).
00181 *                                            258-265 LAST APP
00182 *                                                     RECEIVED
00183 *                                                     DATE
00184 *                                                     (CCYYMMDD)
00185          10  SF-QUALDTE     PIC 9(8).
00186 *                                            266-273 QUALIFIED
00187 *                                                     DATE
00188 *                                                     (CCYYMMDD)
00189          10  SF-COOPSENSHRS PIC 9(6).
00190 *                                            274-279 COOP
00191 *                                                     SENIR SHARES
00192          10  SF-PCTSENSHRS    PIC V9(6).
00193 *                                            280-285 PERCENT
00194 *                                                     SENIR SHARES
00195          10  SF-LIFECARE      PIC X.
00196 *                                            286-286 LIFECARE
00197 *                                                     FACILITY
00198          10  SF-BASE-YR       PIC 9(4).
00199 *                                            286-290 BASE YEAR
00200          10  SF-BSYR-IND      PIC X.
00201 *                                            291-291 BASE YEAR
00202 *                                                     INDICATOR
00203          10  SF-BSYR-EQVAL    PIC 9(9).
00204 *                                            292-300 BASE YEAR
00205 *                                                     ELIGIBLE
00206 *                                                     EQUALIZED
00207 *                                                     VALUE
00208          10  SF-OTHBEN        PIC 9(7)V99.
00209 *                                            301-309 OTHER
00210 *                                                     BENEFITS
00211          10  SF-VETBEN        PIC 9(7)V99.
00212 *                                            310-318 VETERANS
00213 *                                                     BENEFITS
00214          10  FILLER         PIC X(82).
00215 *                                            319-400 FILLER
00216 *----------------------------------------------------------------*
