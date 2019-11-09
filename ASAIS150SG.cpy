00001 *----------------------------------------------------------------*
00002 *               *  SENIOR FREEZE - DETAIL SEGMENT  *
00003      05  C150-SENFRZDETAIL.
00004 *                                              1-850 SENIOR
00005 *                                                     FREEZE
00006 *                                                     DETAIL
00007 *                                                     SEGMENT
00008          10  C150-BIRTHDTE  PIC X(8).
00009 *                                              1-8   KEY -
00010 *                                                     BIRTH DATE
00011 *                                                     (MMDDCCYY)
00012          10  C150-APPLSTNAM PIC X(20).
00013 *                                              9-28  APPLICANT
00014 *                                                     LAST NAME
00015          10  C150-APPFSTNAM PIC X(15).
00016 *                                             29-43  APPLICANT
00017 *                                                     FIRST NAME
00018          10  C150-APPMI     PIC X.
00019 *                                             44-44  APPLICANT
00020 *                                                     MIDDLE INIT.
00021          10  C150-APPTITLE  PIC XX.
00022 *                                             45-46  APPLICANT
00023 *                                                     TITLE
00024          10  C150-APPOLDNAM PIC X(22).
00025 *                                             47-68  APPLICANT
00026 *                                                     OLD NAME
00027          10  C150-APPMAIL.
00028 *                                             69-113 APPLICANT
00029 *                                                     MAILING
00030 *                                                     ADDRESS
00031              15  C150-APPADDR PIC X(22).
00032 *                                             69-90  ADDRESS
00033              15  C150-APPCITY PIC X(12).
00034 *                                             91-102 CITY
00035              15  C150-APPSTATE PIC XX.
00036 *                                            103-104 STATE
00037              15  C150-APPZIP PIC 9(9).
00038 *                                            105-113 ZIP CODE
00039          10  C150-NAMMAINTIND PIC 9.
00040 *                                            114-114 NAME
00041 *                                                     MAINTENANCE
00042 *                                                     INDICATOR
00043 *                                                     FOR ABOVE
00044 *                                                     5 FIELDS
00045          10  C150-SSN       PIC 9(11).
00046 *                                            115-125 SOCIAL
00047 *                                                     SECURITY
00048 *                                                     NUMBER
00049          10  C150-AGE       PIC 999.
00050 *                                            126-128 AGE
00051          10  C150-PHONE     PIC 9(10).
00052 *                                            129-138 TELEPHONE
00053 *                                                     NUMBER
00054          10  C150-MARSTAT   PIC X.
00055 *                                            139-139 MARITAL
00056 *                                                     STATUS
00057          10  C150-SPSREDIND PIC X.
00058 *                                            140-140 SPOUSE
00059 *                                                     RESIDENTIAL
00060 *                                                     INDICATOR
00061          10  C150-SPSLSTNAM PIC X(20).
00062 *                                            141-160 SPOUSE
00063 *                                                     LAST NAME
00064          10  C150-SPSFSTNAM PIC X(15).
00065 *                                            161-175 SPOUSE
00066 *                                                     FIRST NAME
00067          10  C150-SPSMI     PIC X.
00068 *                                            176-176 SPOUSE
00069 *                                                     MIDDLE INIT.
00070          10  C150-SPSTITLE  PIC XX.
00071 *                                            177-178 SPOUSE
00072 *                                                     TITLE
00073          10  C150-SPSMAIL.
00074 *                                            179-250 SPOUSE
00075 *                                                     MAILING
00076 *                                                     ADDRESS
00077              15  C150-SPSHSENO PIC X(5).
00078 *                                            179-183 HOUSE NO.
00079              15  C150-SPSDIR  PIC XX.
00080 *                                            184-185 DIRECTION
00081              15  C150-SPSSTREET PIC X(22).
00082 *                                            186-207 STREET
00083              15  C150-SPSSUFF  PIC X(4).
00084 *                                            208-211 SUFFIX
00085              15  C150-SPSCITY  PIC X(28).
00086 *                                            212-239 CITY
00087              15  C150-SPSSTATE PIC XX.
00088 *                                            240-241 STATE
00089              15  C150-SPSZIP PIC 9(9).
00090 *                                            242-250 ZIP CODE
00091          10  FILLER         PIC X(3).
00092 *                                            251-253 FILLER
00093          10  C150-SFSTAT    PIC X.
00094 *                                            254-254 STATUS
00095          10  C150-DENIALDTE PIC 9(8).
00096 *                                            255-262 DENIAL
00097 *                                                     DATE
00098 *                                                     (CCYYMMDD)
00099          10  C150-FSTAPPDTE PIC 9(8).
00100 *                                            263-270 FIRST APP
00101 *                                                     RECEIVED
00102 *                                                     DATE
00103 *                                                     (CCYYMMDD)
00104          10  C150-LSTAPPDTE PIC 9(8).
00105 *                                            271-278 LAST APP
00106 *                                                     RECEIVED
00107 *                                                     DATE
00108 *                                                     (CCYYMMDD)
00109          10  C150-QUALDTE   PIC 9(8).
00110 *                                            279-286 QUALIFIED
00111 *                                                     DATE
00112 *                                                     (CCYYMMDD)
00113          10  C150-RTNDDTE   PIC 9(8).
00114 *                                            287-294 RETURNED
00115 *                                                     DATE
00116 *                                                     (CCYYMMDD)
00117          10  C150-MANRTNCODES.
00118 *                                            295-314 MANUAL
00119 *                                                     RETURN CODES
00120              15  C150-MANRTNCD PIC 99 OCCURS 10 TIMES.
00121 *
00122          10  C150-RMRKS     PIC X(40).
00123 *                                            315-354 REMARKS
00124          10  C150-MISRTNCODES.
00125 *                                            355-374 MIS
00126 *                                                     RETURN CODES
00127              15  C150-MISRTNCD PIC 99 OCCURS 10 TIMES.
00128 *
00129          10  C150-COOPSENSHRS PIC 9(6).
00130 *                                            375-380 COOP
00131 *                                                     SENIR SHARES
00132          10  C150-PCTSENSHRS  PIC V9(6).
00133 *                                            381-386 PERCENT
00134 *                                                     SENIR SHARES
00135          10  C150-LIFECARE    PIC X.
00136 *                                            387-387 LIFECARE
00137 *                                                     FACILITY
00138          10  C150-BASE-YR     PIC 9(4).
00139 *                                            388-391 BASE YEAR
00140          10  C150-BSYR-IND    PIC X.
00141 *                                            392-392 BASE YEAR
00142 *                                                     INDICATOR
00143          10  C150-BSYR-EQVAL  PIC 9(9).
00144 *                                            393-401 BASE YEAR
00145 *                                                     ELIGIBLE
00146 *                                                     EQUALIZED
00147 *                                                     VALUE
00148          10  FILLER           PIC X(4).
00149 *                                            402-405 FILLER
00150          10  C150-NOINCOME    PIC X.
00151 *                                            406-406 NO INCOME
00152 *                                                     INDICATOR
00153          10  C150-SSINC       PIC 9(7)V99.
00154 *                                            407-415 SOCIAL
00155 *                                                     SECURITY
00156 *                                                     INCOME
00157          10  C150-RRBEN       PIC 9(7)V99.
00158 *                                            416-424 RAILROAD
00159 *                                                     BENEFITS
00160          10  C150-CSBEN       PIC 9(7)V99.
00161 *                                            425-433 CIVIL SERVICE
00162 *                                                     BENEFITS
00163          10  C150-OTHBEN      PIC 9(7)V99.
00164 *                                            434-442 OTHER
00165 *                                                     BENEFITS
00166          10  C150-VETBEN      PIC 9(7)V99.
00167 *                                            443-451 VETERANS
00168 *                                                     BENEFITS
00169          10  C150-PUBAID      PIC 9(7)V99.
00170 *                                            452-460 PUBLIC AID
00171          10  C150-WAGES       PIC 9(7)V99.
00172 *                                            461-469 WAGES
00173          10  C150-INT         PIC 9(7)V99.
00174 *                                            470-478 INTEREST
00175          10  C150-NETRENT     PIC S9(7)V99.
00176 *                                            479-487 NET RENTAL
00177 *                                                     INCOME
00178          10  C150-NETCAPGAIN  PIC S9(7)V99.
00179 *                                            488-496 NET CAPITAL
00180 *                                                     GAINS
00181          10  C150-OTHINC      PIC S9(7)V99.
00182 *                                            497-505 OTHER
00183 *                                                     INCOME
00184          10  C150-TOTINC      PIC S9(7)V99.
00185 *                                            506-514 TOTAL
00186 *                                                     INCOME
00187          10  C150-SIGNED    PIC X.
00188 *                                            515-515 SIGNED
00189          10  C150-NOTARIZED PIC X.
00190 *                                            516-516 NOTARIZED
00191          10  FILLER         PIC X(15).
00192 *                                            517-531 FILLER
00193          10  C150-BATCH     PIC 9(5).
00194 *                                            532-536 BATCH
00195          10  C150-ORIGDTE   PIC 9(8).
00196 *                                            537-544 ORIGINATION
00197 *                                                     DATE
00198 *                                                     (CCYYMMDD)
00199          10  C150-KEYTIME   PIC 9(7).
00200 *                                            545-551 TIME KEYED
00201 *                                                     (0HHMMSS)
00202          10  C150-ORIGEMPNO PIC X(8).
00203 *                                            552-559 ORIGINATION
00204 *                                                     ENTRY
00205 *                                                     EMPLOYEE NO.
00206          10  C150-TERMID    PIC X(4).
00207 *                                            560-563 TERMINAL I.D.
00208          10  C150-LSTUPID   PIC X(8).
00209 *                                            564-571 LAST
00210 *                                                     UPDATE I.D.
00211          10  C150-LSTUPDTE  PIC 9(8).
00212 *                                            572-579 LAST UPDATE
00213 *                                                     DATE
00214 *                                                     (CCYYMMDD)
00215          10  C150-LSTUPTIM  PIC 9(7).
00216 *                                            580-586 LAST UPDATE
00217 *                                                     TIME
00218 *                                                     (0HHMMSS)
00219          10  FILLER         PIC X(15).
00220 *                                            587-601 FILLER
00221          10  C150-HSSTAT    PIC X.
00222 *                                            602-602 HOMESTEAD
00223 *                                                     STATUS
00224          10  C150-SHARES    PIC 9(6).
00225 *                                            603-608 SHARES
00226          10  C150-PCTSHARES PIC 999V999.
00227 *                                            609-614 PERCENT
00228 *                                                     OF SHARES
00229          10  C150-HSBATCH   PIC 9(5).
00230 *                                            615-619 HOMESTEAD
00231 *                                                     BATCH
00232          10  C150-HSORIGEMPNO PIC X(8).
00233 *                                            620-627 HOMESTEAD
00234 *                                                     ORIGINATION
00235 *                                                     ENTRY
00236 *                                                     EMPLOYEE NO.
00237          10  C150-HSORIGDTE   PIC 9(8).
00238 *                                            628-635 HOMESTEAD
00239 *                                                     ORIGINATION
00240 *                                                     DATE
00241 *                                                     (CCYYMMDD)
00242          10  C150-HSKEYTIME   PIC 9(7).
00243 *                                            636-642 HOMESTEAD
00244 *                                                     TIME KEYED
00245 *                                                     (0HHMMSS)
00246          10  C150-HSTERMID  PIC X(4).
00247 *                                            643-646 HOMRSTEAD
00248 *                                                     TERMINAL I.D
00249          10  C150-HSLSTUPD  PIC X(8).
00250 *                                            647-654 HOMESTEAD
00251 *                                                    LAST
00252 *                                                     UPDATE I.D.
00253          10  C150-HSLSTUPDTE PIC 9(8).
00254 *                                            655-662 HOMESTEAD
00255 *                                                     LAST UPDATE
00256 *                                                     DATE
00257 *                                                     (CCYYMMDD)
00258          10  C150-HSLSTUPTIM  PIC 9(7).
00259 *                                            663-669 HOMESTEAD
00260 *                                                     LAST UPDATE
00261 *                                                     TIME
00262 *                                                     (0HHMMSS)
00263          10  C150-HSYRAPPLD   PIC 9(4).
00264 *                                            670-673 HOMESTEAD
00265 *                                                    YEAR APPLIED
00266          10  FILLER         PIC X(26).
00267 *                                            674-699 FILLER
00268          10  C150-HOSTAT    PIC X.
00269 *                                            700-700 HOMEOWNER
00270 *                                                     STATUS
00271          10  C150-HOBASYR   PIC 9(4).
00272 *                                            701-704 HOMEOWNER
00273 *                                                     BASE YEAR
00274          10  C150-HOBYEQFAC PIC 9V9(4).
00275 *                                            705-709 HOMEOWNER
00276 *                                                     BASE YEAR
00277 *                                                     EQUALIZATION
00278 *                                                     FACTOR
00279          10  C150-HOBYAV    PIC 9(9).
00280 *                                            710-718 HOMEOWNER
00281 *                                                     BASE YEAR
00282 *                                                     ASSESSED
00283 *                                                     VALUATION
00284          10  C150-HOBYEV    PIC 9(9).
00285 *                                            719-727 HOMEOWNER
00286 *                                                     BASE YEAR
00287 *                                                     EQUALIZED
00288 *                                                     VALUATION
00289          10  C150-HOTERMID  PIC X(4).
00290 *                                            728-731 HOMEOWNER
00291 *                                                     TERMINAL I.D
00292          10  C150-ELGIND    PIC 9.
00293 *                                            732-732 HOMEOWNER
00294 *                                                    ELIG. IND.
00295          10  FILLER         PIC XXX.
00296 *                                            733-735 FILLER
00297          10  C150-HOLSTUPD  PIC X(8).
00298 *                                            736-743 HOMEOWNER
00299 *                                                     LAST
00300 *                                                     UPDATE I.D.
00301          10  C150-HOLSTUPDTE PIC 9(8).
00302 *                                            744-751 HOMEOWNER
00303 *                                                     LAST UPDATE
00304 *                                                     DATE
00305 *                                                     (CCYYMMDD)
00306          10  C150-HOLSTUPTIM  PIC 9(7).
00307 *                                            752-758 HOMEOWNER
00308 *                                                     LAST UPDATE
00309 *                                                     TIME
00310 *                                                     (0HHMMSS)
00311          10  C150-MAINTIND    PIC 9.
00312 *                                            759-759 MAINTENANCE
00313 *                                                     INDICATOR
00314          10  C150-MTTERMID  PIC X(4).
00315 *                                            760-763 MAINTENANCE
00316 *                                                     TERMINAL I.D
00317          10  C150-MTUPID    PIC X(8).
00318 *                                            764-771 MAINTENANCE
00319 *                                                     UPDATE I.D.
00320          10  C150-MTUPDTE   PIC 9(8).
00321 *                                            772-779 MAINTENANCE
00322 *                                                     UPDATE
00323 *                                                     DATE
00324 *                                                     (CCYYMMDD)
00325          10  C150-MTUPTIM   PIC 9(7).
00326 *                                            780-786 MAINTENANCE
00327 *                                                     UPDATE
00328 *                                                     TIME
00329 *                                                     (0HHMMSS)
00330          10  C150-SFPCT     PIC 9(2).
00331          10  C150-SFPCT-R REDEFINES C150-SFPCT   PIC 9V9.
00332 *                                            787-788 SF PERCENT
00333          10  FILLER         PIC X(62).
00334 *                                            789-850 FILLER
00335 *----------------------------------------------------------------*
